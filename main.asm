.model small
.stack 100h
.data
;messages
welcome_msg db 'welcome to minesweeper',13,10,'$'
end_msg db 'press any key to exit',13,10,'$'
;debug messages
gen_complete_msg db 'grid generated',13,10,'$'
left_button_clicked_msg db 'left mouse button clicked',13,10,'$'
right_button_clicked_msg db 'right mouse button clicked',13,10,'$'

bombs db ?         ;bombs number
start_x dw 50
start_y dw 70
cell_width equ 36
cell_height equ 36
rows db 10
cols db 10

grid db 480 dup(0) ;max grid size 16 * 30
;grid array conventions
;the most significant half byte contains the view
;the second half byte contains the number
;the view can be either 0==>closed , 1==>flaged , 2==>opened
;the data can be either -1==>bomb or number with range(0 to 8)
;cell view constants
CELL_CLOSED equ 0
CELL_FLAGED equ 1
CELL_OPENED equ 2

;the random variable state
rand db 0

numSmall equ 10 ;number of mines for the small grid
numMedium equ 40 ;number of mines for the medium grid
numLarge equ 99 ;number of mines for the large grid

numMines db 10 ;total number of mines in the current active grid
;;numMinesLeft dw ? ;number of mines left in the game
rand_mod db 0

dxAr db 0,0FFh,0FFh,0FFh,0,1,1,1
dyAr db 0FFh,0FFh,0,1,1,1,0,0FFh

;7 segment LED Auxillary Array
led_array db 44h,3dh,6dh,4eh,6bh,7bh,45h,7fh

;colors constants
CLOSED_CELL_BACKGROUND_COLOR equ 8
OPENED_CELL_BACKGROUND_COLOR equ 0
.CODE

delay_1sec MACRO
	LOCAL @@delay
	push ax
	push bx
	push dx
	push di
	push cx
@@delay:
	mov di,dx
	mov ah,0
	int 1ah
	cmp dx,di
	je @@delay
	inc bx
	;19 maps to 1 second approximately
	cmp bx,5
	jne @@delay
	pop cx
	pop di
	pop dx
	pop bx
	pop ax
ENDM

gen_rand_mod MACRO limit
	gen_random
	push ax
	push bx
	push cx
	mov ax,0
	mov al,rand
	mov bl,limit
	mul bl
	mov cl,5
	shr ax,cl
	mov rand_mod,al
	pop cx
	pop bx
	pop ax
ENDM

gen_random MACRO
	;rand = (5*rand+3) % 32
	push ax
	push bx
	xor ax,ax
	mov al,rand
	mov bl,5
	mul bl
	add al,3
	mov bl,32
	div bl
	mov rand,ah
	pop bx
	pop ax
ENDM

print MACRO msg_address
	push ax
	push dx
	mov dx,OFFSET msg_address
	mov ah,9
	int 21h
	pop dx
	pop ax
ENDM print

;private macro used in other macros to expand given row and col to required index in grid
;uses ax as temp register and expand result is stored in bx
_expand MACRO row,col
	;bx <- index = ((row+1)*(cols+2)) + (col+1)
	push ax
	push dx
	
	mov ax,0
	mov al,row
	inc al
	mov bx,0
	mov bl,cols
	add bx,2
	mul bx
	mov bx,0
	mov bl,col
	inc bl
	add ax,bx 
	mov bx,ax
	
	pop dx
	pop ax
ENDM

; two 1-byte args
; result is returned in bx
_expand_proc_caller MACRO row,col
	;bx <- index = ((row+1)*(cols+2)) + (col+1)
	mov bx,0
	mov bl,col
	push bx
	mov bl,row
	push bx
	call _expand_proc
	add sp,4
ENDM

_expand_proc PROC
	;bx <- index = ((row+1)*(cols+2)) + (col+1)
	push bp
	mov bp,sp

	push ax
	push dx
	
	mov ax,[bp+4] ;first parameter
	inc al
	mov bx,0
	mov bl,cols
	add bx,2
	mul bx
	mov bx,[bp+6] ;second parameter
	inc bl
	add ax,bx 
	mov bx,ax
	
	pop dx
	pop ax
	pop bp
	RET
ENDP

;gets the value of the cell view and puts it in the specified memory location 
;note : register can be used as output (except bx,cx) as they are used inside the macro
;input can be passed in registers except cl , bx
get_cell_view MACRO row,col,value_out
	push bx
	push cx
	;_expand row,col
	_expand_proc_caller row,col
	mov bl,[bx + OFFSET grid]
	and bl,0F0h
	mov cl,4
	shr bl,cl
	mov value_out,bl
	pop cx
	pop bx
ENDM

set_cell_opened MACRO row,col
	push ax
	push bx
	_expand_proc_caller row,col
	mov al,[bx + OFFSET grid]
	;clear most significant half byte then set it to 2
	and al,0Fh
	or al,20h
	mov [bx + OFFSET grid],al
	pop bx
	pop ax
ENDM

set_cell_closed MACRO row,col
	push ax
	push bx
	;_expand row,col
	_expand_proc_caller row,col
	mov al,[bx + OFFSET grid]
	and al,0Fh
	mov [bx + OFFSET grid],al
	pop bx
	pop ax
ENDM

set_cell_flaged MACRO row,col
	push ax
	push bx
	;_expand row,col
	_expand_proc_caller row,col
	mov al,[bx + OFFSET grid]
	and al,0Fh
	or al,10h
	mov [bx + OFFSET grid],al
	pop bx
	pop ax
ENDM

;converts screen coordinates at cx and dx to rows and cols
;cl will have col number and dl will have row number
convert_coordinates MACRO
	push ax  	;save ax value
	push bx  	;save bx value
	;get col number	
	sub cx,start_x
	mov ax,cx
	mov bl,cell_width
	div bl
	mov cx,ax
	;get row number	
	sub dx,start_y
	mov ax,dx
	mov bl,cell_height
	div bl
	mov dx,ax
	;restore ax,bx registers
	pop bx
	pop ax
ENDM

;converts given row and col to thier real locations
;the result is stored in cx and dx
;cx will have cell x position and dx will have the cell y position
expand_coordinates MACRO row,col
	push ax  	;save ax value
	push bx  	;save bx value
	;get xpos
	mov al,col
	mov bl,cell_width
	mul bl
	add ax,start_x
	mov cx,ax
	;get ypos
	mov al,row
	mov bl,cell_height
	mul bl
	add ax,start_x
	mov dx,ax
	;restore ax,bx registers
	pop bx
	pop ax
ENDM

;proc used to get screen coordinates from row and col
;takes two inputs row,col (1 word each)
;results are stored in cx,dx
get_screen_coordinates PROC
	push bp
	mov bp,sp
	push ax
	push bx
	;get xpos
	mov ax,[bp+6]
	mov bl,cell_width
	mul bl
	add ax,start_x
	mov cx,ax
	;get ypos
	mov ax,[bp+4]
	mov bl,cell_height
	mul bl
	add ax,start_y
	mov dx,ax
	;restore reg
	pop bx
	pop ax
	pop bp
	RET
ENDP

get_screen_coordinates_caller MACRO row,col
	push col
	push row
	call get_screen_coordinates
	add sp,4
ENDM

;parameters startX,startY,length,Color,Vertical?
draw_line PROC
	push bp
	mov bp,sp
	;building local variables
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	;function logic
	mov al,[bp+10]    ;fourth parameter  (color)
	mov si,[bp+8]     ;third parameter  (length)
	mov dx,[bp+6]     ;second parameter (startY)
	mov cx,[bp+4]     ;first parameter (startX)
	mov ah,0ch
	mov di,[bp+12]    ;fifth parameter (0 = horizontal otherwise vertical)
	mov bh,0
	cmp di,0 
	jnz vertical
	
	horizontal:
		int 10h
		inc cx 
		dec si
		jnz horizontal
		jmp done

	vertical:
		int 10h
		inc dx 
		dec si
		jnz vertical
	done:
		;clear local storage
		;nothing to clear
		;restore registers
		pop si
		pop di
		pop dx
		pop cx
		pop bx
		pop ax
		pop bp
		RET
ENDP

;macro used to ease the invoke the draw line method
draw_line_caller MACRO startX,startY,len,color,vertical
	push vertical
	push color
	push len
	push startY
	push startX
	call draw_line
	add sp,10
ENDM draw_line_caller

;parameters startX,startY,lenX,lenY,color
;=========  [bp+4],[bp+6], 8 , 10  , 12
draw_filled_box PROC
	push bp
	mov bp,sp
	push ax
	push cx
	mov ax,[bp+6]
	mov cx,[bp+10]

	lines:
		;draw_line_caller MACRO startX,startY,len,color,vertical
		draw_line_caller [bp+4],ax,[bp+8],[bp+12],0
		inc ax
		loop lines

	pop cx
	pop ax
	pop bp
	RET
ENDP

draw_filled_box_caller MACRO startX,startY,lenX,lenY,color
	push color
	push lenY
	push lenX
	push startY
	push startX
	call draw_filled_box
	add sp,10
ENDM draw_filled_box_caller

;colors the given cell
;parameters row,col,color
color_cell PROC
	push bp
	mov bp,sp

	push cx
	push dx

	get_screen_coordinates_caller [bp+4],[bp+6]
	;to make boarder lines appear (decrease area of inner boxes)
	inc cx
	inc dx

	draw_filled_box_caller cx,dx,cell_width-1,cell_height-1,[bp+8]
	pop dx
	pop cx

	pop bp
	RET
ENDP

color_cell_caller MACRO row,col,color
	push color
	push col
	push row
	call color_cell
	add sp,6
ENDM

draw_grid MACRO rows,cols,startX,startY,cell_width,cell_height
	;save registers
	push ax
	push bx
	push cx
	push dx
	;logic
	xor cx,cx
	mov cl,rows
	inc cx
	xor ax,ax
	mov al,cols
	mov bx,startY
	mov dl,cell_width
	mul dl
	;ax contains the len of the line
	rows_loop:
		draw_line_caller startX,bx,ax,58,0
		add bx,cell_height
		loop rows_loop

	xor cx,cx
	mov cl,cols
	inc cx
	xor al,al
	mov al,rows
	mov bx,startX
	mov dl,cell_height
	mul dl
		;ax contains the len of the line
	cols_loop:
		draw_line_caller bx,startY,ax,58,1
		add bx,cell_width
		loop cols_loop

		;restore registers
		pop dx
		pop cx
		pop bx
		pop ax
ENDM draw_grid

init_grid MACRO
	;save registers
	push ax
	push cx
	push dx
	
	; init all cells to 0
	mov cl,rows
	dec cl
	mov dl,0
	
	loop_on_rows:
		mov ch,cols
		dec ch
		loop_on_cols:
			_expand_proc_caller cl,ch
			mov [bx + OFFSET grid],dl
			;draw empty cells background
			;----------------
			push cx
			push dx
			mov dl,ch
			xor dh,dh
			xor ch,ch
			color_cell_caller cx,dx,CLOSED_CELL_BACKGROUND_COLOR
			pop dx
			pop cx
			;----------------
			dec ch
			cmp ch,0
		jge loop_on_cols
		dec cl
		cmp cl,0
	jge loop_on_rows
	
	;initialize frame
	mov dl,20h
	mov ch,rows
	mov cl,cols
	
	init_horizontal_frame:
		_expand_proc_caller 0FFh,cl
		mov [bx + OFFSET grid],dl
		
		_expand_proc_caller ch,cl
		mov [bx + OFFSET grid],dl
		
		dec cl
		cmp cl,0FFh
	jge init_horizontal_frame
	
	mov ch,cols
	mov cl,rows
	
	init_vertical_frame:
		_expand_proc_caller cl,0FFh
		mov [bx + OFFSET grid],dl
		
		_expand_proc_caller cl,ch
		mov [bx + OFFSET grid],dl
		
		dec cl
		cmp cl,0
	jge init_vertical_frame
	
	gen_bombs

	;restore registers
	pop dx
	pop cx
	pop ax
ENDM init_grid

gen_bombs MACRO
	;save registers
	push ax
	push cx
	push dx
	push si
	
	mov cx,0
	mov cl,numMines
	gen_bomb_loop:
		gen_rand_mod rows
		mov al,rand_mod ;save row number in al
		gen_rand_mod cols
		mov ah,rand_mod ;save col number in ah
		
		_expand_proc_caller al,ah
		
		; test that this cell doesn't already contain a bomb (duplicate randoms)
		mov ch,[bx + OFFSET grid]
		cmp ch,0Fh
		jne put_bomb
		jmp gen_bomb_loop
		
	put_bomb:		
		; put bomb into cell
		mov ch,0Fh
		mov [bx + OFFSET grid],ch
		
		;increment surrounding cells
		mov si,7
		loop_on_dAr:
			lea bx,dxAr
			mov dl,[bx+si]
			lea bx,dyAr
			mov dh,[bx+si]
			add dl,al
			add dh,ah
	
			_expand_proc_caller dl,dh
		
			mov ch,[bx + OFFSET grid]
			cmp ch,0Fh
			jae no_increment ;cell either contains a bomb OR is border cell (on the frame)
			inc ch
			mov [bx + OFFSET grid],ch
			
		no_increment:
			
			dec si
			cmp si,0
		jge loop_on_dAr
		
		dec cl
		cmp cl,0
		jg cont_loop
		jmp exit_loop
		
	cont_loop:
	jmp gen_bomb_loop
	
exit_loop:
	;restor registers
	pop si
	pop dx
	pop cx
	pop ax
ENDM gen_bombs

;led value takes a number and draw corresponding lines from the 7seg map
draw_led_value PROC
	push bp
	mov bp,sp

	push ax
	push bx
	push cx
	push dx

	mov cx,[bp + 4] ;first parameter ==> xpos
	mov dx,[bp + 6] ;second parameter ==> ypos
	mov ax,[bp + 8] ;third parameter ==> num (only al will be used , ah will be ignored)

	test al,1
	jz seg_2

	add cx,cell_width/3	;division is done in Assemble time
	add dx,cell_height/6
	mov bx,cell_width/3
	draw_line_caller cx,dx,bx,13,0
	
seg_2:
	shr al,1
	test al,1
	jz seg_3
	mov cx,[bp + 4] 
	mov dx,[bp + 6]
	add cx,cell_width/3
	add dx,cell_height/6
	mov bx,cell_height/6*2
	draw_line_caller cx,dx,bx,13,1

seg_3:
	shr al,1
	test al,1
	jz seg_4
	mov cx,[bp + 4] 
	mov dx,[bp + 6]
	add cx,cell_width/3*2
	add dx,cell_height/6
	mov bx,cell_height/6*2
	draw_line_caller cx,dx,bx,13,1
	
seg_4:
	shr al,1
	test al,1
	jz seg_5
	mov cx,[bp + 4] 
	mov dx,[bp + 6]
	add cx,cell_width/3
	add dx,cell_height/6*3
	mov bx,cell_width/3
	draw_line_caller cx,dx,bx,13,0

seg_5:
	shr al,1
	test al,1
	jz seg_6
	mov cx,[bp + 4] 
	mov dx,[bp + 6]
	add cx,cell_width/3
	add dx,cell_height/6*3
	mov bx,cell_height/3
	draw_line_caller cx,dx,bx,13,1

seg_6:
	shr al,1
	test al,1
	jz seg_7
	mov cx,[bp + 4] 
	mov dx,[bp + 6]
	add cx,cell_width/3
	add dx,cell_height/6*5
	mov bx,cell_width/3
	draw_line_caller cx,dx,bx,13,0

seg_7:
	shr al,1
	test al,1
	jz led_finish
	mov cx,[bp + 4] 
	mov dx,[bp + 6]
	add cx,cell_width/3*2
	add dx,cell_height/6*3
	mov bx,cell_height/3
	draw_line_caller cx,dx,bx,13,1

led_finish:
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	RET
ENDP

;prints number specified by value in the location specified by row and col
print_cell_value MACRO row,col,value
	LOCAL @@skip
	push bx
	push dx
	push cx
	push si
	cmp value,0
	je @@skip
	;expand_coordinates row,col
	get_screen_coordinates_caller row,col
	;push parameters
	mov bx,OFFSET led_array
	mov si,value
	push [bx+si-1]
	push dx
	push cx
	call draw_led_value
	add sp,6
@@skip:
	pop si
	pop cx
	pop dx
	pop bx
ENDM

;draws a flag icon in the specified locations
;parameters xpos,ypos
draw_flag_proc PROC
	push bp
	mov bp,sp
	push ax
	push bx

	mov cx,[bp+4]
	mov dx,[bp+6]
	;draw the flag pole
	add cx,cell_width/12*3
	add dx,cell_height/6
	mov ax,cell_width/12
	mov bx,cell_height/6*4

	draw_filled_box_caller cx,dx,ax,bx,7

	;draw the flag itself
	add cx,cell_width/12 ;add only increase in x
	mov ax,cell_width/3  ;adjust flag width
	mov bx,cell_height/6*2 ;adjust flag height

	draw_filled_box_caller cx,dx,ax,bx,12

	pop bx
	pop ax
	pop bp
	RET
ENDP

draw_flag_caller MACRO row,col
	push cx
	push dx
	;expand_coordinates row,col
	get_screen_coordinates_caller row,col
	;push parameters
	push dx
	push cx
	call draw_flag_proc
	add sp,4
	pop dx
	pop cx
ENDM

;draws a bomb icon in the specified locations
;parameters xpos,ypos
draw_bomb_proc PROC
	push bp
	mov bp,sp
	push ax
	push bx

	mov cx,[bp+4]
	mov dx,[bp+6]

	mov ax,cell_width/7
	mov bx,cell_height/7
	;first slice
	add cx,cell_width/7*3
	add dx,cell_height/7

	draw_filled_box_caller cx,dx,ax,bx,7

	;second slice
	sub cx,cell_width/7
	add dx,cell_height/7
	mov ax,cell_width/7*3

	draw_filled_box_caller cx,dx,ax,bx,7

	;third slice
	sub cx,cell_width/7
	add dx,cell_height/7
	mov ax,cell_width/7*5
	draw_filled_box_caller cx,dx,ax,bx,7

	;fourth slice
	add cx,cell_width/7
	add dx,cell_height/7
	mov ax,cell_width/7*3
	draw_filled_box_caller cx,dx,ax,bx,7

	;fifth slice
	add cx,cell_width/7
	add dx,cell_height/7
	mov ax,cell_width/7
	draw_filled_box_caller cx,dx,ax,bx,7

	pop bx
	pop ax
	pop bp
	RET
ENDP

draw_bomb_caller MACRO row,col
	push cx
	push dx
	;expand_coordinates row,col
	get_screen_coordinates_caller row,col
	;push parameters
	push dx
	push cx
	call draw_bomb_proc
	add sp,4
	pop dx
	pop cx
ENDM

;uncover the cell and show its number or bomb
;parameters : row,col
show_cell PROC
	push bp
	mov bp,sp
	push ax
	push bx
	color_cell_caller [bp+4],[bp+6],OPENED_CELL_BACKGROUND_COLOR
	_expand_proc_caller [bp+4],[bp+6]
	mov al,[bx + OFFSET grid]
	;clear most significant half byte then set it to 2 (open)
	and al,0Fh
	cmp al,0fh
	je bmb
	print_cell_value [bp+4],[bp+6],ax
	jmp fin
bmb:
	draw_bomb_caller [bp+4],[bp+6]
fin:	or al,20h
	mov [bx + OFFSET grid],al
	pop bx
	pop ax
	pop bp
	RET
ENDP

show_cell_caller MACRO row,col
	push col
	push row
	call show_cell
	add sp,4
ENDM

;get the specified cell view
;input row,col (2 bytes each)
;returns result in al
get_cell_view_proc PROC
	push bp
	mov bp,sp
	push bx
	push cx
	_expand_proc_caller [bp+4],[bp+6]
	mov bl,[bx + OFFSET grid]
	and bl,0F0h
	mov cl,4
	shr bl,cl
	mov al,bl
	pop cx
	pop bx
	pop bp
	RET
ENDP

get_cell_view_proc_caller MACRO row,col
	push col
	push row
	call get_cell_view_proc
	add sp,4
ENDM


; this MACRO uses ax, so YOU CANNOT SEND THE PARAMETERS TO THIS MACRO IN AX
; WARNING: THIS MACRO MUST BE PLACED BEFORE open_cell PROC, OR ELSE YOU'LL GET ERRORS CUZ IT'LL NEED MULTI-PASS ASSEMBLING
; IMPORTANT: this macro will initially be called by the mouse click handler. the mouse click handler is responsible to check whether this cell 
;			 is a bomb or outside the grid or ... . In other words, it will only call the macro if the clicked cell is closed
open_cell_caller MACRO row,col
	push ax
	mov ax,0
	mov al,col
	push ax
	mov al,row
	push ax
	call open_cell
	add sp,4
	pop ax
ENDM open_cell_caller

open_cell PROC
	; save bp
	push bp
	mov bp,sp
	; save registers
	push ax
	push bx
	push cx
	push dx
	push si
	
	mov ax,[bp+4] ;first parameter (row)
	mov dx,[bp+6] ;second parameter (col)
	_expand_proc_caller al,dl

	;if cell is not "closed" (open or flaged), then return
	mov cl,[bx + OFFSET grid]
	and cl,0F0h
	cmp cl,CELL_CLOSED
	jnz ret_open_cell
	
	show_cell_caller ax,dx

	;if cell has value then return
	mov cl,[bx + OFFSET grid]
	and cl,0Fh
	cmp cl,0
	jne ret_open_cell

	mov ah,dl ;(al,ah) = (row,col)
	; open adjacent cells
	mov si,7
	dAr_loop:
		lea bx,dxAr
		mov dl,[bx+si]
		lea bx,dyAr
		mov dh,[bx+si]
		add dl,al
		add dh,ah
		
		_expand_proc_caller dl,dh
		mov cl,[bx + OFFSET grid]
		cmp cl,0Fh
		jge continue
		open_cell_caller dl,dh

	continue:
		dec si
		cmp si,0
	jge dAr_loop
	
ret_open_cell:
	; restore registers
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	RET
ENDP open_cell

start:
	;set DS to point to the data segment
	mov	ax,@data
	mov  	ds,ax                  

	;start vga
	mov ax,12h
	int 10h


	print 	welcome_msg 
	draw_grid rows,cols,start_x,start_y,cell_width,cell_height
	
	;draw a test box
	;draw_filled_box_caller start_x,start_y,cell_width,cell_height,13
	;test print value
	;mov ax,1
	;mov bx,2
	;mov si,4
	;print_cell_value ax,bx,si
	;test draw flag
	;draw_flag_caller 2,4
	;draw_flag_caller 2,5
	;test draw bomb
	;draw_bomb_caller 3,5

	;init mouse
	mov ax,0
	int 33h
	;show mouse cursor
	mov ax,1
	int 33h

	;init seed using current system time
	mov ah,0
	int 1Ah
	mov rand,dh
	;initialize grid
	init_grid
	print gen_complete_msg

	;debug code to uncover all cells
	;--------------------------------
	xor ah,ah
	mov al,rows
	dec al
	xor bh,bh
loop_r:
	mov bl,cols
	dec bl
	loop_c:
		;show_cell_caller ax,bx
		dec bl
		cmp bl,0ffh
		jne loop_c
	dec al
	cmp al,0ffh
	jne loop_r
	;--------------------------------
	
	mov bx,0
	;di represents mouse buttons status flag (1 when mouse button is down,0 when mouse button is up)
	mov di,0
game_loop:
	;delay_1sec
	mov ax,3
	int 33h
	and di,bx
	;user is holding in mouse
	jnz game_loop

	cmp bx,0
	jz game_loop ;no mouse button is clicked

	;hide mouse cursor
	mov ax,2
	int 33h

	;check right button
	cmp bx,2
	jne aux_jump
		mov di,0fh
		convert_coordinates
		mov dh,cl
		get_cell_view_proc_caller dx,cx
		cmp al,CELL_OPENED
		je check_left_button
		cmp al,CELL_FLAGED
		je cell_has_flag
		set_cell_flaged dl,dh
		draw_flag_caller dx,cx
		jmp check_left_button
	aux_jump:
		cmp bx,2
		jne check_left_button
		cell_has_flag:
			color_cell_caller dx,cx,CLOSED_CELL_BACKGROUND_COLOR
			set_cell_closed dl,dh
	;check left button
	check_left_button:
	cmp bx,1
	jne mouse_reset
	mov di,0fh
	;print left_button_clicked_msg
	convert_coordinates
	open_cell_caller dl,cl

	;set_cell_opened 0,5
	;get_cell_view 0,5,dl
	;cmp dl,CELL_OPENED
	;je close
mouse_reset:
	;show mouse cursor
	mov ax,1
	int 33h
	jmp game_loop

close:
	mov ah,1h		    ;wait for key input to terminate
	int 21h
	print 	end_msg 
	mov ax,3                    ;return to dos mode
	int 10h
	mov  ah,4ch                 ;DOS terminate program function
	int  21h                    ;terminate the program
End start



