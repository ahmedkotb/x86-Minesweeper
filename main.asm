.model small
.stack 100h
.data
welcome_msg db 'welcome to minesweeper',13,10,'$'
end_msg db 'press any key to exit',13,10,'$'
bombs db ?         ;bombs number
start_x dw 50
start_y dw 50
cell_width equ 36
cell_height equ 36
rows db 8
cols db 8

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

numMines db ? ;total number of mines in the current active grid
;;numMinesLeft dw ? ;number of mines left in the game
rand_mod db 0

dxAr db 0,0FFh,0FFh,0FFh,0,1,1,1
dyAr db 0FFh,0FFh,0,1,1,1,0,0FFh

;7 segment LED Auxillary Array
led_array db 44h,3dh,6dh,4eh,6bh,7bh,45h,7fh
.CODE

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
	mov rand_mod,ah
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
;note : register can be used as out (except ax,bx,cx) as they are used inside the macro
get_cell_view MACRO row,col,value_out
	push ax
	push bx
	push cx
	_expand row,col
	mov al,[bx + OFFSET grid]
	and al,0F0h
	mov cl,4
	shr al,cl
	mov value_out,al
	pop cx
	pop bx
	pop ax
ENDM

set_cell_opened MACRO row,col
	push ax
	push bx
	_expand row,col
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
	_expand row,col
	mov al,[bx + OFFSET grid]
	and al,0Fh
	mov [bx + OFFSET grid],al
	pop bx
	pop ax
ENDM

set_cell_flaged MACRO row,col
	push ax
	push bx
	_expand row,col
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
	push bx
	push dx
	push cx
	expand_coordinates row,col
	;push parameters
	mov bx,OFFSET led_array
	push [bx+value-1]
	push dx
	push cx
	call draw_led_value
	add sp,6
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
	expand_coordinates row,col
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
	expand_coordinates row,col
	;push parameters
	push dx
	push cx
	call draw_bomb_proc
	add sp,4
	pop dx
	pop cx
ENDM

start:
	;set DS to point to the data segment
	mov	ax,@data
	mov  	ds,ax                  

	;start vga
	mov ax,12h
	int 10h

;init_grid
;jmp close

	print 	welcome_msg 
	draw_grid rows,cols,start_x,start_y,cell_width,cell_height
	
	;draw a test box
	;draw_filled_box_caller start_x,start_y,cell_width,cell_height,13
	;test print value
	print_cell_value 1,2,3
	;test draw flag
	draw_flag_caller 2,4
	draw_flag_caller 2,5
	;test draw bomb
	draw_bomb_caller 3,5

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

	gen_random
	cmp rand,3
	;je close  ERROR: relative jump out of range
	je jmpClose
	jmp cont
	
jmpClose:
	jmp close
	
cont:	
	mov bx,0

mouseLoop:
	mov ax,3
	int 33h
	cmp bx,2
	je close

	cmp bx,1
	jne mouseLoop
	;draw_line_caller cx,dx,10,78,0
	;convert_coordinates
	;cmp dl,0
	;je close
	set_cell_opened 0,5
	get_cell_view 0,5,dl
	cmp dl,CELL_OPENED
	je close
	jmp mouseLoop

	print 	welcome_msg

close:
	mov ah,1h		    ;wait for key input to terminate
	int 21h
	print 	end_msg 
	mov ax,3                    ;return to dos mode
	int 10h
	mov  ah,4ch                 ;DOS terminate program function
	int  21h                    ;terminate the program
End start



