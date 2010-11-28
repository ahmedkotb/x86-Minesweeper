.model small
.stack 100h
.data
welcome_msg db 'welcome to minesweeper',13,10,'$'
end_msg db 'press any key to exit',13,10,'$'
bombs db ?         ;bombs number
start_x dw 50
start_y dw 50
cell_width equ 30
cell_height equ 30
rows db 8
cols db 8
grid db 480 dup(0) ;max grid size 16 * 30

;the random variable state
rand db 0
.CODE

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
	pop ax
	pop dx
ENDM print

;private macro used in other macros to expand given row and col to required index in grid
;uses ax as temp register and expand result is stored in bx
_expand MACRO row,col
	mov ax,row
	mul cols
	add ax,col
	mov bx,ax
ENDM

set_grid_view_opened MACRO row,col
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

set_grid_view_closed MACRO row,col
	push ax
	push bx
	_expand row,col
	mov al,[bx + OFFSET grid]
	and al,0Fh
	mov [bx + OFFSET grid],al
	pop bx
	pop ax
ENDM

set_grid_view_flaged MACRO row,col
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
	;function logic
	mov al,[bp+10]    ;fourth parameter  (color)
	mov bx,[bp+8]     ;third parameter  (length)
	mov dx,[bp+6]     ;second parameter (startY)
	mov cx,[bp+4]     ;first parameter (startX)
	mov ah,0ch
	mov di,[bp+12]    ;fifth parameter (0 = horizontal otherwise vertical)
	cmp di,0 
	jnz vertical
	
	horizontal:
		int 10h
		inc cx 
		dec bx
		cmp bx,0
		jnz horizontal
		jmp done

	vertical:
		int 10h
		inc dx 
		dec bx
		jnz vertical
	done:
		;clear local storage
		;nothing to clear
		;restore registers
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
	draw_filled_box_caller start_x,start_y,cell_width,cell_height,13

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
	je close

	mov bx,0
mouseLoop:
	mov ax,3
	int 33h
	cmp bx,2
	je close
	cmp bx,1
	jne mouseLoop
	;draw_line_caller cx,dx,10,78,0
	convert_coordinates
	cmp dl,0
	je close
	set_grid_view_opened 0,5
	;mov bl,[OFFSET grid]  ;; WHY IN HELL this is not equal to mov bx,grid ??!!!
	mov bx,OFFSET grid
	mov bx,[bx+5]
	cmp bl,20h
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

