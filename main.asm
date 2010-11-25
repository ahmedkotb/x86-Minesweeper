.model small
.stack 100h
.data
welcome_msg db 'welcome to minesweeper',13,10,'$'
end_msg db 'press any key to exit',13,10,'$'
bombs db ?         ;bombs number
grid db 480 dup(0) ;max grid size 16 * 30
.CODE
print MACRO msg_address
	push ax
	push dx
	mov dx,OFFSET msg_address
	mov ah,9
	int 21h
	pop ax
	pop dx
ENDM print

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

draw_line_caller MACRO startX,startY,len,color,vertical
	push vertical
	push color
	push len
	push startY
	push startX
	call draw_line
	add sp,10
ENDM draw_line_caller

draw_grid MACRO rows,cols,startX,startY,cell_width,cell_height
	;save registers
	push ax
	push bx
	push cx
	push dx
	;logic
	mov cx,rows
	inc cx
	mov ax,cols
	mov bx,startY
	mov dl,cell_width
	mul dl
	;ax contains the len of the line
	rows_loop:
		draw_line_caller startX,bx,ax,58,0
		add bx,cell_height
		loop rows_loop
		
		mov cx,cols
		inc cx
		mov ax,rows
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
	draw_grid 4,4,50,50,30,30
	;print	end_msg

	mov ah,1h		    ;wait for key input to terminate
	int 21h
	mov ax,3                    ;return to dos mode
	int 10h
	mov  ah,4ch                 ;DOS terminate program function
	int  21h                    ;terminate the program
End start
