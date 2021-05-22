; Various sub-routines that will be useful to the boot loader code	

; Output Carriage-Return/Line-Feed (CRLF) sequence to screen using BIOS

Console_Write_CRLF:
	mov 	ah, 0Eh						; Output CR
    mov 	al, 0Dh
    int 	10h
    mov 	al, 0Ah						; Output LF
    int 	10h
    ret

; Write to the console using BIOS.
; 
; Input: SI points to a null-terminated string

Console_Write_16:
	mov 	ah, 0Eh						; BIOS call to output value in AL to screen

Console_Write_16_Repeat:
    mov		al, [si]
	inc     si
    test 	al, al						; If the byte is 0, we are done
	je 		Console_Write_16_Done
	int 	10h							; Output character to screen
	jmp 	Console_Write_16_Repeat

Console_Write_16_Done:
    ret

; Write string to the console using BIOS followed by CRLF
; 
; Input: SI points to a null-terminated string

Console_WriteLine_16:
	call 	Console_Write_16
	call 	Console_Write_CRLF
	ret

Console_Write_Int:
	mov 	si, IntBuffer
	mov		ax, bx
	mov 	cx, 10

GetDigit:
	xor 	dx, dx
	div 	cx
	add		dl, 48
	mov 	[si], dl
	dec 	si
	cmp 	ax, 0
	jne 	GetDigit
	inc 	si
	call    Console_Write_16
	ret

IntBuffer	db '        ', 0
