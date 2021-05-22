; Second stage of the boot loader

BITS 16

ORG 9000h
	jmp 	Second_Stage

%include "functions_16.asm"
%include "graphics_functions.asm"

;	Start of the second stage of the boot loader
	
Second_Stage:
    mov 	si, second_stage_msg	; Output our greeting message
    call 	Console_WriteLine_16

	; Put bootloader into VGA mode
	mov		ah, 0
	mov		al, 13h
	int 	10h

	; Displaying 10 lines
	mov 	cx, 10

Loop_Top:
	; Colour param = loop number plus 5
	mov 	ax, cx
	add 	ax, 5
	push 	ax 			 

	; y offset = cx * 10
	mov 	ax, cx
	mov 	bx, 10
	mul 	bx
	mov 	dx, ax

	; y1 param = 100 - y offset
	mov 	ax, 100
	sub		ax, dx     
	push 	ax

	; x1 param = 160   
	push 	160

	; y0 param = y offset  
	push 	dx

	; x0 param = 0
	push 	0

	call 	Draw_Line
	loop	Loop_Top

	; Displaying 3 rectangles
	push 	25						; Height
	push 	50						; Width
	push 	13						; Colour
	push 	10						; Row
	push 	170						; Column
	call 	Draw_Rect

	push 	50						; Height
	push 	125						; Width
	push 	3						; Colour
	push 	40						; Row
	push 	180						; Column
	call 	Draw_Rect

	push 	50						; Height
	push 	25						; Width
	push 	12					    ; Colour
	push 	10						; Row
	push 	250						; Column
	call 	Draw_Rect

	; Displaying 3 circles
	push 	24						; Radius
	push    11						; Colour
	push 	150						; Row (centre)
	push 	50						; Column (centre)
	call 	Draw_Circle

	push 	10						; Radius
	push    4						; Colour
	push 	150						; Row (centre)
	push 	50						; Column (centre)
	call 	Draw_Circle

	push 	40						; Radius
	push    10						; Colour
	push 	150						; Row (centre)
	push 	110						; Column (centre)
	call 	Draw_Circle

	; Creating gold colour for the star
	push 	0						; Blue component
	push 	240						; Green component
	push 	255						; Red component
	push 	16						; Colour number
	call 	Modify_Colour

	; Displaying a star
	push 	16						; Colour
	push 	10						; Number of points
	push 	star_array 				; Address of array
	call 	Draw_Polygon

	; Displaying a triangle
	push 	1						; Colour
	push 	3						; Number of points
	push 	triangle_array 		    ; Address of array
	call 	Draw_Polygon

	; Displaying a pentagon
	push 	6						; Colour
	push 	5					    ; Number of points
	push 	pentagon_array 		    ; Address of array
	call 	Draw_Polygon

	; Display boundary lines to split screen into quadrants for demonstration purposes
	push 	15						; Colour
	push 	199						; y1
	push 	160						; x1
	push 	0						; y0
	push 	160						; x0
	call 	Draw_Line

	push 	15						; Colour
	push 	100						; y1
	push 	319						; x1
	push 	100						; y0
	push 	0						; x0
	call 	Draw_Line

; This never-ending loop ends the code.  It replaces the hlt instruction
; used in earlier examples since that causes some odd behaviour in 
; graphical programs.
endloop:
	jmp		endloop

second_stage_msg	db 'Second stage loaded', 0

; Defining the arrays of points used to demonstrate the Draw_Polygon function
star_array 			dw 206
					dw 110
					dw 194
					dw 134
					dw 170
					dw 140
					dw 191
					dw 152
					dw 179
					dw 179
					dw 206
					dw 164
					dw 233
					dw 179
					dw 221
					dw 152
					dw 242
					dw 140
					dw 218
					dw 134
					
triangle_array 		dw 240
					dw 110
					dw 255
					dw 140
					dw 270
					dw 110

pentagon_array      dw 280
					dw 140
					dw 260
					dw 154
					dw 268
					dw 178
					dw 292
					dw 178
					dw 300
					dw 154

	
	times 3584-($-$$) db 0	