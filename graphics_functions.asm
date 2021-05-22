; Params = (x0, y0, x1, y1, line_colour) to be passed in reverse
%assign x0				4	
%assign y0				6
%assign x1				8
%assign y1				10
%assign line_colour		12
; Registers used: dx for dx, di for dy, ch for sx, cl for sy, si for err, ax and bx for arithmetic/temp variables
; Draws line using Bresenham's algorithm
Draw_Line:
	push 	bp					
	mov		bp, sp				; Copy stack pointer to bp
	push	ax					; Save registers that will be modified on the stack
    push    dx
    push    bx
    push    cx
    push    si  
    push    di

	; dx := abs(x1 - x0)
	mov 	dx, [bp + x1]
	sub		dx, [bp + x0]
    ; Set dy to absolute value
    mov     ax, dx
    neg     dx          ; Negate dx
    cmovl   dx, ax      ; If the sign flag is set, the number was positive so we go back to positive value

    ; dy := abs(y1 - y0)
	mov 	di, [bp + y1]
	sub		di, [bp + y0]
    ; Set dy to absolute value
	mov     ax, di
    neg     di          ; Negate dx
    cmovl   di, ax      ; If the sign flag is set, the number was positive so we go back to positive value

    ; if x0 < x1 then sx := 1 else sx := -1
    mov     ch, 1
    mov     ax, [bp + x0]
    cmp     ax, [bp + x1]
    jb      sx_Set
    mov     ch, -1

sx_Set:
    ; if y0 < y1 then sy := 1 else sy := -1
    mov     cl, 1
    mov     ax, [bp + y0]
    cmp     ax, [bp + y1]
    jb      sy_Set
    mov     cl, -1

sy_Set:
    ; err := dx - dy
    mov     si, dx
    sub     si, di

Top_Of_Loop:
    ; setPixel(x0, y0, colour)
    push    word [bp + line_colour]
    push    word [bp + y0]
    push    word [bp + x0]
    ; Either set pixel function can be used here
    call    Set_Pixel_Memory ; or: call    Set_Pixel_BIOS

    ; if x0 = x1 and y0 = y1 exit loop
    mov     ax, [bp + x0]
    cmp     ax, [bp + x1]
    je      x0_Equals_x1

Continue_Loop:
    ; e2 := 2 * err, put e2 in bx
    ; shifting to the left by one bit multiplies by 2
    mov     bx, si
    shl     bx, 1

    ; if e2 > -dy then
    ; Copy dy into ax and make it negative
    mov     ax, di
    neg     ax
    cmp     bx, ax
    jle     End_If_1
    ; err := err - dy
    sub     si, di
    ; x0 := x0 + sx
    ; Convert ch to word, result is put in ax
    mov     al, ch
    cbw
    add     [bp + x0], ax

End_If_1:
    ; if e2 < dx then
    cmp     bx, dx
    jge     End_If_2
    ; err := err + dx
    add     si, dx
    ; y0 := y0 + sy
    ; Convert cl to word, result is put in ax
    mov     al, cl
    cbw
    add     [bp + y0], ax

End_If_2:
    ; Jump back to top of the loop
    jmp     Top_Of_Loop

x0_Equals_x1:
    mov     ax, [bp + y0]
    cmp     ax, [bp + y1]
    jne     Continue_Loop
    ; End of the loop
    pop     di                  ; Restore registers to original values	
    pop     si
    pop     cx
    pop     bx                  
    pop     dx
	pop 	ax											
	mov 	sp, bp				
	pop 	bp
	
	ret		10					; Return, removing 5 params off the stack

; Params = (column, row, colour) to be passed in reverse
%assign column		4
%assign row			6	
%assign colour		8
Set_Pixel_BIOS:                 ; Plots pixel using BIOS function
	push 	bp					
	mov		bp, sp				; Copy stack pointer to bp
	push	ax					; Save registers that will be modified on the stack
	push	cx                  
	push 	dx
	push 	bx
    push    si                  ; Int 10h, 0Ch destroys registers AX, SP, BP, SI, DI according to documentation, so we save them too
    push    di

	mov		cx, [bp + column]	; Get column param off stack and store in cx
	mov 	dx, [bp + row]		; Get row param off stack and store in dx
    mov		al, [bp + colour]	; Get colour param off stack and store in al
	mov		ah, 0Ch	
	xor		bh, bh				; Set bh to 0
	int 	10h					; Call BIOS function to draw pixel to the screen

    pop     di                  ; Restore registers to original values
    pop     si
	pop 	bx
	pop		dx
	pop		cx
	pop		ax
	mov		sp, bp
	pop		bp

	ret		6					; Return, removing 3 params off the stack

; Params = (column, row, colour) to be passed in reverse, offsets for these are already assigned above the Set_Pixel_BIOS function
Set_Pixel_Memory:               ; Plots pixel by writing to video memory
    push 	bp					
	mov		bp, sp				; Copy stack pointer to bp
    push    es                  ; Save registers that will be modified on the stack
    push    ax
    push    dx
    push    si

    ; Ensure that the parameters column and row are within the bounds of the screen (320x200)
    cmp     [bp + column], word 0
    jl      Invalid_Pixel_Params
    cmp     [bp + column], word 320
    jge     Invalid_Pixel_Params
    cmp     [bp + row], word 0
    jl      Invalid_Pixel_Params
    cmp     [bp + row], word 200
    jge     Invalid_Pixel_Params
    
    ; Set segment register to A000h - es cannot be set directly so I used ax as temp storage
    ; Another way to do this would be to push A000h to the stack and then pop the value into es
    ; After left shift of 4 bits, this points to VGA memory at A0000h
    mov     ax, word 0A000h     
    mov     es, ax
    
    ; Offset = (row * 320) + column
    mov     ax, [bp + row]
    mov     dx, 320
    mul     dx
    ; row * 320 at maximum = 199 * 320 = 63680 which can be represented using 16 bits,
    ; result of mul (16 bit operands) stored in dx:ax but dx will always be 0000 because result will always be 16 bits or less, 
    ; so we can ignore the result in dx and use ax only but important to remember that anything in dx is destroyed by mul still.
    add     ax, [bp + column]
    mov     si, ax
    mov     al, [bp + colour]

    ; Write to video memory at location A0000h + (row * 320) + column
    mov     [es:si], al

Invalid_Pixel_Params:
    pop     si                  ; Restore registers to original values
    pop     dx
    pop     ax
    pop     es
    mov		sp, bp
	pop		bp

    ret     6					; Return, removing 3 params off the stack

; Params = (column, row, colour, width, height) to be passed in reverse, offsets for column, row and colour are already assigned above the Set_Pixel_BIOS function
%assign width       10
%assign height      12
Draw_Rect:                      ; Draws rectangle by writing to video memory
    push 	bp					
	mov		bp, sp				; Copy stack pointer to bp
    push    es                  ; Save registers that will be modified on the stack
    push    ax
    push    cx
    push    dx                  ; dx destroyed by mul
    push    di      

    ; Ensure that the rectangle is within the bounds of the screen (320x200)
    cmp     [bp + column], word 0
    jl      Invalid_Rect_Params
    mov     ax, [bp + column]
    add     ax, [bp + width]
    cmp     ax, word 320
    jge     Invalid_Rect_Params
    cmp     [bp + row], word 0
    jl      Invalid_Rect_Params
    mov     ax, [bp + row]
    add     ax, [bp + height]
    cmp     ax, word 320
    jge     Invalid_Rect_Params
    
    ; Set segment register to A000h - es cannot be set directly so I used ax as temp storage
    ; Another way to do this would be to push A000h to the stack and then pop the value into es
    ; After left shift of 4 bits, this points to VGA memory at A0000h
    mov     ax, word 0A000h     
    mov     es, ax

    ; Starting point = (row * 320) + column
    mov     ax, [bp + row]
    mov     dx, 320
    mul     dx
    add     ax, [bp + column]
    mov     di, ax
    mov     al, [bp + colour]

    ; Loop through each row by setting cx to height
    mov     cx, [bp + height]
    
Loop_Rows:
    ; Loop through each column by setting cx to width, save current row to stack
    push    cx
    ; stosb sets memory at es:di to value in al
    ; rep stosb repeats this, incrementing di and decrementing cx until cx is zero
    mov     cx, [bp + width]
    rep     stosb
    ; Set di to memory location of next row (di = di + 320 - width)
    add     di, word 320
    sub     di, [bp + width]
    ; Get height off the stack
    pop     cx
    ; Draw next row if cx is not zero, cx is decremented by loop
    loop    Loop_Rows

Invalid_Rect_Params:
    pop     di                  ; Restore registers to original values
    pop     dx
    pop     cx
    pop     ax
    pop     es
    mov		sp, bp
	pop		bp

    ret     10					; Return, removing 5 params off the stack

; Params = (xCentre, yCentre, colour, radius) to be passed in reverse, colour already assigned above the Set_Pixel_BIOS function
%assign xc      4
%assign yc      6
%assign r       10
Draw_Circle:                    ; Draws circle using Bresenham's circle algorithm
    push 	bp					
	mov		bp, sp				; Copy stack pointer to bp
    push    ax                  ; Save registers that will be modified on the stack
    push    bx
    push    cx
    push    dx
    push    di
    push    si

    ; di = x, si = y, bx = d
    ; x = 0
    mov     di, word 0       
    ; y = r
    mov     si, [bp + r]
    ; d = 3 - 2 * r  
    mov     bx, si
    shl     bx, 1
    mov     ax, 3
    sub     ax, bx
    mov     bx, ax

    jmp     Draw_Circle_Octants

Circle_Loop:
    ; x++
    inc     di
    ; if d > 0
    cmp     bx, 0
    jg      d_Greater_Than_0
    ; d <= 0
    ; d = d + 4 * x + 6
    mov     ax, word 4
    mul     di
    add     ax, word 6
    add     ax, bx
    mov     bx, ax
    jmp     Draw_Circle_Octants

d_Greater_Than_0:
    ; y--
    dec     si
    ; d = d + 4 * (x - y) + 10
    mov     cx, di
    sub     cx, si
    mov     ax, word 4
    mul     cx
    add     ax, word 10
    add     ax, bx
    mov     bx, ax

Draw_Circle_Octants:            ; Draws 8 pixels, one in each octant
    ; Set_Pixel(xc+x, yc+y)
    mov     ax, [bp + xc]
    mov     dx, [bp + yc]
    add     ax, di
    add     dx, si
    push    word [bp + colour]
    push    dx
    push    ax
    call    Set_Pixel_Memory
    ; Set_Pixel(xc-x, yc+y)
    mov     ax, [bp + xc]
    sub     ax, di
    push    word [bp + colour]
    push    dx
    push    ax
    call    Set_Pixel_Memory
    ; Set_Pixel(xc-x, yc-y)
    mov     dx, [bp + yc]
    sub     dx, si
    push    word [bp + colour]
    push    dx
    push    ax
    call    Set_Pixel_BIOS
    ; Set_Pixel(xc+x, yc-y)
    mov     ax, [bp + xc]
    add     ax, di
    push    word [bp + colour]
    push    dx
    push    ax
    call    Set_Pixel_Memory
    ; Set_Pixel(xc+y, yc+x)
    mov     ax, [bp + xc]
    mov     dx, [bp + yc]
    add     ax, si
    add     dx, di
    push    word [bp + colour]
    push    dx
    push    ax
    call    Set_Pixel_Memory
    ; Set_Pixel(xc-y, yc+x)
    mov     ax, [bp + xc]
    sub     ax, si
    push    word [bp + colour]
    push    dx
    push    ax
    call    Set_Pixel_Memory
    ; Set_Pixel(xc-y, yc-x)
    mov     dx, [bp + yc]
    sub     dx, di
    push    word [bp + colour]
    push    dx
    push    ax
    call    Set_Pixel_Memory
    ; Set_Pixel(xc+y, yc-x)
    mov     ax, [bp + xc]
    add     ax, si
    push    word [bp + colour]
    push    dx
    push    ax
    call    Set_Pixel_Memory

    ; Loop if y >= x
    cmp     si, di
    jge     Circle_Loop
    ; End of loop
    pop     si                  ; Restore registers to original values  
    pop     di
    pop     dx
    pop     cx
    pop     bx
    pop     ax
    mov		sp, bp
	pop		bp

    ret     8					; Return, removing 4 params off the stack

; Params = address of array of points (x, y), number of sides, colour (to be passed in reverse)
%assign array_start         4
%assign number_of_sides     6
%assign polygon_colour      8
Draw_Polygon:                   ; Draws polygon from array of points, the contents of ax, bx are lost when using this function
    push 	bp					
	mov		bp, sp				; Copy stack pointer to bp
    push    si                  ; Save registers that will be modified
    push    cx

    ; Check if the polygon has at least 3 points, if not then return
    cmp     [bp + number_of_sides], word 3
    jl      Invalid_Polygon_Params

    ; Save array address to si
    mov     si, [bp + array_start]
    ; Set cx to number of sides - 1 for loop
    mov     cx, [bp + number_of_sides]
    dec     cx

    ; Save first point on the stack so we can draw line back to start at the end
    push    word [bp + polygon_colour]
    push    word [ds:si + 2]
    push    word [ds:si]

Draw_Side:
    ; Draw line between two points in the array
    push    word [bp + polygon_colour]
    push    word [ds:si + 6]
    push    word [ds:si + 4]
    push    word [ds:si + 2]
    push    word [ds:si]
    call    Draw_Line
    add     si, 4               ; Point si at next point in the array
    loop    Draw_Side           ; Draw next side until we reach the last side

    ; Push last point. Colour and first point are already on the stack from before the loop, ready for Draw_Line function
    push    word [ds:si + 2]
    push    word [ds:si]
    call    Draw_Line


Invalid_Polygon_Params:
    pop     cx                  ; Restore registers to original values  
    pop     si
    mov		sp, bp
	pop		bp

    ret     6	                ; Return, removing 3 params off the stack

; Params = colour_number, r, g, b (to be passed in reverse)
%assign colour_number       4
%assign r                   6
%assign g                   8
%assign b                   10
Modify_Colour:                  ; Sets colour at defined number using rgb values
    push 	bp					
	mov		bp, sp				; Copy stack pointer to bp
    push    ax                  ; Save registers that will be modified on the stack
    push    bx
    push    cx
    push    dx

    ; bx set to colour number, dh to r, ch to g and cl to b. Int 10 Func 1010 uses these values
    mov 	bx, [bp + colour_number]
	mov 	dh, [bp + r]
	mov 	ch, [bp + g]
    mov 	cl, [bp + b]

	mov 	ax, 1010h           ; Interrupt 10 function 1010 used to modify colour registers
	int 	10h

    pop     dx                  ; Restore Registers to original values
    pop     cx
    pop     bx
    pop     ax
    mov		sp, bp
	pop		bp

    ret     8					; Return, removing 4 params off the stack

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Code below here is not used, tried to get it working but in the end I couldn't. Left code in to show my attempt.

; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Params = x0, y0, x1, y1, x2, y2, colour (to be passed in reverse). Points must be passed in order of ascending y value.
; Must be flat triangle, tried to implement non flat triangles but this requires floating point values which I cannot do here.
; x0, y0, x1, y1 already assigned above Draw_Line function.
%assign x2                  12
%assign y2                  14
%assign triangle_colour     16
Fill_Triangle:                  ; Fills triangle using the Bresenham algorithm
    push 	bp					
	mov		bp, sp				; Copy stack pointer to bp
    push    ax                  ; Save register that will be modified

    ; Check if triangle is bottom flat
    mov     ax, [bp + y1]
    cmp     ax, [bp + y2]
    jne     Not_Bottom_Flat
    push    word [bp + triangle_colour]
    push    word [bp + y2]
    push    word [bp + x2]
    push    word [bp + y1]
    push    word [bp + x1]
    push    word [bp + y0]
    push    word [bp + x0]
    call    Fill_Flat_Sided_Triangle
    jmp     Done

Not_Bottom_Flat:
    ; Check if triangle is top flat
    mov     ax, [bp + y0]
    cmp     ax, [bp + y1]
    jne     Done
    push    word [bp + triangle_colour]
    push    word [bp + y1]
    push    word [bp + x1]
    push    word [bp + y0]
    push    word [bp + x0]
    push    word [bp + y2]
    push    word [bp + x2]
    call    Fill_Flat_Sided_Triangle

Done:
    pop     ax                  ; Restore Registers to original values
    mov		sp, bp
	pop		bp

    ret     14					; Return, removing 7 params off the stack

; Params = x0, y0, x1, y1, x2, y2, colour (to be passed in reverse).
; Params already assigned above Fill_Triangle function
; Registers used: signx1 = bh, signx2 = bl, signy1 = ch, signy2 = cl, e1 = si, e2 = di
; Pre-allocated memory used for: vTmp1x, vTmp1y, vTmp2x, vTmp2y, changed1, changed2, dx1, dy1, dx2, dy2
Fill_Flat_Sided_Triangle:       ; Fills flat sided triangle using the Bresenham algorithm
    push 	bp					
	mov		bp, sp				; Copy stack pointer to bp
    push    ax                  ; Save registers that will be modified
    push    bx
    push    cx
    push    dx
    push    si
    push    di

    ; Note points in comments (reference Java code) uses x1, x2, x3; I am instead using x0, x1, x2
    ; Vertice vTmp1 = new Vertice(v1.x, v1.y);
    mov     ax, [bp + x0]
    mov     [vTmp1x], ax
    mov     ax, [bp + y0]
    mov     [vTmp1y], ax
    ; Vertice vTmp2 = new Vertice(v1.x, v1.y);
    mov     ax, [bp + x0]
    mov     [vTmp2x], ax
    mov     ax, [bp + y0]
    mov     [vTmp2y], ax

    ; boolean changed1 = false;
    mov     [changed1], byte 0
    ; boolean changed2 = false;
    mov     [changed2], byte 0

    ; int dx1 = Math.abs(v2.x - v1.x);
    mov     ax, [bp + x1]
    sub     ax, [bp + x0]
    ; Set ax to absolute value
    mov     dx, ax
    neg     ax          ; Negate ax
    cmovl   ax, dx      ; If the sign flag is set, the number was positive so we go back to positive value
    mov     [dx1], ax
    ; int dy1 = Math.abs(v2.y - v1.y);
    mov     ax, [bp + y1]
    sub     ax, [bp + y0]
    ; Set ax to absolute value
    mov     dx, ax
    neg     ax          ; Negate ax
    cmovl   ax, dx      ; If the sign flag is set, the number was positive so we go back to positive value
    mov     [dy1], ax
    ; int dx2 = Math.abs(v3.x - v1.x);
    mov     ax, [bp + x2]
    sub     ax, [bp + x0]
    ; Set ax to absolute value
    mov     dx, ax
    neg     ax          ; Negate ax
    cmovl   ax, dx      ; If the sign flag is set, the number was positive so we go back to positive value
    mov     [dx2], ax
    ; int dy2 = Math.abs(v3.y - v1.y);
    mov     ax, [bp + y2]
    sub     ax, [bp + y0]
    ; Set ax to absolute value
    mov     dx, ax
    neg     ax          ; Negate ax
    cmovl   ax, dx      ; If the sign flag is set, the number was positive so we go back to positive value
    mov     [dy2], ax

    ; int signx1 = (int)Math.signum(v2.x - v1.x);
    mov     ax, [bp + x1]
    sub     ax, [bp + x0]
    call    Signum
    mov     bh, al
    ; int signx2 = (int)Math.signum(v3.x - v1.x);
    mov     ax, [bp + x2]
    sub     ax, [bp + x0]
    call    Signum
    mov     bl, al
    ; int signy1 = (int)Math.signum(v2.y - v1.y);
    mov     ax, [bp + y1]
    sub     ax, [bp + y0]
    call    Signum
    mov     ch, al
    ; int signy2 = (int)Math.signum(v3.y - v1.y);
    mov     ax, [bp + y2]
    sub     ax, [bp + y0]
    call    Signum
    mov     cl, al

    ; if (dy1 > dx1)
    mov     ax, [dy1]
    cmp     ax, [dx1]
    jle     Dx1_Less_Equal_Dy1
    ;int tmp = dx1;
    ;dx1 = dy1;
    ;dy1 = tmp;
    push    word [dx1]
    mov     [dx1], ax
    pop     word [dy1]
    ;changed1 = true;
    mov     [changed1], byte 1

Dx1_Less_Equal_Dy1:
    ; if (dy2 > dx2)
    mov     ax, [dy2]
    cmp     ax, [dx2]
    jle     Dx2_Less_Equal_Dy2
    ;int tmp = dx2;
    ;dx2 = dy2;
    ;dy2 = tmp;
    push    word [dx2]
    mov     [dx2], ax
    pop     word [dy2]
    ;changed2 = true;
    mov     [changed2], byte 1

Dx2_Less_Equal_Dy2:
    ; int e1 = 2 * dy1 - dx1;
    mov     ax, [dy1]
    sub     ax, [dx1]
    mov     dx, 2
    mul     dx
    mov     si, ax
    ; int e2 = 2 * dy2 - dx2;
    mov     ax, [dy2]
    sub     ax, [dx2]
    mov     dx, 2
    mul     dx
    mov     di, ax

; for (int i = 0; i <= dx1; i++)
    mov     dx, [dx1]
Dx1_Loop:
    ; g.drawLine(vTmp1.x, vTmp1.y, vTmp2.x, vTmp2.y);
    push    word [bp + triangle_colour]
    push    word [vTmp2y]
    push    word [vTmp2x]
    push    word [vTmp1y]
    push    word [vTmp1x]
    call    Draw_Line

; while (e1 >= 0)
E1_Loop:
    ; if (changed1)
    cmp     [changed1], byte 1
    jne     Not_Changed1
    ; vTmp1.x += signx1;
    mov     al, bh
    cbw
    add     [vTmp1x], ax
    jmp     Changed1_End_If
    ; else
Not_Changed1:
    ; vTmp1.y += signy1;
    mov     al, ch
    cbw
    add     [vTmp1y], ax
Changed1_End_If:
    ; e1 = e1 - 2 * dx1;
    push    dx                  ; mul destroys dx, so we preserve it on the stack
    mov     ax, [dx1]
    mov     dx, 2
    mul     dx
    sub     si, ax
    pop     dx

End_E1_Loop_Check:
    cmp     si, 0
    jge     E1_Loop
    ; if (changed1)
    cmp     [changed1], byte 1
    jne     Not_Changed1_2
    ; vTmp1.y += signy1;
    mov     al, ch
    cbw
    add     [vTmp1y], ax
    jmp     Changed1_End_If_2
    ; else
Not_Changed1_2:
    ; vTmp1.x += signx1; 
    mov     al, bh
    cbw
    add     [vTmp1x], ax
Changed1_End_If_2:
    ; e1 = e1 + 2 * dy1;
    push    dx                  ; mul destroys dx, so we preserve it on the stack
    mov     ax, [dy1]
    mov     dx, 2
    mul     dx
    add     si, ax
    pop     dx

; while (vTmp2.y != vTmp1.y)
; while (e2 >= 0)
; Can use same tag for both of these loops
E2_Loop:
    ; if (changed2)
    cmp     [changed2], byte 1
    jne     Not_Changed2
    ; vTmp2.x += signx2;
    mov     al, bl
    cbw
    add     [vTmp2x], ax
    jmp     Changed2_End_If
    ; else
Not_Changed2:
    ; vTmp2.y += signy2;
    mov     al, cl
    cbw
    add     [vTmp1y], ax
Changed2_End_If:
    ; e2 = e2 - 2 * dx2;
    push    dx                  ; mul destroys dx, so we preserve it on the stack
    mov     ax, [dx2]
    mov     dx, 2
    mul     dx
    sub     di, ax
    pop     dx
End_E2_Loop_Check:
    cmp     di, 0
    jge     E2_Loop
    ; if (changed2)
    cmp     [changed2], byte 1
    jne     Not_Changed2_2
    ; vTmp2.y += signy2;
    mov     al, cl
    cbw
    add     [vTmp1y], ax
    jmp     Changed2_End_If_2
    ; else
Not_Changed2_2:
    ; vTmp2.x += signx2;
    mov     al, bl
    cbw
    add     [vTmp2x], ax
Changed2_End_If_2:
    ; e2 = e2 + 2 * dy2;
    push    dx                  ; mul destroys dx, so we preserve it on the stack
    mov     ax, [dy2]
    mov     dx, 2
    mul     dx
    add     di, ax
    pop     dx

    mov     ax, [vTmp1y]
    cmp     [vTmp2y], ax
    jne     E2_Loop

    dec     dx
    cmp     dx, 0
    jg      Dx1_Loop

    pop     di                  ; Restore Registers to original values
    pop     si
    pop     dx
    pop     cx
    pop     bx
    pop     ax
    mov		sp, bp
	pop		bp
    ret     14					; Return, removing 7 params off the stack

; If ax is positive, ax is set to 1. If ax is negative, ax is set to -1
Signum:
    cmp     ax, 0
    mov     ax, 1
    jge     Positive
    mov     ax, -1
Positive:
    ret

; Extra variables used in the Fill_Flat_Sided_Triangle function
vTmp1x      dw 0
vTmp1y      dw 0
vTmp2x      dw 0
vTmp2y      dw 0
changed1    db 0
changed2    db 0
dx1         dw 0
dy1         dw 0
dx2         dw 0
dy2         dw 0