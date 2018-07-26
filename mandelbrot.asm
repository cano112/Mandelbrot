DATA segment
		
		ArgV						db 256 dup(?)				;args table
		ArgC						db 0					;args number
		ArgPtr      					dw 9 dup(?)				;pointers to args
		
		Multiplier					dw 10d
		CompValue					dq 4.0						
		ScreenCoord                 			dw ?					;N,M
		
		integerNumber					dw ?
		floatNumber					dq ?
		
		xMin						dq ?
		xMax						dq ?
		yMin						dq ?
		yMax						dq ?
		
		ScreenWidth					dw 320d
		ScreenHeight					dw 200d
		
		P						dq ?
		Q						dq ?
									
		ErrMsg1						db "No arguments!$"
		ErrMsg2						db "Wrong arguments!$"
		ErrMsg3						db "Wrong argument format!$"

DATA ends
CODE segment
	
	.386									;popa/pusha and FPU flags to AX
	.387									;FPU
	
	START:
									
		mov ax, seg STACK1						;init stack
		mov ss, ax
		mov sp, offset StackPointer
		
		mov ax, seg DATA						;init data segment
		mov ds, ax
		
		finit 
		
		call parseArguments
		call checkArguments
		call parseCoords
		call drawSet
		
	PROGRAM_END:
		mov ax, 04c00h
		int 21h	

		
	parseArguments:
		pusha
			
		xor cx, cx							;same as "mov cx, 0"
		mov cl, byte ptr es:[80h]					;load args length to cx (loop counter)
		mov si, 82h							;set source pointer
		mov al, es:[si]							
		mov di, offset ArgV						;set destination pointer
		
		cmp cl, 0							;check if ArgC>0
		jne parseArgumentsLoop
		
		mov dx, offset ErrMsg1						;print error message
		call handleError						
		
		parseArgumentsLoop:						;load args to ArgV
			mov al, byte ptr es:[si]			
			
			cmp al, 20h						;check if space
			je whiteSpace						;if space, skip to white space removal
			
			cmp al, 9h						;check if tab
			je whiteSpace						;if tab, skip to white space removal
			
			cmp al, 0Dh						;check if CR (enter pressed)
			je endOfArgs					
			
			mov byte ptr ds:[di], al				;if normal char => load to ArgV
			inc si								
			jmp parseArgumentsLoopCont
			
			endOfArgs:
			mov byte ptr ds:[di], '$'				;replace CR with end of text
			inc si
			jmp parseArgumentsLoopCont
			
			whiteSpace:						;if white space found
			mov byte ptr ds:[di], 0					;load args separator
			
			whiteSpaceLoop:						;remove all white spaces left
				inc si								
				mov al, byte ptr es:[si]
				cmp al, 20h					;check if space
				je whiteSpaceLoop				;if space found => skip to next char
				cmp al, 9h					;check if tab
				je whiteSpaceLoop				;if tab found => skip to next char
			
			parseArgumentsLoopCont:		    			;at this point, we do not have any white spaces left
			inc di
		
		loop parseArgumentsLoop
		
		mov si, offset ArgV						;load args source pointer
		mov di, offset ArgPtr						;load arg pointers table
		mov bx, offset ArgC						;load arg number pointer	
		
		mov word ptr ds:[di], si					;load first arg pointer
		inc si
		inc byte ptr ds:[bx]
		add di, 2
		
		
		separateArgsLoop:
			mov al, byte ptr ds:[si]				;load char to al
			
			inc si							;point next char
			cmp al, '$'						;if char == $
			je parseArgumentsCont					;break					
			cmp al, 0						;if char != NUL
			jne separateArgsLoop					;continue
										;else
			mov word ptr ds:[di], si				;load arg pointer to ArgPtr
			inc byte ptr ds:[bx]					;increment ArgC
		
			add di, 2						;go to next arg pointer in ArgPtr (word=2byte)
			jmp separateArgsLoop
			
		parseArgumentsCont:
		
										;fix first argument when there are more than one white spaces before first arg (DOSBox issue)
		mov bx, 0
		call getArgument
		cmp byte ptr ds:[bx], 0
		jne fixSpaceCont
			inc word ptr ds:[ArgPtr]
			
		fixSpaceCont:
		
		
		popa
	ret
	
	getArgumentLength:							;returns in al length of argument with index from al (0, 1, 2, 3, etc.)
		push si
		push bx
		
												
		xor bx, bx							;getArgument uses whole bx
		mov bl, al
		call getArgument
		xor ax, ax							;reset counter
		
		getArgumentLengthLoop:
			inc al	
			inc bx
			cmp byte ptr ds:[bx], 0					;if end of argument => break
			je getArgumentLengthBreak
		
			cmp byte ptr ds:[bx], '$'				;if end of arguments => break
			je getArgumentLengthBreak
			
			jmp getArgumentLengthLoop
			
			getArgumentLengthBreak:
		pop bx
		pop si
	ret
	
	getArgument:								;returns argument offset in bx (arg number from bx)
		push si
		
		sal bl, 1d
		mov si, offset ArgPtr
		mov bx, word ptr ds:[si+bx]
		
		pop si
	ret
	
	checkArguments:
		push dx
		
		cmp byte ptr ds:[ArgC], 4d
		je checkArgumentsCont
			mov dx, offset ErrMsg2
			call handleError
			
		checkArgumentsCont:
		
		pop dx
	ret
	
	asciiToFloat:								;al - argument index (0, 1, 2, 3)
		pusha
		
		xor bx, bx								
		mov bl, al							;input for getArgument
		
		call getArgumentLength						;input: al(arg index)   output: al (length)
		cmp al, 3d
		jb formatErrorMsg
		
		call getArgument						
		mov si, bx							;si - argument pointer
		xor cx, cx							;cl  - arg length    ch - current length
		mov cl, al							;cl - argument length
		xor ax, ax							;ax - place for binary number
		xor dx, dx							;dl - current ASCII number
		xor bx, bx							;bl - sign flag
		
		mov dl, byte ptr ds:[si]
		cmp dl, '-'							;check sign
		jne toFloatLoop
			mov bl, 1d						;1 => -     0 => +
			inc ch
			inc si
		
		toFloatLoop:
			xor dx, dx
			
			mov dl, byte ptr ds:[si]
			cmp dl, '.'
			jne toFloatLoopCont
				inc ch
				inc si
				
				push ax
				xor ax, ax
				mov al, cl
				sub al, ch
				mov di, ax
				pop ax
				
				cmp ch, cl					;number must have fractional part
				jnb formatErrorMsg
		
					cmp ch, 1d				;it means .1234
					je formatErrorMsg
		
						cmp bl, 1d
						jne skipFormatCheck
				
							cmp ch, 2d		;it means: -.1234
							je formatErrorMsg
				
				skipFormatCheck:
				
				jmp toFloatLoop
		
			toFloatLoopCont:
			
			cmp dl, "9"
			ja formatErrorMsg
			
				cmp dl, "0"
				jb formatErrorMsg
			
			push dx
			mul word ptr ds:[Multiplier]				;move ax left, mul uses dx
			pop dx
			
			sub dl, "0"
			add ax, dx
			inc si
			inc ch
			
			cmp ch, cl
			jne toFloatLoop

		mov word ptr ds:[integerNumber], ax
		
					
		fild word ptr ds:[Multiplier]			;st(1)
		fild word ptr ds:[integerNumber]		;st(0)
		
		mov cx, di
		
		makeFractionLoop:
			fdiv st(0), st(1)					;st(0) = st(0)/st(1)
		loop makeFractionLoop
	
		cmp bl, 1d
		jne skipSignChange
			fchs							;change sign
		
		skipSignChange:
			fstp qword ptr ds:[floatNumber]				;move to mem
			
			fstp st(0)						;remove from stack
		
		jmp asciiToFloatEnd
		
		formatErrorMsg:
			mov dx, offset ErrMsg3
			call handleError
		
		asciiToFloatEnd:
		
		popa
	ret
	
	parseCoords:
		push ax
		push bx
		
		xor al, al
		mov bx, offset xMin
		
		parseCoordsLoop:
			call asciiToFloat
			
			fld qword ptr ds:[floatNumber]
			fstp qword ptr ds:[bx]
			
			inc al
			add bx, 8d
			
			cmp al, 4d
			jne parseCoordsLoop
			
		pop bx
		pop ax
	ret
	
	getP:									;ax - x coord
		mov word ptr ds:[ScreenCoord], ax				;cannot send ax directly to FPU
		
		fild word ptr ds:[ScreenCoord]					;st(4)
		fld qword ptr ds:[xMax]						;st(3)
		fld qword ptr ds:[xMin]						;st(2)
		fild word ptr ds:[ScreenWidth]					;st(1)
		fldz								;st(0) - calculated p
		
		fadd st(0), st(3)						;p = xMax
		fsub st(0), st(2)						;p = xMax - xMin
		fmul st(0), st(4)						;p = (xMax - xMin)*N
		fdiv st(0), st(1)						;p = (xMax - xMin)*N/SCREEN_W
		fadd st(0), st(2)						;p = (xMax - xMin)*N/SCREEN_W + xMin
		
		fstp qword ptr ds:[P]					
		fstp st(0)							;remove from stack
		fstp st(0)
		fstp st(0)
		fstp st(0)
	ret
	
	getQ:									;ax - y coord
		mov word ptr ds:[ScreenCoord], ax				;cannot send ax directly to FPU
		
		fild word ptr ds:[ScreenCoord]					;st(4)
		fld qword ptr ds:[yMax]						;st(3)
		fld qword ptr ds:[yMin]						;st(2)
		fild word ptr ds:[ScreenHeight]					;st(1)
		fldz								;st(0) - calculated p
		
		fadd st(0), st(3)						;p = yMax
		fsub st(0), st(2)						;p = yMax - yMin
		fmul st(0), st(4)						;p = (yMax - yMin)*M
		fdiv st(0), st(1)						;p = (yMax - yMin)*M/SCREEN_H
		fadd st(0), st(2)						;p = (yMax - yMin)*M/SCREEN_W + yMin
		
		fstp qword ptr ds:[Q]					
		fstp st(0)							;remove from stack
		fstp st(0)
		fstp st(0)
		fstp st(0)
	ret
	
	determinePixelColor:							;al = 1 || al = 0
		push cx
		
		fld qword ptr ds:[P]						;st(7) = p
		fld qword ptr ds:[Q]						;st(6) = q
		fldz								;st(5) = x
		fldz								;st(4) = y
		fldz								;st(3) = x*x
		fldz								;st(2) = y*y
		fldz								;st(1) = tmp
		fldz								;st(0) - auxiliary variable
		
		mov cx, 1000d
		detPixelColLoop:
		
			;tmp (1st iteration: x*x == 0 && y*y == 0)
			fsub st(0), st(0)					;av = 0
			fadd st(0), st(3)					;av = x*x
			fsub st(0), st(2)					;av = x*x - y*y
			fadd st(0), st(7)					;av = x*x - y*y + p
			fxch st(1)						;swap(av, tmp)
			
			
			;y
			fsub st(0), st(0)					;av = 0
			fadd st(0), st(5)					;av = x
			fmul st(0), st(4)					;av = x*y
			fadd st(0), st(0)					;av = 2*x*y
			fadd st(0), st(6)					;av = 2*x*y + q
			fxch st(4)						;swap(av, y)
			
			;x
			fsub st(0), st(0)					;av = 0
			fadd st(0), st(1)					;av = tmp
			fxch st(5)						;swap(av, x)
			
			;x*x
			fsub st(0), st(0)					;x*x = 0
			fadd st(0), st(5)					;x*x = x
			fmul st(0), st(0)					;x*x = x*x
			fxch st(3)						;swap(av x*x)
			
			;y*y
			fsub st(0), st(0)					;y*y = 0
			fadd st(0), st(4)					;y*y = y
			fmul st(0), st(0)					;y*y = y*y
			fxch st(2)						;swap(av, y*y)
			
			;x*x + y*y to auxiliary variable
			fsub st(0), st(0)					;av = 0
			fadd st(0), st(3)					;av = x*x
			fadd st(0), st(2)					;av = x*x + y*y
			
			fcom qword ptr ds:[CompValue]				;compares st(0) with CompValue and sets FPU flags
			fstsw ax						;copies FPU flag register to AX
			sahf							;copies AH to CPU's flags register
			ja breakDetPixelColLoop
			
		loop detPixelColLoop
		
		mov al, 1d							;1000 iterations done correctly
		jmp detPixelColEnd
		
		breakDetPixelColLoop:
		mov al, 0
			
		detPixelColEnd:
		fstp st(0)
		fstp st(0)
		fstp st(0)
		fstp st(0)
		fstp st(0)
		fstp st(0)
		fstp st(0)
		fstp st(0)
		
		pop cx
	ret
	
	drawSet:
		push cx
		push dx
		push ax
		
		xor ah, ah							;switch to graphic mode
		mov al, 13d
		int 10h
		
		xor cx, cx							;columns (Y)
		xor dx, dx							;rows (X)
		
		loopY:
			cmp dx, word ptr ds:[ScreenHeight]
			je loopYBreak
			
			loopX:
				cmp cx, word ptr ds:[ScreenWidth]
				je loopXBreak
				
				mov ax, cx
				call getP
				
				mov ax, dx
				call getQ
				
				call determinePixelColor
				
				cmp al, 0d
				je blackPixel
					mov al, 0Fh				;white color
					jmp showPixel
				
				blackPixel:
					xor al, al				;black color
				
				showPixel:
				mov ah, 0Ch					;draw pixel
				int 10h
				inc cx
				jmp loopX

			loopXBreak:
				inc dx
				xor cx, cx
				jmp loopY
			
		loopYBreak:
		
		xor ax, ax							;wait for any key
		int 16h
		
		xor ah, ah
		mov al, 3d							;end of graphic mode
		int 10h
		
		pop ax
		pop dx
		pop cx
	ret
	
	print:									;prints text from ds:dx, use "mov dx, offset <label>" before
		push ax
		
		mov ah, 9h
		int 21h
		
		pop ax
	ret
	
	handleError:
		push ax
		
		call print
		mov ax, 04c00h
		int 21h	
		
		pop ax
	ret
	
CODE ends

STACK1 segment STACK
					dw 127 dup(?)
		StackPointer		dw ?

STACK1 ends

end START	
