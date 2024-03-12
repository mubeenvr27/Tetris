;COAL PROJECT
;SUBMITTED BY: ALI TARIQ (22L-6765)
;              MUHAMMAD MUBEEN (22L-6707)


[org 0x0100] 

;mov ax,0x0054
;int 0x10

jmp start
 
;String Data and length of each string
gameLevel: dw 0
gamePauseMessage: db 'Game Paused, Press any key to continue . . . '
gamePauseMessageLength: dw 45
LeveledUpMessage: db 'You Leveled Up! Get Ready for a tougher challenge!'
LeveledUpMessageLength: dw 50
gameLevelMessage: db 'Game Level : '
gameLevelMessageLength: dw 13
message1: db 'Score' ;
length1: dw 5 
message2: db 'Time'
length2: dw 4 
message3: db 'Next Block'
length3: dw 10 
starting_message: db 'Press any key to start the game',0
length4: dw 31
ending_message: db 'Game Over',0
length5:dw 9
end_score: db 'Your score is: ',0
length6 :dw 13
your_score: dw 0

stdrow: dw 5
stdcol: dw 82

oldisr: dd 0
tick_count: dw 0
total_sec: dw 0
total_min: dw 0


nextRow: dw 30
nextCol: dw 22

blockPos: dw 0,0,0,0 ;general block position, to use for down movement
checkDownPos: dw 0,0,0,0 ;block pos to check if a block can move down
downCheckDim: dw 0 ;number of block to check, if down movement is possible

LCheckPos: dw 0,0,0,0 ;block pos to check if a block can move left 
RCheckPos: dw 0,0,0,0 ;block pos to check if a block can move right
LRCheckDim: dw 0      ;number of blocks to check if Right left movement is possible

LBlockPos: dw 0,0,0,0 ;Block pos to move left
RBlockPos: dw 0,0,0,0 ;Block pos to move right



freq:   incbin "freq.bin"
dura:   incbin "duration.bin"
ticks_left: dw 0
loc: dw 0


randNum: dw 0

randGen:
        push bp
        mov bp, sp
        push cx
        push dx
        push ax
        rdtsc                   ;getting a random number in ax dx
        xor dx,dx               ;making dx 0
        mov cx, [bp + 4]
        div cx                  ;dividing by 'Paramter' to get numbers from 0 - Parameter
        mov [randNum], dl      ;moving the random number in variable
        pop ax
        pop dx
        pop cx
        pop bp

        ret 2


clrscrn:
         push ax
         push di
         push es
         mov ax,0xb800
         mov es,ax
         mov di,0
    nextloc1234:
                 mov word[es:di],0x6EB2
                 add di,2
                 cmp di,11352
                 jne nextloc1234
         pop es
         pop di
         pop ax
ret

Ending_Clear_Screen:
         push ax
         push di
         push es
         mov ax,0xb800
         mov es,ax
         mov di,0
    nextloc12:
                mov word[es:di],0x70B2
                add di,2
                cmp di,11352
                jne nextloc12
         pop es
         pop di
         pop ax
ret

printstr: ;Parameters: Attribute, StringLoc, Lenght of String.

          ;Preserving Registers:
          push bp 
          mov bp, sp 
          push es 
          push ax 
          push cx 
          push si 
          push di 

          mov ax, 0xb800 
          mov es, ax 

          ;Calculating Address to print at:
          mov al, 132
          mul byte [bp+10]
          add ax, [bp+12]
          shl ax, 1
          mov di,ax ; point di to required location 

          mov si, [bp+6] ; point si to string 
          mov cx, [bp+4] ; load length of string in cx 
          mov ax, [bp+8] ; load attribute in ah 
    
    ;Printing all characters of the string on screen:
    nextchar:
             mov al, [si]
             mov [es:di],ax
             add di, 2
             add si, 1
             loop nextchar

    ;Restoring Registers:
    pop di 
    pop si 
    pop cx 
    pop ax 
    pop es 
    pop bp 
ret 10 

R1:
         ;Preserving Registers:
          push bp 
          mov bp, sp 
          push es 
          push ax 
          push cx 
          push si 
          push di 

          mov ax, 0xb800 
          mov es, ax
          
          ;Address Calculation:
          xor ax,ax
          mov al, 132         ; load al with columns per row 
          mul word[bp+8]      ; multiply with y position 
          add ax, [bp+6]      ; add x position 
          shl ax, 1           ; turn into byte offset 
          mov di,ax           ; point di to required location 

          mov cx, [bp+4] 
          sub cx, [bp+6]      ; load length of string in cx 
          mov ah, 0x7B
          mov al, 0x20

    nextchar1:
             mov [es:di], ax 
             add di, 2
             loop nextchar1 

    ;Restoring Registers
    pop di 
    pop si  
    pop cx 
    pop ax 
    pop es 
    pop bp 
ret 6 


C1: ;Parameters: Start row, Start Col, End row
     push bp 
     mov bp, sp 
     push es 
     push ax
     push bx
     push cx 
     push si 
     push di 
     
     mov ax, 0xb800 
     mov es, ax         ; point es to video base 
     xor ax,ax
     mov al, 132         ; load al with columns per row 
     mul word[bp+8]   ; multiply with y position 
     add ax, [bp+6]  
     shl ax,1
     mov di,ax
     mov cx, [bp+4]
     sub cx, [bp+8]
     add cx,1
     mov ah, 0x7B
     mov al, 0x20
    l1:
        mov [es:di], ax 
        add di,264
        loop l1
    
     pop di
     pop si
     pop cx
     pop bx
     pop ax
     pop es
     pop bp
ret 6





Middle_Screen_Main_Rectangle:
          push bp 
          mov bp, sp 
          push es 
          push ax 
          push cx 
          push si 
          push di 
    ; Upper row of main triangle
     push word [bp+10]
     push word [bp+8]
     push word [bp+4]
     call R1

     push word [bp+6]
     push word [bp+8]
     push word [bp+4]
     call R1
     
     ; Middle column
     push word 0x0001
     push word 40
     push word 41
     call C1


    ;Left column 
     push word [bp+10]
     push word [bp+8]
     push word [bp+6]
     call C1


    ; Right column
     push word [bp+10]
     push word [bp+4]
     push word [bp+6]
     call C1


  pop di 
     pop si  
     pop cx 
     pop ax 
     pop es 
     pop bp 
 ret 8




R112:
          push bp 
          mov bp, sp 
          push es 
          push ax 
          push cx 
          push si 
          push di 
          mov ax, 0xb800 
          mov es, ax         ; point es to video base 
          xor ax,ax
          mov al, 132         ; load al with columns per row 
          mul word[bp+8]   ; multiply with y position 
          add ax, [bp+6]    ; add x position 
          shl ax, 1          ; turn into byte offset 
          mov di,ax          ; point di to required location 
          mov cx, [bp+4] 
          sub cx, [bp+6]    ; load length of string in cx 
          mov ah, 0xC8
          mov al, 0x20
          mov [es:di], ax        ; load attribute in ah 
    nextchar112:
             mov [es:di], ax ; show this char on screen 
             add di, 2       ; move to next screen location 
             loop nextchar112   ; repeat the operation cx times 
     pop di 
     pop si  
     pop cx 
     pop ax 
     pop es 
     pop bp 
 ret 6 


C112: ;Start row, Start Col, end row
     push bp 
     mov bp, sp 
     push es 
     push ax
     push bx
     push cx 
     push si 
     push di 
     mov ax, 0xb800 
     mov es, ax         ; point es to video base 
     xor ax,ax
     mov al, 132         ; load al with columns per row 
     mul word[bp+8]   ; multiply with y position 
     add ax, [bp+6]  
     shl ax,1
     mov di,ax
     mov cx, [bp+4]
     sub cx, [bp+8]
     add cx,1
     mov ah, 0xC8
     mov al, 0x20
    l112:
        mov [es:di], ax 
        add di,264
        loop l112
    
     pop di
     pop si
     pop cx
     pop bx
     pop ax
     pop es
     pop bp
ret 6


Next_Block_Rectangle:
          push bp 
          mov bp, sp 
          push es 
          push ax 
          push cx 
          push si 
          push di 
    ; Upper row of main triangle
     push word [bp+10]
     push word [bp+8]
     push word [bp+4]
     call R112

     push word [bp+6]
     push word [bp+8]
     push word [bp+4]
     call R112
     


    ;Left column 
     push word [bp+10]
     push word [bp+8]
     push word [bp+6]
     call C112


    ; Right column
     push word [bp+10]
     push word [bp+4]
     push word [bp+6]
     call C112


  pop di 
    pop si  
    pop cx 
    pop ax 
    pop es 
    pop bp 
 ret 8









ending_screen:
     push es
     push di
     push ax
     push cx

     mov ax,8                                      ;ending screen
     push ax
     mov ax,11
     push ax
     mov ax,0x0A00
     push ax
     mov ax,ending_message
     push ax
     push word[length5]
     call printstr
   ; call delay

     mov ax,8
     push ax
     mov ax,13
     push ax
     mov ax,0x0A00
     push ax
     mov ax,end_score
     push ax
     push word[length6]
     call printstr
    ;call delay

     mov ax,3490                     ;end score position
     push ax
     mov ax,[your_score]
     push ax
     call print_num

     pop cx                                           ;ending of ending screen
     pop ax
     pop di
     pop es
ret

gamePause:
    push ax
    push bx

    pauseGame:
        call printGamePause
        xor ax,ax
        mov ah,0x1
        int 21h
        call clearGamePaused
        pop ax
        pop bx
        jmp returnGamePause


printGamePause:
    push ax
    push bx

    mov ax,62
    push ax
    mov ax,3
    push ax
    mov ax,0x0700
    push ax
    mov ax,gamePauseMessage
    push ax
    push word[gamePauseMessageLength]
    call printstr

    pop bx
    pop ax
    ret


clearGamePaused:
    push ax
    push bx
    push cx
    push es
    push di

    mov ax,0xb800
    mov es,ax
    mov cx,50
    mov ax,0x6EB2
    mov di,916

    clearMessage:
        mov [es:di],ax
        add di,2
        sub cx,1
        jnz clearMessage

    pop di
    pop es
    pop cx
    pop bx
    pop ax
    ret

    

starting_screen:
     push es
     push di
     push ax
     push cx

     mov ax,51                                      ;start
     push ax
     mov ax,37
     push ax
     mov ax,0x0A00
     push ax
     mov ax,starting_message
     push ax
     push word [length4]
     call printstr

    pop cx                                           ;ending of starting screen
    pop ax
    pop di
    pop es
    ret


print_num:                                               ;printing the numerical data
     push bp 
     mov  bp, sp 
     push es 
     push di 
     push ax 
     push bx 
     push cx 
     push dx 

     mov  ax, 0xb800 
     mov  es, ax         
     mov  ax, [bp+4]
     mov  bx, 10           
     mov  cx, 0

nextdigit12:
     mov  dx, 0
     div  bx  
     add  dl, 0x30
     push dx          
     inc  cx                
     cmp  ax, 0       
     jnz  nextdigit12   
     mov  di, [bp+6]      

nextpos65:
     pop  dx                 
     mov  dh, 0x0A  
     mov [es:di], dx
     add  di, 2             
     loop nextpos65           
 
     pop  dx 
     pop  cx 
     pop  bx 
     pop  ax 
     pop  di 
     pop  es 
     pop  bp 
ret  4








printBlock:;Parameter: Location(rows, column), colour(ah: attribute,al: 0).
    ;preserving resgisters:
     push bp
     mov bp,sp
     push es
     push ax
     push bx

    ;calculating location to print the block
     xor ax,ax
     mov al, 132 ; load al with columns per row 
     mul word[bp+8]  ; multiply with x position 
     add ax, [bp+6]  
     shl ax,1
     mov di,ax ;location moved to di

     mov ax, 0xb800
     mov es, ax ; Setting es to video base
     xor ax,ax
     mov ax,[bp+4]

    return:
     mov [es:di], ax ;printing at the specific location.
     add di,2
     mov [es:di], ax ;printing at the specific location.
    
     pop bx
     pop ax
     pop es
     pop bp
ret 6

TBlock: ;parameters: locations (row,col)
     push bp
     mov bp,sp
     push ax ;to use for rows
     push bx ;to use for cols
     push cx ;to use for attributes 

     mov ax,[bp+6]
     push ax
     mov bx,[bp+4]
     push bx
     mov cx, 0x2AB2
     push cx
call printBlock ;top block
    sub di,2
    mov word[blockPos+6], di
    mov word[LBlockPos+0],di
    mov word[RCheckPos+0],di
    mov word[RBlockPos+0],di
    mov word[LCheckPos+0],di

     add ax,1
     push ax
     sub bx,2
     push bx
     mov cx, 0x2AB2
     push cx
call printBlock ;borrom left
    sub di,2
    mov word[blockPos+0], di
    mov word[checkDownPos+0],di
    mov word[LCheckPos+2],di
    mov word[LBlockPos+2],di
    mov word[RBlockPos+6],di

     push ax
     add bx,2
     push bx
     mov cx, 0x2A20
     push cx
call printBlock ;bottom middle
    sub di,2
    mov word[blockPos+2], di
    mov word[checkDownPos + 2],di
    mov word[LBlockPos+4],di
    mov word[RBlockPos+4],di

     push ax
     add bx,2
     push bx
     mov cx, 0x2AB2
     push cx
call printBlock ;bottom right
    sub di,2
    mov word[blockPos+4], di
    mov word[checkDownPos + 4],di
    mov word[RCheckPos+2],di
    mov word[RBlockPos+2],di
    mov word[LBlockPos+6],di

    mov ax,3
    mov word[downCheckDim], ax
    mov ax,2
    mov word[LRCheckDim],ax

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

LBlock:
    push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
    push ax
    mov bx,[bp+4]
    push bx
    mov cx, 0x4CB2
    push cx
    call printBlock ;top block
    sub di,2
    mov word[blockPos+6],di
    mov word[LCheckPos+4],di
    mov word[LBlockPos+6],di
    mov word[RCheckPos+4],di
    mov word[RBlockPos+6],di

    add ax,1
    push ax
    push bx
    mov cx, 0x4C20
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+4],di
    mov word[LCheckPos+2],di
    mov word[LBlockPos+4],di
    mov word[RCheckPos+2],di
    mov word[RBlockPos+4],di

    add ax,1
    push ax
    push bx
    mov cx, 0x4CB2
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+2],di
    mov word[checkDownPos + 2],di
    mov word[LCheckPos+0],di
    mov word[LBlockPos+0],di
    mov word[RBlockPos+2],di

    push ax
    add bx,2
    push bx
    mov cx, 0x4C20
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+0],di
    mov word[checkDownPos + 0],di
    mov word[LBlockPos+2],di
    mov word[RCheckPos+0],di
    mov word[RBlockPos+0],di

    mov ax,2
    mov word[downCheckDim],ax
    mov ax,3
    mov word[LRCheckDim],ax

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

BoxBlock:
    push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
     push ax
     mov bx,[bp+4]
    push bx
    mov cx, 0x19B2
    push cx
    call printBlock ;top block
    sub di,2
    mov word[blockPos+4],di
    mov word[LCheckPos+2],di
    mov word[LBlockPos+4],di
    mov word[RBlockPos+6],di

    push ax
    add bx,2
    push bx
    mov cx, 0x1920
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+6],di
    mov word[LBlockPos+6],di
    mov word[RCheckPos+2],di
    mov word[RBlockPos+4],di

    add ax,1
    push ax
    push bx
    mov cx, 0x19B2
    push cx
    call printBlock ;bottom right
    sub di,2
    mov word[blockPos+2],di
    mov word[checkDownPos + 2],di
    mov word[LBlockPos+2],di
    mov word[RCheckPos+0],di
    mov word[RBlockPos+0],di

    push ax
    sub bx,2
    push bx
    mov cx, 0x1920
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+0],di
    mov word[checkDownPos + 0],di
    mov word[LCheckPos + 0],di
    mov word[LBlockPos+0],di
    mov word[RBlockPos +2],di

    mov ax,2
    mov word[downCheckDim],ax
    mov word[LRCheckDim],ax

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

SBlock:
     push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
    push ax
    mov bx,[bp+4]
    push bx
    mov cx, 0x3B20
    push cx
    call printBlock ;top left
    sub di,2
    mov word[blockPos+4],di
    mov word[LCheckPos+2],di
    mov word[LBlockPos+4],di
    mov word[RBlockPos+6],di


    push ax
    add bx,2
    push bx
    mov cx, 0x3BB2
    push cx
    call printBlock ;top right
    sub di,2
    mov word[blockPos+6],di
    mov word[LBlockPos+6],di
    mov word[RCheckPos+2],di
    mov word[RBlockPos+4],di

    add ax,1
    push ax
    sub bx,2
    push bx
    mov cx, 0x3BB2
    push cx
    call printBlock ;bottom right
    sub di,2
    mov word[blockPos+2],di
    mov word[checkDownPos+2],di
    mov word[LBlockPos + 2],di
    mov word[RCheckPos+ 0],di
    mov word[RBlockPos+0],di

    push ax
    sub bx,2
    push bx
    mov cx, 0x3B20
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+0],di
    mov word[checkDownPos+0],di
    mov word[LCheckPos+0],di
    mov word[LBlockPos+0],di
    mov word[RBlockPos+2],di

    mov ax,2
    mov word[downCheckDim],ax
    mov word[LRCheckDim],ax

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

IBlock:
     push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
     push ax
     mov bx,[bp+4]
    push bx
    mov cx, 0x5D20
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+6],di
    mov word[LCheckPos+6],di
    mov word[LBlockPos+6],di
    mov word[RCheckPos+6],di
    mov word[RBlockPos+6],di

    add ax,1
    push ax
    push bx
    mov cx, 0x5DB2
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+4],di
    mov word[LCheckPos+4],di
    mov word[LBlockPos+4],di
    mov word[RCheckPos+4],di
    mov word[RBlockPos+4],di

    add ax,1
    push ax
    push bx
    mov cx, 0x5D20
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+2],di
    mov word[LCheckPos+2],di
    mov word[LBlockPos+2],di
    mov word[RCheckPos+2],di
    mov word[RBlockPos+2],di

    add ax,1
    push ax
    push bx
    mov cx, 0x5DB2
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+0],di
    mov word[checkDownPos+0],di
    mov word[LCheckPos+0],di
    mov word[LBlockPos+0],di
    mov word[RCheckPos+0],di
    mov word[RBlockPos+0],di

    mov ax,1
    mov word[downCheckDim],ax
    mov ax,4
    mov word[LRCheckDim],ax

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

JBlock:
     push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
     push ax
     mov bx,[bp+4]
    push bx
    mov cx, 0x6E20
    push cx
    call printBlock ;top
    sub di,2
    mov word[blockPos+6],di
    mov word[LCheckPos+4],di
    mov word[LBlockPos+6],di
    mov word[RCheckPos+4],di
    mov word[RBlockPos+6],di

    add ax,1
    push ax
    push bx
    mov cx, 0x6EB2
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+4],di
    mov word[LCheckPos+2],di
    mov word[LBlockPos+4],di
    mov word[RCheckPos+2],di
    mov word[RBlockPos+4],di

    add ax,1
    push ax
    push bx
    mov cx, 0x6E20
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+2],di
    mov word[checkDownPos+2],di
    mov word[LBlockPos+2],di
    mov word[RCheckPos+0],di
    mov word[RBlockPos+0],di

    push ax
    sub bx,2
    push bx
    mov cx, 0x6EB2
    push cx
    call printBlock
    sub di,2
    mov word[blockPos+0],di
    mov word[checkDownPos+0],di
    mov word[LCheckPos+0],di
    mov word[LBlockPos+0],di
    mov word[RBlockPos+2],di

    mov ax,2
    mov word[downCheckDim],ax
    mov ax,3
    mov word[LRCheckDim],ax

    pop cx
    pop bx
    pop ax
    pop bp
ret 4


Next_Rectangle_Block:
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,30
    push ax
    mov bx,22
    push bx
    mov cx, 0x1920
    push cx
    call printBlock

    push ax
    add bx,2
    push bx
    mov cx, 0x19B2
    push cx
    call printBlock

    add ax,1
    push ax
    push bx
    mov cx, 0x1920
    push cx
    call printBlock

    push ax
    add bx,2
    push bx
    mov cx, 0x19B2
    push cx
    call printBlock

    pop ax
    pop bx
    pop cx
ret


;------------------------------------------------------------------------- START OF TALAL -----------------------------------------------------------------------------------

magic:
    jmp Main
    totalcells: dw  11352	
    total_rows: dw 43
    total_col: dw 132
    divider1: dw 0  ;storing ending address of 1/3 of screen
    divider2: dw 0	;storing ending address of 2/3 of screen
    space: dw 40	

Animation:
	call Left
	call delay
	call Right
ret

delay:
    push cx
    mov cx,0x3fff
llll1:
    loop llll1
    pop cx
    ret


Left:
push ax
push bx
push cx
push dx
push di
push si

 mov ax,0xb800
 mov es,ax
 mov ds,ax


;shifts screen to left
mov dx,14
mov si,3962
mov di,3960
mov bx,3960

lop:
mov cx,132
mov di,bx
mov si,di
add si,2

rep movsw
 
add bx,264
dec dx
jnz lop

mov cx,14 
mov si,3960
mov di,4222
mov bx,3960

;shifts  leftmost corner
cornerL:
mov si,bx
mov di,si
add di,262
movsw
add bx,264
loop cornerL


pop si
pop di
pop dx
pop cx
pop bx
pop ax
ret


Right:
push ax
push bx
push cx
push dx
push ds
push es
push di
push si

 mov ax,0xb800
 mov es,ax
 mov ds,ax

;shifts screen to right
mov dx,14
mov si,260
mov di,262
mov bx,262
mov bp,262

R:
mov cx,132

 shiftR:
 mov di,bx
 mov si,di
 sub si,2
 movsw
 sub bx,2
 loop shiftR
 
 add bp,264
 mov bx,bp
 dec dx
 jnz R

mov cx,14
mov si,262
mov bp,262

mov di,0
mov bx,0

;shifts  rightmost corner
cornerR:
 mov di,bx
 mov si,bp
 movsw
 add bx,264
 add bp,264
 loop cornerR

call delay
call delay
call delay
call delay
call delay
call delay

pop si
pop di
pop es
pop ds
pop dx
pop cx
pop bx
pop ax
ret

clrscr:		push es
			push ax
			push di

			mov ax, 0xb800
			mov es, ax					; point es to video base
			mov di, 0					; point di to top left column
			mov ah, 0x07
			mov al, 0x20

    nextloc121:	mov word [es:di], AX	; clear next char on screen
                add di, 2					; move to next screen location
                cmp di,	[totalcells]          		; 132x43x2
                jb nextloc121				; if no clear next position

                pop di
                pop ax
                pop es
                ret

rectangles:    			;parameters(starting offset,lenght,width,colour)//lenght and width are in number of cells
		push bp
		mov bp,sp
		push ax
		push bx
		push cx
		push dx
		push di
		
		mov ax,0xb800
		mov es,ax
		mov di,[bp+4]		;starting offset
		mov cx,[bp+6]		;lenght
		mov dx,[bp+8]		;width
		mov bx,dx			;storing width because used multiple times
		mov ax,[bp+10]		;setting style
		mov al,0x20			;space ascii
outer:				;for multiple rows
	mov dx,bx		;restoring width for counter
nex:				;for each row
		mov word [es:di],ax
		add di,2
		sub dx,1
		jnz nex
	add word [bp+4],264	;going to next row
	mov di,[bp+4]		;going back to starting column
	sub cx,1
	jnz outer
	
		pop di
		pop dx
		pop cx
		pop bx
		pop ax
		pop bp
		ret 8
		
		
		
dividers:
   
    push es
    push ax
    push bx
    push cx
    push dx 
    push di

;pointing to videobase
   mov ax,0xb800
   mov  es,ax
   
   ;132*(rows/3)+col   1/3 divider
	mov ax,0x2B
	mov bx,0x3
	xor dx,dx
	
	div bx
	mov bx,ax
	
	mov  ax, [total_col]
	mul bx
	shl ax,1
	mov [divider1],ax
	mov di,ax

    mov ah, 0x07
    mov al, '-'
	
	mov cx,[total_col]
	
loop1:
    mov [es:di], ax
    add di,2
    dec cx
    jnz loop1
	
	
	  ;132*rows*2/3+col  2/3 divider
	mov ax,0x2B
	shl ax,1
	mov bx,0x3
	xor dx,dx
	
	div bx
	mov bx,ax
	
	mov  ax, [total_col]
	mul bx
	shl ax,1
	mov [divider2],ax
	mov di,ax

    mov ah, 0x07
    mov al, '-'
	
	mov cx,[total_col]
	
loop2:
mov [es:di], ax
    add di,2
    dec cx
    jnz loop2

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es
    ret 
	
windows:		;(offset,lenght,width)
	push bp
	mov bp,sp
	sub sp,4	;2 local variables
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	
	mov ax,[bp+8] ;width
	mov bx,3		;making 3 windows width wise
	mov dx,0
	div bx
	sub ax,1
	mov word [bp-2],ax	;storing width
	
	mov ax,[bp+6] ;lenght
	mov bx,3		;making 3 windows lenght wise
	mov dx,0
	div bx
	mov word [bp-4],ax		;storing lenght
	
	mov di,[bp+4]			;starting address
	mov cx,[bp-4]			;counter for reaching correct offset
	mov dx,[bp-2]			;counter for reaching correct offset
	mov bx,3
	add di,[total_col]		;skipping a row
	add di,[total_col]
	
	l1211:
		mov ax,3
		mov si,di
		l12i:
			mov dx,[bp-2]
			k:
				add si,2
				dec dx
				jnz k
			push 0x7000
			push 1
			push 1
			push si
			call rectangles
			dec ax
			jnz l12i
		mov cx,[bp-4]
		w:
			add di,[total_col]
			add di,[total_col]
			dec cx
			jnz w
		dec bx
		jnz l1211
	
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	add sp,4
	pop bp
	ret 6

buildings:
	push 0x4000
	push 10
	push 13
	push 280
	call rectangles
	push 10
	push 13
	push 280
	call windows
	push 0x1000
	push 15
	push 10
	push 1094
	call rectangles
	push 15
	push 10
	push 1094
	call windows
	push 0x6000
	push 35
	push 7
	push 1924
	call rectangles
	push 35
	push 7
	push 1924
	call windows
	push 0x4000
	push 10
	push 13
	push 420
	call rectangles
	push 10
	push 13
	push 420
	call windows
	push 0x1000
	push 15
	push 10
	push 1240
	call rectangles
	push 15
	push 10
	push 1240
	call windows
	ret
background:
	push ax
	push cx
	push di
	
	mov ax,0xb800
	mov es,ax
	mov di,0
	mov cx,[divider1]  ;only working till first dividing
	shr cx,1		;number of cells from offset
	
	
nextchar123:		;background colour
	mov word [es:di], 0x3020	
			add di, 2					
			loop nextchar123
			
    mov di,[divider1]
    add di,264
    mov cx,[divider1]
    shr cx,1
    sub cx,132

    nextchar124:		;background colour
	mov word [es:di], 0x7020	
			add di, 2					
			loop nextchar124

    mov di,[divider2]
    add di,264
    mov cx,[divider1]
    shr cx,1

     nextchar125:		;background colour
	mov word [es:di], 0x3020	
			add di, 2					
			loop nextchar125
    
	call buildings	;building in background
			
		pop di
		pop cx
		pop di
		ret

printscreen1122:
	call dividers
	call background
ret
	

Main:	
; following code just changes your screen  resolution to 43x132 Mode
    mov ah,0x00
    mov al, 0x54
    int 0x10
	call printscreen1122

system_pause:
    push ax

    mov ah,0x1
    int 0x21

    pop ax
    ret
ret



clear_time_cell:
push es
push ax

mov ax,0xb800
mov es,ax
mov word[es:5064],0x7020

pop ax
pop es
ret



; timer interrupt service routine 
timer: 

	push ax
	push bx
	push si
	cmp word[ticks_left],0
	ja hehe
	
	
	
	
	mov si,[loc]
	mov ax,[dura + si]
	mov [ticks_left],ax
	mov ax,[freq+si]
	add si,2
	mov [loc],si
	call speakerOn
	
	mov ax,dura
	sub si,2
	mov bx,freq
	add bx,si
	cmp ax,bx
	jae yille
	mov word[loc],0
	mov word[ticks_left],0
	
	jmp yille
hehe:
	dec word[ticks_left]
	
	

yille:
	pop si
	pop bx
	pop ax

push ax 
inc word [cs:tick_count]            ; increment tick count 
cmp word [cs:tick_count],18
jl less
inc word[cs:total_sec]
mov word[cs:tick_count],0

cmp word[cs:total_sec],60
jl nnn
inc word[cs:total_min]
mov word[cs:total_sec],0
nnn:
call clear_time_cell


push 5062
push word [cs:total_sec] 
call print_num
push 5054
push word [cs:total_min] 


call print_num                      ; print tick count
less: 
;mov al, 0x20 
;out 0x20, al                       ; end of interrupt 
pop ax 
jmp far [cs:oldisr]
;iret                               ; return from interrupt 



speakerOn:
        and     ax, 0xfffe
        push    ax
        cli
        mov     al, 0xb6
        out     0x43, al
        pop     ax
        out     0x42, al
        mov     al, ah
        out     0x42, al
        in      al, 0x61
        mov     al, ah
        or      al, 3
        out     0x61, al
        sti
    ret

;;; speakerOff - silences the PC Speaker.
;;; Trashes AX.
speakerOff:
        in      al, 0x61
        and     al, 0xfc
        out     0x61, al
        ret





start:
; following code just changes your screen  resolution to 43x132 Mode
     mov ax, 0x0054        
     int 10h
     cli
        mov dx, 0x43
        mov al, 0x36
        out dx, al
        mov dx, 0x40
        mov al, 0xA9 ;(1193180 / 1000) & 0xFF
        out dx, al
        mov al, 0x04 ;(1193180 / 1000) >> 8
        out dx, al
        sti
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                              ; enable interrupts 




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    mov word[tick_count],0
    mov word[total_sec],0
    mov word[total_min],0




 call clrscrn

call magic   ; picture in starting screen eg: buildings , roads etc
call starting_screen  ; "Press Enter" message
animationLoop:
    xor ax,ax

    mov ah,0x1
    int 0x16

    jz nokeyanimation
    jmp afterloop
    nokeyanimation: call Right
    jmp animationLoop

afterloop:
call starting_screen  ; "Press Enter" message
call system_pause

      ; Print Score String
        call clrscrn


   xor ax, ax 
mov es, ax                         ; pointes to IVT base 
mov ax,[es:8*4]
mov [oldisr],ax
mov ax,[es:8*4+2]
mov [oldisr+2],ax
cli                                ; disable interrupts 
mov word [es:8*4], timer           ; store offset at n*4 
mov [es:8*4+2], cs                 ; store segment at n*4+2 
sti  


        

        mov ax, 4
        push ax ; push y position 
        mov ax, 3 
        push ax ; push x position 
        mov ax, 0x0A00 ; blue on black attribute 
        push ax ; push attribute 
        mov ax, message1 
        push ax ; push address of message 
        push word [length1] ; push message length 
        call printstr ; call the printstr subroutine 


        ;  print Time String
        mov ax, 4
        push ax ; push y position 
        mov ax, 14 
        push ax ; push x position 
        mov ax, 0x0A00 ; blue on black attribute 
        push ax ; push attribute 
        mov ax, message2 
        push ax ; push address of message 
        push word [length2] ; push message length 
        call printstr ; call the printstr subroutine 


        
        ; Print next block String
        mov ax, 4
        push ax ; push y position 
        mov ax, 24 
        push ax ; push x position 
        mov ax, 0x0A00 ; blue on black attribute 
        push ax ; push attribute 
        mov ax, message3
        push ax ; push address of message 
        push word [length3] ; push message length 
        call printstr ; call the printstr subroutine 
 

 
       ; Main Rectangle 
        mov ax, 1 
        push ax               ; push x position 
        mov ax, 1 
        push ax               ; push y position 
        mov ax, 41
        push ax               ; push x position 
        mov ax, 130
        push ax               ; push y position 
        call Middle_Screen_Main_Rectangle         ; call the printstr subroutine 
        
        
        call printArena
        mov ax,0xb800
        mov es,ax
        mov di,9636
        mov ax, 0xffb2
        mov [es:di],ax

        ; Small Rectangle middle screen next block recatngle
        mov ax, 26 
        push ax               ; push x position 
        mov ax, 3 
        push ax               ; push y position 
        mov ax, 39
        push ax               ; push x position 
        mov ax, 36
        push ax               ; push y position
        call Next_Block_Rectangle         ; call the printstr subroutine 

        call printArena

        mov ax,3
        push ax
        mov ax,8
        push ax
        mov ax,0x0700
        push ax
        mov ax,gameLevelMessage
        push ax
        push word[gameLevelMessageLength]
        call printstr

        gameLoop:
            
            call printRandomBlock
            call blockCycle
            call clearGamePaused
            call rowCheck

            mov ax,1600
            push ax
            mov ax,[your_score]
            push word ax 
            call print_num

            mov ax,2690
            push ax
            mov ax,[gameLevel]
            push word ax 
            call print_num

            call FirstrowCheck
            mov ax,[total_min]
            cmp ax,5
            jne gameLoop

        call system_pause

        gameEnds:



         mov ax, [oldisr] ; read old offset in ax
 mov bx, [oldisr+2] ; read old segment in bx
 cli ; disable interrupts
 mov [es:8*4], ax ; restore old offset from ax
 mov [es:8*4+2], bx ; restore old segment from bx
 sti ; enable interrupts


        call speakerOff
        call Ending_Clear_Screen          ; call the clrscr subroutine 
        call ending_screen                ; Final score and game over string is printed in this call



        ; Final screen rectangle
        mov ax, 9 
        push ax               ; push x position 
        mov ax, 6 
        push ax               ; push y position 
        mov ax, 16
        push ax               ; push x position 
        mov ax, 35
        push ax               ; push y position
        call Next_Block_Rectangle         ; Final screen rectangle



  
        call system_pause
mov ax, 0x4c00 ; terminate program 
int 0x21

;----------------------------------------------------------------------PHASE 2 CODE---------------------------------------------------------------------------------------------


checkTopRow:
    push ax
    push bx;stores column number
    push cx;stores row number
    push di
    push es



addRow11: ;helper function for Print_NextBlock_Background
    add di,198 ;adding a row
    mov bx,0
    add cx,1
    jmp returnRowAdd11

Print_NextBlock_Background:
         push ax
         push bx;stores column number
         push cx;stores row number
         push di
         push es

         mov bx,0 ;col number is 0 at start
         mov cx,0 ;row number is 0 at start
         mov dx,0
         mov ax,0xb800
         mov es,ax
         mov di,6870 ;row 3, col 65
    nextloc1234511:
                 mov word[es:di],0x0720
                 add di,2
                  

                 add bx,1
                 cmp bx,33 ;20 column arena
                 je addRow11
                 returnRowAdd11: cmp cx,13 ;35 row arena
                 jne nextloc1234511

         pop es
         pop di
         pop cx
         pop bx
         pop ax
ret


addRow: ;helper function for printArena
    add di,188 ;adding a row
    mov bx,0
    add cx,1
    jmp returnRowAdd

printArena:
         push ax
         push bx;stores column number
         push cx;stores row number
         push di
         push es

         mov bx,0 ;col number is 0 at start
         mov cx,0 ;row number is 0 at start
         mov dx,0
         mov ax,0xb800
         mov es,ax
         mov di,1452 ;row 3, col 65
    nextloc12345:
                 mov word[es:di],0x0720
                 add di,2
                  

                 add bx,1
                 cmp bx,38 ;20 column arena
                 je addRow
                 returnRowAdd: cmp cx,33 ;35 row arena
                 jne nextloc12345

         pop es
         pop di
         pop cx
         pop bx
         pop ax
ret

incDXD:
    add dx,1
    jmp backToCheckD

incDXR:
    add dx,1
    jmp backToCheckR

incDXL:
    add dx,1
    jmp backToCheckL

noDown:
    mov dx,0
    jmp returnMove

movedDown:
    push cx
    push bx
    mov cx,0
    mov bx,0
    downPosLoop:
        mov di,[RCheckPos + bx]
        add di, 264
        mov word[RCheckPos + bx],di

        mov di,word[RBlockPos + bx]
        add di,264
        mov word[RBlockPos + bx],di

        mov di,word[LCheckPos + bx]
        add di,264
        mov word[LCheckPos + bx],di

        mov di,word[LBlockPos + bx]
        add di,264
        mov word[LBlockPos + bx],di

        mov di,word[blockPos + bx]
        add di,264
        mov word[blockPos + bx],di

        mov di,word[checkDownPos + bx]
        add di,264
        mov word[checkDownPos + bx],di
        add bx,2
        cmp bx,8
        jne downPosLoop
    pop bx
    pop cx
    jmp downEnd

downCheck: ;doesnt preserve dx value, if dx value is 0, then down move not possible
    push ax
    push bx
    push cx
    push ds
    push es
    push di
    push si

    mov ax,0xb800
    mov es,ax
    mov cx,0
    mov bx,0
    mov dx,0 ;using
    downCheckLoop:
        mov di,[checkDownPos + bx]
        add di, 264
        add bx,2
        inc cx
        mov ax, [es:di]
        cmp ax,0x0720
        je incDXD
        backToCheckD: cmp cx,[downCheckDim]
        jne downCheckLoop
    
    cmp dx,[downCheckDim]
    je callBlockDown
    jne noDown
    returnMove:

    pop si
    pop di
    pop es
    pop ds
    pop cx
    pop bx
    pop ax
ret

callBlockDown:
    call moveBlockDown
    jmp returnMove


moveBlockDown:
    push ax
    push bx
    push cx
    push dx
    push ds
    push es
    push di
    push si

    mov ax,0xb800
    mov es,ax

    mov bx,0
    blockDown:
        mov di,[blockPos + bx]
        mov si,di
        add si,264
        mov ax, [es:di]
        mov word[es:si],ax
        mov ax,0x0720
        mov word[es:di],ax
        add di,2
        add si,2
        mov ax, [es:di]
        mov word[es:si],ax
        mov ax,0x0720
        mov word[es:di],ax
        add bx,2
        cmp bx,8
        jne blockDown

    jmp movedDown
    downEnd:

    pop si
    pop di
    pop es
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
ret

movedRight:
    push cx
    push bx
    mov cx,0
    mov bx,0
    rightPosLoop:
        mov di,[RCheckPos + bx]
        add di, 4
        mov word[RCheckPos + bx],di

        mov di,word[RBlockPos + bx]
        add di,4
        mov word[RBlockPos + bx],di

        mov di,word[LCheckPos + bx]
        add di,4
        mov word[LCheckPos + bx],di

        mov di,word[LBlockPos + bx]
        add di,4
        mov word[LBlockPos + bx],di

        mov di,word[blockPos + bx]
        add di,4
        mov word[blockPos + bx],di

        mov di,word[checkDownPos + bx]
        add di,4
        mov word[checkDownPos + bx],di
        add bx,2
        cmp bx,8
        jne rightPosLoop
    pop bx
    pop cx
    jmp rightEnd

callBlockRight:
    call moveBlockRight
    jmp returnMoveR

rightCheck:
    push ax
    push bx
    push cx
    push dx
    push ds
    push es
    push di
    push si

    mov ax,0xb800
    mov es,ax
    mov cx,0
    mov bx,0
    mov dx,0 ;using
    downCheckLoopR:
        mov di,[RCheckPos + bx]
        add di,4
        add bx,2
        inc cx
        mov ax, [es:di]
        cmp ax,0x0720
        je incDXR
        backToCheckR: cmp cx,[LRCheckDim]
        jne downCheckLoopR
    
    cmp dx,[LRCheckDim]
    je callBlockRight
    returnMoveR:

    pop si
    pop di
    pop es
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
ret


moveBlockRight:
    push ax
    push bx
    push cx
    push dx
    push ds
    push es
    push di
    push si

    mov ax,0xb800
    mov es,ax

    mov bx,0
    blockRight:
        mov di,[RBlockPos + bx]
        mov si,di
        add si,4
        mov ax, [es:di]
        mov word[es:si],ax
        mov ax,0x0720
        mov word[es:di],ax
        add di,2
        add si,2
        mov ax, [es:di]
        mov word[es:si],ax
        mov ax,0x0720
        mov word[es:di],ax
        add bx,2
        cmp bx,8
        jne blockRight

    jmp movedRight
    rightEnd:

    pop si
    pop di
    pop es
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
ret

movedLeft:
    push cx
    push bx
    mov cx,0
    mov bx,0
    leftPosLoop:
        mov di,[RCheckPos + bx]
        sub di, 4
        mov word[RCheckPos + bx],di

        mov di,word[RBlockPos + bx]
        sub di,4
        mov word[RBlockPos + bx],di

        mov di,word[LCheckPos + bx]
        sub di,4
        mov word[LCheckPos + bx],di

        mov di,word[LBlockPos + bx]
        sub di,4
        mov word[LBlockPos + bx],di

        mov di,word[blockPos + bx]
        sub di,4
        mov word[blockPos + bx],di

        mov di,word[checkDownPos + bx]
        sub di,4
        mov word[checkDownPos + bx],di
        add bx,2
        cmp bx,8
        jne leftPosLoop
    pop bx
    pop cx
    jmp LeftEnd


callBlockLeft:
    call moveBlockLeft
    jmp returnMoveL

leftCheck:
    push ax
    push bx
    push cx
    push dx
    push ds
    push es
    push di
    push si

    mov ax,0xb800
    mov es,ax
    mov cx,0
    mov bx,0
    mov dx,0 ;using
    downCheckLoopL:
        mov di,[LCheckPos + bx]
        sub di,2
        add bx,2
        inc cx
        mov ax, [es:di]
        cmp ax,0x0720
        je incDXL
        backToCheckL: cmp cx,[LRCheckDim]
        jne downCheckLoopL
    
    cmp dx,[LRCheckDim]
    je callBlockLeft
    returnMoveL:

    pop si
    pop di
    pop es
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
ret

moveBlockLeft:
    push ax
    push bx
    push cx
    push dx
    push ds
    push es
    push di
    push si

    mov ax,0xb800
    mov es,ax

    mov bx,0
    blockLeft:
        mov di,[LBlockPos + bx]
        mov si,di
        sub si,4
        mov ax, [es:di]
        mov word[es:si],ax
        mov ax,0x0720
        mov word[es:di],ax
        add di,2
        add si,2
        mov ax, [es:di]
        mov word[es:si],ax
        mov ax,0x0720
        mov word[es:di],ax
        add bx,2
        cmp bx,8
        jne blockLeft

    jmp movedLeft
    LeftEnd:

    pop si
    pop di
    pop es
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
ret

callLeftCheck:
    call leftCheck
    jmp returnLeftCheck

callRightCheck:
    call rightCheck
    jmp returnRightCheck

callDownCheck:
    call downCheck
    call DelayLevel
    jmp returnDownCheck

callDelayLevel0:
    call LongDelayLevel0
    jmp afterDelay

callDelayLevel1:
    call LongDelayLevel1
    jmp afterDelay

callDelayLevel2:
    call LongDelayLevel2
    jmp afterDelay

callDelayLevel3:
    call LongDelayLevel3
    jmp afterDelay

DelayLevel:
    push ax
    
    mov ax, [gameLevel]
    cmp ax,0
    je callDelayLevel0
    cmp ax,1
    je callDelayLevel1
    cmp ax,2
    je callDelayLevel2
    jne callDelayLevel3
    afterDelay:

    pop ax
ret

printLeveledUp:
    push ax
    push bx

    mov ax,62
    push ax
    mov ax,3
    push ax
    mov ax,0x0700
    push ax
    mov bx,LeveledUpMessage
    push bx
    push word[LeveledUpMessageLength]
    call printstr

    pop bx
    pop ax
    ret

blockCycle:
    push ax
    push bx
    push cx
    push dx
    push ds
    push es
    push di
    push si


    blockCycleLoop:
        xor ax,ax
        
        mov ah,0x1
        int 0x16

        jz nokey
        mov ah,0x00
        int 0x16
        cmp al,97
        je callLeftCheck
        returnLeftCheck: cmp al,100
        je callRightCheck
        returnRightCheck:cmp al,112
        je gamePause
        cmp al,80
        je gamePause
        returnGamePause


        nokey:
            jmp callDownCheck
            returnDownCheck: cmp dx,0
            jne blockCycleLoop

    pop si
    pop di
    pop es
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax

    ret

printRandomBlock: ;prints a random block and moves a block to next block
    push ax
    mov ax,[randNum]
    cmp ax,1
    je callTBlockMain
    cmp ax,2
    je callLBlockMain
    cmp ax,3
    je callBoxBlockMain
    cmp ax,4
    je callSBlockMain
    cmp ax,5
    je callIBlockMain
    cmp ax,6
    je callJBlockMain
  

    returnPrintMain:

    mov ax,6
    push ax
    call randGen

    call printNextBlock

    pop ax
ret

callTBlockMain:
    push word[stdrow]
    push word[stdcol]
    call TBlock
jmp returnPrintMain
callLBlockMain:
    push word[stdrow]
    push word[stdcol]
    call LBlock
jmp returnPrintMain
callBoxBlockMain:
    push word[stdrow]
    push word[stdcol]
    call BoxBlock
jmp returnPrintMain
callSBlockMain:
    push word[stdrow]
    push word[stdcol]
    call SBlock
jmp returnPrintMain
callIBlockMain:
    push word[stdrow]
    push word[stdcol]
    call IBlock
jmp returnPrintMain
callJBlockMain:
    push word[stdrow]
    push word[stdcol]
    call JBlock
jmp returnPrintMain






printNextBlock:
    push ax

    mov ax,[randNum]
    call Print_NextBlock_Background

    cmp ax,1
    je callTBlockNext
    cmp ax,2
    je callLBlockNext
    cmp ax,3
    je callBoxBlockNext
    cmp ax,4
    je callSBlockNext
    cmp ax,5
    je callIBlockNext
    cmp ax,6
    je callJBlockNext
    cmp ax,7
    

    returnPrintNext:

    pop ax
ret


callTBlockNext:
    push word[nextRow]
    push word[nextCol]
    call TBlockNext
jmp returnPrintNext

callLBlockNext:
    push word[nextRow]
    push word[nextCol]
    call LBlockNext
jmp returnPrintNext

callBoxBlockNext:
    push word[nextRow]
    push word[nextCol]
    call BoxBlockNext
jmp returnPrintNext

callSBlockNext:
    push word[nextRow]
    push word[nextCol]
    call SBlockNext
jmp returnPrintNext

callIBlockNext:
    push word[nextRow]
    push word[nextCol]
    call IBlockNext
jmp returnPrintNext

callJBlockNext:
    push word[nextRow]
    push word[nextCol]
    call JBlockNext
jmp returnPrintNext


    
jmp returnPrintNext

LongDelayLevel0:
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay

       
        ret

LongDelayLevel1:
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay

       
        ret

LongDelayLevel2:
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay

       
        ret

LongDelayLevel3:
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay
        call delay

       
        ret


;------------------------------------------------------------------------------------------------------------------
TBlockNext: ;parameters: locations (row,col)
     push bp
     mov bp,sp
     push ax ;to use for rows
     push bx ;to use for cols
     push cx ;to use for attributes 

     mov ax,[bp+6]
     push ax
     mov bx,[bp+4]
     push bx
     mov cx, 0x2AB2
     push cx
call printBlock ;top block

     add ax,1
     push ax
     sub bx,2
     push bx
     mov cx, 0x2AB2
     push cx
call printBlock ;borrom left

     push ax
     add bx,2
     push bx
     mov cx, 0x2A20
     push cx
call printBlock ;bottom middle

     push ax
     add bx,2
     push bx
     mov cx, 0x2AB2
     push cx
call printBlock ;bottom right

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

LBlockNext:
    push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
    push ax
    mov bx,[bp+4]
    push bx
    mov cx, 0x4CB2
    push cx
    call printBlock ;top block

    add ax,1
    push ax
    push bx
    mov cx, 0x4C20
    push cx
    call printBlock

    add ax,1
    push ax
    push bx
    mov cx, 0x4CB2
    push cx
    call printBlock

    push ax
    add bx,2
    push bx
    mov cx, 0x4C20
    push cx
    call printBlock

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

BoxBlockNext:
    push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
     push ax
     mov bx,[bp+4]
    push bx
    mov cx, 0x19B2
    push cx
    call printBlock ;top block

    push ax
    add bx,2
    push bx
    mov cx, 0x1920
    push cx
    call printBlock

    add ax,1
    push ax
    push bx
    mov cx, 0x19B2
    push cx
    call printBlock ;bottom right

    push ax
    sub bx,2
    push bx
    mov cx, 0x1920
    push cx
    call printBlock

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

SBlockNext:
     push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
    push ax
    mov bx,[bp+4]
    push bx
    mov cx, 0x3B20
    push cx
    call printBlock ;top left

    push ax
    add bx,2
    push bx
    mov cx, 0x3BB2
    push cx
    call printBlock ;top right

    add ax,1
    push ax
    sub bx,2
    push bx
    mov cx, 0x3BB2
    push cx
    call printBlock ;bottom right

    push ax
    sub bx,2
    push bx
    mov cx, 0x3B20
    push cx
    call printBlock

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

IBlockNext:
     push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
     push ax
     mov bx,[bp+4]
    push bx
    mov cx, 0x5D20
    push cx
    call printBlock

    add ax,1
    push ax
    push bx
    mov cx, 0x5DB2
    push cx
    call printBlock

    add ax,1
    push ax
    push bx
    mov cx, 0x5D20
    push cx
    call printBlock

    add ax,1
    push ax
    push bx
    mov cx, 0x5DB2
    push cx
    call printBlock
    sub di,2

    pop cx
    pop bx
    pop ax
    pop bp
ret 4

JBlockNext:
     push bp
    mov bp,sp
    push ax ;to use for rows
    push bx ;to use for cols
    push cx ;to use for attributes

    mov ax,[bp+6]
     push ax
     mov bx,[bp+4]
    push bx
    mov cx, 0x6E20
    push cx
    call printBlock ;top

    add ax,1
    push ax
    push bx
    mov cx, 0x6EB2
    push cx
    call printBlock

    add ax,1
    push ax
    push bx
    mov cx, 0x6E20
    push cx
    call printBlock

    push ax
    sub bx,2
    push bx
    mov cx, 0x6EB2
    push cx
    call printBlock

    pop cx
    pop bx
    pop ax
    pop bp
ret 4


incDXRC:
    add dx,1
    jmp returnRowCheck

removeRow:
    push ax
    push cx
    push es
    push di
    push si

    mov ax,[your_score]
    add ax,10
    mov [your_score],ax
    mov ax,[gameLevel]
    add ax,1
    mov [gameLevel],ax

    mov cx,38
    colRemoveLoop:
            mov ax,0x0720
            mov [es:di],ax
            add di,2
            sub cx,1
            jnz colRemoveLoop
    mov si,di
    sub si,264
    call printLeveledUp
    push ds
    mov ax,0xb800
    mov ds,ax
    std
    
    moveDown:
        mov cx,39
        rep movsw
        call LongDelayLevel3
        sub di,186
        sub si,186
        cmp si,1450
        ja moveDown

   

    pop ds
    pop si
    pop di
    pop es
    pop cx
    pop ax
    jmp rowStart


rowCheck:
    push ax
    push bx ;used to track the number of rows
    push cx ;used to itterate through the row by column
    push dx ;used to check the number of cols checked
    push ds
    push es
    push di
    push si
   rowStart: 
    mov ax,0xb800
    mov es,ax
    mov di,9900 ;mov di to the bottom left of the arena
    mov cx,37
    mov bx,33
    mov dx,0

    rowLoop:
        colLoop:
            mov ax,[es:di]
            cmp ax,0x0720
            jne incDXRC
            returnRowCheck:
            add di,2
            sub cx,1
            jnz colLoop
        sub di,74
        cmp dx,37
        je removeRow
        add di,74
        endRow:
        mov cx,37
        mov dx,0
        sub di,338
        sub bx,1
        jnz rowLoop
        returnRemoveRow:



    pop si
    pop di
    pop es
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
    ret

    incDXFRC:
    add dx,1
    jmp returnFirstRowCheck

    FirstrowCheck:
    push ax
    push bx ;used to track the number of rows
    push cx ;used to itterate through the row by column
    push dx ;used to check the number of cols checked
    push ds
    push es
    push di
    push si

    mov ax,0xb800
    mov es,ax
    mov di,1452 ;mov di to the bottom left of the arena
    mov dx,0
    mov cx,37

        colLoopF:
            mov ax,[es:di]
            cmp ax,0x0720
            jne incDXFRC
            returnFirstRowCheck:
            add di,2
            sub cx,1
            jnz colLoopF
        cmp dx,0
        jne endgame

    pop si
    pop di
    pop es
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
    ret

    

    endgame:
        pop si
        pop di
        pop es
        pop ds
        pop dx
        pop cx
        pop bx
        pop ax
     jmp gameEnds
