;=================
; Initialise DASM
;=================
	
	PROCESSOR 6502

	include vcs.h

	include macro.h

;==================
; Define Constants
;==================

PF_HEIGHT	= 192  

P0_HEIGHT   = 25

;==================
; MACROS
;==================

;---------------------------------
;
; KERNEL MACRO
;
;---------------------------------

        MAC SCREEN_BANDS 
.BAND   SET {1}


;=================================
;
; Preload GRP0 (Che) Graphics
;
;=================================

    lda #P0_HEIGHT      ; 2
    dcp Player0Offset   ; 5
    bcc .p0FlipPre0     ; 2 (3)
    ldy Player0Offset   ; 2

    lda (Player0Ptr),y  ; 5
    tax                 ; 3
    lda (Player0Clr),y  ; 5
    tay                 ; 3 (27)

.p0FlipPre0             ;(3 XX)
                        ;(  XX) - Longest way

;=================================
; Position GRP1 independently for
; each band of the screen.
;
; Accumulator holds X value.
;=================================

.PositionGRP1

    lda EnvObjectX+.BAND
    sec       
    sta WSYNC

;----------------------------------------------------- ENTER FIRST SCANLINE
    
    stx GRP0        ; 3  3
    sty COLUP0      ; 3  6
    
DivideLoop1
    sbc #15         ; 2  8
    bcs DivideLoop1 ; 2 10
    eor #7          ; 2 12
    asl             ; 2 14
    asl             ; 2 16
    asl             ; 2 18
    asl             ; 2 20
    sta RESP1       ; 2 23 <- set object position
    sta HMP1        ; 3 26
    sta WSYNC
;------------------------------------------------------ ENTER SECOND SCANLINE
    sta HMOVE           ; 3  3
    
;=================================
; We haven't had time to preload
; any graphics, so now we have to
; rush and try and make sure we have
; something to draw on this line
;=================================
    
    lda #P0_HEIGHT      ; 2  5
    dcp Player0Offset   ; 5 10
    bcc .p0FlipPre1     ; 2 12 (3 10)
    ldy Player0Offset   ; 3 15

    lda (Player0Ptr),y  ; 5 20
    sta GRP0            ; 3 23     
    lda (Player0Clr),y  ; 5 28
    sta COLUP0          ; 3 31 (28) <-- delayed. So we'll have to restrict
                        ;               Che's movement on the left of the
                        ;               screen?

.p0FlipPre1             ;(3 10)
                        ;  (35)
;=================================
; Preload Che Graphics for THIRD
; scanline.
;=================================

                        
    lda #P0_HEIGHT      ; 2 37
    dcp Player0Offset   ; 5 42
    bcc .p0FlipPre2     ; 2 44 (3 45)
    ldy Player0Offset   ; 3 47

    lda (Player0Ptr),y  ; 5 52
    sta CheGfxTemp      ; 3 55
    lda (Player0Clr),y  ; 5 60
    sta COLUP0          ; 3 63 (27)

.p0FlipPre2             ;(3 45)
                        ;(  63) - Longest way                      
                        
    ; Load the scanline counter
    
    ldx Heights+.BAND   ; 3 66  - This variable equals the band height - 2
    
; Prime the NUSIZ1 register with the right copies/sizing data

    lda EnvCopies+.BAND ; 3 69
    sta NUSIZ1          ; 3 72    
    

;=================================
;
; Start of the actual kernel
;
;=================================
    


	sta WSYNC           ; 3 75

	
.KernelLoop
;----------------------------------------------------- ENTER THIRD SCANLINE
	
;=================================
;
; Draw Che Graphics in time for left-most
; side of the screen.
;
;=================================

    lda CheGfxTemp      ; 3  3
    sta GRP0            ; 3  6
    lda EnvGfxTemp      ; 3  9
    sta GRP1            ; 3 12
    
;=================================
;
; Decrement line counter, and 
; branch out if we're at zero.
;
; Even though we're using FlipDraw,
; we still need a scanline counter
; to keep track of which section
; we're in.
;
;=================================
    
    dex                ; 2 14
    beq .EndKernel     ; 2 16 (3 17)
    
;=================================
;
; Calculate Environment Graphics
;
;=================================

    lda EnvHeight+.BAND         ; 3 19
    dcp EnvGfxOffset+.BAND      ; 5 24
    bcc .p1FlipDraw             ; 2 26 (3 27)
    ldy EnvGfxOffset+.BAND      ; 3 29 (13)

    lda (EnvGfxPtr+.BAND*2),y   ; 5 34
    sta EnvGfxTemp              ; 3 37
    lda (EnvClrPtr+.BAND*2),y   ; 5 42
    sta COLUP1                  ; 3 45 (29)

.p1FlipDraw                     ;(3 27)
                                ;(  45) - Longest way
 
;=================================
;
; Calculate Che Graphics
;
;=================================

    lda #P0_HEIGHT      ; 2 47
    dcp Player0Offset   ; 5 52
    bcc .p0FlipDraw     ; 2 54 (3 54)
    ldy Player0Offset   ; 3 57

    lda (Player0Ptr),y  ; 5 62
    sta CheGfxTemp      ; 3 65
    lda (Player0Clr),y  ; 5 70
    sta COLUP0          ; 3 73 (27)

.p0FlipDraw             ;(3 54)
                        ;(  73) - Longest way
    
;=================================
;
; We branch out of the kernel at the
; beginning of the last scanline AT
; THE TOP. So if we are allowed to
; reach this part of the kernel,
; we unconditionally jump back to the
; top!
;
;=================================
        
    jmp .KernelLoop     ; 3 76/0
    
;=================================
; END OF THE KERNEL.
; Here we "fall into" the next band
; of the screen!
;=================================
    
.EndKernel              ; (3 17)

    ENDM


;==================
; Define RAM Usage
;==================

	SEG.U VARS

	ORG $80

	; Object X positions in $80-84
ObjectX:	ds 5	; player0, 4 x player1 objects

	; Object Y positions in $85-89
ObjectY:	ds 5	; player0, 4 x player1 objects

;===========Graphic Pointers in $90-95===========

; Che's graphics pointer	
Player0Ptr:	ds 2

; Environment graphics pointers
EnvGfxPtr0: ds 2
EnvGfxPtr1: ds 2
EnvGfxPtr2: ds 2
EnvGfxPtr3: ds 2
EnvGfxPtr4: ds 2    ; $95

;================================================

; Environment NUSIZX variables in $96-9A
EnvCopies:  ds 5


	; Indexes for player animation sequences
Animation0:	ds 1	; Stored in $9B
Animation1: ds 1    ; $9C

    ; Local animation indexes for Che animation
    
AnimateHor0:    ds 1    ; $9D
AnimateUp0:     ds 1    ; $9E
AnimateDown0:   ds 1    ; $9F

AnimFrameCounter:   ds 1    ; $A0

;==============Color Pointers====================
;========== in $A1 to $B2 =======================

; Che's colour pointer     
Player0Clr: ds 2    ; player0, stored in $A1-A2

; Environment colour pointers
EnvClrPtr0: ds 2
EnvClrPtr1: ds 2
EnvClrPtr2: ds 2
EnvClrPtr3: ds 2
EnvClrPtr4: ds 2    ; $B2

;================================================

    ; Scratch Variable
CheGfxTemp:     ds 1    ; $B3
EnvGfxTemp:     ds 1    ; $B4
Temp2:          ds 1    ; $B5 -- Scratch variable for animation. May remove.

    ; Playfield Scanline Height and Colour

; Playfield colour pointer
PFColourPtr:    ds 2    ; $B6-$B7

; Playfield bands heights
Heights:        ds 5    ; $B8-$BC

;==============Graphics Y positions==============
;=========== in $BD to C2 =======================

; Che Y Offsets
Player0Offset:  ds 1    ; $BD

; Environment Y Offsets
EnvGfxOffset0:  ds 1
EnvGfxOffset1:  ds 1
EnvGfxOffset2:  ds 1
EnvGfxOffset3:  ds 1
EnvGfxOffset4:  ds 1    ; $C2

    ; X and Y variables for returning Player0 to collision position
    
SavedX:         ds 1    ; $C3
SavedY:         ds 1    ; $C4

; Still 59 bytes of RAM left.



;===========================
; Define Start of Cartridge
;===========================

	; Define a segment for code
	SEG CODE

	; 4K ROM starts at $F000
	ORG $F000

;========================================
; PosObject
; ---------
;
; A - Holds the X position of the object
; X - holds the object to position
;     0 = player0
;     1 = player1
;
; player 0 -> Range is 0-159
;========================================

PosObject:
	sec
	sta WSYNC
DivideLoop
	sbc #15		; 2  2
	bcs DivideLoop	; 2  4
	eor #7		; 2  6
	asl		; 2  8
	asl		; 2 10
	asl		; 2 12
	asl		; 2 14
	sta.wx HMP0,X	; 5 19 - store fine tuning of X
	sta RESP0,X	; 4 23 - set coarse X position of object
	rts		; 6 29

;==================
; Initialise Atari
;==================
InitSystem:

	CLEAN_START

	; From here we fall into the main loop

;===================
; Main Program Loop
;===================

Main:

    jsr VerticalSync
    jsr VerticalBlank
    jsr Kernel
    jsr OverScan
    jmp Main

===============
; Vertical Sync
;===============
VerticalSync:
	lda #2
	ldx #47
	sta WSYNC
	sta VSYNC
	stx TIM64T

	sta WSYNC
	sta WSYNC
	lda #0
	sta PF0
	sta PF1
	sta PF2
	
	sta GRP0   ; Blanks GRP0 if VDELP0 was off 
	sta GRP1   ; Blanks GRP0 if VDELP0 was on, or blanks GRP1 if VDELP1 was off 
	sta GRP0   ; Blanks GRP1 if VDELP1 was on
	sta VDELP0 ; turn off vertical delay
	sta VDELP1 ; turn off vertical delay
	
	sta WSYNC
	sta VSYNC

Sleep12:	; JSR here to sleep for 12 cycles
	rts
    
;================
; Vertical Blank
; --------------
; Game Logic
;================

VerticalBlank:
    
    jsr ProcessJoystick
    jsr PositionObjects
	
	rts

;=================================
; Process Joystick
;
;   76543210
;   RLDUrldu    - RIGHT LEFT DOWN UP right left down up
;
; UPPERCASE denotes the left joystick directions
; lowercase denotes the right joystick directions
;=================================

ProcessJoystick:
    lda SWCHA

; Store Player0 position to "bounce back"
; after a collision with something.
    
    ldy ObjectX
    sty SavedX
    ldy ObjectY
    sty SavedY

;=================================
; Is Joystick held right?
;=================================    

    asl             ; Put R in the carry bit
    bcs CheckLeft   ; If R wasn't 1, then check for L
    
    ldy ObjectX
    iny             ; Move ObjectX right
    
    cpy #160        ; Test for edge of screen
    bne SaveX       ; If not at edge of screen, save X position
    
    ldy #0          ; Else, wrap to the left edge.
    
SaveX:

    sty ObjectX     ; Save X position
    ldy #0          
    sty REFP0       ; Graphics are stored facing right.
                    ; So, turn off reflect player to make it face
                    ; right, because we are moving right.

;=================================
; Is Joystick held left?
;=================================    
                    
CheckLeft:
    asl             ; Shift A one bit left. Now L is in the carry bit.
    bcs CheckDown   ; Branch if joystick not held left.

    ldy ObjectX     ; Get X position
    dey             ; Move it left
    
    cpy #255        ; Test for edge of screen
    bne SaveX2      ; Save X if not at edge of screen

    ldy #159        ; Else wrap to right edge
    
SaveX2:

    sty ObjectX
    ldy #8
    sty REFP0       ; We are moving left, so flip graphics
                    ; to make face left.

;=================================
; Is Joystick held down?
;=================================    

CheckDown:

    asl             ; Shift A one bit left. D is in carry.
    bcs CheckUp     ; Branch if joystick not held down.
    ldy ObjectY     ; Get object's Y position
    iny             ; Move it down
    cpy #PF_HEIGHT*2+2  ; Test for bottom of screen
    bne SaveY       ; Save Y if we're not at botom
    ldy #P0_HEIGHT  ; Else wrap to top (is this right?)
    
SaveY:
    
    sty ObjectY     ; Save Y.
    
;=================================
; Is Joystick held up?
;=================================    

CheckUp:

    asl             ; Shift A one bt left. U is in now in carry.
    bcs OnePlayer   ; Branch if joystick not up. Only one player for now.
    ldy ObjectY     ; Get Y position
    dey             ; Move it up.
    cpy #P0_HEIGHT  ; Test for top of screen (is this right?)
    bne SaveY2      ; Save Y if we're not at top
    ldy #PF_HEIGHT*2+2  ; Else wrap to bottom.
    
SaveY2:

    sty ObjectY     ; Save Y.
    
OnePlayer:
    
    ; We're done
    
    rts
    
;=================================
; Position Objects
;
; This is an early version of the entire game,
; so we're only positioning GRP0.
;
;=================================

PositionObjects:

    ldx #1          ; We're only gonna position GRP0 and one version of GRP1

.p0loop
    
    lda ObjectX,x
    jsr PosObject
    
    dex
    bpl .p0loop
    
    sta WSYNC       ; Not sure if this code should be inside
    sta HMOVE       ; or outside of the loop.
    
    
    
;=================================
;
; Prep Che's Y position, and calculate
; for VDELP0
;
;=================================
    
    lda ObjectY
    sta Player0Offset   ; We are using a different method of calculating
                        ; the Y position of GRP0 than /Collect/

;=================================
;
; Animation for Che, GRP0
;
;=================================

    lda ObjectX
    cmp SavedX
    beq YMovement
    jsr AnimationDelay
    cmp #1
    beq AnimateHorizontalYes
    bne AnimateHorizontalNo

YMovement:
    lda ObjectY
    cmp SavedY
    beq StopAnimation
    bcc AnimationDelayUp
    bcs AnimationDelayDown

AnimationDelayUp:
    jsr AnimationDelay
    cmp #1
    beq AnimateUpYes
    bne AnimateUpNo
    
AnimationDelayDown:
    jsr AnimationDelay
    cmp #1
    beq AnimateDownYes
    bne AnimateDownNo
    
StopAnimation:
    lda #3
    sta Animation0
    jmp SaveFrame
    
;=================================
; Animation Delay Counter
; 
; We want one frame of animation
; every 10 screen frames
;=================================
    
AnimationDelay:
    inc AnimFrameCounter
    lda AnimFrameCounter
    cmp #9
    beq .IncFrameYes
    bcc .IncFrameNo
    
.ResFrmCntr
    clc
    lda #0
    sta AnimFrameCounter
    rts
    
.IncFrameNo
    lda #0
    rts
    
.IncFrameYes    
    lda #1
    rts
    

    
AnimateHorizontalYes:
    inc AnimateHor0

AnimateHorizontalNo:
    lda AnimateHor0
    cmp #2
    beq .CntHorCntr
    bcc .CntHorCntr
    
.ResHorCntr
    lda #0
.CntHorCntr
    sta AnimateHor0
    sta Animation0
    jmp SaveFrame

AnimateUpYes:
    inc AnimateUp0

AnimateUpNo:
    lda AnimateUp0
    and #3
    sta AnimateUp0
    clc
    adc #7
    sta Animation0
    jmp SaveFrame
    
AnimateDownYes:
    inc AnimateDown0

AnimateDownNo:
    lda AnimateDown0
    and #3
    sta AnimateDown0
    clc
    adc #3
    sta Animation0
    
SaveFrame:
    lda Animation0
    
    tax
    lda ShapePtrLow,x
    clc
    adc Temp2           ; Here we add the Y position
    sta Player0Ptr
    lda ShapePtrHi,x
    adc #0
    sta Player0Ptr+1

    lda ColourPtrLow,x
    clc
    adc Temp2
    sta Player0Clr
    lda ColourPtrHi,x
    adc #0
    sta Player0Clr+1
    
;=================================
;
; Prep Y position of GRP1: Section 1
;
;=================================
    
    ; Come back to this, I wonder if I need to use VDELP1
    ; for each section of GRP1 graphics down the screen
    
    lda ObjectY+1
    sta EnvGfxOffset
    
    rts
    
ShapePtrLow:
    .byte <(CheHorizontal0 - PF_HEIGHT - 1 - 1)     ; 0
    .byte <(CheHorizontal1 - PF_HEIGHT - 1 - 1)     ; 1
    .byte <(CheHorizontal2 - PF_HEIGHT - 1 - 1)     ; 2
    .byte <(CheFrontLeftStand - PF_HEIGHT - 1 - 1)  ; 3
    .byte <(CheFrontRightStep - PF_HEIGHT - 1 - 1)  ; 4
    .byte <(CheFrontRightStand - PF_HEIGHT - 1 - 1) ; 5
    .byte <(CheFrontLeftStep - PF_HEIGHT - 1 - 1)   ; 6
    .byte <(CheBackLeftStand - PF_HEIGHT - 1 - 1)  ; 7
    .byte <(CheBackRightStep - PF_HEIGHT - 1 - 1)  ; 8
    .byte <(CheBackRightStand - PF_HEIGHT - 1 - 1) ; 9
    .byte <(CheBackLeftStep - PF_HEIGHT - 1 - 1)   ; 10

        
ShapePtrHi:
    .byte >(CheHorizontal0 - PF_HEIGHT - 1 - 1)     ; 0
    .byte >(CheHorizontal1 - PF_HEIGHT - 1 - 1)     ; 1
    .byte >(CheHorizontal2 - PF_HEIGHT - 1 - 1)     ; 2
    .byte >(CheFrontLeftStand - PF_HEIGHT - 1 - 1)  ; 3
    .byte >(CheFrontRightStep - PF_HEIGHT - 1 - 1)  ; 4
    .byte >(CheFrontRightStand - PF_HEIGHT - 1 - 1) ; 5
    .byte >(CheFrontLeftStep - PF_HEIGHT - 1 - 1)   ; 6
    .byte >(CheBackLeftStand - PF_HEIGHT - 1 - 1)  ; 7
    .byte >(CheBackRightStep - PF_HEIGHT - 1 - 1)  ; 8
    .byte >(CheBackRightStand - PF_HEIGHT - 1 - 1) ; 9
    .byte >(CheBackLeftStep - PF_HEIGHT - 1 - 1)   ; 10

        
ColourPtrLow:
    .byte <(CheHorizontalClr0 - PF_HEIGHT - 1 - 1)
    .byte <(CheHorizontalClr1 - PF_HEIGHT - 1 - 1)
    .byte <(CheHorizontalClr2 - PF_HEIGHT - 1 - 1)
    .byte <(CheFrontLeftStandClr - PF_HEIGHT - 1 - 1)  ; 3
    .byte <(CheFrontRightStepClr - PF_HEIGHT - 1 - 1)  ; 4
    .byte <(CheFrontRightStandClr - PF_HEIGHT - 1 - 1) ; 5
    .byte <(CheFrontLeftStepClr - PF_HEIGHT - 1 - 1)   ; 6
    .byte <(CheBackLeftStandClr - PF_HEIGHT - 1 - 1)  ; 7
    .byte <(CheBackRightStepClr - PF_HEIGHT - 1 - 1)  ; 8
    .byte <(CheBackRightStandClr - PF_HEIGHT - 1 - 1) ; 9
    .byte <(CheBackLeftStepClr - PF_HEIGHT - 1 - 1)   ; 10

    
ColourPtrHi:
    .byte >(CheHorizontalClr0 - PF_HEIGHT - 1 - 1)
    .byte >(CheHorizontalClr1 - PF_HEIGHT - 1 - 1)
    .byte >(CheHorizontalClr2 - PF_HEIGHT - 1 - 1)
    .byte >(CheFrontLeftStandClr - PF_HEIGHT - 1 - 1)  ; 3
    .byte >(CheFrontRightStepClr - PF_HEIGHT - 1 - 1)  ; 4
    .byte >(CheFrontRightStandClr - PF_HEIGHT - 1 - 1) ; 5
    .byte >(CheFrontLeftStepClr - PF_HEIGHT - 1 - 1)   ; 6
    .byte >(CheBackLeftStandClr - PF_HEIGHT - 1 - 1)  ; 7
    .byte >(CheBackRightStepClr - PF_HEIGHT - 1 - 1)  ; 8
    .byte >(CheBackRightStandClr - PF_HEIGHT - 1 - 1) ; 9
    .byte >(CheBackLeftStepClr - PF_HEIGHT - 1 - 1)   ; 10

;=================================
; Kernel
;
; The flow of the kernel works like this:
;
; 1. Blank out the playfield
; 
;=================================

Kernel:
	sta WSYNC
;---------------------------------
	lda INTIM		; 4  4
	bne Kernel		; 2  6
	sta VBLANK		; 3  9 - Accumulator D1=0

;------------------------------------------------------

    lda #0			; 2  2
	sta PF0			; 3  5
	sta PF1			; 3  8
	sta PF2         ; 3 11
	
	lda #$D2        ; 2 13
	sta COLUBK      ; 3 16















;==========
; Overscan
;==========

OverScan:
	sta WSYNC
	lda #2
	sta VBLANK
	lda #36
	sta TIM64T
	sta WSYNC

.Wait
	lda INTIM
	bne .Wait
	
	rts

	

;=================================
;=================================
; GRAPHICS
;=================================
;=================================

;=================================
; GRAPHICS IMAGE DATA
;=================================

; HORIZONTAL IMAGES

    align 256

CheHorizontal0
        .byte #%10110100;$00
        .byte #%00000011;$E0
        .byte #%00000010;$E0
        .byte #%10000010;$E0
        .byte #%10000010;$E0
        .byte #%11110010;$E0
        .byte #%00110110;$D4
        .byte #%00111110;$D4
        .byte #%00111110;$D4
        .byte #%00011100;$D4
        .byte #%00011110;$D0
        .byte #%01111111;$10
        .byte #%11111110;$10
        .byte #%01011100;$C4
        .byte #%01111100;$D4
        .byte #%00111100;$D4
        .byte #%00111000;$D4
        .byte #%00111100;$00
        .byte #%00111000;$00
        .byte #%00111000;$FE
        .byte #%00111100;$00
        .byte #%00111100;$00
        .byte #%00011100;$00
        .byte #%00001100;$00
        .byte #%00000000;$00
        
CheHorizontal1
        .byte #%11101100;$00
        .byte #%00001100;$E0
        .byte #%00011000;$E0
        .byte #%00011000;$E0
        .byte #%00001000;$E0
        .byte #%00001100;$E0
        .byte #%00001100;$D4
        .byte #%00011100;$D4
        .byte #%00011100;$D4
        .byte #%00011100;$D4
        .byte #%00011110;$D0
        .byte #%01111111;$10
        .byte #%11111110;$10
        .byte #%01011100;$C4
        .byte #%01111100;$D4
        .byte #%00111100;$D4
        .byte #%00111000;$D4
        .byte #%00111100;$00
        .byte #%00111000;$00
        .byte #%00111000;$FE
        .byte #%00111100;$00
        .byte #%00111100;$00
        .byte #%00011100;$00
        .byte #%00001100;$00
        .byte #%00000000;$00
        
CheHorizontal2
        .byte #%01011101;$00
        .byte #%00100000;$E0
        .byte #%01000010;$E0
        .byte #%01000011;$E0
        .byte #%01000010;$E0
        .byte #%01100010;$E0
        .byte #%00110110;$D4
        .byte #%00111110;$D4
        .byte #%00111110;$D4
        .byte #%00011100;$D4
        .byte #%00011110;$D0
        .byte #%01111111;$10
        .byte #%11111110;$10
        .byte #%01011100;$C4
        .byte #%01111100;$D4
        .byte #%00111100;$D4
        .byte #%00111000;$D4
        .byte #%00111100;$00
        .byte #%00111000;$00
        .byte #%00111000;$FE
        .byte #%00111100;$00
        .byte #%00111100;$00
        .byte #%00011100;$00
        .byte #%00001100;$00
        .byte #%00000000;$00

CheHorizontalClr0
        .byte #$E0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$10;
        .byte #$10;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$FE;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        
CheHorizontalClr1
        .byte #$E0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$10;
        .byte #$10;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$FE;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;

CheHorizontalClr2
        .byte #$E0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$10;
        .byte #$10;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$FE;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;


; VERTICAL IMAGES

CheFrontLeftStand
        .byte #%01101100;$D0
        .byte #%00101000;$D0
        .byte #%00101000;$D0
        .byte #%01101100;$D0
        .byte #%01101100;$D4
        .byte #%01101100;$D4
        .byte #%01111100;$D4
        .byte #%01111100;$D4
        .byte #%00111000;$D4
        .byte #%01111100;$D0
        .byte #%01011000;$D4
        .byte #%10101100;$D4
        .byte #%10101100;$D4
        .byte #%11011100;$D4
        .byte #%11111100;$D4
        .byte #%11111100;$D4
        .byte #%01111000;$D4
        .byte #%00110000;$00
        .byte #%00110000;$FE
        .byte #%00110000;$FE
        .byte #%01111000;$00
        .byte #%01110000;$00
        .byte #%01100000;$00
        .byte #%00000000;$00
        .byte #%00000000;$00

CheFrontRightStep
        .byte #%00001110;$E0
        .byte #%00111100;$E0
        .byte #%01100000;$E0
        .byte #%00000100;$D0
        .byte #%00000110;$D0
        .byte #%01001110;$D0
        .byte #%01101100;$D4
        .byte #%01111100;$D4
        .byte #%00111100;$D4
        .byte #%00111100;$D4
        .byte #%01111000;$D0
        .byte #%00101000;$D4
        .byte #%01010110;$D4
        .byte #%01010110;$D4
        .byte #%01101110;$D4
        .byte #%01111100;$D4
        .byte #%00111100;$D4
        .byte #%00111100;$D4
        .byte #%00011000;$00
        .byte #%00011000;$FE
        .byte #%00011000;$FE
        .byte #%00111100;$00
        .byte #%00011100;$00
        .byte #%00001100;$00
        .byte #%00000000;$00

CheFrontRightStand
        .byte #%01101100;$D0
        .byte #%00101000;$D0
        .byte #%00101000;$D0
        .byte #%01101100;$D0
        .byte #%01101100;$D4
        .byte #%01101100;$D4
        .byte #%01111100;$D4
        .byte #%01111100;$D4
        .byte #%00111000;$D4
        .byte #%01111100;$D0
        .byte #%01011000;$D4
        .byte #%10101100;$D4
        .byte #%10101100;$D4
        .byte #%11011100;$D4
        .byte #%11111100;$D4
        .byte #%11111100;$D4
        .byte #%01111000;$D4
        .byte #%00110000;$00
        .byte #%00110000;$FE
        .byte #%00110000;$FE
        .byte #%01111000;$00
        .byte #%00111000;$00
        .byte #%00011000;$00
        .byte #%00000000;$00
        .byte #%00000000;$00

CheFrontLeftStep
        .byte #%11100000;$E0
        .byte #%01111000;$E0
        .byte #%00001100;$E0
        .byte #%01000000;$D0
        .byte #%11000000;$D0
        .byte #%11100100;$D0
        .byte #%01101100;$D4
        .byte #%01111100;$D4
        .byte #%01111000;$D4
        .byte #%01111000;$D4
        .byte #%00111100;$D0
        .byte #%01011000;$D4
        .byte #%10101100;$D4
        .byte #%10101110;$D4
        .byte #%11011010;$D4
        .byte #%11111110;$D4
        .byte #%01111100;$D4
        .byte #%01111100;$D4
        .byte #%00110000;$00
        .byte #%00110000;$FE
        .byte #%00110000;$FE
        .byte #%01111000;$00
        .byte #%01110000;$00
        .byte #%01100000;$00
        .byte #%00000000;$00

CheBackLeftStand
        .byte #%01101100;$D0
        .byte #%00101000;$D0
        .byte #%00101000;$D0
        .byte #%01101100;$D0
        .byte #%01101100;$D4
        .byte #%01101100;$D4
        .byte #%01111100;$D4
        .byte #%01111100;$D4
        .byte #%00111000;$D4
        .byte #%01111100;$D0
        .byte #%00111100;$D4
        .byte #%01110110;$D4
        .byte #%11110010;$D4
        .byte #%11101010;$D4
        .byte #%11011110;$D4
        .byte #%11111100;$D4
        .byte #%00111000;$D4
        .byte #%00110000;$FE
        .byte #%00110000;$FE
        .byte #%00110000;$FE
        .byte #%01111000;$00
        .byte #%01110000;$00
        .byte #%01100000;$00
        .byte #%00000000;--
        .byte #%00000000;--

CheBackRightStep
        .byte #%01100000;$E0
        .byte #%00111000;$E0
        .byte #%00001100;$E0
        .byte #%00001100;$E0
        .byte #%01000000;$D0
        .byte #%01100000;$D0
        .byte #%01100100;$D0
        .byte #%00100100;$D0
        .byte #%00110100;$D4
        .byte #%00111100;$D4
        .byte #%01111000;$D0
        .byte #%00111100;$D4
        .byte #%01110110;$D4
        .byte #%01101010;$D4
        .byte #%01101110;$D4
        .byte #%01011100;$D4
        .byte #%01011100;$D4
        .byte #%00111000;$D4
        .byte #%00011000;$FE
        .byte #%00011000;$FE
        .byte #%00011000;$FE
        .byte #%00111100;$00
        .byte #%00011100;$00
        .byte #%00001100;$00
        .byte #%00000000;$00

CheBackRightStand
        .byte #%01101100;$D0
        .byte #%00101000;$D0
        .byte #%00101000;$D0
        .byte #%01101100;$D0
        .byte #%01101100;$D4
        .byte #%01101100;$D4
        .byte #%01111100;$D4
        .byte #%01111100;$D4
        .byte #%00111000;$D4
        .byte #%01111100;$D0
        .byte #%00111100;$D4
        .byte #%01110110;$D4
        .byte #%11110010;$D4
        .byte #%11101010;$D4
        .byte #%11011110;$D4
        .byte #%11111100;$D4
        .byte #%00111000;$D4
        .byte #%00110000;$FE
        .byte #%00110000;$FE
        .byte #%00110000;$FE
        .byte #%01111000;$00
        .byte #%00111000;$00
        .byte #%00011000;$00
        .byte #%00000000;--
        .byte #%00000000;--

CheBackLeftStep
        .byte #%00001100;$E0
        .byte #%00111000;$E0
        .byte #%01100000;$E0
        .byte #%01100000;$E0
        .byte #%00000100;$D0
        .byte #%00001100;$D0
        .byte #%01001100;$D0
        .byte #%01001000;$D0
        .byte #%01011000;$D4
        .byte #%01111000;$D4
        .byte #%00111100;$D0
        .byte #%01111000;$D4
        .byte #%11111100;$D4
        .byte #%10110110;$D4
        .byte #%11101010;$D4
        .byte #%11011110;$D4
        .byte #%10111110;$D4
        .byte #%01111100;$D4
        .byte #%00110000;$FE
        .byte #%00110000;$FE
        .byte #%00110000;$FE
        .byte #%01111000;$00
        .byte #%01110000;$00
        .byte #%01100000;$00
        .byte #%00000000;$00

CheFrontLeftStandClr
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$FE;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;

CheFrontRightStepClr
        .byte #$E0;
        .byte #$E0;
        .byte #$E0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$FE;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;

CheFrontRightStandClr
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$FE;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;

CheFrontLeftStepClr
        .byte #$E0;
        .byte #$E0;
        .byte #$E0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$FE;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;

CheBackLeftStandClr
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        
CheBackRightStepClr
        .byte #$E0;
        .byte #$E0;
        .byte #$E0;
        .byte #$E0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;

CheBackRightStandClr
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        
CheBackLeftStepClr
        .byte #$E0;
        .byte #$E0;
        .byte #$E0;
        .byte #$E0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;

;=================================
; free space check before End of Cartridge
;=================================        
    echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"
    
;=================================
; Define End of Cartridge
;=================================
        ORG $FFFA        ; set address to 6507 Interrupt Vectors 
        ;.WORD InitSystem ; NMI
        ;.WORD InitSystem ; RESET
        ;.WORD InitSystem ; IRQ
        .byte <InitSystem, >InitSystem
        .byte <InitSystem, >InitSystem
        .byte <InitSystem, >InitSystem
