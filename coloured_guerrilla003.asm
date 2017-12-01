;=================
; Initialise DASM
;=================
	
	PROCESSOR 6502

	include vcs.h

	include macro.h

;==================
; Define Constants
;==================

PF_HEIGHT	= 95
                         ; PF_HEIGHT divded by 2, because PF
                         ; is drawn every line, and Players and
                         ; missiles are drawn every second line
P0_HEIGHT   = SET LATER
BULL_HGHT   = SET LATER  ; (Maybe 2?)

;==================
; Define RAM Usage
;==================

	SEG.U VARS

	ORG $80

	; Object X positions in $80
ObjectX:	ds 1	; player0

	; Object Y positions in $81
ObjectY:	ds 1	; player0

	; DoDraw Storage in $82-$85
Player0Draw:	ds 1	; player0
Player1Draw:    ds 1    ; Player1
Missile0Draw    ds 1    
Missile1Draw    ds 1

	; DoDraw Graphic Pointers in $86-87
Player0Ptr:	ds 2	; Used for drawing player0
Player1Ptr: ds 2

	; Frame Counter
Frame:		ds 1	; Stored in $88

	; Indexes for player animation sequences
Animation0:	ds 1	; Stored in $89
Animation1: ds 1    ; $8A

    ; Color Pointers
Player0Clr: ds 2    ; player0, stored in $8B-8C
Player1Clr: ds 2    ; $8D-8E

    ; Scratch Variable
Temp:       ds 1    ; $8F

    ; Playfield Scanline Height and Colour
    
PFIndex:        ds 1    ; $90
PFColourPtr:    ds 2    ; $91-92
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
	sta.wx HMPO,X	; 5 19 - store fine tuning of X
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

;===============
; Vertical Sync
;===============
VerticalSync:
	lda #2
	ldx #43
	sta WSYNC
	sta VSYNC
	stx TIM64T
	lda Frame
	and #$3F
	bne VSskip

VSskip: 
	inc Frame
	sta WSYNC
	sta WSYNC
	lda #0
	sta PF0
	sta PF1
	sta PF2
	sta GRP0
	sta GRP1
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
	bit GameState
	bpl NotActive
	jsr ProcessJoystick
NotActive:
	jsr PositionObjects
	jsr SetObjectColours
	rts

;========
; Kernel
;
; The flow of the kernel works like this:
;
; 1. Blank out the playfield
; 2. PRECALC:
;    A. Set Playfield colour
;    B. Calcuate Player0
;    C. Draw Left and Right Playfield
;    D. Draw Player0 with Colour
; 3. Move into the kernel loop:
;    A. Line 1: DRAW [Player1, Left portion of Asymmetrical Playfield]
;               PRECALC [Player0 and Missile0]
;    B. Line 2: DRAW [Player0 and Missile0, Right portion of Playfield]
;               PRECALC [Player1 and, Playfield Colour]
;========
Kernel:
	sta WSYNC
;---------------------------------
	lda INTIM		; 4  4
	bne Kernel		; 2  6
	sta VBLANK		; 3  9 - Accumulator D1=0

	lda#0			; 2  2
	sta PF0			; 3  5
	sta PF1			; 3  8
	sta PF2            ; 3 11
	ldy #PF_HEIGHT     ; 2 13    - Two line resolution for playfield.
    sty PF_Index       ; 3 16    - It used to be single-line resolution,
    ldy #2LK_HGHT      ; 2 18    - but a multi-coloured, asymmetrical 
    sty K_Index        ; 3 21    - playfield takea up a LOT of cycles.

    ; We don't need to calculate Missile0 to appear on the very first
    ; scanline of the frame. This helps us speed up our preloading of
    ; the graphics for the Playfield and Player0.

;---------------------------------
; Preload Playfield Scanline Colour
;---------------------------------
	
	lda (PFColourPtr),y   ; 5 26 - Playfield Colour Pointer 
	sta COLUPF     	      ; 3 29 
	
;---------------------------------
; Preload the Player0 Graphics
;---------------------------------
    
    lda #P1_HEIGHT-1	; 2 31 (2)
	dcp Player0Draw		; 5 36 (7)
	bcs DoDrawGRP0		; 2 38 (9) - (3 39)
	lda #0			    ; 2 40 (11)
	.byte $2C		    ; 4 44 (15)

DoDrawGRP0:			    ;  (39)(10)
	lda (Player0Ptr),y	; 5 44 (15)
	sta Player0GFX      ; 3 47 (18)
                        
  	lda (Player0Clr),y     ; 5 52    -   Precalculate Player0 Colour to land on
                           ;         -   the first scanline
    
;---------------------------------
; Prime scanline index
;---------------------------------

    ldy PF_Index

    sta WSYNC
;-------------------------------------------------------
	
PFLoop:

;=================================
; Start Line 1 of the Kernel
;=================================

;---------------------------------
; Here we:
; - Set the graphics of Player0
;---------------------------------

	sta COLUP0             ; 3  3
	lda Player0GFX         ; 3  6
	sta GRP0               ; 3  9

;----------------------------------
; Load the graphics for LeftPF0
;----------------------------------

    lda (LeftPF0Ptr),y      ; 5 14 (5)
    sta PF0                 ; 3 17 (8)  -   (@55-22) PHEW. MADE IT.
    lda (LeftPF1Ptr),y      ; 5 22 (13)
    sta PF1                 ; 3 25 (16) -   (@66-28) PHEW. With 3 cycles to spare
    lda (LeftPF2Ptr),y      ; 5 30 (21) 
    sta PF2                 ; 3 33 (24) -   (@0-38) 5 cycles to spare. I was sweating
                            ;                       bullets there...

;---------------------------------
; Load Kernel Index
;---------------------------------

    ldy K_Index         ; 3 36
                            
;---------------------------------
; Calculate whether or not Missile1 is being drawn
;---------------------------------
    
    ldx #1              ; 2 38 (2)
    lda #BULL_HGHT-1    ; 2 40 (4)
    dcp Missile1Draw    ; 5 45 (9)
    bcs DoEnam1         ; 2 47 (11) (3 48)(12)
    .byte $24           ; 3 50 (14)
    
DoEnam1:                ;   48 (12)
    inx                 ; 2 50 (14)
    
;---------------------------------
; Calculate the Player1 Graphics
;---------------------------------
    
    lda #P1_HEIGHT-1	; 2 52 (2)
	dcp Player1Draw		; 5 56 (7)
	bcs DoDrawGRP1		; 2 58 (9) - (3 59)
	lda #0			    ; 2 60 (11)
	.byte $2C		    ; 4 64 (15)

DoDrawGRP1:			    ;  (59)(10)
	lda (Player1Ptr),y	; 5 64 (15)
	sta Player1GFX      ; 3 67 (18)
    lda (Player1Clr),y  ; 5 72

    sta WSYNC           ; 3 75
;--------------------------------------------------------------



;=================================
; Line 2 of the Kernel
;=================================

;---------------------------------
; Draw:
; - Player1
; - Missile1
;---------------------------------

	sta COLUP1             ; 3  3
	lda Player1GFX         ; 3  6
	sta GRP1               ; 3  9
	stx ENAM1              ; 3 12

;---------------------------------
; We've finished drawing all the Players and Missiles,
; so we can decrement the Kernel Index.
;---------------------------------

	dec K_Index            ; 5 17

;---------------------------------
; Calculate Player0 Graphics
;---------------------------------
    
    lda #P1_HEIGHT-1    ; 2 19 (2)
    dcp Player0Draw     ; 5 24 (7)
    bcs DoDrawP0        ; 2 26 (9) - (3 27) (10)
    lda #0              ; 2 28 (11)
    .byte $2C           ; 4 32 (15)
    
DoDrawP0:               ;  (27)(10)
    lda (Player0Ptr),y  ; 5 32 (15)
    sta Player0GFX      ; 3 35 (18)	

;---------------------------------
; Now we have to load the Playfield Index
; The playfield is at a single line resolution
; So for every two lines we go through the kernel
;---------------------------------

    ldy PF_Index        ; 3 38
	
;---------------------------------
; Draw the right side of the playfield
;---------------------------------
    
    lda (RightPF0Ptr),y      ; 5 43
    sta PF0                  ; 3 46 (8)  -   (@28-49) 
    lda (RightPF1Ptr),y      ; 5 51
    sta PF1                  ; 3 54 (16) -   (@39-54)   -   PHEW. JUST MADE IT....
    lda (RightPF2Ptr),y      ; 5 59  
    sta PF2                  ; 3 62 (24) -   (@50-66) 

;---------------------------------
; Now we decrement the Playfield Index
;---------------------------------


    

	lda (PFColourPtr),y   ; 5 18 - Playfield Colour Pointer 
	sta COLUPF     	      ; 3 21 (8)
    

    
;---------------------------------
; We've now finished DRAWING the 2 line
; kernel, so we can decrement the scanline
; index register.
;---------------------------------
    
    bne PFLoop         ; 2 21 (3 22) - Branch if more Playfield to show.


	rts			; the RTS for the whole Kernel

;==========
; Overscan
;==========

OverScan:
	sta WSYNC
	lda #2
	sta VBLANK

	lda #35
	sta TIM64T
	sta WSYNC
	lda INTIM
	bne OverScan
	
	rts


	
