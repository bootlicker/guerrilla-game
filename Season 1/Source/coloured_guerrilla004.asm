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
	ldx #44
	sta WSYNC
	sta VSYNC
	stx TIM64T

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
; 
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
	sta PF2         ; 3 11

    LDA #PLAYER0HEIGHT      ; Load Sprite Height
    DCP Player0Offset       ; DEC SpriteOffset and CMP with Sprite Height
    BCC Environment           ; SKIP DRAWING if SpriteOffset greater than or equal to Sprite Height (SKIP if Sprite Height less than or equal to SpriteOffset) 
    LDY spriteOffset       ; So if Sprite Height greater than SpriteOffset, draw Sprite
    LDA (Player0Ptr),Y
Continue
    STA GRP0

    LDA #TREEHEIGHT      ; Load Sprite Height
    DCP TreeOffset       ; DEC SpriteOffset and CMP with Sprite Height
    BCC FlipDraw           ; SKIP DRAWING if SpriteOffset greater than or equal to Sprite Height (SKIP if Sprite Height less than or equal to SpriteOffset) 
    LDY spriteOffset       ; So if Sprite Height greater than SpriteOffset, draw Sprite
    LDA (TreePtr),Y
Continue
    STA GRP1
	
	
	
	
	
	

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

Guerrilla0
	.byte #%01101100
	.byte #%00101000
	.byte #%00101000
	.byte #%01101100
	.byte #%01101100
	.byte #%01101100	
	.byte #%01111100
	.byte #%01111100
	.byte #%00111000
	.byte #%01111100
	.byte #%00111100
	.byte #%01111110
	.byte #%11111010
	.byte #%10111010
	.byte #%10111110
	.byte #%11111100
	.byte #%11111100
	.byte #%01111100
	.byte #%01111100
	.byte #%00110000
	.byte #%01111100
	.byte #%01111100
	.byte #%01111100
	.byte #%00111100
	.byte #%00111100

Tree0
    .byte $E0 ; |XXX     |
    .byte $70 ; | XXX    |
    .byte $70 ; | XXX    |
    .byte $30 ; |  XX    |
    .byte $38 ; |  XXX   |
    .byte $18 ; |   XX   |
    .byte $18 ; |   XX   |
    .byte $18 ; |   XX   |
    .byte $0C ; |    XX  |
    .byte $0C ; |    XX  |
    .byte $0C ; |    XX  |
    .byte $0C ; |    XX  |
    .byte $0C ; |    XX  |
    .byte $06 ; |     XX |
    .byte $06 ; |     XX |
    .byte $06 ; |     XX |
    .byte $06 ; |     XX |
    .byte $06 ; |     XX |
    .byte $06 ; |     XX |
    .byte $0C ; |    XX  |
    .byte $0C ; |    XX  |
    .byte $1E ; |   XXXX |
    .byte $1E ; |   XXXX |
    .byte $8D ; |X   XX X|
    .byte $FF ; |XXXXXXXX|
    .byte $FE ; |XXXXXXX |
    .byte $7D ; | XXXXX X|
    .byte $9F ; |X  XXXXX|
    .byte $FF ; |XXXXXXXX|
    .byte $76 ; | XXX XX |
