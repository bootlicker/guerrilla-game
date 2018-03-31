;=================
; Initialise DASM
;=================
	
	PROCESSOR 6502

	include vcs.h

	include macro.h
	
	include che_kernel_macro.asm
	
	include level_processing.asm
	
	include process_joystick.asm
	
	include position_objects.asm
	
	include che_mask.asm
	
	include che_graphics.asm

;==================
; Define Constants
;==================

PF_HEIGHT   = 192  

P0_HEIGHT   = 25

;==================
; Define RAM Usage
;==================

	SEG.U VARS

	ORG $80

	; Object X positions in $80-84
ObjectX:	ds 6	; player0, 5 x player1 objects

	; Object Y positions in $85-89
ObjectY:	ds 6	; player0, 5 x player1 objects

;===========Graphic Pointers in $90-95===========

; Che's graphics pointer	
Player0Ptr:	ds 10

; Environment graphics pointers
EnvGfxPtr: ds 10 ; $95

;==============Color Pointers====================
;========== in $A1 to $B2 =======================

; Che's colour pointer     
Player0Clr: ds 10    ; player0, stored in $A1-A2

; Environment colour pointers
EnvClrPtr: ds 10   ; $B2

;==============Mask Pointers=====================

; Che's mask pointer
Player0Msk: ds 10

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


    ; Scratch Variable
CheGfxTemp:     ds 1    ; $B3
EnvGfxTemp:     ds 1    ; $B4
CheClrTemp:     ds 1
EnvClrTemp:     ds 1

Temp2:          ds 1    ; $B5 -- Scratch variable for animation. May remove.

    ; Playfield Scanline Height and Colour

; Playfield colour pointer
PFColourPtr:    ds 2    ; $B6-$B7

; Playfield bands heights
Heights:        ds 5    ; $B8-$BC

;==============Graphics Y positions==============
;=========== in $BD to C2 =======================

; Che Y Offsets
CheY:  ds 1    ; $BD

; Environment Y Offsets
EnvGfxOffset:   ds 5    ; $C2

    ; X and Y variables for returning Player0 to collision position
    
SavedX:         ds 1    ; $C3
SavedY:         ds 1    ; $C4

    ; Heights of Evironment Graphics
    
EnvHeight:      ds 5    ; $C9

; Still 52 bytes of RAM left.


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
    
    LEVEL_PROCESSING
    PROCESS_JOYSTICK
    POSITION_OBJECTS
    
    sta HMCLR           ; Reset the horizontal movement registers
                        ; so the HMOVEs for GRP1 in the kernel don't
                        ; interfere with the HMOVE we made for GRP0
                        ; in VBLANK.
	
	rts
    
ShapePtrLow:
    .byte <(CheHorizontal0)     ; 0
    .byte <(CheHorizontal1)     ; 1
    .byte <(CheHorizontal2)     ; 2
    .byte <(CheFrontLeftStand)  ; 3
    .byte <(CheFrontRightStep)  ; 4
    .byte <(CheFrontRightStand) ; 5
    .byte <(CheFrontLeftStep)   ; 6
    .byte <(CheBackLeftStand)  ; 7
    .byte <(CheBackRightStep)  ; 8
    .byte <(CheBackRightStand) ; 9
    .byte <(CheBackLeftStep)   ; 10

        
ShapePtrHi:
    .byte >(CheHorizontal0)     ; 0
    .byte >(CheHorizontal1)     ; 1
    .byte >(CheHorizontal2)     ; 2
    .byte >(CheFrontLeftStand)  ; 3
    .byte >(CheFrontRightStep)  ; 4
    .byte >(CheFrontRightStand) ; 5
    .byte >(CheFrontLeftStep)   ; 6
    .byte >(CheBackLeftStand)  ; 7
    .byte >(CheBackRightStep)  ; 8
    .byte >(CheBackRightStand) ; 9
    .byte >(CheBackLeftStep)   ; 10

        
ColourPtrLow:
    .byte <(CheHorizontalClr0)
    .byte <(CheHorizontalClr1)
    .byte <(CheHorizontalClr2)
    .byte <(CheFrontLeftStandClr)  ; 3
    .byte <(CheFrontRightStepClr)  ; 4
    .byte <(CheFrontRightStandClr) ; 5
    .byte <(CheFrontLeftStepClr)   ; 6
    .byte <(CheBackLeftStandClr)  ; 7
    .byte <(CheBackRightStepClr)  ; 8
    .byte <(CheBackRightStandClr) ; 9
    .byte <(CheBackLeftStepClr)   ; 10

    
ColourPtrHi:
    .byte >(CheHorizontalClr0)
    .byte >(CheHorizontalClr1)
    .byte >(CheHorizontalClr2)
    .byte >(CheFrontLeftStandClr)  ; 3
    .byte >(CheFrontRightStepClr)  ; 4
    .byte >(CheFrontRightStandClr) ; 5
    .byte >(CheFrontLeftStepClr)   ; 6
    .byte >(CheBackLeftStandClr)  ; 7
    .byte >(CheBackRightStepClr)  ; 8
    .byte >(CheBackRightStandClr) ; 9
    .byte >(CheBackLeftStepClr)   ; 10

;=================================
; Kernel
;
; The flow of the kernel works like this:
;
; 1. Blank out the playfield
; 
;=================================

Kernel:

    lda #0			; 2  2
	sta PF0			; 3  5
	sta PF1			; 3  8
	sta PF2         ; 3 11
	
	lda #$D2        ; 2 13
	sta COLUBK      ; 3 16

KernelWait:
	
    sta WSYNC
;---------------------------------
	lda INTIM		; 4  4
	bne KernelWait  ; 2  6
	sta VBLANK		; 3  9 - Accumulator D1=0

;------------------------------------------------------

    SCREEN_BANDS 4
    SCREEN_BANDS 3
    SCREEN_BANDS 2
    SCREEN_BANDS 1
    SCREEN_BANDS 0

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
    
    CHE_MASK

    CHE_GRAPHICS

; ENVIRONMENT PLACEHOLDER GRAPHICS

BatistaSoldierImg
    ds 15, $00
    
    .byte $6C ; | XX XX  |
    .byte $28 ; |  X X   |
    .byte $28 ; |  X X   |
    .byte $6C ; | XX XX  |
    .byte $6C ; | XX XX  |
    .byte $6C ; | XX XX  |
    .byte $7C ; | XXXXX  |
    .byte $7C ; | XXXXX  |
    .byte $38 ; |  XXX   |
    .byte $7C ; | XXXXX  |
    .byte $3C ; |  XXXX  |
    .byte $76 ; | XXX XX |
    .byte $F2 ; |XXXX  X |
    .byte $EA ; |XXX X X |
    .byte $DE ; |XX XXXX |
    .byte $FC ; |XXXXXX  |
    .byte $38 ; |  XXX   |
    .byte $78 ; | XXXX   |
    .byte $30 ; |  XX    |
    .byte $78 ; | XXXX   |
    .byte $78 ; | XXXX   |
    .byte $58 ; | X XX   |
    .byte $30 ; |  XX    |

BatistaSoldierClr
    ds 15, $00

    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    
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
