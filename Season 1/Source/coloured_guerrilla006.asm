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

    ; Player Y Offsets
    
Player0Offset:  ds 1    ; $93
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

    lda ShapePtrLow
    sta Player0Ptr
    lda ShapePtrHi
    sta Player0Ptr+1
    
    lda ColourPtrLow
    sta Player0Clr
    lda ColourPtrHi
    sta Player0Clr+1


Main:

    jsr VerticalSync
    jsr VerticalBlank
    jsr Kernel
    jsr OverScan
    jmp Main

;================
; Vertical Blank
; --------------
; Game Logic
;================

VerticalBlank:
    sta WSYNC
    lda #2
    sta VBLANK

    lda #43
    sta TIM64T
    sta WSYNC
    
    lda #40
    ldx #0
    
    jsr PosObject
	
	
	rts
	
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

	lda #0			; 2  2
	sta PF0			; 3  5
	sta PF1			; 3  8
	sta PF2         ; 3 11
	
	lda #$D2
	sta COLUBK
	
	lda #80             ; 2 13
    sta Player0Offset   ; 3 16
    
    ldy #PF_HEIGHT      ; 2 18
	
    lda #P0_HEIGHT      ; 2 20
    dcp Player0Offset   ; 5 25
    bcs DoDrawGrp0pre   ; 2 27 (3 28)
    lda #0              ; 2 29
    .byte $2C           ; 4 33
                     
DoDrawGrp0pre:          ; 3 28
    lda (Player0Ptr),y  ; 5 33
    sta Temp            ; 3 36
    lda (Player0Clr),y  ; 5 41
	
    dey                 ; 2 43
	
.KernelLoop

	sta WSYNC           ; 3 46
;-----------------------------------------------------
    
    sta COLUP0          ; 3  3
    lda Temp            ; 3  6
    sta GRP0            ; 3  9

    lda #P0_HEIGHT      ; 2 11
    dcp Player0Offset   ; 5 16
    bcs DoDrawGrp0      ; 2 18 (3 19)
    lda #0              ; 2 20
    .byte $2C           ; 4 24
                     
DoDrawGrp0:             ; 3 19
    lda (Player0Ptr),y  ; 5 24
    sta Temp            ; 3 27
    lda (Player0Clr),y  ; 5 32

    dey
    bne .KernelLoop
    
    rts

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

ShapePtrLow:
    .byte <(Guerrilla0 - PF_HEIGHT - 1 + 80 - 1)
        
ShapePtrHi:
    .byte >(Guerrilla0 - PF_HEIGHT - 1 + 80 - 1)
        
ColourPtrLow:
    .byte <(GuerrillaClr0 - PF_HEIGHT - 1 + 80 - 1)

ColourPtrHi:
    .byte >(GuerrillaClr0 - PF_HEIGHT - 1 + 80 - 1)
	
	
.Wait
	lda INTIM
	bne .Wait
	
	rts

	align 256
	
Guerrilla0:
        .byte #%01101100;$E0
        .byte #%00101000;$E0
        .byte #%00101000;$E0
        .byte #%01101100;$E0
        .byte #%01101100;$D4
        .byte #%01101100;$D4
        .byte #%01111100;$D4
        .byte #%01111100;$D4
        .byte #%00111000;$D4
        .byte #%01111100;$E0
        .byte #%00111100;$D4
        .byte #%01111110;$D4
        .byte #%11111010;$D4
        .byte #%10111010;$D4
        .byte #%10111110;$D4
        .byte #%01111100;$D4
        .byte #%00111000;$00
        .byte #%01111100;$00
        .byte #%00111000;$FE
        .byte #%00111000;$FE
        .byte #%00111000;$FE
        .byte #%01111100;$00
        .byte #%01111100;$00
        .byte #%00111100;$00
        .byte #%00011100;$00

GuerrillaClr0:
        .byte #$E0;
        .byte #$E0;
        .byte #$E0;
        .byte #$E0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$E0;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$D4;
        .byte #$00;
        .byte #$00;
        .byte #$FE;
        .byte #$FE;
        .byte #$FE;
        .byte #$00;
        .byte #$00;
        .byte #$00;
        .byte #$00;
    
;===============================================================================
; Define End of Cartridge
;===============================================================================
        ORG $FFFA        ; set address to 6507 Interrupt Vectors 
        ;.WORD InitSystem ; NMI
        ;.WORD InitSystem ; RESET
        ;.WORD InitSystem ; IRQ
        .byte <InitSystem, >InitSystem
        .byte <InitSystem, >InitSystem
        .byte <InitSystem, >InitSystem
