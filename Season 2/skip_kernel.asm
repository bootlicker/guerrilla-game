
    MAC SKIP_KERNEL

;=================================
; Wait for Vertical Blank to End
;=================================

; What we're going to do is use up a blank frame of kernel time
; in order to have enough time to calculate a full row of screens:
; 256 positions on the RNG counter.

	lda #$00        ; 2 13
	sta COLUBK      ; 3 16

RNGStartWait:
	
    sta WSYNC
;---------------------------------
	lda INTIM		    ; 4  4
	bne RNGStartWait    ; 2  6
	sta VBLANK		    ; 3  9 - Accumulator D1=0

;=================================
; Set timer for blank frame
;
; (192 * 76) / 64 = 228
;=================================

RNG_Timer:
	ldx #228
	stx TIM64T

	ENDM
