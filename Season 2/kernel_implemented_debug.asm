
    include che_kernel_macro.asm
    
    MAC KERNEL_IMPLEMENTED_DEBUG

;=================================
; Kernel
;
; The flow of the kernel works like this:
;
; 1. Blank out the playfield
; 
;=================================

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

    SCREEN_BANDS_DEBUG 3
    SCREEN_BANDS_DEBUG 2
    SCREEN_BANDS_DEBUG 1
    SCREEN_BANDS_DEBUG 0

    ENDM
