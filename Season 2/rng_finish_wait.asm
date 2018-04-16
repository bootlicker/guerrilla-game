
    MAC RNG_FINISH_WAIT

    sta WSYNC
;---------------------------------
	lda INTIM		; 4  4
	bne RNGWait     ; 2  6

    ENDM
