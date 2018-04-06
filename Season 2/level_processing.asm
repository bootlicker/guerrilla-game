;=================================
; Level Processing
;
; This is where we will load in all the data we need to
; draw the new screens that Che moves between
;
;=================================

    MAC LEVEL_PROCESSING

    ; But for now, we will just be statically
    ; setting the Heights variable for each band
    ; up and down the screen.
    
    lda #48
    sta Heights+3
    
    lda #48
    sta Heights+2
    
    lda #48
    sta Heights+1
    
    lda #48
    sta Heights
        
    ENDM
