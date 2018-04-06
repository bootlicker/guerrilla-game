
    MAC ENVGFX_POINTERS

    ; Now we need to index the Environment
    ; graphics so that they show up at the right
    ; Y position. We will set them here STATICALLY
    ; for now. They will all be set 5 scan lines
    ; from the top of each band.

    ; BAND 0
    
    lda #<BatistaSoldierImg
    sta EnvGfxPtr
    lda #>BatistaSoldierImg
    sta EnvGfxPtr+1
    
    lda #<BatistaSoldierClr
    sta EnvClrPtr
    lda #>BatistaSoldierClr
    sta EnvClrPtr+1
    
;=================================
; BANDS 1-3
;=================================

    lda EnvGfxPtr
    sta EnvGfxPtr+2
    sta EnvGfxPtr+4
    sta EnvGfxPtr+6
    
    lda EnvGfxPtr+1
    sta EnvGfxPtr+3
    sta EnvGfxPtr+5
    sta EnvGfxPtr+7
    
    lda EnvClrPtr
    sta EnvClrPtr+2
    sta EnvClrPtr+4
    sta EnvClrPtr+6
    
    lda EnvClrPtr+1
    sta EnvClrPtr+3
    sta EnvClrPtr+5
    sta EnvClrPtr+7
    
    ; Now we set the X positions of the
    ; environment graphics.

    ENDM
