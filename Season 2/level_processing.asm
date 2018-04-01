;=================================
; Level Processing
;
; This is where we will load in all the data we need to
; draw the new screens that Che moves between
;
;=================================

    MAC LEVEL_PROCESSING

LevelProcessing:

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

    lda #20
    sta ObjectX+4
    
    lda #40
    sta ObjectX+3
    
    lda #60
    sta ObjectX+2
    
    lda #80 
    sta ObjectX+1
    
    ENDM
