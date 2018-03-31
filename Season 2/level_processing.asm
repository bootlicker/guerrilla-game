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

    lda #32
    sta Heights+4
    
    lda #40
    sta Heights+3
    
    lda #40
    sta Heights+2
    
    lda #40
    sta Heights+1
    
    lda #40
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
; BANDS 1-4
;=================================

    lda EnvGfxPtr
    sta EnvGfxPtr+2
    sta EnvGfxPtr+4
    sta EnvGfxPtr+6
    sta EnvGfxPtr+8
    
    lda EnvGfxPtr+1
    sta EnvGfxPtr+3
    sta EnvGfxPtr+5
    sta EnvGfxPtr+7
    sta EnvGfxPtr+9
    
    lda EnvClrPtr
    sta EnvClrPtr+2
    sta EnvClrPtr+4
    sta EnvClrPtr+6
    sta EnvClrPtr+8
    
    lda EnvClrPtr+1
    sta EnvClrPtr+3
    sta EnvClrPtr+5
    sta EnvClrPtr+7
    sta EnvClrPtr+9
    
    ; Now we set the X positions of the
    ; environment graphics.
    
    lda #20
    sta ObjectX+5

    lda #40
    sta ObjectX+4
    
    lda #60
    sta ObjectX+3
    
    lda #80
    sta ObjectX+2
    
    lda #100 
    sta ObjectX+1
    
    ENDM
