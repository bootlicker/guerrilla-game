;=================================
; Position Objects
;
; This is an early version of the entire game,
; so we're only positioning GRP0.
;
;=================================

    MAC POSITION_ANIMATE_CHE

PositionObjects:

    ldx #0
    lda ObjectX
    jsr PosObject
    
    sta WSYNC       ; Not sure if this code should be inside
    sta HMOVE       ; or outside of the loop.
    
    
    
;=================================
;
; Prep Che's Y position
;
;
;=================================
    
    lda ObjectY
    sta CheY   ; We are using a different method of calculating
                        ; the Y position of GRP0 than /Collect/

;=================================
;
; Animation for Che, GRP0
;
;=================================

    lda ObjectX
    cmp SavedX
    beq YMovement
    jsr AnimationDelay
    cmp #1
    beq AnimateHorizontalYes
    bne AnimateHorizontalNo

YMovement:
    lda ObjectY
    cmp SavedY
    beq StopAnimation
    bcc AnimationDelayDown
    bcs AnimationDelayUp

AnimationDelayUp:
    jsr AnimationDelay
    cmp #1
    beq AnimateUpYes
    bne AnimateUpNo
    
AnimationDelayDown:
    jsr AnimationDelay
    cmp #1
    beq AnimateDownYes
    bne AnimateDownNo
    
StopAnimation:
    lda #3
    sta Animation0
    jmp SaveFrame
    
;=================================
; Animation Delay Counter
; 
; We want one frame of animation
; every 10 screen frames
;=================================
    
AnimationDelay:
    inc AnimFrameCounter
    lda AnimFrameCounter
    cmp #9
    beq .IncFrameYes
    bcc .IncFrameNo
    
.ResFrmCntr
    clc
    lda #0
    sta AnimFrameCounter
    rts
    
.IncFrameNo
    lda #0
    rts
    
.IncFrameYes    
    lda #1
    rts
    
AnimateHorizontalYes:
    inc AnimateHor0

AnimateHorizontalNo:
    lda AnimateHor0
    cmp #2
    beq .CntHorCntr
    bcc .CntHorCntr
    
.ResHorCntr
    lda #0
.CntHorCntr
    sta AnimateHor0
    sta Animation0
    jmp SaveFrame

AnimateUpYes:
    inc AnimateUp0

AnimateUpNo:
    lda AnimateUp0
    and #3
    sta AnimateUp0
    clc
    adc #7
    sta Animation0
    jmp SaveFrame
    
AnimateDownYes:
    inc AnimateDown0

AnimateDownNo:
    lda AnimateDown0
    and #3
    sta AnimateDown0
    clc
    adc #3
    sta Animation0
    
SaveFrame:
    lda Animation0
   
;=================================
; 
; Prepare Pointers for Che Graphic
;
;=================================

    tax

;=================================
; SECTION 0
;=================================
    
; Shape 
    
    lda ShapePtrLow,x
    sta Player0Ptr
    lda ShapePtrHi,x
    sta Player0Ptr+1

; Colour

    lda ColourPtrLow,x
    sta Player0Clr
    lda ColourPtrHi,x
    sta Player0Clr+1

; Mask 

    SET_POINTER Player0Msk, CheMask
    
; Y POSITION
    
    sec
    lda Player0Ptr
    sbc CheY
    sta Player0Ptr
    lda Player0Ptr+1
    sbc #0
    sta Player0Ptr+1
        
    sec
    lda Player0Clr
    sbc CheY
    sta Player0Clr
    lda Player0Clr+1
    sbc #0
    sta Player0Clr+1
        
    sec
    lda Player0Msk
    sbc CheY
    sta Player0Msk
    lda Player0Msk+1
    sbc #0
    sta Player0Msk+1
    
;=================================
; SECTIONS 1-3
;=================================

    ldx #0
    ldy #0

.SectionsLoop

    clc
    lda Player0Ptr,y
    adc Heights,x
    iny ; 1
    iny ; 2
    sta Player0Ptr,y
    dey ; 1
    lda Player0Ptr,y
    adc #0
    iny ; 2
    iny ; 3
    sta Player0Ptr,y
        
    dey ; 2
    dey ; 1
    dey ; 0
    clc
    lda Player0Clr,y
    adc Heights,x
    iny ; 1
    iny ; 2
    sta Player0Clr,y
    dey ; 1
    lda Player0Clr,y
    adc #0
    iny ; 2
    iny ; 3
    sta Player0Clr,y              
        
    dey ; 2
    dey ; 1
    dey ; 0
    clc
    lda Player0Msk,y
    adc Heights,x
    iny ; 1
    iny ; 2
    sta Player0Msk,y
    dey ; 1
    lda Player0Msk,y
    adc #0
    iny ; 2
    iny ; 3
    sta Player0Msk,y           
        
    dey ; 2
    inx        
    cpx #3
    bcc .SectionsLoop

;=================================
;
; SpiceWare's Code for tweaking
; the pointers so that no page
; breaks occur.
;
;=================================
    
    ldy #1
MaskLoop        
    lda Player0Msk,y
    cmp #>BlankGraphic
    beq MaskCheck2
    lda #>BlankGraphic
    sta Player0Msk,y     
    sta Player0Ptr,y
    sta Player0Clr,y
    dey
    lda #0
    sta Player0Msk,y
    sta Player0Ptr,y
    sta Player0Clr,y
    iny
    bne NextMask
MaskCheck2
    dey
    lda Player0Msk,y
    cmp #<CheMask
    bcc NextMask2
    lda #0
    sta Player0Msk,y
    sta Player0Ptr,y
    sta Player0Clr,y
    iny
    lda #>BlankGraphic
    sta Player0Msk,y
    sta Player0Ptr,y
    sta Player0Clr,y
    bne NextMask
NextMask2        
    iny
NextMask
    iny
    iny
    cpy #4*2
    bcc MaskLoop

    ENDM
