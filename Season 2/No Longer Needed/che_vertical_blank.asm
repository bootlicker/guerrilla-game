;================
; Vertical Blank
; --------------
; Game Logic
;================

    MAC VERTICAL_BLANK

VerticalBlank:
    
    jsr LevelProcessing
    jsr ProcessJoystick
    jsr PositionObjects
    
    sta HMCLR           ; Reset the horizontal movement registers
                        ; so the HMOVEs for GRP1 in the kernel don't
                        ; interfere with the HMOVE we made for GRP0
                        ; in VBLANK.
	
	rts

;=================================
; Level Processing
;
; This is where we will load in all the data we need to
; draw the new screens that Che moves between
;
;=================================

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
    
    rts


;=================================
; Process Joystick
;
;   76543210
;   RLDUrldu    - RIGHT LEFT DOWN UP right left down up
;
; UPPERCASE denotes the left joystick directions
; lowercase denotes the right joystick directions
;=================================

ProcessJoystick:
    lda SWCHA

; Store Player0 position to "bounce back"
; after a collision with something.
    
    ldy ObjectX
    sty SavedX
    ldy ObjectY
    sty SavedY

;=================================
; Is Joystick held right?
;=================================    

    asl             ; Put R in the carry bit
    bcs CheckLeft   ; If R wasn't 1, then check for L
    
    ldy ObjectX
    iny             ; Move ObjectX right
    
    cpy #160        ; Test for edge of screen
    bne SaveX       ; If not at edge of screen, save X position
    
    ldy #0          ; Else, wrap to the left edge.
    
SaveX:

    sty ObjectX     ; Save X position
    ldy #0          
    sty REFP0       ; Graphics are stored facing right.
                    ; So, turn off reflect player to make it face
                    ; right, because we are moving right.

;=================================
; Is Joystick held left?
;=================================    
                    
CheckLeft:
    asl             ; Shift A one bit left. Now L is in the carry bit.
    bcs CheckDown   ; Branch if joystick not held left.

    ldy ObjectX     ; Get X position
    dey             ; Move it left
    
    cpy #255        ; Test for edge of screen
    bne SaveX2      ; Save X if not at edge of screen

    ldy #159        ; Else wrap to right edge
    
SaveX2:

    sty ObjectX
    ldy #8
    sty REFP0       ; We are moving left, so flip graphics
                    ; to make face left.

;=================================
; Is Joystick held down?
;=================================    

CheckDown:

    asl             ; Shift A one bit left. D is in carry.
    bcs CheckUp     ; Branch if joystick not held down.
    ldy ObjectY     ; Get object's Y position
    dey             ; Move it down
    cpy #P0_HEIGHT  ; Test for bottom of screen
    bne SaveY       ; Save Y if we're not at botom
    ldy #PF_HEIGHT  ; Else wrap to top (is this right?)
    
SaveY:
    
    sty ObjectY     ; Save Y.
    
;=================================
; Is Joystick held up?
;=================================    

CheckUp:

    asl             ; Shift A one bt left. U is in now in carry.
    bcs OnePlayer   ; Branch if joystick not up. Only one player for now.
    ldy ObjectY     ; Get Y position
    iny             ; Move it up.
    cpy #PF_HEIGHT  ; Test for top of screen (is this right?)
    bne SaveY2      ; Save Y if we're not at top
    ldy #P0_HEIGHT  ; Else wrap to bottom.
    
SaveY2:

    sty ObjectY     ; Save Y.
    
OnePlayer:
    
    ; We're done
    
    rts
    
;=================================
; Position Objects
;
; This is an early version of the entire game,
; so we're only positioning GRP0.
;
;=================================

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
; SECTIONS 1-4
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
    cpx #4
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
    cpy #5*2
    bcc MaskLoop

    
    rts
    
    ENDM
