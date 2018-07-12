;======================
; Vertical Blank Bank =
; --------------      =
; Game Logic          =
;======================

OverworldVB_Code:

;===============
; Vertical Sync
;===============
VerticalSync:
	lda #2
	ldx #47
	sta WSYNC
	sta VSYNC
	stx TIM64T

	sta WSYNC
	sta WSYNC
	lda #0
	sta PF0
	sta PF1
	sta PF2
	
	sta GRP0   ; Blanks GRP0 if VDELP0 was off 
	sta GRP1   ; Blanks GRP0 if VDELP0 was on, or blanks GRP1 if VDELP1 was off 
	sta GRP0   ; Blanks GRP1 if VDELP1 was on
	
	sta WSYNC
	sta VSYNC

Sleep12:	; JSR here to sleep for 12 cycles
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

;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
; This sets the X and Y variables for Che Guevara, GRP0.
; It also sets flags if the boundary of a screen has been crossed.
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

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
    
    ldy #8          ; Else, wrap to the left edge. 
                    ; (8 pixels in, so that you can 
                    ; see the whole width of Che.
    
; Set the Map_Coords flag bit to tell the program
; to calculate the next screen in the RNG sequence.

    ldx #%10000000
    stx Map_Coords
    
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
    
; Set the Map_Coords flag bit to tell the program
; to calculate the next screen in the RNG sequence.

    ldx #%01000000
    stx Map_Coords
    
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

; Set the Map_Coords flag bit to tell the program
; to calculate the next screen in the RNG sequence.

    ldx #%00100000
    stx Map_Coords
    
    
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

; Set the Map_Coords flag bit to tell the program
; to calculate the next screen in the RNG sequence.

    ldx #%00010000
    stx Map_Coords
    
SaveY2:

    sty ObjectY     ; Save Y.
    
OnePlayer:
    
    ; We're done
    
;=================================
; Process Map Movement
;
;   76543210
;   RLDUXXXX    - RIGHT LEFT DOWN UP
;
;=================================

;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
; This logic takes the flags set in PROCESS_JOYSTICK and
; checks them to see if the map coordinates need updating.
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

ProcessMapMovement:

    lda Map_Coords
    
;=================================
; Has Che Moved Off the Right Edge of Screen?
;=================================    
    
    clc
    rol                 ; Put R in the carry bit
    bcc CheckMapLeft    ; If R wasn't 1, then check for L
    ldy CheMapX
    iny             ; Move ObjectX right
    
;    cpy #0          
;    bne SaveMapX    
;    ldy #0          
    
SaveMapX:

    sty CheMapX     ; Save X position
    
;=================================
; Has Che Moved Off Left Edge of Screen?
;=================================    
                    
CheckMapLeft:
    rol                 ; Shift A one bit left. Now L is in the carry bit.
    bcc CheckMapDown    ; Branch if no left map movement.
    ldy CheMapX         ; Get X position
    dey                 ; Move it left
    
;    cpy #255        - We don't need this code, because the 
;    bne SaveMapX2   - numerical limit of the registers in 
;    ldy #255        - this CPU equal the numerical limit
;                    - of the size of the map!    

SaveMapX2:

    sty CheMapX

;=================================
; Has Che Moved Off Bottom Edge of Screen?
;=================================    

CheckMapDown:

    rol             ; Shift A one bit left. D is in carry.
    bcc CheckMapUp  ; Branch if no down map movement.
    ldy CheMapY     ; Get object's Y position
    iny             ; Move it down

;    cpy #100        
;    bne SaveMapY    
;    ldy #0          
    
SaveMapY:
    
    sty CheMapY     ; Save Y.
    
;=================================
; Has Che Moved Off Top Edge of Screen?
;=================================    

CheckMapUp:

    rol             ; Shift A one bt left. U is in now in carry.
    bcc MapMoveDone ; Branch if no up map movement. Only one player for now.
    ldy CheMapY     ; Get Y position
    dey             ; Move it up.

;    cpy #255        
;    bne SaveMapY2   
;    ldy #99         
    
SaveMapY2:

    sty CheMapY     ; Save Y.
    
.MapMoveDone

;=================================
; Conditional logic for starting the Random Number Generator
;=================================

; Bit 5 of Map_Coords, the Up bit, is in the carry bit.
; The status register should look like this now:
;
; C76543210
; UXXXX0RLD

;==================================
;Is the UP flag set in Map_Coords?
;==================================

;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>                        
; This logic takes the flags set in PROCESS_JOYSTICK and performs
; the random generation needed to set the pointers for the
; right objects to appear on the screen for the correct map cell.
;
; It branches down to [No_RNG] if we have not moved into a new
; map cell. No new map cell means no new random generation, and
; that means we don't draw a blank frame.
;
;
; If we DO need to perform random generation, to update the
; pointers for the Environment Graphics, then we "fall into"
; the code below.
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

DoWePerformRNG:

    bcc CheckRNG_Down   ; If UP flag not set, check DOWN flag
    ldx #0
    jmp ShiftBackwards
    
.CheckRNG_Down

    ror                 ; Put D in carry bit
    bcc CheckRNG_Left   ; If DOWN flag not set, check LEFT flag
    ldx #0
    jmp ShiftForwards
    
.CheckRNG_Left

    ror                 ; Put L in carry bit
    bcc CheckRNG_Right  ; If LEFT flag not set, check RIGHT flag
    ldx #255
    jmp ShiftBackwards
    
.CheckRNG_Right

    ror           ; Put R in carry bit
    bcc No_RNG    ; If RIGHT flag not set, then no flags set, then exit
    ldx #255

;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
; No recalculation of graphics pointers is needed because a new map
; screen has been entered into.
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  

NoRNG:

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

;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
; Calculate the X coordinates for the environment graphics in the kernel
;
; This is currently hardwired/hardcoded.
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> 
    
X_Coords_For_EnvGfx:
    
    lda #20
    sta ObjectX+4
    
    lda #40
    sta ObjectX+3
    
    lda #60
    sta ObjectX+2
    
    lda #80 
    sta ObjectX+1
    
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
; End of No Random Generation Pathway Reached, Jump to Kernel
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> 

    jmp OverworldKernel

;=================================
; Wait for Vertical Blank to End
;=================================

; What we're going to do is use up a blank frame of kernel time
; in order to have enough time to calculate a full row of screens:
; 256 positions on the RNG counter.
    
Skip_Kernel:

	lda #$00        ; 2 13
	sta COLUBK      ; 3 16

RNGStartWait:
	
    sta WSYNC
;---------------------------------
	lda INTIM		    ; 4  4
	bne RNGStartWait    ; 2  6
	sta VBLANK		    ; 3  9 - Accumulator D1=0
	
    rts

;=================================
; Set timer for blank frame
;
; (192 * 76) / 64 = 228
;=================================

RNG_Timer:
	ldx #228
	stx TIM64T
                        
    rts

Random:


;=================================
; Random Number Generator Routine
;=================================

; SHIFT FORWARDS 

ShiftForwards:

    jsr Skip_Kernel

.ShiftForwardsLoop

; FIRST BATCH of 16-BIT RNG
    
    lda Rand8       ; 3  3
    lsr             ; 2  5
    rol Rand16      ; 5 10
    bcc noeor_for   ; 2 12
    eor #$D4        ; 2 14 - $D4 is the only number I know the inverse to 

.noeor_for          ;
    sta Rand8       ; 3 17
    eor Rand16      ; 3 20
    
; SECOND BATCH OF 16-BIT RNG
    
    lda Rand24      ; 3 23
    lsr             ; 2 25
    rol Rand32      ; 5 30
    bcc noeor_for_2 ; 2 32
    eor #$??        ; 2 34 

.noeor_for_2        ;
    sta Rand24      ; 3 37
    eor Rand32      ; 3 40

    inx             ; 2 42
    clc             ; 2 44
    bne ShiftForwardsLoop   ; 2 46
    beq Pointer_Calc        ; 2 48
    
; TOTAL TIME FOR RNG = 228 * 64 = 14 592
; TOTAL TIME USED FOR 256 CELL ROTATION:
; ~48 * 256 ~= 12 288 
    
    
; SHIFT BACKWARDS

ShiftBackwards:

    jsr Skip_Kernel

.ShiftBackwardsLoop

; FIRST BATCH of 16-BIT RNG

    lda Rand8
    lsr
    rol Rand16
    bcc noeorleft
    eor #$A9    ; $D4 is the only number I know the inverse to 

.noeorleft 
    sta Rand8
    eor Rand16

; SECOND BATCH OF 16-BIT RNG
    
    lda Rand24       ; 3 23
    lsr              ; 2 25
    rol Rand32       ; 5 30
    bcc noeor_left_2 ; 2 32
    eor #$??         ; 2 34 

.noeor_left_2        ;
    sta Rand24       ; 3 37
    eor Rand32       ; 3 40

    inx                     ; 2 42
    clc                     ; 2 44
    bne ShiftBackwardsLoop  ; 2 46
    beq Pointer_Calc        ; 2 48
    
; TOTAL TIME FOR RNG = 228 * 64 = 14 592
; TOTAL TIME USED FOR 256 CELL ROTATION:
; ~48 * 256 ~= 12 288

;================================================
; Translation of the output of the random number
; generator into a useful index for selecting
; a random object in ROM.
; 
; Here we:
; - Buffer the output of the RNG into RAM
; - Enter into a 4 cycle loop, which masks off
;   different portions of the 16 bit random number
;   so that a number between 0-63 is produced.
;
; This translates the random number output into
; an index that is 64 positions long, and allows
; us to randomly select 4 different objects
; that 64 bit number.
;
; A second level of randomness could be introduced
; by generating an 8 bit random number, and masking
; THOSE bits off. But that would make the game
; impossible to seed the same way every time.
;
;================================================
    
Pointer_Calc:
    
    ldy #3
    
.Pointer_Calc_Loop

    ldx Rand8
    stx Rand_Pointer_Calc8
    
    ldx Rand16
    stx Rand_Pointer_Calc16
    
    ldx Rand24
    stx Rand_Pointer_Calc24
    
    ldx Rand32
    stx Rand_Pointer_Calc24
    
    cpy #3
    beq Band_3_Calc
    cpy #2
    beq Band_2_Calc
    cpy #1
    beq Band_1_Calc
    cpy #0
    beq Band_0_Calc
    
.Band_3_Calc

    lda Rand_Pointer_Calc16
    and #%11111100
    lsr
    lsr
    sta Band_3_Index

    jmp Done_Calc
    
.Band_0_Calc

    lda Rand_Pointer_Calc8
    and #%00111111
    sta Band_0_Index
    jmp Done_Calc
    
.Band_2_Calc

    lda Rand_Pointer_Calc16
    and #%00000111
    sta Rand_Pointer_Calc16
    
    lda Rand_Pointer_Calc8
    and #%11100000
    lsr
    lsr
    clc
    and Rand_Pointer_Calc16
    sta Band_2_Index
    
    
    jmp Done_Calc

.Band_1_Calc

    lda Rand_Pointer_Calc16
    and #%0011100
    asl
    sta Rand_Pointer_Calc16
    
    lda Rand_Pointer_Calc8
    and #%0011100
    lsr
    lsr
    and Rand_Pointer_Calc16
    sta Band_1_Index

.Done_Calc    

    dey
    bpl Pointer_Calc_Loop

; Alright! We're outta here!

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
    
    lda #48
    sta Heights+3
    
    lda #48
    sta Heights+2
    
    lda #48
    sta Heights+1
    
    lda #48
    sta Heights
        
    ENDM

;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    ; Now we need to translate the different
    ; Band_X_Index numbers into the debugging symbols.
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
    
EnvGfx_Pointers:
    
    lda Band_0_Index
    and #%11110000
    sta Band_0_Index16s
    
    lda Band_0_Index
    and #%00001111
    sta Band_0_Index
    
    lda Band_1_Index
    and #%11110000
    sta Band_1_Index16s
    
    lda Band_1_Index
    and #%00001111
    sta Band_1_Index
    
    lda Band_2_Index
    and #%11110000
    sta Band_2_Index16s
    
    lda Band_2_Index
    and #%00001111
    sta Band_2_Index

    lda Band_3_Index
    and #%11110000
    sta Band_3_Index16s
    
    lda Band_3_Index
    and #%00001111
    sta Band_3_Index
    
    ; BAND 0
    
    lda #<BatistaSoldierClr
    sta EnvClrPtr
    lda #>BatistaSoldierClr
    sta EnvClrPtr+1
    
;=================================
; BANDS 1-3
;=================================

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

EnvGfxPtrTableLow:
    .byte <(Number_0)   ; 0
    .byte <(Number_1)   ; 1
    .byte <(Number_2)   ; 2
    .byte <(Number_3)   ; 3
    .byte <(Number_4)   ; 4
    .byte <(Number_5)   ; 5
    .byte <(Number_6)   ; 6
    .byte <(Number_7)   ; 7
    .byte <(Number_8)   ; 8
    .byte <(Number_9)   ; 9
    .byte <(Number_A)   ; 10
    .byte <(Number_B)   ; 11
    .byte <(Number_C)   ; 12
    .byte <(Number_D)   ; 13
    .byte <(Number_E)   ; 14
    .byte <(Number_F)   ; 15
        
EnvGfxPtrTableHi:
    .byte >(Number_0)   ; 0
    .byte >(Number_1)   ; 1
    .byte >(Number_2)   ; 2
    .byte >(Number_3)   ; 3
    .byte >(Number_4)   ; 4
    .byte >(Number_5)   ; 5
    .byte >(Number_6)   ; 6
    .byte >(Number_7)   ; 7
    .byte >(Number_8)   ; 8
    .byte >(Number_9)   ; 9
    .byte >(Number_A)   ; 10
    .byte >(Number_B)   ; 11
    .byte >(Number_C)   ; 12
    .byte >(Number_D)   ; 13
    .byte >(Number_E)   ; 14
    .byte >(Number_F)   ; 15

RNGFinishWait:

    sta WSYNC
;---------------------------------
	lda INTIM		; 4  4
	bne RNGWait     ; 2  6

;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
; End of YES Random Generation Pathway Reached, Jump to Overscan
;<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><> 
	
    jmp OverworldOS
