;=================================
; Process Joystick
;
;   76543210
;   RLDUrldu    - RIGHT LEFT DOWN UP right left down up
;
; UPPERCASE denotes the left joystick directions
; lowercase denotes the right joystick directions
;=================================

    MAC PROCESS_JOYSTICK

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
    
    ENDM
