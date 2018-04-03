    MAC RANDOM_GENERATION
    
Random:


;=================================
; Process Map Movement
;
;   76543210
;   RLDUXXXX    - RIGHT LEFT DOWN UP
;
;=================================

ProcessMapMovement:

    lda Map_Coords
    
; Store Che's map position just in case
; we need to return him to the last map screen.
    
    ldy CheMapX
    sty CheMapSavedX
    ldy CheMapY
    sty CheMapSavedY
    
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
; Forming the input of the Random Number Generator
;=================================

; Bit 5 of Map_Coords, the Up bit, is in the carry bit.
; The status register should look like this now:
;
; C76543210
; UXXXX0RLD

;==================================
;Is the UP flag set in Map_Coords?
;==================================

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
    
;=================================
; Wait for Vertical Blank to End
;=================================

; What we're going to do is use up a blank frame of kernel time
; in order to have enough time to calculate a full row of screens:
; 256 positions on the RNG counter.

	lda #$00        ; 2 13
	sta COLUBK      ; 3 16

RNGStartWait:
	
    sta WSYNC
;---------------------------------
	lda INTIM		    ; 4  4
	bne RNGStartWait    ; 2  6
	sta VBLANK		    ; 3  9 - Accumulator D1=0

;=================================
; Set timer for blank frame
;
; (192 * 76) / 64 = 228
;=================================

RNG_Timer:
	ldx #228
	stx TIM64T

ShiftForwards:
    lda Rand8   ; 3  3
    lsr         ; 2  5
    rol Rand16  ; 5 10
    bcc noeor   ; 2 12
    eor #$D4    ; 2 14 - $D4 is the only number I know the inverse to 

.noeor          ;
    sta Rand8   ; 3 17
    eor Rand16  ; 3 20
    
; t

ShiftBackwards:

        lda Rand8
        lsr
        rol Rand16
        bcc noeorleft
        eor #$A9    ; $D4 is the only number I know the inverse to 
.noeorleft 
        sta Rand8
        eor Rand16
        
RNGFinishWait:
	
    sta WSYNC
;---------------------------------
	lda INTIM		; 4  4
	bne RNGWait     ; 2  6
        
NoRNG:

; Alright! We're outta here!
