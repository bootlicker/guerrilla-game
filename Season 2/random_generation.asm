    MAC RANDOM_GENERATION
    
Random:

;=================================
; Process Map Movement
;
;   76543210
;   RLDUXXXX    - RIGHT LEFT DOWN UP
;
;=================================

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

    asl                 ; Put R in the carry bit
    bcc CheckMapLeft    ; If R wasn't 1, then check for L
    
    ldy CheMapX
    iny             ; Move ObjectX right
    
    cpy #100        ; Test for edge of map.
    bne SaveMapX    ; If not at edge of the map, save X map coordinate.
    
    ldy #0          ; Else, wrap to the left edge.
    
SaveMapX:

    sty CheMapX     ; Save X position
    
;=================================
; Has Che Moved Off Left Edge of Screen?
;=================================    
                    
CheckMapLeft:
    asl                 ; Shift A one bit left. Now L is in the carry bit.
    bcc CheckMapDown    ; Branch if no left map movement.

    ldy CheMapX     ; Get X position
    dey             ; Move it left
    
    cpy #255            ; Test for left edge of map
    bne SaveMapX2       ; Save X if not at edge of map

    ldy #99        ; Else wrap to right edge
    
SaveMapX2:

    sty CheMapX

;=================================
; Has Che Moved Off Bottom Edge of Screen?
;=================================    

CheckMapDown:

    asl             ; Shift A one bit left. D is in carry.
    bcc CheckMapUp  ; Branch if no down map movement.
    ldy CheMapY     ; Get object's Y position
    iny             ; Move it down
    cpy #100        ; Test for bottom of map
    bne SaveMapY    ; Save Y if we're not at botom
    ldy #0          ; Else wrap to top (is this right?)
    
SaveMapY:
    
    sty CheMapY     ; Save Y.
    
;=================================
; Has Che Moved Off Top Edge of Screen?
;=================================    

CheckMapUp:

    asl             ; Shift A one bt left. U is in now in carry.
    bcc MapMoveDone ; Branch if no up map movement. Only one player for now.
    ldy CheMapY     ; Get Y position
    dey             ; Move it up.
    cpy #255        ; Test for top of map 
    bne SaveMapY2   ; Save Y if we're not at top
    ldy #99         ; Else wrap to bottom.
    
SaveMapY2:

    sty CheMapY     ; Save Y.

.MapMoveDone

.ShiftDown
        ldx #100

.ShiftRight
        lda Rand8   ; 3  3
        lsr         ; 2  5
        rol Rand16  ; 5 10
        bcc noeor   ; 2 12
        eor #$D4    ; 2 14 - $D4 is the only number I know the inverse to 
.noeor              ;
        sta Rand8   ; 3 17
        eor Rand16  ; 3 20

.ShiftLeft
        lda Rand8
        lsr
        rol Rand16
        bcc noeorleft
        eor #$A9    ; $D4 is the only number I know the inverse to 
.noeorleft 
        sta Rand8
        eor Rand16
