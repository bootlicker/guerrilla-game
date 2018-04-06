
    MAC UPDATE_MAP

;=================================
; Process Map Movement
;
;   76543210
;   RLDUXXXX    - RIGHT LEFT DOWN UP
;
;=================================

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

    ENDM
