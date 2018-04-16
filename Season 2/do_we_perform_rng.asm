
    MAC DO_WE_PERFORM_RNG

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

    ENDM
