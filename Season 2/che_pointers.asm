
    MAC CHE_POINTERS

Che_Pointers:

ShapePtrLow:
    .byte <(CheHorizontal0)     ; 0
    .byte <(CheHorizontal1)     ; 1
    .byte <(CheHorizontal2)     ; 2
    .byte <(CheFrontLeftStand)  ; 3
    .byte <(CheFrontRightStep)  ; 4
    .byte <(CheFrontRightStand) ; 5
    .byte <(CheFrontLeftStep)   ; 6
    .byte <(CheBackLeftStand)  ; 7
    .byte <(CheBackRightStep)  ; 8
    .byte <(CheBackRightStand) ; 9
    .byte <(CheBackLeftStep)   ; 10

        
ShapePtrHi:
    .byte >(CheHorizontal0)     ; 0
    .byte >(CheHorizontal1)     ; 1
    .byte >(CheHorizontal2)     ; 2
    .byte >(CheFrontLeftStand)  ; 3
    .byte >(CheFrontRightStep)  ; 4
    .byte >(CheFrontRightStand) ; 5
    .byte >(CheFrontLeftStep)   ; 6
    .byte >(CheBackLeftStand)  ; 7
    .byte >(CheBackRightStep)  ; 8
    .byte >(CheBackRightStand) ; 9
    .byte >(CheBackLeftStep)   ; 10

        
ColourPtrLow:
    .byte <(CheHorizontalClr0)
    .byte <(CheHorizontalClr1)
    .byte <(CheHorizontalClr2)
    .byte <(CheFrontLeftStandClr)  ; 3
    .byte <(CheFrontRightStepClr)  ; 4
    .byte <(CheFrontRightStandClr) ; 5
    .byte <(CheFrontLeftStepClr)   ; 6
    .byte <(CheBackLeftStandClr)  ; 7
    .byte <(CheBackRightStepClr)  ; 8
    .byte <(CheBackRightStandClr) ; 9
    .byte <(CheBackLeftStepClr)   ; 10

    
ColourPtrHi:
    .byte >(CheHorizontalClr0)
    .byte >(CheHorizontalClr1)
    .byte >(CheHorizontalClr2)
    .byte >(CheFrontLeftStandClr)  ; 3
    .byte >(CheFrontRightStepClr)  ; 4
    .byte >(CheFrontRightStandClr) ; 5
    .byte >(CheFrontLeftStepClr)   ; 6
    .byte >(CheBackLeftStandClr)  ; 7
    .byte >(CheBackRightStepClr)  ; 8
    .byte >(CheBackRightStandClr) ; 9
    .byte >(CheBackLeftStepClr)   ; 10

    ENDM
