
    MAC ENVGFX_POINTERS

    ; Now we need to translate the different
    ; Band_X_Index numbers into the debugging symbols.
    
    
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
    
    ENDM
