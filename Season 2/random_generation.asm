
    MAC RANDOM_GENERATION
    
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
; us to randomly select 4 different objects from
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

    ENDM
