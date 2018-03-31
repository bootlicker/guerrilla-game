;==================
; MACRO
;==================

;---------------------------------
;
; KERNEL MACRO
;
;---------------------------------

        MAC SCREEN_BANDS 
.BAND   SET {1}

;=================================
;
; Load Line Counter for this band
;
;=================================

    ldy Heights+.BAND

;=================================
;
; Preload GRP0 (Che) Graphics
;
;=================================

    lda (Player0Ptr+.BAND*2),y  ; 5  5
    and (Player0Msk+.BAND*2),y  ; 5 10
    tax                         ; 2 12
    lda (Player0Clr+.BAND*2),y  ; 5 17
    tay                         ; 2 19


;=================================
; Position GRP1 independently for
; each band of the screen.
;
; Accumulator holds X value.
;=================================

    lda ObjectX+.BAND+1

.PositionGRP1

    sec       
    sta WSYNC

;----------------------------------------------------- ENTER FIRST SCANLINE
    
    stx GRP0        ; 3  3
    sty COLUP0      ; 3  6
    
.DivideLoop1
    sbc #15         ; 2  8
    bcs .DivideLoop1 ; 2 10
    eor #7          ; 2 12
    asl             ; 2 14
    asl             ; 2 16
    asl             ; 2 18
    asl             ; 2 20
    sta RESP1       ; 2 23 <- set object position
    sta HMP1        ; 3 26
    sta WSYNC
;------------------------------------------------------ ENTER SECOND SCANLINE
    sta HMOVE           ; 3  3
    
;=================================
; We haven't had time to preload
; any graphics, so now we have to
; rush and try and make sure we have
; something to draw on this line
;=================================

;=================================
;
; Load Scanline Counter Again
; and Decrement
;
;=================================

    ldy Heights+.BAND             ; 3  6
    dey                           ; 2  8



    lda (Player0Ptr+.BAND*2),y  ; 5 13
    and (Player0Msk+.BAND*2),y  ; 5 18
    sta GRP0                    ; 3 21
    lda (Player0Clr+.BAND*2),y  ; 5 26
    sta COLUP0                  ; 3 29      <-- delayed. So we'll have to restrict
                                ;               Che's movement on the left of the
                                ;               screen?

;=================================
;
; Decrement Scanline Counter
;
;=================================

    dey                 ; 2 31
                        
; Prime the NUSIZ1 register with the right copies/sizing data

    lda EnvCopies+.BAND ; 3 34
    sta NUSIZ1          ; 3 37    
                        
;=================================
; Preload Che Graphics for THIRD
; scanline.
;=================================

                        
    lda (Player0Ptr+.BAND*2),y  ; 5 42
    and (Player0Msk+.BAND*2),y  ; 5 47

;=================================
;
; Start of the actual kernel
;
;=================================
.KernelLoop

    sta WSYNC                   ; 3 50
;----------------------------------------------------- ENTER THIRD SCANLINE
	
;=================================
;
; Draw Che Graphics in time for left-most
; side of the screen.
;
;=================================

    sta GRP0                    ; 3 3
    lda (Player0Clr+.BAND*2),y  ; 5 8
    sta COLUP0                  ; 3 11
    

    
;=================================
;
; Calculate Environment Graphics
;
;=================================

    lda (EnvGfxPtr+.BAND*2),y   ; 5 16
    sta GRP1                    ; 3 19
    lda (EnvClrPtr+.BAND*2),y   ; 5 24
    sta COLUP1                  ; 3 27


;=================================
; Decrement line counter
;=================================
    
    dey                ; 2 29

;=================================
;
; Calculate Che Graphics
;
;=================================

    lda (Player0Ptr+.BAND*2),y  ; 5 34
    and (Player0Msk+.BAND*2),y  ; 5 39

;=================================
; Test line counter, branch if zero
;=================================
    
    cpy #0                      ; 2 41
    bne .KernelLoop             ; 2 43 (3 44)


    
;=================================
; END OF THE KERNEL.
; Here we "fall into" the next band
; of the screen!
;=================================
    
.EndKernel              ; (3 17)

    ENDM
