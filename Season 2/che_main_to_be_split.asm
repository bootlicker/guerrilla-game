;=================
; Initialise DASM
;=================
	
	PROCESSOR 6502

	include vcs.h

	include macro.h
	
    include che_variables.asm      
	
	include che_kernel_macro.asm
	
	include che_kernel_macro_debug.asm
	
	include kernel_implemented.asm
	
	include kernel_implemented_debug.asm
	
	include position_animate_che.asm
	
	include skip_kernel.asm
	
	include rng_finish_wait.asm
	
	include update_map_coords.asm
	
	include do_we_perform_rng.asm
	
	include random_generation.asm
	
	include level_processing.asm   
	
	include x_coords_for_envgfx.asm
	
	include env_gfx_pointers.asm
	
	include process_joystick.asm
	
	include position_objects.asm
	
	include che_pointers.asm
	
	include che_mask.asm
	
	include che_graphics.asm
	
	
    MAC JUMP_TABLE ; put this at the start of every bank
    RORG $F000
InitSystem
    cmp SelectBank8   ; inits system then goes to the menu
    jmp InitSystemCode   ; jump to Main Menu
Kernel
    cmp SelectBank7    ; draws score, top castles, dragons
    jmp KernelCode
OverScan
    cmp SelectBank8
    jmp OverScanCode


    MAC BANKS_AND_VECTORS ; put this at the end of every bank
    RORG $FFF4
SelectBank1 .byte $00
SelectBank2 .byte $00
SelectBank3 .byte $00
SelectBank4 .byte $00
SelectBank5 .byte $00
SelectBank6 .byte $00
SelectBank7 .byte $00
SelectBank8 .byte $00
    .word InitSystem ; NMI and 8 overlap NMI
    .word InitSystem ; RESET
    .word InitSystem ; IRQ
    ENDM

	
	
;===================
; Define Constants =
;===================

PF_HEIGHT   = 192  

P0_HEIGHT   = 25

;==================
; Define RAM Usage
;==================

	SEG.U VARS

	ORG $80

    CHE_VARIABLES
    
    echo "----",($00FE - *) , "bytes of RAM left"

;========================================
; PosObject
; ---------
;
; A - Holds the X position of the object
; X - holds the object to position
;     0 = player0
;     1 = player1
;
; player 0 -> Range is 0-159
;========================================

PosObject:
	sec
	sta WSYNC
DivideLoop
	sbc #15		; 2  2
	bcs DivideLoop	; 2  4
	eor #7		; 2  6
	asl		; 2  8
	asl		; 2 10
	asl		; 2 12
	asl		; 2 14
	sta.wx HMP0,X	; 5 19 - store fine tuning of X
	sta RESP0,X	; 4 23 - set coarse X position of object
	rts		; 6 29

;==================
; Initialise Atari
;==================
InitSystem:

	CLEAN_START

	; From here we fall into the main loop
	
;===================
; Main Program Loop
;===================

Main:

    jsr VerticalSync


;================
; Vertical Blank
; --------------
; Game Logic
;================

VerticalBlank:
    
    PROCESS_JOYSTICK    ; This sets the X and Y variables for Che Guevara, GRP0.
                        ; It also sets flags if the boundary of a screen has been crossed.
                        
    UPDATE_MAP          ; This logic takes the flags set in PROCESS_JOYSTICK and
                        ; checks them to see if the map coordinates need updating.
                        
    DO_WE_PERFORM_RNG   ; This logic takes the flags set in PROCESS_JOYSTICK and performs
                        ; the random generation needed to set the pointers for the
                        ; right objects to appear on the screen for the correct map cell.
                        ;
                        ; It branches down to [No_RNG] if we have not moved into a new
                        ; map cell. No new map cell means no new random generation, and
                        ; that means we don't draw a blank frame.
    
                        
                        ; If we DO need to perform random generation, to update the
                        ; pointers for the Environment Graphics, then we "fall into"
                        ; the code below.

Skip_Kernel:
                        
    SKIP_KERNEL         ;  ---  <--SKIP_KERNEL macro is logic that will wait until 
                        ;   |
    rts                 ;   |
                        ;   |      the end of Vertical Blank, 
Random:                 ;   |      and then start a timer for a full blank frame.
                        ;   |
    RANDOM_GENERATION   ;<--|---- We actually fall into THIS logic, but this logic calls
                        ;   |     Skip_Kernel as a subroutine.
                        ;   |
LevelProcessing:        ;   |------BLANK FRAME HERE
                        ;   |
    LEVEL_PROCESSING    ;<--|---- not much actual logic going on here, will move this.
                        ;   |
EnvGfx_Pointers:        ;   |
                        ;   |
    ENVGFX_POINTERS     ;   |
                        ;   |
RNGFinishWait:          ;   |
                        ;   |
    RNG_FINISH_WAIT     ;  ---  <--RNG_FINISH_WAIT macro is logic that will wait
                        ;          until end of the blank frame.
    
    jmp Skip_to_OverScan; DON'T DRAW THE KERNEL AFTER RNG.
                        ; WE HAVE ALREADY USED UP AN ENTIRE
                        ; FRAME OF KERNEL TIME.
                        ;
                        ; I am worried that this jump is going to be too large.
                        ; We'll check this after I have compiled.
NoRNG:
    POSITION_ANIMATE_CHE
    X_COORDS_FOR_ENVGFX
    
;=================================
;
; KERNEL
;
;=================================
    
Kernel:
    
    sta HMCLR           ; Reset the horizontal movement registers
                        ; so the HMOVEs for GRP1 in the kernel don't
                        ; interfere with the HMOVE we made for GRP0
                        ; in VBLANK.

    KERNEL_IMPLEMENTED_DEBUG

Skip_to_OverScan:

    jsr OverScan
    jmp Main

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
; Pointer Logic For Che's Graphics
;=================================
	
    CHE_POINTERS
    
;=================================
;=================================
; GRAPHICS
;=================================
;=================================

    align 256
    
    CHE_MASK

    CHE_GRAPHICS

; ENVIRONMENT PLACEHOLDER GRAPHICS
Number_0
    ds 38, $00
    
    .byte #%11100111
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%11100111
    
Number_1
    ds 38, $00
    
    .byte #%11100111
    .byte #%01000010
    .byte #%01000010
    .byte #%01000010
    .byte #%01000010
    .byte #%01000010
    .byte #%01000010
    .byte #%11000110
    
Number_2
    ds 38, $00
    
    .byte #%11100111
    .byte #%10000100
    .byte #%10000100
    .byte #%10000100
    .byte #%11100111
    .byte #%00100001
    .byte #%00100001
    .byte #%11100111    

Number_3
    ds 38, $00
    
    .byte #%11100111
    .byte #%00100001
    .byte #%00100001
    .byte #%00100001
    .byte #%11100111
    .byte #%00100001
    .byte #%00100001
    .byte #%11100111    

Number_4
    ds 38, $00
    
    .byte #%00100001
    .byte #%00100001
    .byte #%00100001
    .byte #%00100001
    .byte #%11100111
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101    

Number_5
    ds 38, $00
    
    .byte #%11100111
    .byte #%00100001
    .byte #%00100001
    .byte #%00100001
    .byte #%11100111
    .byte #%10000100
    .byte #%10000100
    .byte #%11100111    

Number_6
    ds 38, $00
    
    .byte #%11100111
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%11100111
    .byte #%10000100
    .byte #%10000100
    .byte #%11100111
    
Number_7
    ds 38, $00
    
    .byte #%01000010
    .byte #%01000010
    .byte #%01000010
    .byte #%01000010
    .byte #%00100001
    .byte #%00100001
    .byte #%00100001
    .byte #%11100111    

Number_8
    ds 38, $00
    
    .byte #%11100111
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%11100111
    .byte #%10100101
    .byte #%10100101
    .byte #%11100111    

Number_9
    ds 38, $00
    
    .byte #%11100111
    .byte #%00100001
    .byte #%00100001
    .byte #%00100001
    .byte #%11100111
    .byte #%10100101
    .byte #%10100101
    .byte #%11100111
    
Number_A
    ds 38, $00
    
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%11100111
    .byte #%10100101
    .byte #%10100101
    .byte #%11100111    


Number_B
    ds 38, $00
    
    .byte #%11000110
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%11000110
    .byte #%10100101
    .byte #%10100101
    .byte #%11000110
    
Number_C
    ds 38, $00
    
    .byte #%11100111
    .byte #%10000100
    .byte #%10000100
    .byte #%10000100
    .byte #%10000100
    .byte #%10000100
    .byte #%10000100
    .byte #%11100111    

Number_D
    ds 38, $00
    
    .byte #%11000110
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%10100101
    .byte #%11000110
    
Number_E
    ds 38, $00
    
    .byte #%11100111
    .byte #%10000100
    .byte #%10000100
    .byte #%10000100
    .byte #%11100111
    .byte #%10000100
    .byte #%10000100
    .byte #%11100111

Number_F
    ds 38, $00
    
    .byte #%10000100
    .byte #%10000100
    .byte #%10000100
    .byte #%10000100
    .byte #%11100111
    .byte #%10000100
    .byte #%10000100
    .byte #%11100111

EnvGfxClrTable
    ds 38, $00

    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    .byte $FE
    
;==========
; Overscan
;==========

OverScan:
	sta WSYNC
	lda #2
	sta VBLANK
	lda #36
	sta TIM64T
	sta WSYNC

.Wait
	lda INTIM
	bne .Wait
	
	rts

;=================================
; free space check before End of Cartridge
;=================================        
    echo "------", [$FFFA - *]d, "bytes free before End of Cartridge"
    
;=================================
; Define End of Cartridge
;=================================
        ORG $FFFA        ; set address to 6507 Interrupt Vectors 
        BANKS_AND_VECTORS
