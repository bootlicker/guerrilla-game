
; Thanks to JeremiahK for all of this help with teaching me how bank switching works!

	PROCESSOR 6502
	
	include vcs.h
	include macro.h

;===========================
;= Definition of Constants =
;===========================

;-------------------------------------
;- Definition of Bank Switch Vectors - 
;-------------------------------------
	
SelectBank1 equ $1FF4
SelectBank2 equ $1FF5
SelectBank3 equ $1FF6
SelectBank4 equ $1FF7
SelectBank5 equ $1FF8
SelectBank6 equ $1FF9
SelectBank7 equ $1FFA
SelectBank8 equ $1FFB

;====================================
;= Definition of Bank Switch Macros =
;====================================

;--------------------
;- Bank End Vectors -
;--------------------

; put this at the end of every bank

    MAC BANKS_AND_VECTORS 

    ORG $1FF4
    
SelectBank1 .byte $00
SelectBank2 .byte $00
SelectBank3 .byte $00
SelectBank4 .byte $00
SelectBank5 .byte $00
SelectBank6 .byte $00
SelectBank7 .byte $00
SelectBank8 .byte $00
    
;   .word InitSystem ; NMI and 8 overlap NMI (SO THIS MUST BE COMMENTED OUT!)
    .word InitSystem ; RESET
    .word InitSystem ; IRQ
    
    ENDM
    
;---------------------------------
;- Bank Start Jump Table Vectors -
;---------------------------------

; put this at the start of every bank

    MAC SELECT_BANK 
    
    SEG BANK_{1}

    RORG $F000	
	ORG [{1} - 1] * $1000
	
	SUBROUTINE

InitSystem
	nop SelectBank1
	jmp InitSystemCode
OverworldVB
	nop SelectBank2
	jmp OverworldVB_Code
OverworldKernel
	nop SelectBank3
	jmp OverworldKernel_Code
OverworldOS
    nop SelectBank4
    jmp OverworldOS_Code
BattleVB
    nop SelectBank5
    jmp BattleVB_Code
BattleKernel
    nop SelectBank6
    jmp BattleKernel_Code
BattleOS
    nop SelectBank7
    jmp BattleOS_Code
	
	
	ENDM

; <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
; Include 4K banks
; <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

	SELECT_BANK 1
	include InitSystem.asm
	END_BANK 1
	
	SELECT_BANK 2
	include OverworldVB.asm
	END_BANK 2
	
	SELECT_BANK 3
	include OverworldKernel.asm
	END_BANK 3
	
	SELECT_BANK 4
	include OverworldOS.asm
	END_BANK 4
	
    SELECT_BANK 5
    include BattleVB.asm
    END_BANK 5
    
    SELECT_BANK 6
    include BattleKernel.asm
    END_BANK 6

    SELECT_BANK 7
    include BattleOS.asm
    END_BANK 7

