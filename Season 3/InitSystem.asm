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

	; Object X positions
ObjectX:	ds 5	; player0, 4 x player1 objects

	; Object Y positions
ObjectY:	ds 5	; player0, 4 x player1 objects

;===========Graphic Pointers===========

; Che's graphics pointer	
Player0Ptr:	ds 8

; Environment graphics pointers
EnvGfxPtr: ds 8

;==============Color Pointers====================

; Che's colour pointer     
Player0Clr: ds 8    ; player0

; Environment colour pointers
EnvClrPtr: ds 8   ;

;==============Mask Pointers=====================

; Che's mask pointer
Player0Msk: ds 8

;================================================


; Environment NUSIZX variables
EnvCopies:  ds 4


	; Indexes for player animation sequences
Animation0:	ds 1	; 
Animation1: ds 1    ; 

    ; Local animation indexes for Che animation
    
AnimateHor0:    ds 1
AnimateUp0:     ds 1
AnimateDown0:   ds 1

AnimFrameCounter:   ds 1


    ; Scratch Variable
CheGfxTemp:     ds 1
EnvGfxTemp:     ds 1
CheClrTemp:     ds 1
EnvClrTemp:     ds 1

Temp2:          ds 1    ; -- Scratch variable for animation. May remove.

    ; Playfield Scanline Height and Colour

; Playfield colour pointer
PFColourPtr:    ds 2

; Playfield bands heights
Heights:        ds 4

;==============Graphics Y positions==============

; Che Y Offsets
CheY:  ds 1    ; $BD

; Environment Y Offsets
EnvGfxOffset:   ds 4    ; $C2

    ; X and Y variables for returning Player0 to collision position
    
SavedX:         ds 1    ; $C3
SavedY:         ds 1    ; $C4

;=========Random Generation Variables============

Rand8:          ds 1
Rand16:         ds 1
Rand24:         ds 1
Rand32:         ds 1

Rand_Pointer_Calc8:     ds 1
Rand_Pointer_Calc16:    ds 1
Rand_Pointer_Calc24:    ds 1
Rand_Pointer_Calc32:    ds 1

Band_0_Index:   ds 1
Band_1_Index:   ds 1
Band_2_Index:   ds 1
Band_3_Index:   ds 1

Band_0_Index16s:   ds 1
Band_1_Index16s:   ds 1
Band_2_Index16s:   ds 1
Band_3_Index16s:   ds 1

;======Overworld Map Coordinate Variables========
Map_Coords:     ds 1
CheMapX:        ds 1
CheMapY:        ds 1

    
    echo "----",($00FE - *) , "bytes of RAM left"
    
;==================
; Initialise Atari
;==================
InitSystem:

	CLEAN_START

	; From here we fall into the main loop
	
    jmp OverworldVB
