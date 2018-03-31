
    MAC CHE_VARIABLES

	; Object X positions in $80-84
ObjectX:	ds 6	; player0, 5 x player1 objects

	; Object Y positions in $85-89
ObjectY:	ds 6	; player0, 5 x player1 objects

;===========Graphic Pointers in $90-95===========

; Che's graphics pointer	
Player0Ptr:	ds 10

; Environment graphics pointers
EnvGfxPtr: ds 10 ; $95

;==============Color Pointers====================
;========== in $A1 to $B2 =======================

; Che's colour pointer     
Player0Clr: ds 10    ; player0, stored in $A1-A2

; Environment colour pointers
EnvClrPtr: ds 10   ; $B2

;==============Mask Pointers=====================

; Che's mask pointer
Player0Msk: ds 10

;================================================


; Environment NUSIZX variables in $96-9A
EnvCopies:  ds 5


	; Indexes for player animation sequences
Animation0:	ds 1	; Stored in $9B
Animation1: ds 1    ; $9C

    ; Local animation indexes for Che animation
    
AnimateHor0:    ds 1    ; $9D
AnimateUp0:     ds 1    ; $9E
AnimateDown0:   ds 1    ; $9F

AnimFrameCounter:   ds 1    ; $A0


    ; Scratch Variable
CheGfxTemp:     ds 1    ; $B3
EnvGfxTemp:     ds 1    ; $B4
CheClrTemp:     ds 1
EnvClrTemp:     ds 1

Temp2:          ds 1    ; $B5 -- Scratch variable for animation. May remove.

    ; Playfield Scanline Height and Colour

; Playfield colour pointer
PFColourPtr:    ds 2    ; $B6-$B7

; Playfield bands heights
Heights:        ds 5    ; $B8-$BC

;==============Graphics Y positions==============
;=========== in $BD to C2 =======================

; Che Y Offsets
CheY:  ds 1    ; $BD

; Environment Y Offsets
EnvGfxOffset:   ds 5    ; $C2

    ; X and Y variables for returning Player0 to collision position
    
SavedX:         ds 1    ; $C3
SavedY:         ds 1    ; $C4

    ; Heights of Evironment Graphics
    
EnvHeight:      ds 5    ; $C9

    ENDM
