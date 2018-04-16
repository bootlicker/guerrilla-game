        PROCESSOR 6502

; Stay Frosty
; Last Update: November 22, 2007
; Design by:   Nathan Strum
; Program by:  Darrell Spice, Jr.

; bank 1 = KERNEL - all the code to draw the screen and the graphics to do so
; bank 2 = everything else

; process flow
;   KERNEL
;          - Horizon (sun or moon, hills)
;          - Section 4 - topmost platform
;          - Section 3
;          - Section 2 - middle platform
;          - Section 1
;          - Section 0 - special case of Section routine to draw
;                        platform across the entire bottom of screen
;          - Score
;          - Reserve Lives
;
;   OVERSCAN
;          - check for end-of-level
;          - Process collisions with fireballs
;          - Process collisions with platforms
;
;   VERTICAL BLANK
;          - Trigger Vertical Blank
;          - Process Sound Effects
;          - Sun's Heat (melts Ice & Frosty)
;          - Fireball Processing (flicker, image pointer prep)
;          - Level Specific Processing
;          - Prepare Horizon (sun, moon, sky color, etc)

NTSC            = 0
PAL             = 1

; Set this to NTSC or PAL to select which version to compile
; note: due to lack of time, PAL=PAL60, only the colors are changed
COMPILE_VERSION = NTSC

; set this to 1 to show VB and OS time remaining in Score+1 and Score+2
DEBUG_INFO = 0



 IF COMPILE_VERSION = NTSC
PLATFORM_COLOR   = $64 ; purple
ICE_TOP_COLOR    = $9E ; light blue
ICE_COLOR        = $98 ; blue
ICE_BOTTOM_COLOR = $94 ; dark blue
SUN_COLOR        = $18 ; yellow
DAY_SKY_COLOR    = $80 ; blue
FIREBALL_COLOR   = $38 ; reddish orange
HAT_BAND_COLOR   = $42 ; red
 ELSE
PLATFORM_COLOR   = $A4 ; purple
ICE_TOP_COLOR    = $9E ; light blue
ICE_BOTTOM_COLOR = $96 ; dark blue
ICE_COLOR        = $BA ; blue 
SUN_COLOR        = $28 ; yellow
DAY_SKY_COLOR    = $D0 ; blue
FIREBALL_COLOR   = $48 ; reddish orange
HAT_BAND_COLOR   = $62 ; red 
 ENDIF

TIA_BASE_READ_ADDRESS = $30
        include VCS.H
        include macro.h
        include graphics.h
                
        
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
PosObject   ; A holds X value
        sec ; X holds object, 0=P0, 1=P1, 2=M0, 3=M1, 4=Ball
        sta WSYNC
DivideLoop
        sbc #15        ; 2
        bcs DivideLoop ; 2    4
        eor #7         ; 2    6
        asl            ; 2    8
        asl            ; 2   10
        asl            ; 2   12
        asl            ; 2   14
        sta.wx HMP0,X  ; 5   19
        sta RESP0,X    ; 4   23 <- set object position
        sta WSYNC
        sta HMOVE      ; 3
        rts            ; 6    9
        ENDM

        MAC BANKS_AND_VECTORS ; put this at the end of every bank
        RORG $FFF4
SelectBank1 .byte $00
SelectBank2 .byte $00
SelectBank3 .byte $00
SelectBank4 .byte $00
SelectBank7 .byte $00
SelectBank8 .byte $00
;SelectBank7 .byte $00
;SelectBank8 .byte $00
       .word InitSystem ; NMI and 8 overlap NMI
        .word InitSystem ; RESET
        .word InitSystem ; IRQ
        ENDM

        
; usage SECTION #
; # = 0 thru 4 for 5 sections
; level 0 is lowest section and has special condition
; of being solid in the platform area.
        MAC SECTION 
.SEC    SET {1}
        ldy Heights+.SEC                ;    load Y with height of current section
        lda #ICE_TOP_COLOR
        sta COLUPF
        lda (FrostyImagePtr+.SEC*2),y
        and (FrostyMaskPtr+.SEC*2),y
        tax
        lda (FrostyColorPtr+.SEC*2),y
        tay
        lda FireBallX+.SEC*2
        ; on 1 scan line PosPlayer1 draws & colors Frosty
        ; positions player 1
        ; returns at cycle 9 on the next scan line
        jsr PosPlayer1                ; 9
        ldy Heights+.SEC              ; 3  12
        dey                           ; 2  14
        lda (FrostyImagePtr+.SEC*2),y ; 5  19
        and (FrostyMaskPtr+.SEC*2),y  ; 5  24
        sta GRP0                      ; 3  27 <- slightly delayed
        lda (FrostyColorPtr+.SEC*2),y ; 5  32
        sta COLUP0                    ; 3  35 <- delayed
        lsr FrostyMeltY               ; 5  40
        dey                           ; 2  42
        cpy #11                       ; 2  44
        bcc .iceLoop                  ; 2  46
        ; shift melt-enable bit into position for this section

.airLoop
        lda (FrostyImagePtr+.SEC*2),y ; 5  47
        and (FrostyMaskPtr+.SEC*2),y  ; 5  52
        sta WSYNC                     ; 3  55
        sta GRP0                      ; 3
        lda (FrostyColorPtr+.SEC*2),y ; 5  8
        sta COLUP0                    ; 3 11
        lda (FireImagePtr+.SEC*2),y   ; 5 16
        sta GRP1                      ; 3 19
        dey                           ; 2 21
        cpy IceSize                   ; 2 23
        bcs .airLoop                  ; 3 26
        
; this draws top row of ice with light blue        
        lda (FrostyImagePtr+.SEC*2),y ; 5  64
        and (FrostyMaskPtr+.SEC*2),y  ; 5  69
        sta WSYNC                     ; 3  72
        sta GRP0                      ; 3
        lda (FrostyColorPtr+.SEC*2),y ; 5   8
        sta COLUP0                    ; 3  11
        lda (FireImagePtr+.SEC*2),y   ; 5  16
        sta GRP1                      ; 3  19
        ldx IceData+.SEC*4            ; 3  22
        stx PF1                       ; 3  25
        ldx IceData+.SEC*4+1          ; 3  28
        stx PF2                       ; 3  31
        ldx IceData+.SEC*4+2          ; 3  34
        SLEEP 9                       ; 9  43
        dey                           ; 2  45
        stx PF2                       ; 3  48 <- must be at 48
        ldx IceData+.SEC*4+3          ; 3  51
        stx PF1                       ; 3  54
        
        lda (FrostyImagePtr+.SEC*2),y ; 5  64
        and (FrostyMaskPtr+.SEC*2),y  ; 5  69
        sta WSYNC                     ; 3  72
        sta GRP0                      ; 3
        lda (FrostyColorPtr+.SEC*2),y ; 5   8
        sta COLUP0                    ; 3  11
        lda (FireImagePtr+.SEC*2),y   ; 5  16
        sta GRP1                      ; 3  19
        ldx IceData+.SEC*4            ; 3  22
        stx PF1                       ; 3  25
        ldx IceData+.SEC*4+1          ; 3  28
        stx PF2                       ; 3  31
        ldx IceData+.SEC*4+2          ; 3  34
        SLEEP 9                       ; 9  43
        dey                           ; 2  45
        stx PF2                       ; 3  48 <- must be at 48
        ldx IceData+.SEC*4+3          ; 3  51
        stx PF1                       ; 3  54
        
        lda (FrostyImagePtr+.SEC*2),y ; 5  59
        and (FrostyMaskPtr+.SEC*2),y  ; 5  64
        ldx #ICE_COLOR                ; 2  66
        SLEEP 4                       ; 4  70
        stx COLUPF                    ; 3  73
        bne .skipIL                   ; 3  76
        
.iceLoop
        lda (FrostyImagePtr+.SEC*2),y ; 5  64
        and (FrostyMaskPtr+.SEC*2),y  ; 5  69
        sta WSYNC                     ; 3  72
.skipIL        
        sta GRP0                      ; 3
        lda (FrostyColorPtr+.SEC*2),y ; 5   8
        sta COLUP0                    ; 3  11
        lda (FireImagePtr+.SEC*2),y   ; 5  16
        sta GRP1                      ; 3  19
        ldx IceData+.SEC*4            ; 3  22
        stx PF1                       ; 3  25
        ldx IceData+.SEC*4+1          ; 3  28
        stx PF2                       ; 3  31
        ldx IceData+.SEC*4+2          ; 3  34
        SLEEP 9                       ; 9  43
        dey                           ; 2  45
        stx PF2                       ; 3  48 <- must be at 48
        ldx IceData+.SEC*4+3          ; 3  51
        stx PF1                       ; 3  54
        cpy #8                        ; 2  56
        bcs .iceLoop                  ; 3  59 (2 58 not taken)
        
        lda (FrostyImagePtr+.SEC*2),y ; 5  63
        and (FrostyMaskPtr+.SEC*2),y  ; 5  68
        ldx #ICE_BOTTOM_COLOR         ; 2  70
        stx COLUPF                    ; 3  73
        sta WSYNC                     ; 3  76
        
        sta GRP0                      ; 3
        lda (FrostyColorPtr+.SEC*2),y ; 5   8
        sta COLUP0                    ; 3  11
        lda (FireImagePtr+.SEC*2),y   ; 5  16
        sta GRP1                      ; 3  19
        ldx IceData+.SEC*4            ; 3  22
        stx PF1                       ; 3  25
        ldx IceData+.SEC*4+1          ; 3  28
        stx PF2                       ; 3  31
        ldx IceData+.SEC*4+2          ; 3  34
        SLEEP 9                       ; 9  43
        dey                           ; 2  45
        stx PF2                       ; 3  48 <- must be at 48
        ldx IceData+.SEC*4+3          ; 3  51
        stx PF1                       ; 3  54
        cpy #8                        ; 2  56
        bcs .iceLoop                  ; 3  59 (2 58 not taken)        
        
        
        lda (FrostyImagePtr+.SEC*2),y ; 5  63
        and (FrostyMaskPtr+.SEC*2),y  ; 5  68
        ldx FrostyMeltY               ; 3  71
        ;sta WSYNC          ; 3
        NOP                           ; 2  73
        stx ENAM0                     ; 3  76
        sta GRP0                      ; 3   
        lda (FrostyColorPtr+.SEC*2),y ; 5   8
        sta COLUP0                    ; 3  11
        lda (FireImagePtr+.SEC*2),y   ; 5  16
        sta GRP1                      ; 3  19
        ldx IceData+.SEC*4            ; 3  22
        stx PF1                       ; 3  25 <- needs to be before 28
        ldx IceData+.SEC*4+1          ; 3  28
        stx PF2                       ; 3  31
        ldx IceData+.SEC*4+2          ; 3  34
        SLEEP 9                       ; 9  43
        dey                           ; 2  45
        stx PF2                       ; 3  48 <- must be at 48
        ldx IceData+.SEC*4+3          ; 3  51  
        stx PF1                       ; 3  54
        
 if .SEC = 0
.platformLoop
        ldx #PLATFORM_COLOR
        lda #0
        sta WSYNC
        sta GRP0
        sta GRP1
        sta ENAM0 ; turn off melt
        stx COLUPF
        ldx #255
        stx PF0
        stx PF1
        stx PF2
        dey
        cpy #2
        bcs .platformLoop
 else
        lda (FrostyImagePtr+.SEC*2),y ; 5  59
        and (FrostyMaskPtr+.SEC*2),y  ; 5  64
        ldx #PLATFORM_COLOR           ; 2  66
        stx.w COLUPF                  ; 4  70
        sta WSYNC                     ; 3  73
        sta GRP0                      ; 3
        lda (FrostyColorPtr+.SEC*2),y ; 5   8
        sta COLUP0                    ; 3  11
        lda #0                        ; 2  13
        sta GRP1                      ; 3  16
        sta ENAM0                     ; 3  19 turn off melt     
        ldx Platforms+.SEC*4-4        ; 3  22
        stx PF1                       ; 3  25
        ldx Platforms+.SEC*4-4+1      ; 3  28
        stx PF2                       ; 3  31
        ldx Platforms+.SEC*4-4+2      ; 3  34
        SLEEP 9                       ; 9  43
        dey                           ; 2  45
        stx PF2                       ; 3  48 <- must be at 48
        ldx Platforms+.SEC*4-4+3      ; 3  51
        stx PF1                       ; 3  54
        
.platformLoop
        lda (FrostyImagePtr+.SEC*2),y ; 5  64
        and (FrostyMaskPtr+.SEC*2),y  ; 5  69
        sta WSYNC                     ; 3  72
        sta GRP0                      ; 3
        lda (FrostyColorPtr+.SEC*2),y ; 5   8
        sta COLUP0                    ; 3  11
        ldx Platforms+.SEC*4-4        ; 3  14
        stx PF1                       ; 3  17
        ldx Platforms+.SEC*4-4+1      ; 3  20
        stx PF2                       ; 3  23
        ldx Platforms+.SEC*4-4+2      ; 3  26
        SLEEP 17                      ;17  43
        dey                           ; 2  45
        stx PF2                       ; 3  48 <- must be at 48
        ldx Platforms+.SEC*4-4+3      ; 3  51
        stx PF1                       ; 3  54
        cpy #2                        ; 2  56
        bcs .platformLoop             ; 3  59
 ENDIF        

        lda (FrostyImagePtr+.SEC*2),y ; 5
        and (FrostyMaskPtr+.SEC*2),y  ; 5
        sta WSYNC                     ; 3
        sta GRP0                      ; 3
        lda (FrostyColorPtr+.SEC*2),y ; 5
        sta COLUP0                    ; 3
        lda #0
        sta PF0
        sta PF1
        sta PF2
        ENDM
        
OVERSCAN_DELAY = $1a

HMOVE_L7          =  $70
HMOVE_L6          =  $60
HMOVE_L5          =  $50
HMOVE_L4          =  $40
HMOVE_L3          =  $30
HMOVE_L2          =  $20
HMOVE_L1          =  $10
HMOVE_0           =  $00
HMOVE_R1          =  $F0
HMOVE_R2          =  $E0
HMOVE_R3          =  $D0
HMOVE_R4          =  $C0
HMOVE_R5          =  $B
HMOVE_R6          =  $A0
HMOVE_R7          =  $90
HMOVE_R8          =  $80


        SEG.U VARS
        ORG $80
; song player variables for main menu- note song player scratch variables
Frame             ds 1

FROSTYHEIGHT = 26
FrostyX           ds 1
FrostyY           ds 1
FrostyNoseY       ds 1
FrostyControl     ds 1 ; 76543210
                       ; 5 = 0 facing right, 1 = facing left
                       ; 43210 - 0 to 31, jump height
FrostyMeltSize    ds 1 ; use 1 & 0 in FrostyMeltY if RAM gets tight		       
FrostyMeltX       ds 1
FrostyMeltY       ds 1 ; 76543210
                       ; 6 = Melt on level 0
                       ; 5 = Melt on level 1
                       ; 4 = Melt on level 2
                       ; 3 = Melt on level 3
                       ; 2 = Melt on level 4
                       
OnLevel0 = %01000000 ; melt on level 0
OnLevel1 = %00100000 ; melt on level 1
OnLevel2 = %00010000 ; melt on level 2
OnLevel3 = %00001000 ; melt on level 3
OnLevel4 = %00000100 ; melt on level 4
                       
                      

ReserveLives      ds 1

; moon or sun
CELESTIALHEIGHT = 8
CelestialX        ds 1
CelestialY        ds 1
CelestialImagePtr ds 2


Score             ds 3

GameState         ds 1 ; 76543210
                       ; 7 - 0= game mode, 1 = attract
                       ; 6 - 
                       ; 5 - 
CurrentLevel      ds 1 ; current level

SoundLeft         ds 1
SoundRight        ds 1

IceSize           ds 1

MeltCountDown     ds 1

SECTIONS = 5 ; sections 0-4, 0 is bottom area just above score
DATA_BYTES_PER_SECTION = 11
Heights          ds 1*SECTIONS    ; heights of the levels
FireBallX        ds 2*SECTIONS    ; fireball positions, 2 per section
                                  ; x=0 means no fireball
FireBallSize     ds 2             ; 1=large, 0=small
Platforms        ds 4*SECTIONS-4  ; platforms
IceData          ds 4*SECTIONS    ; ice blocks
LevelControl     ds 2             ; level specific data-moving platform state, etc)
                                
FrostyImagePtr   ds 2*SECTIONS
FrostyMaskPtr    ds 2*SECTIONS
FrostyColorPtr   ds 2*SECTIONS
FireImagePtr     ds 2*SECTIONS

G48      EQU FrostyImagePtr ; reuse of RAM, 12 bytes
G48temp1 EQU FireImagePtr   ; reuse of RAM, 1 byte - lines remaining during Show48graphic
G48temp2 EQU FireImagePtr+1 ; reuse of RAM, 1 byte - holds temp image data during Show48graphic

LevelDataPtr EQU FrostyImagePtr ; reuse of RAM, 2 bytes when initing a level
; reuse of RAM, for subroutines PFcheckON, PFturnOFF
PFptr EQU FrostyColorPtr
PFtemp EQU FrostyColorPtr+2
; reuse of RAM, for preping fireball images
FBtemp EQU FrostyColorPtr+3

 if DEBUG_INFO
DebugOS ds 1
DebugVB ds 1
 endif

 echo "----",($00FE - *) , "bytes of RAM left"
; $FE - $FF stack
; reuse of stack RAM - no JSRs can be called when these are used

; end reuse of stack RAM


        SEG CODE
        
; - bank 1 - KERNEL and graphics    
        ORG $E000
        JUMP_TABLE


        


        
PosPlayer1         ; A holds X value
        sec        ; X holds GRP0
        sta WSYNC  ; Y holds COLUP0
        stx GRP0       ; 3
        sty COLUP0     ; 3    6
DivideLoop1
        sbc #15        ; 2    8
        bcs DivideLoop1; 2   10
        eor #7         ; 2   12
        asl            ; 2   14
        asl            ; 2   16
        asl            ; 2   18
        asl            ; 2   20
        sta RESP1      ; 2   23 <- set object position
        sta HMP1       ; 3   26
        sta WSYNC
        sta HMOVE
        rts        

.skipCelestialDraw
        lda #0
        beq .celestialDraw
	
	
	
	
        
KernelCode
 if DEBUG_INFO
        lda INTIM
        sta DebugVB
        sta Score+1
        lda DebugOS
        sta Score+2
 endif
.VblankWait
        lda INTIM
        bpl .VblankWait
        lda #0
        sta WSYNC
        sta VBLANK ; turn on video output
	lda Frame
	and #%11
	tax
.phStarLoop	
	dex
	nop
	nop
	bpl .phStarLoop
	sta RESM0 ; set missile 0 at known starting point for stars
	
        
        ldx #16
.HorizonLoop
        lda HillColor,x
        sta WSYNC
        sta COLUPF                    ; 3 
        lda Hill0,x                   ; 4  7
        sta PF0                       ; 3 10
        lda Hill1,x                   ; 4 14
        sta PF1                       ; 3 17
        lda Hill2,x                   ; 4 21
        sta PF2                       ; 3 24
        txa                           ; 2 26
        sec                           ; 2 28
        sbc CelestialY                ; 3 31
        adc #CELESTIALHEIGHT          ; 2 33
        bcc .skipCelestialDraw        
        tay
        lda (CelestialImagePtr),y     ; 
.celestialDraw        
        sta GRP0                      ;
        dex                           ; 2 44
        bpl .HorizonLoop              ; 2 46
        inx
        lda #%1
        stx WSYNC
	stx ENAM0
        stx COLUBK
        stx PF0
        stx GRP0
        sta CTRLPF
        stx PF1
        stx PF2
        lda FrostyControl
        lsr
        lsr
        sta REFP0   ; set Frosty facing left or right
        lda FrostyX
        ldx #0
        jsr PosObject
        SLEEP 17
        lda #0
        sta CXCLR ; reset collision latches
        sta HMCLR
	lda FrostyMeltX
        ldx #2
        jsr PosObject
	SLEEP 17
        lda FrostyMeltSize ; 3
        sta NUSIZ0         ; 3
	sta HMCLR         

        SECTION 4
        SECTION 3
        SECTION 2
        SECTION 1        
        SECTION 0
        
        
; display SCORE and Reserve Lives
        sta WSYNC
        lda #0               ; 2
        sta GRP0             ; 3   5
        sta HMP1             ; 3   8
        sta PF0              ; 3  11
        sta PF1              ; 3  14
        sta PF2              ; 3  17
        lda #3               ; 2  19
        sta NUSIZ0           ; 3  22
        sta NUSIZ1           ; 3  25
        sta VDELP0           ; 3  28
        sta VDELP1           ; 3  31
        lda #HMOVE_R1             ; 2  33
        sta HMP0             ; 3  36 x adjust for P0  
        sta RESP0            ; 3  39 set x for P0
        sta RESP1            ; 3  42 set x for P1
        sta WSYNC
        SLEEP 38             ; 38
        sta RESM1            ;  3  41 set x for M1 - used to draw nose in lives remaining
        sta WSYNC
        sta HMOVE   ; fine adjust for P0
        lda #15 ; WHITE NTSC & PAL
        sta COLUP0
        sta COLUP1
        lda #0
        sta HMP0        
        sta REFP0       ; reset in case Frosty was facing left
        
; Load G48 with SCORE graphics        
        lda #>ZeroGraphic
        sta G48+1
        sta G48+3
        sta G48+5
        sta G48+7
        sta G48+9
        sta G48+11
        
        lda Score
        and #$F0
        lsr
        sta G48
        lda Score
        and #$0f
        asl
        asl
        asl
        sta G48+2
        
        lda Score+1
        and #$F0
        lsr
        sta G48+4
        lda Score+1
        and #$0F
        asl
        asl
        asl
        sta G48+6
        
        lda Score+2
        and #$F0
        lsr
        sta G48+8
        lda Score+2
        and #$0F
        asl
        asl
        asl
        sta G48+10
        
        sta WSYNC                 
        ldx #0
BlankZeroLoop 
        lda G48,x
        bne BZLdone
        inx
        lda #>BlankGraphic
        sta G48,x
        inx
        cpx #10
        bcc BlankZeroLoop
BZLdone    
        SLEEP 10
        inx
        inx
        cpx #10
        bcc BZLdone

        ; show score
        ldy #7        
Show48graphic 
        ; call with Y holding the lines-1 to show
        ; G48 thru G48+$B must be preloaded with pointers to the
        ; 48 pixel graphic image
        STY G48temp1
        sta WSYNC
.graphicLoop
        ldy G48temp1         ;+3  63  189
        lda (G48),y          ;+5  68  204
        sta GRP0             ;+3  71  213      D1     --      --     --
        sta WSYNC            ;go
        lda (G48+$2),y       ;+5   5   15
        sta GRP1             ;+3   8   24      D1     D1      D2     --
        lda (G48+$4),y       ;+5  13   39
        sta GRP0             ;+3  16   48      D3     D1      D2     D2
        lda (G48+$6),y       ;+5  21   63
        sta G48temp2         ;+3  24   72
        lda (G48+$8),y       ;+5  29   87
        tax                  ;+2  31   93
        lda (G48+$A),y       ;+5  36  108
        tay                  ;+2  38  114
        lda G48temp2         ;+3  41  123              !
        sta GRP1             ;+3  44  132      D3     D3      D4     D2!
        stx GRP0             ;+3  47  141      D5     D3!     D4     D4
        sty GRP1             ;+3  50  150      D5     D5      D6     D4!
        sta GRP0             ;+3  53  159      D4*    D5!     D6     D6
        dec G48temp1         ;+5  58  174                             !
        bpl .graphicLoop     ;+2  60  180
        lda #0
        sta GRP0
        sta GRP1
        sta GRP0
        sta GRP1
        sta WSYNC
        
        sta WSYNC
        ldx #0 ; black for nose when no lives remaining        
        lda ReserveLives
        beq Only2left  ; doesn't matter - graphic will be blank
        ldx #FIREBALL_COLOR ; Orange for nose
        cmp #2
        bcc Only1left
        beq Only2left
        lda #$23       ; show 3 close
        .byte $2c
Only2left        
        lda #$21       ; show 2 close
        .byte $2c
Only1left        
        lda #$20       ; show 1
        sta NUSIZ0
        stx COLUP1
        
        sta NUSIZ1
        lda #0
        sta VDELP0           ; 3  28
        sta VDELP1           ; 3  31          
        
        lda ReserveLives
        bne AtLeast1left
        lda #<BlankGraphic
        .byte $2c ; sucks up the next command
AtLeast1left        
        lda #<LivesGraphic
        sta G48
        lda #>LivesGraphic
        sta G48+1

        ldx #ENAM1
        txs
        ldy #12
.graphicLoop2
        lda LivesColor,y
        sta WSYNC
        sta COLUP0
        lda (G48),y
        sta GRP0
        cpy #3 ; enable carrot nose on line 3
        php
        pla
        dey
        bpl .graphicLoop2
        lda #0
        sta WSYNC
        sta GRP0
        sta NUSIZ0           ; 3  22
        ldx #$FF
        txs        

        jmp OverScan
        

        
SunGraphic
        .byte zz___XX___
        .byte zz__X__X__
        .byte zz_X_XX_X_
        .byte zz_XXXXXX_
        .byte zz_X_X_XX_
        .byte zz_X_X_XX_
        .byte zz__XXXX__
        .byte zz___XX___
        
MoonGraphic
        .byte zz___XX___
        .byte zz__XX____
        .byte zz_XX_____
        .byte zz_XX_____
        .byte zz_XX_____
        .byte zz_XXX____
        .byte zz__XXXX__
        .byte zz___XX___
        
HillColor
        .byte 4          ; 0
        .byte 4          ; 1
        .byte 8          ; 2
        .byte 8          ; 3
        .byte 15         ; 4
        .byte 15         ; 5
        .byte 15         ; 6
        .byte 15         ; 7 greyscale, no PAL version needed
        .byte zz________ ; 8
        .byte zz________ ; 9
        .byte zz________ ;10
        .byte zz________ ;11
        .byte zz________ ;12
        .byte zz________ ;13
        .byte zz________ ;14
        .byte zz________ ;15
        .byte zz________ ;16        
        
Hill0
        .byte zzXXXX____ ; 0
        .byte zzXXXX____ ; 1
        .byte zzXXXX____ ; 2
        .byte zzXXXX____ ; 3
        .byte zzXXXX____ ; 4
        .byte zzXXX_____ ; 5
        .byte zzXX______ ; 6
        .byte zzX_______ ; 7
        .byte zz________ ; 8
        .byte zz________ ; 9
        .byte zz________ ;10
        .byte zz________ ;11
        .byte zz________ ;12
        .byte zz________ ;13
        .byte zz________ ;14
        .byte zz________ ;15
        .byte zz________ ;16
        
Hill1
        .byte zzXXXXXXXX ; 0
        .byte zzXXXXXXXX ; 1
        .byte zzXXXXXXXX ; 2
        .byte zzXXXXXXXX ; 3
        .byte zzXXXXXXX_ ; 4
        .byte zzXXXXXXX_ ; 5
        .byte zzX_XXXX__ ; 6
        .byte zz___XX___ ; 7
        .byte zz________ ; 8
        .byte zz________ ; 9
        .byte zz________ ;10
        .byte zz________ ;11
        .byte zz________ ;12
        .byte zz________ ;13
        .byte zz________ ;14
        .byte zz________ ;15
        .byte zz________ ;16        
        
        
Hill2   
        .byte zzXXXXXXXX ; 0
        .byte zz_XXXXXXX ; 1
        .byte zz__XXXXXX ; 2
        .byte zz___XXXX_ ; 3
        .byte zz____XX__ ; 4
        .byte zz________ ; 5
        .byte zz________ ; 6
        .byte zz________ ; 7
        .byte zz________ ; 8
        .byte zz________ ; 9
        .byte zz________ ;10
        .byte zz________ ;11
        .byte zz________ ;12
        .byte zz________ ;13
        .byte zz________ ;14
        .byte zz________ ;15
        .byte zz________ ;16        
        
        align 256
BlankGraphic
        repeat 50 ; 148-26 
        .byte 0
        repend        
        repeat 26        
        .byte $ff
        repend
FrostyMask        
        repeat 50 ; 148-26
        .byte 0
        repend

LivesGraphic
        .byte zz___XX___ ; 0
        .byte zz__X__X__ ; 1
        .byte zz_X_XX_X_ ; 2
        .byte zz_XXX____ ; 3  ; gap for nose drawn by missile 1
        .byte zz_XX_X_X_ ; 4
        .byte zz_XX_X_X_ ; 5
        .byte zz__XXXX__ ; 6
        .byte zz___XX___ ; 7
        .byte zz__XXXX__ ; 8
        .byte zz___XX___ ; 9
        .byte zz___XX___ ; 10
        .byte zz___XX___ ; 11
        .byte zz___XX___ ; 12

LivesColor
        .byte 15,15,15,15,15,15,15,15,4,HAT_BAND_COLOR,4,4,6
        
        .byte zz__XXXX__ ; 0
        .byte zz_XXXXXX_ ; 1
        .byte zzXXXXXXXX ; 2
        .byte zzXXXXXXXX ; 3
        .byte zzXXXXXXXX ; 4
        .byte zzXXXXXXXX ; 5
        .byte zzXXXX_XXX ; 6
        .byte zz_XXXXXX_ ; 7
        .byte zz__XXXX__ ; 8
        .byte zz_XXXXXX_ ; 9
        .byte zz_XXX_XX_ ; 10
        .byte zz_XXXXXX_ ; 11
        .byte zz_XXXXXX_ ; 12
        .byte zz_XXX_XX_ ; 13
        .byte zz__XXXX__ ; 14
        .byte zz___XX___ ; 15
        .byte zz__X__X__ ; 16
        .byte zz__XXXX__ ; 17
        .byte zz__X_X___ ; 18
        .byte zz__XXXX__ ; 19
        .byte zz___XX___ ; 20
        .byte zz__XXXX__ ; 21
        .byte zz___XX___ ; 22
        .byte zz___XX___ ; 23
        .byte zz___XX___ ; 24
        .byte zz___XX___ ; 25
FrostyGraphic
        
        .byte 8   ; 0
        .byte 10  ; 1
        .byte 12  ; 2
        .byte 12  ; 3
        .byte 14  ; 4
        .byte 14  ; 5
        .byte 14  ; 6
        .byte 12  ; 7
        .byte 8   ; 8
        .byte 10  ; 9
        .byte 12  ; 10
        .byte 14  ; 11
        .byte 14  ; 12
        .byte 14  ; 13
        .byte 12  ; 14
        .byte 10  ; 15
        .byte 12  ; 16
        .byte 14  ; 17
        .byte 14  ; 18
        .byte 14  ; 19
        .byte 10  ; 20
        .byte 4   ; 21
        .byte HAT_BAND_COLOR
        .byte 4   ; 23
        .byte 4   ; 24
        .byte 6   ; 25        
FrostyColors    
        
        align 256
ZeroGraphic  
        .byte zz___XX___ ; 0
        .byte zz__X__X__ ; 1
        .byte zz_X____X_ ; 2
        .byte zz_X____X_ ; 3
        .byte zz_X____X_ ; 4
        .byte zz_X____X_ ; 5
        .byte zz__X__X__ ; 6
        .byte zz___XX___ ; 7

OneGraphic
        .byte zz__XXXXX_ ; 0
        .byte zz____X___ ; 1
        .byte zz____X___ ; 2
        .byte zz____X___ ; 3
        .byte zz____X___ ; 4
        .byte zz__X_X___ ; 5
        .byte zz___XX___ ; 6
        .byte zz____X___ ; 7
        
TwoGraphic
        .byte zz_XXXXXX_ ; 0
        .byte zz_X______ ; 1
        .byte zz_X______ ; 2
        .byte zz__X_____ ; 3
        .byte zz___XX___ ; 4
        .byte zz_____X__ ; 5
        .byte zz_X____X_ ; 6
        .byte zz__XXXX__ ; 7
        
ThreeGraphic
        .byte zzXXXXX___ ; 0
        .byte zz___XXX__ ; 1
        .byte zz____XX__ ; 2
        .byte zz___XXX__ ; 3
        .byte zz__XXX___ ; 4
        .byte zz____XX__ ; 5
        .byte zz_XX__XX_ ; 6
        .byte zz__XXXX__ ; 7
        
FourGraphic
        .byte zz____X___ ; 0
        .byte zz____X___ ; 1
        .byte zz____X___ ; 2
        .byte zz____X___ ; 3
        .byte zz_XXXXXX_ ; 4
        .byte zz__X_X___ ; 5
        .byte zz___XX___ ; 6
        .byte zz____X___ ; 7
        
FiveGraphic
        .byte zzXXXXX___ ; 0
        .byte zz____XX__ ; 1
        .byte zz_____X__ ; 2
        .byte zz____XX__ ; 3
        .byte zz_XXXXX__ ; 4
        .byte zz_XXX____ ; 5
        .byte zz_XX_____ ; 6
        .byte zz__XXXXX_ ; 7

SixGraphic
        .byte zz___XX___ ; 0
        .byte zz__X__X__ ; 1
        .byte zz_X____X_ ; 2
        .byte zz_XX__X__ ; 3
        .byte zz_X_XX___ ; 4
        .byte zz_X______ ; 5
        .byte zz__X__X__ ; 6
        .byte zz___XX___ ; 7
        
SevenGraphic
        .byte zzXX______ ; 0
        .byte zz_XX_____ ; 1
        .byte zz__XX____ ; 2
        .byte zz__XX____ ; 3
        .byte zz___XX___ ; 4
        .byte zz___XX___ ; 5
        .byte zz____XX__ ; 6
        .byte zz_XXXXXX_ ; 7
        
EightGraphic
        .byte zz___XX___ ; 0
        .byte zz__X__X__ ; 1
        .byte zz_X____X_ ; 2
        .byte zz_X____X_ ; 3
        .byte zz__XXXX__ ; 4
        .byte zz_X____X_ ; 5
        .byte zz__X__X__ ; 6
        .byte zz___XX___ ; 7
        
NineGraphic
        .byte zz___XX___ ; 0
        .byte zz__X__X__ ; 1
        .byte zz______X_ ; 2
        .byte zz___XX_X_ ; 3
        .byte zz__X__XX_ ; 4
        .byte zz_X____X_ ; 5
        .byte zz__X__X__ ; 6
        .byte zz___XX___ ; 7
        
; alpha graphics used for HEX output for debugging
Agraphic
        .byte zz_X____X_ ; 0
        .byte zz_X____X_ ; 1
        .byte zz_X____X_ ; 2
        .byte zz_X____X_ ; 3
        .byte zz_XXXXXX_ ; 4
        .byte zz_X____X_ ; 5
        .byte zz__X__X__ ; 6
        .byte zz___XX___ ; 7        

Bgraphic
        .byte zz_XXXXX__ ; 0
        .byte zz_X____X_ ; 1
        .byte zz_X____X_ ; 2
        .byte zz_X____X_ ; 3
        .byte zz_XXXXX__ ; 4
        .byte zz_X____X_ ; 5
        .byte zz_X____X_ ; 6
        .byte zz_XXXXX__ ; 7        
Cgraphic
        .byte zz__XXXX__ ; 0
        .byte zz_X____X_ ; 1
        .byte zz_X______ ; 2
        .byte zz_X______ ; 3
        .byte zz_X______ ; 4
        .byte zz_X______ ; 5
        .byte zz_X____X_ ; 6
        .byte zz__XXXX__ ; 7        
Dgraphic
        .byte zz_XXXXX__ ; 0
        .byte zz_X____X_ ; 1
        .byte zz_X____X_ ; 2
        .byte zz_X____X_ ; 3
        .byte zz_X____X_ ; 4
        .byte zz_X____X_ ; 5
        .byte zz_X____X_ ; 6
        .byte zz_XXXXX__ ; 7        
Egraphic
        .byte zz_XXXXXX_ ; 0
        .byte zz_X______ ; 1
        .byte zz_X______ ; 2
        .byte zz_X______ ; 3
        .byte zz_XXXXXX_ ; 4
        .byte zz_X______ ; 5
        .byte zz_X______ ; 6
        .byte zz_XXXXXX_ ; 7        
Fgraphic
        .byte zz_X______ ; 0
        .byte zz_X______ ; 1
        .byte zz_X______ ; 2
        .byte zz_X______ ; 3
        .byte zz_XXXXXX_ ; 4
        .byte zz_X______ ; 5
        .byte zz_X______ ; 6
        .byte zz_XXXXXX_ ; 7        
        
        
FireballLargeA
        .byte 0,0,0,0,0,0
        .byte zz__XXX___
        .byte zz__XXXX__
        .byte zz_XXXXXX_
        .byte zzXXXXXXX_
        .byte zzXXXXXXX_
        .byte zzX_X_XXX_
        .byte zzX_X_XXX_
        .byte zzXXXXXXX_
        .byte zzXXXXX___
        .byte zz_XXXX___
        .byte zz_XXXXX__
        .byte zz_X_X_X__
        .byte zz_X_X_X__
        .byte zz___X____
        .byte zz_X_X____
        .byte zz__X_X___
        .byte zz____X___
        .byte 0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0
        
FireballSmallA
        .byte 0,0,0,0,0,0
        .byte zz__XXX___
        .byte zz__XXXX__
        .byte zz_XXXXXX_
        .byte zzXXXXXXX_
        .byte zzX_X_XXX_
        .byte zzX_X_XXX_
        .byte zzXXXXX___
        .byte zz_XXXXX__
        .byte zz_X_X_X__
        .byte zz___X____
        .byte zz_X_X____
        .byte zz__X_X___
        .byte zz____X___
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0
        
FireballLargeB
        .byte 0,0,0,0,0,0
        .byte zz__XXX___
        .byte zz__XXXX__
        .byte zz_XXXXXX_
        .byte zzXXXXXXX_
        .byte zzXXXXXXX_
        .byte zzX_X_XXX_
        .byte zzX_X_XXX_
        .byte zzXXXXXXX_
        .byte zzXXXXX___
        .byte zz_XXXX___
        .byte zz_XXXXX__
        .byte zz_X_X_X__
        .byte zzX__X__X_
        .byte zz___X____
        .byte zz___X_X__
        .byte zz__X_X___
        .byte zz_X______
        .byte 0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0
        
FireballSmallB
        .byte 0,0,0,0,0,0
        .byte zz__XXX___
        .byte zz__XXXX__
        .byte zz_XXXXXX_
        .byte zzXXXXXXX_
        .byte zzX_X_XXX_
        .byte zzX_X_XXX_
        .byte zzXXXXX___
        .byte zz_XXXXX__
        .byte zzX__X__X_
        .byte zz_X_X____
        .byte zz__XXX___
        .byte zz_____X__
        .byte zz________
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0,0,0,0,0,0,0
        
FireballLargeAright
        .byte 0,0,0,0,0,0
        .byte zz___XXX__
        .byte zz__XXXX__
        .byte zz_XXXXXX_
        .byte zz_XXXXXXX
        .byte zz_XXXXXXX
        .byte zz_XXX_X_X
        .byte zz_XXX_X_X
        .byte zz_XXXXXXX
        .byte zz___XXXXX
        .byte zz___XXXX_
        .byte zz__XXXXX_
        .byte zz__X_X_X_
        .byte zz__X_X_X_
	.byte zz____X___
        .byte zz__X_X___
        .byte zz___X_X__
        .byte zz_____X__
        .byte 0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0
        
FireballSmallAright
        .byte 0,0,0,0,0,0
        .byte zz___XXX__
        .byte zz__XXXX__
        .byte zz_XXXXXX_
        .byte zz_XXXXXXX
        .byte zz_XXX_X_X
        .byte zz_XXX_X_X
        .byte zz___XXXXX
        .byte zz__XXXXX_
        .byte zz__X_X_X_
        .byte zz____X___
        .byte zz__X_X___
        .byte zz___X_X__
        .byte zz_____X__
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0
        
FireballLargeBright
        .byte 0,0,0,0,0,0
        .byte zz___XXX__
        .byte zz__XXXX__
        .byte zz_XXXXXX_
        .byte zz_XXXXXXX
        .byte zz_XXXXXXX
        .byte zz_XXX_X_X
        .byte zz_XXX_X_X
        .byte zz_XXXXXXX
        .byte zz___XXXXX
        .byte zz___XXXX_
        .byte zz__XXXXX_
        .byte zz__X_X_X_
        .byte zz_X__X__X
        .byte zz____X___
        .byte zz____X_X_
        .byte zz___X_X__
        .byte zz__X_____
        .byte 0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0
        
FireballSmallBright
        .byte 0,0,0,0,0,0
        .byte zz___XXX__
        .byte zz__XXXX__
        .byte zz_XXXXXX_
        .byte zz_XXXXXXX
        .byte zz_XXX_X_X
        .byte zz_XXX_X_X
        .byte zz___XXXXX
        .byte zz__XXXXX_
        .byte zz_X__X__X
        .byte zz__X_X___
        .byte zz___XXX__
        .byte zz______X_
        .byte zz________
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0,0,0,0,0,0,0


        echo "----",($FFF4 - *) , "bytes of ROM left KERNEL & Graphics"
        
        ORG $EFF4
        BANKS_AND_VECTORS        
        
        
; bank 2

        ORG $F000
        JUMP_TABLE
        
InitSystemCode
InitFrostyCode    ; starting point of this game
        CLEAN_START
InitNewGame
        lda #3
        sta ReserveLives
        
        lda #0
        sta Score
        sta Score+1
        sta Score+2
        
        lda #0
        sta CurrentLevel     ; set initial level
        jsr PrepLevel ; load level 1 data
        
        jmp VerticalBlankCode
        
        
OverScanCode
        sta WSYNC
        lda #2
        sta VBLANK ; turn off video output
        lda #OVERSCAN_DELAY
        sta TIM64T
        
; check for end of level        
        lda FrostyY
        cmp #$20     ; check for bottom
        bne FrostyNotAtEnd
        lda FrostyX
        cmp #152     ; check for right
        bne FrostyNotAtEnd
        ldy #19
        lda #0
IceGoneCheck        
        ora IceData,y
        dey
        bpl IceGoneCheck
        cmp #0
        bne FrostyNotAtEnd
; level cleared, advance to next level
        ldx CurrentLevel
        inx
        cpx #8
        bne SaveLevel
        ldx #0
SaveLevel        
        stx CurrentLevel
        jsr PrepLevel
        jmp OverScanWait
        
FrostyNotAtEnd        
        
        lda INPT4 ; fire
        bmi nofire
        lda Frame
        and #%11
        beq nofire
        jmp OverScanWait
nofire        
        lda SWCHA
        bmi SkipRight
        inc FrostyX
        tay
        lda FrostyControl
        and #%11011111     ; set facing right 
        sta FrostyControl
        tya
SkipRight
        rol
        bmi SkipLeft
        dec FrostyX
        tay
        lda FrostyControl
        ora #%00100000     ; set facing left
        sta FrostyControl
        tya
SkipLeft
        rol
        bmi SkipDown
        dec FrostyY
SkipDown
        rol
        bmi SkipUp
        inc FrostyY
        inc FrostyY
SkipUp     

; check X and Y values, prevening wrap-around
        lda FrostyX
        cmp #153
        bne .nextXcheck
        lda #152
        bne .saveNewX
.nextXcheck
        cmp #255
        bne .noXadjust
        lda #0
.saveNewX        
        sta FrostyX
.noXadjust        
        lda FrostyY
        cmp #$1f
        bne .nextYcheck
        lda #$20
        bne .saveNewY
.nextYcheck
        cmp #$96
        bne .noYadjust
        lda #$95
.saveNewY
        sta FrostyY
.noYadjust        
        
        
 
OverScanWait
 if DEBUG_INFO
        lda INTIM
        sta DebugOS
 endif        
.OscanWait
        lda SWCHB
        eor #$FF
        and #%11
        bne SelectOrReset
        lda INTIM
        bpl .OscanWait
        jmp VerticalBlankCode
        
SelectOrReset
        jmp InitFrostyCode
        
	
;******************
;*   S T A R T    *
;* Vertical Blank *
;******************
VerticalBlankCode
        lda #$02
        sta WSYNC
        sta VSYNC  ; 3   start vertical sync, D1=1
        sta VBLANK ; 3 6 start vertical blank, D1=1
        lda #$2C   ; 2 8 set timer for end of Vertical Blank
        sta TIM64T
        sta WSYNC  ; 1st line of vertical sync
        inc Frame  ; 5
        sta WSYNC  ; 2nd line of vertical sync
        lda #0     ; 2
        sta WSYNC  ; 3rd line of vertical sync
        sta VSYNC  ; stop vertical sync, D1 = 0
;******************
;*     E N D      *
;* Vertical Blank *
;******************
        
;**********************
;*     S T A R T      *
;* Play Sound Effects *
;**********************
        ldx SoundLeft
        lda SoundVol,x
        sta AUDV0
        lda SoundFreq,x
        sta AUDF0
        lda SoundChannel,x
        sta AUDC0
        beq SkipSLdec
        dec SoundLeft
SkipSLdec
        ldx SoundRight
        lda SoundVol,x
        sta AUDV1
        lda SoundFreq,x
        sta AUDF1
        lda SoundChannel,x
        sta AUDC1
        beq SkipSRdec
        dec SoundRight
SkipSRdec        
;**********************
;*       E N D        *
;* Play Sound Effects *
;**********************


;***********************
;*     S T A R T       *
;* Process Sun's Heat  *
;* melt Ice and Frosty *
;***********************
        lda Frame
        and #%11
        bne EndSunHeat
        sec
        lda MeltCountDown
        sbc CurrentLevel
        sta MeltCountDown
        bcs EndSunHeat
        ldx IceSize
        dex
        cpx #10   ; Size 17-11
        beq EndSunHeat
SaveIceSize
        stx IceSize
EndSunHeat	
;***********************
;*       E N D         *
;* Process Sun's Heat  *
;* melt Ice and Frosty *
;***********************






;***********************
;*      S T A R T      *
;* Fireball Processing *
;***********************
	ldx #FIREBALL_COLOR
        stx COLUP1
        
;+-------------------+
;| flicker fireballs |
;+-------------------+
        ldy #0
        ldx #1
FlickerLoop
        lda FireBallX,y
        pha
        lda FireBallX,x
        sta FireBallX,y
        pla
        sta FireBallX,x
        iny
        iny
        inx
        inx
        cpy #SECTIONS*2+2 ; include FireBallSize
        bcc FlickerLoop    
	
;+---------------------+
;| set fireball images |
;+---------------------+
        ldy #0
	sty NUSIZ1 ; only 1 fireball per frame
        ldx FireBallSize
	stx FBtemp
FireBallImageLoop
        lda FireBallX,y
        beq FBnoImage
	lsr FBtemp
	bcc FBsmallImage
	ldx #1
	.byte $2c
FBsmallImage
	ldx #2
	lda Frame
	and #%100
	beq FBcheckDirection
	inx ; add 2 to point to alternate image
	inx ; ^^^
	
FBcheckDirection
	lda FireBallX,y
	cmp FrostyX
	bcs FBsetImage
	inx ; add 4 to face right
	inx ; ^^^
	inx ; ^^^
	inx ; ^^^
	bne FBsetImage
FBnoImage
	ldx #0
	lsr FBtemp ; shift image size
FBsetImage
	txa
	asl
	tax
	lda FireballImages,x
        sta FireImagePtr,y
        iny
	inx
        lda FireballImages,x
        sta FireImagePtr,y
        iny
        cpy #9
        bcc FireBallImageLoop
;***********************
;*        E N D        *
;* Fireball Processing *
;***********************




        
        ; position melt
        lda FrostyControl
        and #%00100000
        beq .meltOnRight
        lda #9
        .byte $2c
.meltOnRight
        lda #$F9
        clc
        adc FrostyX
        cmp #160
        bcc .positionMelt
        clc
        adc #160
.positionMelt
	sta FrostyMeltX
        ; ldx #2
        ; jsr PosObject
        ; lda #0 ; was $30
        ; sta NUSIZ0
	lda #$30
	sta FrostyMeltSize
        
        lda #32
        ldy #0
.meltLoop
        cmp FrostyY
        beq .haveMelt
        clc
        adc Heights,y
        iny
        cpy #5
        bne .meltLoop
        
.haveMelt        
        lda MeltEnable,y
        sta FrostyMeltY
        lda #0
        sta HMM0  
        
        cpy #5
        bne FrostyOnPlatform
        jmp FrostyNotOnPlatform

FrostyOnPlatform
        ; prep PFptr for call to PFcheckON
        lda #0
        sta PFptr+1
        tya
        asl ; times 2
        asl ; times 4
;        clc - not needed, Y was 0-4
        adc #<Platforms
        sec
        sbc #4
        sta PFptr
        
        clc
        lda FrostyX
        adc #5
        lsr
        lsr ; value now 0-39
        sec
        sbc #4  ; adjust for 32 valid values
        tax
        jsr PFcheckON
        bne IceCheck
        
PlatformCheck2
        clc
        lda FrostyX
        adc #2
        lsr
        lsr ; value now 0-39
        sec
        sbc #4  ; adjust for 32 valid values
        tax
        jsr PFcheckON
        beq FrostyNotOnPlatform
        
IceCheck
        clc
        lda PFptr
        adc #20
        sta PFptr ; point to IceData
        
        clc
        lda FrostyX
        adc #3 ; 3 or 4 work
        lsr
        lsr
        sec
        sbc #4
        tax
        jsr PFcheckON
;        beq IceCheck2
        beq LevelSpecificRoutines
        jsr PFturnOFF
        dex
        jsr PFturnOFF
        inx
        inx
        jsr PFturnOFF
        ldy #SoundIce
        jsr SetSound
        sec
        lda IceSize
        sbc 7
        cmp #$0a
        bne IceScore
        lda #$10; convert
IceScore
        jsr IncScore
        jmp LevelSpecificRoutines

        
FrostyNotOnPlatform ; so fall
        ldx FrostyY
        dex
        cpx #$1f
        bne .saveX
        ldx #$20
.saveX
        stx FrostyY
	
	
	
;*****************************
;*        S T A R T          *
;* Level Specific Processing *
;*****************************
; note: fireball X range = 0-147
LevelSpecificRoutines
        lda Frame
        and #%1111
        beq .skipExitJump
        jmp FinishedLevelProcessing
.skipExitJump
        lda CurrentLevel
        asl
        tax
        lda LPjumpTable+1,x
        pha 
        lda LPjumpTable,x
        pha 
        rts        
        
LPjumpTable
        .word Level1Processing-1
        .word Level2Processing-1
        .word Level3Processing-1
        .word Level4Processing-1
        .word Level4Processing-1
        .word Level3Processing-1
        .word Level2Processing-1
        .word Level1Processing-1        
        
Level1Processing SUBROUTINE        
; test moving platforms        
        lda LevelControl
        bne .Moving1Up ; moving platform 1 up
        lda FrostyMeltY
        and #OnLevel1
        beq .noAdjustFrostyDown
        dec FrostyY
.noAdjustFrostyDown
        dec Heights+0
        inc Heights+1
        lda Heights+0
        cmp #$18
        bne .DoneMoving
        sta LevelControl ; change direction
.Moving1Up        
        lda FrostyMeltY
        and #OnLevel1
        beq .noAdjustFrostyUp
        inc FrostyY
.noAdjustFrostyUp        
        inc Heights+0
        dec Heights+1
        lda Heights+0
        cmp #40
        bne .DoneMoving
        lda #0        
        sta LevelControl
.DoneMoving      
        jmp FinishedLevelProcessing
        
Level2Processing SUBROUTINE        
; test moving platforms        
        lda LevelControl
        bne .Moving3Up
        lda FrostyMeltY
        and #OnLevel3
        beq .noAdjustFrostyDown
        dec FrostyY
.noAdjustFrostyDown        
        dec Heights+2
        inc Heights+3
        lda Heights+3
        cmp #35
        bne .DoneMoving
        sta LevelControl ; change direction
.Moving3Up        
        lda FrostyMeltY
        and #OnLevel3
        beq .noAdjustFrostyUp
        inc FrostyY
.noAdjustFrostyUp        
        inc Heights+2
        dec Heights+3
        lda Heights+3
        cmp #25
        bne .DoneMoving
        lda #0        
        sta LevelControl
.DoneMoving 
        jmp FinishedLevelProcessing
        
Level3Processing SUBROUTINE        
; test moving platforms        
        lda LevelControl
        bne .Moving2Up
        lda FrostyMeltY
        and #OnLevel2
        beq .noAdjustFrostyDown
        dec FrostyY
.noAdjustFrostyDown
        dec Heights+1
        inc Heights+2
        lda Heights+1
        cmp #$18
        bne .DoneMoving
        sta LevelControl ; change direction
.Moving2Up 
        lda FrostyMeltY
        and #OnLevel2
        beq .noAdjustFrostyUp
        inc FrostyY
.noAdjustFrostyUp        
        inc Heights+1
        dec Heights+2
        lda Heights+1
        cmp #40
        bne .DoneMoving
        lda #0        
        sta LevelControl
.DoneMoving      
        jmp FinishedLevelProcessing
        
Level4Processing SUBROUTINE        
; test moving platforms        
        lda LevelControl+1
        bne .MovingLeft
        lda FrostyMeltY
        and #OnLevel3
        beq .noAdjustFrostyR
        lda FrostyX
        clc
        adc #4
        sta FrostyX
.noAdjustFrostyR        
        lda FireBallX+2*3
        beq .noAdjustFireball1R
        clc
        adc #4
        sta FireBallX+2*3
.noAdjustFireball1R
        lda FireBallX+2*3+1
        beq .noAdjustFireball2R
        clc
        adc #4
        sta FireBallX+2*3+1        
.noAdjustFireball2R
        lda IceData+4*3
        lsr
        sta IceData+4*3
        lda IceData+4*3+1
        rol
        sta IceData+4*3+1
        lda IceData+4*3+2
        ror
        sta IceData+4*3+2
        lda IceData+4*3+3
        rol
        sta IceData+4*3+3
        
        lda Platforms+4*3-4
        lsr
        sta Platforms+4*3-4
        lda Platforms+4*3+1-4
        rol
        sta Platforms+4*3+1-4
        lda Platforms+4*3+2-4
        ror
        sta Platforms+4*3+2-4
        lda Platforms+4*3+3-4
        rol
        sta Platforms+4*3+3-4
        and #%10000000
        beq .DoneMoving
        sta LevelControl+1 ; change direction
.MovingLeft
        lda FrostyMeltY
        and #OnLevel3
        beq .noAdjustFrostyL
        lda FrostyX
        sec
        sbc #4
        sta FrostyX
.noAdjustFrostyL
        lda FireBallX+2*3
        beq .noAdjustFireball1L
        sec
        sbc #4
        sta FireBallX+2*3
.noAdjustFireball1L
        lda FireBallX+2*3+1
        beq .noAdjustFireball2L
        sec
        sbc #4
        sta FireBallX+2*3+1        
.noAdjustFireball2L
        lda IceData+4*3+3
        lsr
        sta IceData+4*3+3
        lda IceData+4*3+2
        rol
        sta IceData+4*3+2
        lda IceData+4*3+1
        ror
        sta IceData+4*3+1
        lda IceData+4*3
        rol
        sta IceData+4*3
        
        lda Platforms+4*3+3-4
        lsr
        sta Platforms+4*3+3-4
        lda Platforms+4*3+2-4
        rol
        sta Platforms+4*3+2-4
        lda Platforms+4*3+1-4
        ror
        sta Platforms+4*3+1-4
        lda Platforms+4*3-4
        rol
        sta Platforms+4*3-4
        and #%10000000
        
        beq .DoneMoving
        lda #0        
        sta LevelControl+1
.DoneMoving 
        lda CurrentLevel
        cmp #4
        bne .notLevel5
        jmp Level2Processing
.notLevel5        
        jmp FinishedLevelProcessing

FinishedLevelProcessing
; moving platforms could have changed the results, so redo them
        lda #32
        ldy #0
.meltLoop
        cmp FrostyY
        beq .haveMelt
        clc
        adc Heights,y
        iny
        cpy #5
        bne .meltLoop
        
.haveMelt        
        lda MeltEnable,y
        sta FrostyMeltY
;*****************************
;*          E N D            *
;* Level Specific Processing *
;*****************************



;**************************
;*       S T A R T        *
;* Prep Frosty's pointers *
;**************************

;+-----------------------------+
;| prep pointers for Section 0 |
;+-----------------------------+
        SET_POINTER FrostyImagePtr, FrostyGraphic
        SET_POINTER FrostyColorPtr, FrostyColors
        SET_POINTER FrostyMaskPtr, FrostyMask
	
        sec
        lda FrostyImagePtr
        sbc FrostyY
        sta FrostyImagePtr
        lda FrostyImagePtr+1
        sbc #0
        sta FrostyImagePtr+1
        
        sec
        lda FrostyColorPtr
        sbc FrostyY
        sta FrostyColorPtr
        lda FrostyColorPtr+1
        sbc #0
        sta FrostyColorPtr+1
        
        sec
        lda FrostyMaskPtr
        sbc FrostyY
        sta FrostyMaskPtr
        lda FrostyMaskPtr+1
        sbc #0
        sta FrostyMaskPtr+1        
        
;+--------------------------------+
;| prep pointers for Sections 1-4 |
;+--------------------------------+
        ldx #0
        ldy #0
pfLoop        
        clc
        lda FrostyImagePtr,y
        adc Heights,x
        iny ; 1
        iny ; 2
        sta FrostyImagePtr,y
        dey ; 1
        lda FrostyImagePtr,y
        adc #0
        iny ; 2
        iny ; 3
        sta FrostyImagePtr,y
        
        dey ; 2
        dey ; 1
        dey ; 0
        clc
        lda FrostyColorPtr,y
        adc Heights,x
        iny ; 1
        iny ; 2
        sta FrostyColorPtr,y
        dey ; 1
        lda FrostyColorPtr,y
        adc #0
        iny ; 2
        iny ; 3
        sta FrostyColorPtr,y              
        
        dey ; 2
        dey ; 1
        dey ; 0
        clc
        lda FrostyMaskPtr,y
        adc Heights,x
        iny ; 1
        iny ; 2
        sta FrostyMaskPtr,y
        dey ; 1
        lda FrostyMaskPtr,y
        adc #0
        iny ; 2
        iny ; 3
        sta FrostyMaskPtr,y           
        
        dey ; 2
        inx        
        cpx #SECTIONS-1
        bcc pfLoop

;+-------------------------+
;| tweek Frosty's pointers |
;| so no page breaks occur |
;+-------------------------+
        ldy #1
MaskLoop        
        lda FrostyMaskPtr,y
        cmp #>BlankGraphic
        beq MaskCheck2
        lda #>BlankGraphic
        sta FrostyMaskPtr,y     
        sta FrostyImagePtr,y
        sta FrostyColorPtr,y
        dey
        lda #0
        sta FrostyMaskPtr,y
        sta FrostyImagePtr,y
        sta FrostyColorPtr,y
        iny
        bne NextMask
MaskCheck2
        dey
        lda FrostyMaskPtr,y
        cmp #<FrostyMask
        bcc NextMask2
        lda #0
        sta FrostyMaskPtr,y
        sta FrostyImagePtr,y
        sta FrostyColorPtr,y
        iny
        lda #>BlankGraphic
        sta FrostyMaskPtr,y
        sta FrostyImagePtr,y
        sta FrostyColorPtr,y
        bne NextMask
NextMask2        
        iny
NextMask
        iny
        iny
        cpy #SECTIONS*2
        bcc MaskLoop
;**************************
;*         E N D          *
;* Prep Frosty's pointers *
;**************************


;*******************
;*    S T A R T    *
;* Prepare Horizon *
;*******************
PrepHorizon SUBROUTINE
        lda CelestialX
        ldx #0 ; sun/moon in Sprite 0
        jsr PosObject
        lda CurrentLevel
        bne PrepSunHorizon
        lda #$0f ; white
	sta COLUPF
        sta COLUP0
	sta ENAM0; turn stars on
        lda #0   ; M0ack
        beq PrepMoonDone
PrepSunHorizon
        lda FrostyX
        cmp CelestialX
        bcc SunFacingLeft
        lda #%00001000
        .byte $2c
SunFacingLeft
        lda #0
        sta REFP0
        
        lda #SUN_COLOR
        sta COLUP0
        lda #15
        sta COLUPF
        lda CurrentLevel
        asl
        ora #DAY_SKY_COLOR
PrepMoonDone
        sta COLUBK;
        lda #%101  ; Playfield Priority
        sta CTRLPF ; reflected playfield for HORIZON
        lda #0
        sta HMP0
	ldx #$70 ; need the cosmic ark star trick?
	stx HMM0
	sta WSYNC
	sta HMOVE     ; 3
	SLEEP 16
	ldx #$60
	stx HMM0      ; 3  15
;*******************
;*      E N D      *
;* Prepare Horizon *
;*******************

        
;*************************
;* END OF VERTICAL BLANK *
;*************************
        jmp Kernel


        
        
;***************
;* SUBROUTINES *
;***************

;+---------------------------+
;| load level data for level |
;| stored in CurrentLevel    |
;+---------------------------+        
PrepLevel SUBROUTINE
        lda #255
        sta MeltCountDown
        
        lda #0
        sta FrostyX
        lda #$20
        sta FrostyY
        
        lda #17
        sta IceSize
        
        lda CurrentLevel
        and #%00111111
        asl
        tay
        lda LevelData,y
        sta LevelDataPtr
        lda LevelData+1,y
        sta LevelDataPtr+1
        
        ldy #SECTIONS*DATA_BYTES_PER_SECTION-4-1+2+2
        ; -4 for the 4 unused platform bytes
        ; +2 for fireball size info
        ; +2 for level control init
.plLoop        
        lda (LevelDataPtr),y
        sta Heights,y
        dey
        bpl .plLoop
        
        lda CurrentLevel
        bne .prepSun
        lda #20
        sta CelestialX
        lda #$11
        sta CelestialY
        
        SET_POINTER CelestialImagePtr, MoonGraphic
        rts
.prepSun        
        lda #76
        sta CelestialX
        lda CurrentLevel
        asl
        clc
        adc #2
        sta CelestialY
        SET_POINTER CelestialImagePtr, SunGraphic
        rts
        
;+----------------------------------+
;| Check if Playfield bit is on     |
;| used to check for platform & ice |
;+----------------------------------+
PFcheckON SUBROUTINE
        ; before calling set PFptr to IceData or Platforms to check
        ; call with X holding 0-31, X not changed
        ; return with non-zero value (Z = 0) if PF is on
        lda PFptr
        cmp #<(Platforms-4)
        bne .notOnBottomRow
        lda #1
        rts ; Z=0 for on-bottom-row
.notOnBottomRow
        cpx #32
        bcc .validValueToCheck
        lda #0
        rts ;Z=1 as X >=32, so PF cannot be on
        
.validValueToCheck
        lda PFmask,x
        sta PFtemp
        txa
        lsr ; divide by 2
        lsr ; divide by 4
        lsr ; divide by 8
        tay
        lda (PFptr),y
        and PFtemp
        rts ; if value = 0 then not on platform
	
;+--------------------------+
;| Turn off Playfield bit   |
;| used to remove ice block |
;+--------------------------+
PFturnOFF SUBROUTINE
        ; before calling set PFptr to IceData to turn off
        ; call with X holding 0-31, X not changed
        cpx #32
        bcc .validValueToTurnOff
        rts ; do nothing if X out of range
.validValueToTurnOff
        lda PFmask,x
        eor #$FF
        sta PFtemp
        txa
        lsr ; divide by 2
        lsr ; divide by 4
        lsr ; divide by 8
        tay
        lda (PFptr),y
        and PFtemp
        sta (PFptr),y
        rts
	
;+--------------------------+
;| Increase Score           |
;| A holds BCD value to add |
;+--------------------------+
IncScore SUBROUTINE
        sed
        clc
        adc Score+2
        sta Score+2
        lda #0       ; could TXA or TYA for > 2 digit score increments
        adc Score+1
        sta Score+1
        lda #0
        adc Score
        sta Score
        cld
        rts
        
PFmask  .byte %10000000 ;  0 = PF1a bit 7
        .byte %01000000 ;  1 = PF1a bit 6
        .byte %00100000 ;  2 = PF1a bit 5
        .byte %00010000 ;  3 = PF1a bit 4
        .byte %00001000 ;  4 = PF1a bit 3
        .byte %00000100 ;  5 = PF1a bit 2
        .byte %00000010 ;  6 = PF1a bit 1
        .byte %00000001 ;  7 = PF1a bit 0
        .byte %00000001 ;  8 = PF2a bit 0
        .byte %00000010 ;  9 = PF2a bit 1
        .byte %00000100 ; 10 = PF2a bit 2
        .byte %00001000 ; 11 = PF2a bit 3
        .byte %00010000 ; 12 = PF2a bit 4
        .byte %00100000 ; 13 = PF2a bit 5
        .byte %01000000 ; 14 = PF2a bit 6
        .byte %10000000 ; 15 = PF2a bit 7
        .byte %10000000 ; 16 = PF2b bit 7
        .byte %01000000 ; 17 = PF2b bit 6
        .byte %00100000 ; 18 = PF2b bit 5
        .byte %00010000 ; 19 = PF2b bit 4
        .byte %00001000 ; 20 = PF2b bit 3
        .byte %00000100 ; 21 = PF2b bit 2
        .byte %00000010 ; 22 = PF2b bit 1
        .byte %00000001 ; 23 = PF2b bit 0
        .byte %00000001 ; 24 = PF1b bit 0
        .byte %00000010 ; 25 = PF1b bit 1
        .byte %00000100 ; 26 = PF1b bit 2
        .byte %00001000 ; 27 = PF1b bit 3
        .byte %00010000 ; 28 = PF1b bit 4
        .byte %00100000 ; 29 = PF1b bit 5
        .byte %01000000 ; 30 = PF1b bit 6
        .byte %10000000 ; 31 = PF1b bit 7
        
        
        
;****************************
;*        S T A R T         *
;* Data defining each level *
;****************************
LevelData
        .word Level1
        .word Level2
        .word Level3
        .word Level4
        .word Level1
        .word Level2
        .word Level3
        .word Level4
	
        ; section 4's height must be 31 for Frosty to fit on the platform
        ;       else hat will disappear
        ; section must be a minimum of 10 in height
        ; all sections must sum to 148
        ; don't set section 0 to height of 29
        ;       else red band in hat disappears for first 3rd of scanline
Level1 
       
        .byte 35,35,34,34,10 ; section heights add up to 148            -   5  5
        .byte 80, 0 ; section 0 fireballs                               -   2  7
        .byte 80, 100 ; section 1 fireballs                             -
        .byte 0, 0 ; section 2 fireballs
        .byte 0, 90 ; section 3 fireballs
        .byte 0, 0 ; section 4 fireballs
        .byte %00000000, %00011111; fireball size                       -   2 17
        
        ; no section 0 platform - bottom of screen is fully on  
        .byte 0,zzX_______,zzXXXXXXXX,zz_______X ; Section 1 platform   -   4  4
        .byte 0,0,zz______XX,zzXXXXXXXX ; Section 2 platform            -   4  8
        .byte 0,zzXXX_____,zzXXXXXXX_,0 ; Section 3 platform            -   4 12
        .byte 0,0,0,0 ; Section 4 platform                              -   4 16
        .byte 0, 0,0,zzXX_XX_XX ; Section 0 ice                         -   4 20
        .byte 0,0,zzXX_XX_XX,0 ; Section 1 ice                          -   4 24
        .byte 0,0,zz_______X,zz_XX_XX_X ; Section 2 ice                 -   4 28
        .byte 0,zzXX______,zz_XX_XX__,0 ; Section 3 ice                 -   4 32
        .byte 0,0,0,0 ; Section 4 ice                                   -   4 36
        .byte 0,0 ; level control                                       -   2 38
        
       
Level2
        .byte 35,35,34,34,10 ; section heights add up to 148
        .byte 80, 0 ; section 0 fireballs
        .byte 80, 100 ; section 1 fireballs
        .byte 0, 0 ; section 2 fireballs
        .byte 0, 90 ; section 3 fireballs
        .byte 0, 0 ; section 4 fireballs
        .byte %00000000, %00011111; fireball size
        
        ; no section 0 platform - bottom of screen is fully on  
        .byte 0,zzX_______,zzXXXXXXXX,zz_______X ; Section 1 platform
        .byte 0,0,zz______XX,zzXXXXXXXX ; Section 2 platform
        .byte 0,zzXXX_____,zzXXXXXXX_,0 ; Section 3 platform
        .byte 0,0,0,0 ; Section 4 platform
        .byte 0, 0,0,zzXX_XX_XX ; Section 0 ice
        .byte 0,0,zzXX_XX_XX,0 ; Section 1 ice
        .byte 0,0,zz_______X,zz_XX_XX_X ; Section 2 ice
        .byte 0,zzXX______,zz_XX_XX__,0 ; Section 3 ice
        .byte 0,0,0,0 ; Section 4 ice
        .byte 0,0 ; level control
        
Level3  ; section 4's height must be 31 for Frosty to fit on the platform
        .byte 30,29,29,29,31 ; section heights add up to 148
        .byte 80, 120 ; section 0 fireballs
        .byte 50, 120 ; section 1 fireballs
        .byte 30, 100 ; section 2 fireballs
        .byte 20, 60 ; section 3 fireballs
        .byte 100, 130 ; section 4 fireballs
        .byte %00001111, %00011111; fireball size
        
        ; no section 0 platform - bottom of screen is fully on  
        .byte 0,zz_XXXXXXX,0,zz_XXXXXXX ; Section 1 platform
        .byte zz_XXXXXXX,0,zz_XXXXXXX,0 ; Section 2 platform
        .byte 255,255,0,0 ; Section 3 platform
        .byte 0,0,255,255 ; Section 4 platform        
        .byte zzXX_XX_XX, zzX_XX_XX_, zzX_XX_XX_,zzXX_XX_XX ; Section 0 ice
        .byte 0,zz__XX_XX_,0,0 ; Section 1 ice
        .byte zz__XX_XX_,0,zz__XX_XX_,0 ; Section 2 ice
        .byte zz_XX_XX_X,zz_XX_XX_X,0,0 ; Section 3 ice
        .byte 0,0,zz_XX_XX_X,zz_XX_XX_X ; Section 4 ice   
        .byte 0,0 ; level control
        
Level4  ; section 4's height must be 31 for Frosty to fit on the platform
        .byte 30,29,29,29,31 ; section heights add up to 148
        .byte 80, 120 ; section 0 fireballs
        .byte 50, 120 ; section 1 fireballs
        .byte 30, 100 ; section 2 fireballs
        .byte 20, 60 ; section 3 fireballs
        .byte 100, 130 ; section 4 fireballs
        .byte %00011111, %00011111; fireball size
        
        ; no section 0 platform - bottom of screen is fully on  
        .byte 0,zz_XXXXXXX,0,zz_XXXXXXX ; Section 1 platform
        .byte zz_XXXXXXX,0,zz_XXXXXXX,0 ; Section 2 platform
        .byte 255,255,0,0 ; Section 3 platform
        .byte 0,0,255,255 ; Section 4 platform        
        .byte zzXX_XX_XX, zzX_XX_XX_, zzX_XX_XX_,zzXX_XX_XX ; Section 0 ice
        .byte 0,zz__XX_XX_,0,0 ; Section 1 ice
        .byte zz__XX_XX_,0,zz__XX_XX_,0 ; Section 2 ice
        .byte zz_XX_XX_X,zz_XX_XX_X,0,0 ; Section 3 ice
        .byte 0,0,zz_XX_XX_X,zz_XX_XX_X ; Section 4 ice    
        .byte 0,0 ; level control
;****************************
;*          E N D           *
;* Data defining each level *
;****************************


        
MeltEnable
        .byte OnLevel0 ; melt on level 0
        .byte OnLevel1 ; melt on level 1
        .byte OnLevel2 ; melt on level 2
        .byte OnLevel3 ; melt on level 3
        .byte OnLevel4 ; melt on level 4
        .byte 0         ; no melt

SetSound SUBROUTINE ; call with Y holding sound effect to generate
        ldx SoundLeft
        lda SoundVol,x
        bne .ssCheckRightActive
        sty SoundLeft
        rts
.ssCheckRightActive
        ldx SoundRight
        lda SoundVol,x
        bne .ssCheckLeftPriority
        sty SoundRight
        rts
.ssCheckLeftPriority
        rts ; not done
        
SoundVol ; 0-15
        .byte 0,1,1,1,2,2,2,3,3,3,4,4,4,4 ; Ice Vol
SoundIce EQU * - SoundVol - 1
        .byte 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14 ; Fire Vol
SoundFire EQU * - SoundVol - 1

SoundFreq ; 0-31 0 = highest freq
        .byte 0,10+4,10+5,10+6,10+7,10+8,10+9,10+10,10+11,10+12,10+13,10+14,10+15,10+16 ; Ice
        .byte 0,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a,$1a ; Fire

SoundChannel ; 0-15
        .byte 0,4,4,4,4,4,4,4,4,4,4,4,4,4 
        .byte 0,8,8,8,8,8,8,8,8,8,8,8,8,8,8 ; Fire
	
FireballImages
	.word BlankGraphic
	.word FireballLargeA
	.word FireballSmallA
	.word FireballLargeB
	.word FireballSmallB
	.word FireballLargeAright
	.word FireballSmallAright
	.word FireballLargeBright
	.word FireballSmallBright
        

        
        echo "----",($FFF4 - *) , "bytes of ROM left Overscan & Vertical Blank"
        
        ORG $FFF4
        BANKS_AND_VECTORS
