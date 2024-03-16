; ==============================================================================
; Humbug 09
;
; 'AD' FORMATTED ASCII DUMP
; 'AI' ASCII INPUT
; 'AO' ASCII OUT
; 'BP' PRINT BP LOCATIONS
; 'BR' SET/RESET BPS
; 'CL' CLEAR SCREEN
; 'CO' CONTINUE AFTER BP
; 'CS' TWO-BYTE CHECKSUM
; 'DE' DISASSEMBLE MEMORY
; 'ED' ELEKTRA DISK BOOT
; 'EN' END OF TAPE FORMATTING
; 'FA' SWITCH PORT 1 I/O TO/FROM FASTYPE
; 'FD' REGULAR DISK BOOT
; 'FI' FIND BYTES COMMAND
; 'FM' FILL MEMORY
; 'HA' COMBINED HEX AND ASCII DUMP
; 'HD' HEX DUMP ROUTINE
; 'HE' HELP
; 'JU' JUMP COMMAND
; 'LO' LOAD MIKBUG TAPE
; 'LR' PRINT LRA
; 'MC' MEMORY COMPARE
; 'ME' MEMORY EXAMINE
; 'MO' MOVE MEMORY CONTENTS
; 'MT' MEMORY TEST
; 'PU' PUNCH MIKBUG TAPE
; 'RC' REGISTER CHANGE
; 'RD' RETURN TO DOS
; 'RE' REGISTER EXAMINE
; 'SS' SINGLE-STEP
; 'ST' START SINGLE-STEPPING
; '!!' FORCE POWERUP INITIALIZATION
; ==============================================================================
        NAM HUMBUG-09
	
IFC &A,INST
;* MODIFIED 4-17-84 TO SUBSTITUTE ELEKTRA BOOT
;*   BY DEFAULT IN ALL TERMINAL-BASED VERSIONS
;*   ELEKTRA BOOT ONLY IN STANDARD,TERMINAL,$X000
;* MODIFIED 3-19-82 TO FIX STACK IN BOOT,
;*   LENGTHEN FLBOOT DELAY FOR 2 MHZ,
;*   AND ADD REGISTER CHANGE COMMAND (TE VERS ONLY)
;* MODIFIED 6-24 TO LENGTHEN FLBOOT DELAY
;*   EVEN MORE.
;* MOD 12-5-84 TO FIX NESTING
;* MOD 3-23-85 BY CHANGING THE $FF00 PAGE SO IT
;*   PROPERLY INITIALIZES THE DAT. NOW
;*   WORKS WITH COMPUTER EXCELLENCE BOARD.
;* CALLING SEQUENCE FOR ASSEMBLY:
;*    ASMB
;*    INPUT FILE NAME
;*    OBJECT FILE NAME
;*    + OPTIONS
;*    + PARAMETER 1 : INST, STANDARD, OR MY
;*      PARAMETER 2 : TERMINAL, PERCOM, OR THOMAS
;*      PARAMETER 3 : $E000 OR $8000
END
ENDIF

IFC &B,TERMINAL
        ;; * TERMINAL-BASED VERSION
ENDIF

IFC &B,PERCOM
        ;; * PERCOM VIDEO BOARD VERSION
ENDIF

IFC &B,THOMAS
;* THOMAS VIDEO BOARD VERSION
ENDIF

;* FOR I/O LOCATED AT &C
;* COPYRIGHT (C) 1982 BY PETER A. STARK
;* EQUATES
ROM     EQU $F000

IFNC &B,THOMAS
SCRPAD  EQU $DFBD
ENDIF

IFC &B,THOMAS
SCRPAD  EQU $DFBA
ENDIF

IFNC &B,TERMINAL
VIDRAM  EQU $B800 VIDEO RAM
ENDIF
	
USTACK  EQU $C080 USER STACK
MSTACK  EQU $C040 MON STACK
DLATCH  EQU $E014 DISK DRIVE LATCH
DSTATS  EQU $E014 CONTROLLER BOARD STATUS
CMDREG  EQU $E018 FDC COMMAND REGISTER
STAREG  EQU CMDREG FDC STATUS REGISTER
TRKREG  EQU CMDREG+1 FDC TRACK REGISTER
SECREG  EQU CMDREG+2 FDC SECTOR REGISTER
DATREG  EQU CMDREG+3 FDC DATA REGISTER
ACIA0   EQU $E000 PORT 0 ACIA
 IFC &A,MY
ACIA0B  EQU $E002 PORT 0B ACIA
 ENDIF
ACIA1   EQU $E004 PORT 1 ACIA
DAT     EQU $FFF0 DAT RAM
RETADD  EQU $CC16 DOS RETURN ADDRESS
;* DATA STORAGE
 ORG SCRPAD
BRANCH  RMB 3 ADDR & OP CODE OF POS BR IN SS
 IFC &B,THOMAS
NEXT    RMB 3 ADDR & OP CODE OF NEXT INSTR IN SS
FINDNO  EQU NEXT USED IN FI
WHAT    EQU NEXT+1 USED IN FI
NEWLOC  EQU NEXT USED IN MO
TEMP    EQU NEXT USED IN PUNCH
 ENDIF
RESRVD  RMB 2 DFC0 RESERVED
SWI3    RMB 2 DFC2 SWI3 VECTOR
SWI2    RMB 2 DFC4 SWI4 VECTOR
FIRQ    RMB 2 DFC6 FIRQ VECTOR
IRQ     RMB 2 DFC8 IRQ VECTOR
SWI     RMB 2 DFCA SWI VECTOR
SUPVEC  RMB 2 DFCC SUPERVISOR CALL VECTOR BEGIN
SUPEND  RMB 2 DFCE SUPERVISOR CALL VECTOR END
LRATAB  RMB 16 DFD0 LRA ADDRESS TABLE
PORADD  RMB 2 DFE0 CONTROL PORT
PORECH  RMB 1 ECHO IS <> 0
BKTAB   RMB 12 BP TABLE (LOC1, OP1,...
STATUS  RMB 1 STATUS: P0,P1,VID,PD,PA,X,X,X
PTRINZ  RMB 1 PORT 1 ACIA INITIALIZATION
KBDINZ  RMB 1 PORT 1 ACIA INITIALIZATION
DVECTR  RMB 2 USER PORT D VECTOR
PAUCTR  RMB 1 PAUSE FEATURE LINE COUNTER
BEGA    RMB 2 BEGINNING ADDRESS
ENDA    RMB 2 ENDING ADDRESS
SP      RMB 2 STACK PTR

IFNC &B,TERMINAL
CURV    RMB 1 CURSOR VERT POS
CURH    RMB 1 CURSOR HORIZ POS
ENDIF

IFNC &B,THOMAS
NEXT    RMB 3 ADDR & OP CODE OF NEXT INSTR IN SS
FINDNO  EQU NEXT USED IN FI
WHAT    EQU NEXT+1 USED IN FI
NEWLOC  EQU NEXT USED IN MO
TEMP    EQU NEXT USED IN PUNCH
ENDIF

IFC &B,THOMAS
TOP     RMB 2 TOP OF SCREEN MEMORY
ENDIF
IFC &B,PERCOM
;* VIDEO DRIVER STORAGE
TOP     EQU VIDRAM+$07FE TOP OF SCREEN MEMORY
BOT     EQU VIDRAM+$04FF END OF VIDEO RAM
PCURV   EQU VIDRAM+$07CD CURSOR LINE REGISTER
PCURH   EQU VIDRAM+$07CC CURSOR HORIZONTAL REGISTER
BOTLNE  EQU VIDRAM+$07C6 BOTTOM LINE
ENDIF
IFC &B,THOMAS
;* VIDEO DRIVER STORAGE
BOT     EQU VIDRAM+$03FF END OF VIDEO RAM
ENDIF
	
        ;; 
        ;; * PROGRAM BEGINNING
	;; 
	ORG ROM
        ;; * JSR-TYPE VECTORS FOR 6800 COMPATIBILITY

        JMP RESTRT              ;* COMPLETE RESTART
        JMP WARMST              ;* WARM START
        JMP NXTCMD              ;* NEXT COMMAND
        JMP INEEE               ;* INPUT CHARACTER
        JMP INHEX               ;* INPUT HEX DIGIT
        JMP BADDR               ;* BUILD ADDRESS
        JMP OUTEEE              ;* OUTPUT CHARACTER
        JMP PDATA               ;* PRINT DATA STRING
        JMP CRLF                ;* PRINT CR-LF
        JMP OUTS                ;* OUTPUT A SPACE
        JMP OUTHR               ;* OUTPUT RIGHT HEX DIGIT
        JMP OUT2HS              ;* OUTPUT 2 HEX DIGITS
        JMP OUT4HS              ;* OUTPUT 4 HEX DIGITS

START   LDS #MSTACK             ;* SET STACK TO MON AREA
;* INITIALIZE I/O PORTS
        LDA #$03                ;*  RESET PORT 0 AND 1 ACIA
        STA ACIA0               ;
IFC &A,MY
        STA ACIA0B
ENDIF
        STA ACIA1
IFNC &B,TERMINAL
        JSR VINIT               ;* INITIALIZE VIDEO
ENDIF

IFNC &A,MY
RESET   LDX #RTSAD             ;* POINT TO RTS
ENDIF
IFC &A,MY
RESET   LDX #PORTD             ;* GET ADDR OF PORT D
ENDIF
        STX DVECTR              ;* AND PUT IN PORT D VECTOR

        ;* WARMSTART INITIALIZATION
WARMST  LDS #MSTACK             ;* SET STACK PTR TO MON AREA
        LDA #$60                ;* PORT 0 AND D OFF, PAUSE OFF
        STA STATUS              ;* PORT 1 (AND VIDEO) ON
        LDA #$0F

        STA PAUCTR              ;* INIT PAUSE LINE COUNTER
        LDX #ACIA1
        STX PORADD              ;* SET CONTROL PORT ADDRESS
        LDA #$FF
        STA PORECH              ;* CTRL PORT ECHO ON
        LDA #$15                ;* ACIA INPUT INITIALIZATION
        STA KBDINZ
        LDA #$11                ;* ACIA OUTPUT INITIALIZATION
        STA PTRINZ
        LDA #$13                ;* TURN READER OFF
        JSR OUTCHM
        INC A                   ;* TURN PUNCH OFF
        JSR OUTCHM
        LBSR CRLF
        LDX #H09MSG
        LBSR PDATA              ;* PRINT SIGN-ON
        BRA NXTCMD
H09MSG  FCC 'HUMBUG-09'
        FCB 4

        ;; * NXTCMD - INITIALIZATION COMPLETE. READY FOR COMMAND

NXTCMD  LDS #MSTACK             ;* RESET STACK PTR TO MON AREA
NXTCM1  JSR CRLF                ;* PRINT CR/LF
        LDA #'*                 ;* PRINT PROMPT
        JSR OUTEEE
        JSR INEEE               ;* GET FIRST COMMAND CHARACTER
        PSHS A                  ;* SAVE FIRST CHARACTER OF COMMAND
        JSR INEEE               ;* GET SECOND COMMAND CHARACTER
        TAB                     ;* MOVE SECOND TO B
        JSR OUTS
        PULS A                  ;* RESTORE FIRST COMMAND
        ;;* CHECK COMMAND
COMAND  LDX #COMTAB-4           ;* GET ADDR OF COMMAND TABLE
LOOKUP  LEAX 4,X
        CMPX #TABEND            ;* END OF TABLE?
        BEQ COMEND              ;* YES
        CMP A 0,X               ;* NO, CHECK FIRST CHARACTER
        BNE LOOKUP              ;* WRONG
        CMP B 1,X               ;* CHECK SECOND CHARACTER
        BNE LOOKUP              ;* WRONG, SKIP TO NEXT
        LDX 2,X                 ;* GET ADDRESS IF OK
        JSR OUTS                ;* PRINT A SPACE
        JSR 0,X                 ;* JUMP TO APPROP COMMAND ROUTINE
        BRA NXTCMD
COMEND  LDA #'?
        LBSR OUTEEE
        BRA NXTCMD

        ;;* COMMAND TABLE

COMTAB  FCC 'AD'                ;* FORMATTED ASCII DUMP
        FDB ASCDMP
        FCC 'AI'                ;* ASCII INPUT
        FDB ASCIN
        FCC 'AO'                ;* ASCII OUT
        FDB ASCOUT
        FCC 'BP'                ;* PRINT BP LOCATIONS
        FDB BPRINT
        FCC 'BR'                ;* SET/RESET BPS
        FDB BREAK
        FCC 'CL'                ;* CLEAR SCREEN
        FDB CLEAR
        FCC 'CO'                ;* CONTINUE AFTER BP
        FDB CONT
        FCC 'CS'                ;* TWO-BYTE CHECKSUM
        FDB SUM
        FCC 'DE'                ;* DISASSEMBLE MEMORY
        FDB DESEMB
IFC &B,TERMINAL
        FCC 'ED'                ;* ELEKTRA DISK BOOT
        FDB ELBOOT
ENDIF
        FCC 'EN'                ;* END OF TAPE FORMATTING
        FDB PNCHS9
        FCC 'FA'                ;* SWITCH PORT 1 I/O TO/FROM FASTYPE
        FDB SWFAST
        FCC 'FD'                ;* REGULAR DISK BOOT
        FDB FLBOOT
        FCC 'FI'                ;* FIND BYTES COMMAND
        FDB FIND
        FCC 'FM'                ;* FILL MEMORY
        FDB FILL
        FCC 'HA'                ;* COMBINED HEX AND ASCII DUMP
        FDB HEXASC
        FCC 'HD'                ;* HEX DUMP ROUTINE
        FDB HEXDMP
        FCC 'HE'                ;* HELP
        FDB HELP
        FCC 'JU'                ;* JUMP COMMAND
        FDB JUMP
        FCC 'LO'                ;* LOAD MIKBUG TAPE
        FDB LOAD
        FCC 'LR'                ;* PRINT LRA
        FDB LRPRNT
        FCC 'MC'                ;* MEMORY COMPARE
        FDB COMPAR
        FCC 'ME'                ;* MEMORY EXAMINE
        FDB CHANGE
        FCC 'MO'                ;* MOVE MEMORY CONTENTS
        FDB MOVE
        FCC 'MT'                ;* MEMORY TEST
        FDB MTEST
        FCC 'PU'                ;* PUNCH MIKBUG TAPE
        FDB PUNCH
IFC &B,TERMINAL
        FCC 'RC'                ;* REGISTER CHANGE
        FDB REGCHG
ENDIF
        FCC 'RD'                ;* RETURN TO DOS
        FDB RETDOS
        FCC 'RE'                ;* REGISTER EXAMINE
        FDB REGIST
        FCC 'SS'                ;* SINGLE-STEP
        FDB STEP
        FCC 'ST'                ;* START SINGLE-STEPPING
        FDB STRTSS
        FCC '!!'                ;* FORCE POWERUP INITIALIZATION
        FDB RESTRT
TABEND  EQU *

        ;;* MEMORY EXAMINE AND CHANGE FUNCTION

CHANGE  BSR BADDR
CHANG0  TFR X,U
CHANG1  JSR CRLF
        PSHS U                  ;* PUT ON STACK
        TFR S,X                 ;* POINT TO IT
        JSR OUT4HS              ;* PRINT ADDRESS
        PULS X                  ;* GET ADDR AND FIX STACK
        JSR OUT2HS              ;* PRINT OLD DATA
        JSR OUTS
CHANG2  JSR INEEE               ;* INPUT COMMAND CHAR
        CMP A #$20
        BEQ CHANG2              ;* ON SPACE, LOOK FOR MORE
        CMP A #$0D
        LBEQ NXTCMD             ;* QUIT WHEN DONE
        CMP A #'^               ;* UP ARROW?
        BNE CHANG3              ;* NO
        LEAU -1,U               ;* YES, STEP BACK
        BRA CHANG1              ;* AND REPEAT
CHANG3  TFR X,U
        CMP A #'0               ;* CHECK FOR VALID HEX DIGIT
        BCS CHANG1              ;* NOT VALID
        CMP A #'F
        BHI CHANG1              ;* NOT VALID
        BSR CVTHEX              ;* CONVERT TO HEX DIGIT
        BSR BYTE1               ;* GET SECOND DIGIT AND COMBINE
        STA -1,X                ;* STORE NEW NUMBER
        CMP A -1,X              ;* CHECK IT
        BEQ CHANG1              ;* STORED OK
        LDA #'?                 ;* ERROR IF NOT STORED OK
        LBSR OUTEEE
        BRA CHANG1

        ;;* BUILD ADDRESS

BADDR   PSHS B,A
        BSR BYTE
        PSHS A
        BSR BYTE
        TFR A,B
        PULS A
        TFR D,X
        PULS A,B
RTSAD   RTS

        ;;* INPUT BYTE (TWO HEX DIGITS)

BYTE    BSR INHEX               ;* GET ONE HEX DIGIT
BYTE1   ASL A
        ASL A
        ASL A
        ASL A
        PSHS B
        TFR A,B
        BSR INHEX
        ABA
        PULS B,PC               ;* AND RETURN

        ;;* OUTHL AND R - OUTPUT ONE HEX DIGIT

OUTHL   LSR A                   ;* OUTPUT LEFT DIGIT ENTRY
        LSR A
        LSR A
        LSR A
OUTHR   AND A #$F               ;* OUTPUT HEX RIGHT DIGIT ENTRY
        ADDA #'0
        CMP A #'9
        LBLS OUTEEE
        ADDA #$7                ;* CONVERT FOR A - F
        LBRA OUTEEE

        ;;* INPUT HEX CHARACTER

INHEX   BSR INEEE
CVTHEX  SUB A #$30 
        LBMI NXTCMD
        CMP A #$9
        BLE IHEXIT
        CMP A #$11
        LBMI NXTCMD
        CMP A #$16
        LBGT NXTCMD             ;* NOT HEX DIGIT
        SUB A #7                ;* HEX, SO CONVERT
IHEXIT  RTS

        ;;* OUT2 AND 4 HEX ROUTINES

OUT2H   LDA 0,X                 ;* OUTPUT 2 HEX CHARACTERS
        BSR OUTHL               ;* OUTPUT LEFT HEX CHAR
        LDA 0,X
        INX
        BRA OUTHR               ;* OUTPUT RIGHT HEX CHAR

OUT4HS  BSR OUT2H
OUT2HS  BSR OUT2H
OUTS    PSHS A
        LDA #$20                ;* SPACE
        LBSR OUTEEE
        PULS A,PC               ;* AND RETURN

        ;;* JUMP TO USER PROGRAM COMMAND

JUMP    BSR BADDR              ;* GET ADDRESS
        LDS #USTACK            ;* INITIALIZE STACK TO USER AREA
        JSR 0,X                ;* JUMP TO USER PROGRAM
        JMP WARMST             ;* ON RTS, RETURN TO WARM START

;* PSTRNG - CRLF FOLLOWED BY PDATA

PSTRNG  BSR CRLF
        BRA PDATA

;* CRLF - PRINT CR/LF
CRLF    PSHS X,B,A
        LDX #CRLFST
        BSR PDATA
        PULS A,B,X,PC           ;* AND RETURN
CRLFST  FCB $D,$A,0,0,0,0,4

;* PDATA - PRINT DATA STRING
PDATA   LDA 0,X                 ;* GET CHARACTER
        CMP A #4                ;* END?
        BEQ PDEXIT
        LBSR OUTEEE
        INX                     ;* POINT TO NEXTC
        BRA PDATA               ;* AND REPEAT
PDEXIT  RTS

;* INEEE - CHARACTER INPUT ROUTINE
INEEE   PSHS X,B,A              ;* SAVE REGISTERS
INRPT   BSR INCH7               ;* GET INPUT CHARACTER
        CMP A #$13              ;* IS IT CONTROL-S?
        BEQ GOTCS               ;* YES
        TST PORECH              ;* ECHO?
        BEQ NOECHO              ;* NO
        LBSR OUTEEE             ;* YES
NOECHO  LEAS 1,S                ;* BYPASS OLD A
        PULS B,X,PC             ;* AND RETURN

;* CONTROL-S DETECTED. GET AND INTERPRET COMMAND
GOTCS   BSR GETCMD              ;* DO COMMAND
        BRA INRPT

;* SUBR TO GET AND DO COMMAND
GETCMD  LDA #$07
        JSR OUTCHM              ;* ECHO CONTROL-G (BELL) ON CTL PORT
        BSR INCH7               ;* GET SECOND CHARACTER OF CMD
        CMP A #'0               ;* PORT 0 COMMAND?
        BNE NOT0                ;* NO
        LDB STATUS
        EOR B #$80              ;* FLIP PORT 0
        STB STATUS
        RTS                     ;*AND RETURN
NOT0    CMP A #'1               ;* PORT 1 COMMAND?
        BNE NOT1                ;* NO
        LDB STATUS
        EOR B #$40              ;* FLIP PORT 1
        STB STATUS
        RTS
NOT1    CMP A #'D               ;* PORT D COMMAND?;*
        BNE NOTD                ;* NO
        LDB STATUS
        EOR B #$10              ;* FLIP PORT D
        STB STATUS
        RTS
NOTD    CMP A #'P               ;* PAUSE COMMAND?
        BNE NOTP                ;* NO
        LDB STATUS
        EOR B #$08              ;* FLIP PAUSE
        STB STATUS
        LDA #$F
        STA PAUCTR              ;* RESET PAUSE LINE CNTR
        RTS                     ;* AND RETURN
NOTP    CMP A #$0D              ;* CR COMMAND TO QUIT?
        BNE NOTCR               ;* NO
QUIT    PULS A,B,X              ;* RESTORE REGISTERS
        LEAS 2,S                ;* REMOVE SUBR CALL
        JMP WARMST              ;* AND QUIT
NOTCR   RTS RETURN              ;* W/O DOING ANYTHING
;* ACTUAL CONTROL PORT INPUT ROUTINES
INCH7   BSR INCH8               ;* GET 7-BIT CHARACTER
        AND A #$7F              ;* MASK OUT PARITY
        RTS
;* GET 8-BIT CHARACTER
INCH8   PSHS X,B
 IFC &B,PERCOM
        LDA CURH                ;* TURN ON CURSOR
        ADD A #2
        STA PCURH
        LDA CURV
        STA PCURV
 ENDIF
 IFC &B,THOMAS
        LDA CURV
        LDB #64
        MUL COMPUTE             ;* CURSOR POSITION
        ADD B CURH
        ADC A #VIDRAM/256
        TFR D,X                 ;* SAVE CURSOR ADDRESS
        LDB 0,X                 ;* GET OLD CHAR AND SAVE IT
        LDA #$5F                ;* UNDERLINE
        STA 0,X                 ;* INSERT CURSOR
 ENDIF
        LDA KBDINZ              ;* CONFIGURE ACIA
        STA [PORADD]
ACIAIN  LDA [PORADD]            ;* CHECK RDRF
        ASR A
        BCC ACIAIN              ;* WAIT FOR CHARACTER
 IFC &B,PERCOM
        LDA #$FF
        STA PCURV               ;* TURN OFF CURSOR
        STA PCURH
 ENDIF
 IFC &B,THOMAS
        STB 0,X                 ;* RESTORE OLD CHARACTER
 ENDIF
        LDX PORADD              ;* POINT TO ACIA
        LDA 1,X                 ;* GET CHAR
        PULS B,X,PC             ;* AND RETURN
;* INCHEK - CHECK FOR CHAR IN CTRL PORT
INCHEK  PSHS A
        LDA [PORADD]            ;* LOOK AT ACIA
        BIT A #$01              ;* RDRF?
        PULS A,PC               ;* AND RETURN
;* OUTEEE - CHARACTER OUTPUT ROUTINE
OUTEEE  PSHS X,B,A              ;* SAVE REGISTERS
        LDX PORADD
        LDA 0,X                 ;* CHECK CONTROL PORT
        ASR A
        BCC NOTEST              ;* NO CHARACTER
        LDA 1,X                 ;* CHARACTER; GET IT
        AND A #$7F              ;* MASK OUT PARITY BIT;*
        CMP A #$13              ;* IS IT CONTROL-S?;*
        BNE NOTEST              ;* NO
        LBSR GETCMD             ;* YES; GET COMMAND AND DO IT
NOTEST  PULS A                  ;* FINISHED TESTING FOR COMMAND
        PSHS A                  ;* SAVE AGAIN
;* CHECK FOR PAUSE
        LDB STATUS
        BIT B #$08              ;* PAUSE ON?
        BEQ NOPAUS              ;* NO
        CMP A #$10              ;* CLEAR SCREEN?
        BNE NOCLR               ;* NO
        LDB #$0F                ;* YES; RESET PAUSE COUNTER
        STB PAUCTR
        BRA NOPAUS
NOCLR   CMP A #$0D              ;* CR?
        BNE NOPAUS              ;* ONLY PAUSE AT END OF LINE
        DEC PAUCTR              ;* DECR PAUSE LINE CNTR
        BNE NOPAUS              ;* AND CHECK IT
        LDA #$0F                ;* MUST PAUSE. RESET CNTR
        STA PAUCTR
        BSR INCH7               ;* WAIT FOR RESTART CHAR
        CMP A #$0D              ;* QUIT IF IT'S A CR
        LBEQ QUIT
NOPAUS  PULS A                  ;* RESTORE CHAR
        TST STATUS              ;* PRINT ON PORT 0?
        BPL NOTPT0              ;* NO
        BSR OUTCH0              ;* YES
NOTPT0  LDB STATUS              ;* PRINT ON CTL PORT?
        BIT B #$40
        BEQ NOTPTM              ;* NO
        BSR OUTCHM              ;* YES
 IFNC &B,TERMINAL
NOTPTM  LDB STATUS              ;* VIDEO OUT?
        BIT B #$20
        BEQ NOTVID              ;* NO
        BSR OUTCHV              ;* OUTPUT ON VIDEO
NOTVID  LDB STATUS              ;* OUTPUT ON D?
 ENDIF
 IFC &B,TERMINAL
NOTPTM  LDB STATUS              ;* OUTPUT ON D?
 ENDIF
        BIT B #$10
        BEQ NOTPTD              ;* NO
        PSHS A SAVE             ;* CHAR
        JSR [DVECTR]            ;* YES
        PULS A RESTORE          ;* CHAR
NOTPTD  PULS B,X,PC             ;* AND RETURN
;* ALTERNATE OUTEEE ENTRY W/O CTRL-S TEST
OUTNOT  PSHS X,B,A              ;* SAVE REGISTERS
        BRA NOTEST
;* OUTPUT ON PORT 0
OUTCH0  PSHS X,B
        LDX #ACIA0              ;* OUTPUT TO PORT 0
        BRA OUTCHE
;* OUTPUT ON CONTROL PORT
OUTCHM  PSHS X,B
        LDX PORADD
OUTCHE  LDB PTRINZ              ;* ACIA INITIALIZATION
        STB 0,X                 ;* INITIALIZE FOR 8 BITS, 2 SB
OUTM2   LDB 0,X                 ;* WAIT UNTIL READY
        ASR B
        ASR B
        BCC OUTM2
        STA 1,X                 ;* PRINT IT
        PULS B,X,PC             ;* AND RETURN
 IFNC &B,TERMINAL
;* OUTCHV - OUTPUT ON VIDEO
OUTCHV  PSHS X,B,A
        CMP A #$08              ;* BACKSPACE
        BEQ BS
        CMP A #$0D              ;* CARRIAGE RETURN
        BEQ CR
        CMP A #$0A              ;* LINE FEED
        BEQ LF
        CMP A #$10              ;* CLEAR SCREEN
        BEQ CLRSCR
        CMP A #$0B              ;* VERT TAB
        BEQ VT
        CMP A #$0C              ;* NON-DESTRUCTIVE SPACE
        BEQ FF
        CMP A #$7F              ;* RUBOUT
        BEQ VRTS
        BIT A #$E0              ;* MASK CONTROL CHARACTERS
        BEQ VRTS
        PSHS A                  ;* SAVE CHARACTER
        LDB CURV                ;* AND COMPUTE VIDRAM LOC
 ENDIF
 IFC &B,PERCOM
        LDA #80
 ENDIF
 IFC &B,THOMAS
        LDA #64
 ENDIF
 IFNC &B,TERMINAL
        MUL
 ENDIF
 IFC &B,PERCOM
        ADDD #VIDRAM
 ENDIF
 IFNC &B,TERMINAL
        ADD B CURH
 ENDIF
 IFC &B,PERCOM
        ADC A #0
 ENDIF
 IFC &B,THOMAS
        ADC A #VIDRAM/256
 ENDIF
 IFNC &B,TERMINAL
        TFR D,X                 ;* MOVE INTO INDEX
        PULS A                  ;* RESTORE CHARACTER
        STA 0,X                 ;* DISPLAY CHAR ON SCREEN
FF      INC CURH                ;* MOVE CURSOR RIGHT
        LDA CURH
 ENDIF
 IFC &B,PERCOM
        CMP A #79               ;* END OF LINE?
 ENDIF
 IFC &B,THOMAS
        CMP A #63               ;* END OF LINE?
 ENDIF
 IFNC &B,TERMINAL
        BLE VRTS                ;* NO
        CLR CURH                ;* MOVE CURSOR
LF      LDA CURV                ;* GET VERTICAL POSITON
        CMP A #$F               ;* BOTTOM OF SCREEN?
        BEQ SCROLL              ;* YES, SO GO SCROLL
;* NOT YET AT BOTTOM
        INC CURV                ;* MOVE CURSOR DOWN 1
VRTS    PULS A,B,X,PC           ;* AND RETURN
;* ONCE AT BOTTOM, SCROLL
SCROLL  LDX TOP                 ;* POINT TO TOP OF SCREEN RAM
 ENDIF
 IFC &B,PERCOM
SCRO1   LDA 80,X
 ENDIF
 IFC &B,THOMAS
SCRO1   LDA 64,X
 ENDIF
 IFNC &B,TERMINAL
        STA 0,X                 ;* MOVE UP
 INX
 ENDIF
 IFC &B,PERCOM
        CMPX #BOT-79            ;* DONE SCROLLING?
 ENDIF
 IFC &B,THOMAS
        CMPX #BOT-63            ;* DONE SCROLLING?
 ENDIF
 IFNC &B,TERMINAL
        BNE SCRO1               ;* NO
;* ERASE BOTTOM LINE
ERASE   BRA EEOS1
BS      LDA CURH                ;* GET POSITION
        BEQ VRTS                ;* YES, IGNORE
        DEC CURH                ;* MOVE CURSOR LEFT 1
        BRA VRTS
VT      LDA CURV                ;* GET POSITION
        CMP A #0                ;* TOP OF RAM?;*
        BEQ VRTS                ;* YES, IGNORE
        DEC CURV                ;* MOVE UP 1 LINE
        BRA VRTS
CR      CLR CURH                ;* CARRIAGE RETURN ROUTINE
        BRA VRTS
;* CLEAR SCREEN
CLRSCR  CLR CURH                ;* SET UP CURSOR HORIZ POSITION
        CLR A                   ;* ALLOW FOR 1 OR 2 EMPTIES
        LDB TOP+1
 ENDIF
 IFC &B,PERCOM
HOUP1   SUB B #80
 ENDIF
 IFC &B,THOMAS
HOUP1   SUB B #64
 ENDIF
 IFNC &B,TERMINAL
        BMI HOUP2
        INC A                   ;* MOVE DOWN TO NEXT
        BRA HOUP1
HOUP2   STA CURV                ;* CURSOR VERTICAL
 ENDIF
 IFC &B,PERCOM
        LDA #$F                 ;* BOTTOM OF MEMORY
        STA BOTLNE              ;* SCROLL START
 ENDIF
 IFNC &B,TERMINAL
EEOS    LDX TOP                 ;* START AT TOP OF SCREEN
EEOS1   LDD #$2020              ;* ERASE TO END OF SCREEN
EEOS2   STD 0,X                 ;* CLEAR NEXT
        STD 2,X
        LEAX 4,X
        CMPX #BOT+1             ;* BOTTOM?
        BNE EEOS2
        BRA VRTS
;* VINIT - VIDEO INITIALIZATION
VINIT   PSHS X,A
 ENDIF
 IFC &B,PERCOM
        CLR A
        LDX #VIDRAM+$07C0
        STA $A,X                ;* RESET CONTROLLER
        STA $E,X                ;* SET UP TIMING CHAIN
        LDA #$65
        STA 0,X                 ;* HORIZONTAL LINE COUNT
        LDA #$64
        STA 1,X                 ;* INTERLACE H SYNC
        LDA #$6D
        STA 2,X                 ;* ROW SCAN CHAR
        LDA #$8F
        STA 3,X                 ;* ROW FRAME
        LDA #$03
        STA 4,X                 ;* LINE FRAME
        LDA #$20
        STA 5,X                 ;* VERT START
        LDA #$0F
        STA 6,X                 ;* LAST ROW
        CLR A
        STA $A,X                ;* RESET CONTROLLER
        STA $E,X                ;* START TIMING CHAIN
        LDA #VIDRAM/256
        STA TOP                 ;* SET TOP = D800
        CLR TOP+1
        LDA #$10                ;* CLEAR SCREEN
        LBSR OUTCHV
        LDA #$65                ;* REPEAT
        STA VIDRAM+$07C0
        PULS A,X,PC             ;* AND RETURN
 ENDIF
 IFC &B,THOMAS
        LDX #VIDRAM
        STX TOP                 ;* SET TOP = B800;*
        LDA #$10                ;* CLEAR SCREEN
        LBSR OUTCHV
        PULS A,X,PC             ;* AND RETURN
 ENDIF
 IFNC &B,TERMINAL
 ENDIF
 IFC &B,TERMINAL

;* 'ED' - ELEKTRA SUPER FLOPPY DISK CONTROLLER
;*     RELOCATABLE BOOTSTRAP LOADER
;* VERSION 1.1 LATEST REVISION 07-MAR-83

ELBOOT  LDS #SCRPAD             ;* INIT STACK POINTER
        LDA #$D0                ;* ABORT DISK COMMAND IN PROGRESS
        BSR WRFDC               ;* WRITE COMMAND TO FDC
        LDA #$00                ;* SETUP 5 INCH CONTROL WORD
        LDB DSTATS              ;* GET DISK BOARD STATUS REGISTER
        BITB #$08               ;* BOOT SWITCH SET FOR 5 INCH DATA?
        BEQ BOOT2               ;* BRANCH IF YES - 5 INCH
        LDA #$90                ;* SETUP 8 INCH CONTROL WORD
        STA DLATCH              ;* WRITE CONTROL WORD TO DISK BOARD
;* 8 INCH DATA RECOVERY HAS BEEN SELECTED.
;* LOOK FOR AN INDEX PULSE FROM DRIVE
;* TO DETERMINE WHETHER BOOT DRIVE IS A
;* STANDARD 8 INCH DRIVE OR A 5 INCH
;* SUPER FLOPPY DRIVE.
        LDA #$D4                ;* FORCE INTERRUPT ON INDEX PULSE
        BSR WRFDCI              ;* WRITE COMMAND TO FDC
        LDX #17858              ;* INITIALIZE TIMEOUT COUNTER
BOOT1   BITB DSTATS             ;* HAS INDEX PULSE CAUSED INTRQ?
        BNE BOOT3               ;* BRANCH IF YES-8 INCH CABLE
        BSR EDELAY              ;* WAIT FOR 32 CYCLES
        DEX                     ;* DECREMENT TIMEOUT COUNTER
        BNE BOOT1               ;* LOOP UNTIL INTRQ OR 1,000,048 CYC
        LDA #$10                ;* MUST BE 5 INCH SUPER FLOPPY
;*                          SO SWITCH TO 5 INCH CABLE
;* DISK BOARD CONTROL WORD HAS BEEN SELECTED.
;* RESTORE DRIVE AND WAIT FOR MOTOR STARTUP DELAY
BOOT2   STA DLATCH              ;* WRITE CONTROL WORD TO DISK BOARD
BOOT3   LDA #$0B                ;* RESTORE DRIVE WITH SLOW STEPPING
        LDB #$10                ;* GET MOTOR DELAY BIT MASK
        BSR WRFDC               ;* WRITE COMMAND TO FDC
BOOT4   BITB DSTATS             ;* CHECK MOTOR DELAY TIMER
        BNE BOOT4               ;* LOOP UNTIL DELAY TIMEOUT OCCURS
        BSR WTINT               ;* WAIT FOR RESTORE TO FINISH
        BEQ READ                ;* BRANCH IF NO ERRORS-DRIVE READY
;* FDC STATUS HAS RETURNED AN ERROR.
;* PRINT MESSAGE AND RETURN TO MONITOR.
ERROR   BPL ERROR1              ;* BRANCH IF DRIVE WAS READY
ERRRDY  LDX #NOTRDY             ;* DRIVE NOT READY
        BRA ERROR2              ;* GO PRINT ERROR MESSAGE
ERROR1  LDX #BTERR              ;* BOOT ERROR
ERROR2  JSR PSTRNG              ;* PRINT CR,LF,ERROR MESSAGE
        JMP START               ;* RETURN TO ROM MONITOR
;* WRFDCI - WRITE COMMAND TO FDC
;*           LOOK FOR INTRQ
;* WRFDC - WRITE COMMAND TO FDC
WRFDCI  LDB #$04                ;* GET INTRQ ONLY MASK;*
WRFDC   STA CMDREG              ;* START REQUESTED COMMAND
;*                          FALL INTO DELAY SUBROUTINE
;* EDELAY = 32 CYCLES
EDELAY  LDA #5                  ;*GET DELAY LOOP COUNT (2)
EDELA1  DECA                    ;* DECREMENT COUNT (2)
        BNE EDELA1              ;* LOOP UNTIL DELAY COMPLETED (3)
        RTS                     ;* RETURN (5)
;* WTINT - WAIT FOR FDC INTERRUPT
;*           RETURN WITH FDC STATUS
WTINT   LDB #$04                ;* GET INTRQ ONLY MASK
WTINT1  BITB DSTATS             ;* CHECK FOR INTRQ SET
        BEQ WTINT1              ;* LOOP UNTIL INTRQ OCCURS
        LDB STAREG              ;* GET FDC ERROR STATUS
        BITB #$98               ;* SET Z BIT ACCORDING TO ERRORS
        RTS                     ;* RETURN WITH ERROR CODE IN B
;* DRIVE IS ON TRACK ZERO AND READY.
;* READ LOADER INTO MEMORY.
READ    LDX #$C000              ;* POINT TO LOAD ADDRESS
        LDA #1                  ;* GET SECTOR NUMBER
        STA SECREG              ;* WRITE SECTOR NUMBER TO FDC
        LDA #$80                ;* READ SINGLE SECTOR
        LDB #$84                ;* GET DRQ AND INTRQ MASK
        BSR WRFDC               ;* WRITE COMMAND TO FDC
        BRA READ2               ;* ENTER READ LOOP-WAIT FOR FDC
;* TIGHT READ LOOP
READ1   LDA DATREG              ;* GET DATA BYTE FROM FDC
        STA 0,X+                ;* STORE DATA BYTE IN MEMORY
READ2   BITB DSTATS             ;* TEST DRQ AND INTRQ LINES
        BMI READ1               ;* BRANCH IF DATA AVAILABLE
        BEQ READ2               ;* LOOP UNTIL INTERRUPT OCCURS
;* CHECK FOR ERRORS
READ3   LDB STAREG              ;* GET FDC ERROR STATUS
        BITB #$9C               ;* SET Z BIT ACCORDING TO ERRORS
        BNE ERROR               ;* BRANCH IF ERRORS
        JMP $C000               ;* NO ERRORS-JUMP TO DOS LOADER
;* ERROR MESSAGES
NOTRDY  FCC 'DRIVE NOT READY'
        FCB 4
BTERR   FCC 'BOOT ERROR'
        FCB 4
;* 'RC' - REGISTER CHANGE
REGCHG  LDX #WHMESG
        JSR PDATA               ;* ASK WHICH REGISTER
        JSR INEEE               ;* GET LETTER
        LDB #11                 ;* COUNTER
        LDY #RNAMES             ;* POINT TO NAME TABLE
        LDX SP                  ;* GET USER STACK POINTER
RCLOOP  CMPA 0,Y+               ;* COMPARE NAME
        BEQ RCOK                ;* IF LETTER FOUND
        INX POINT               ;* TO NEXT LETTER
        DECB CHECK              ;* COUNTER
        BNE RCLOOP              ;* LOOP TIL DONE
        JMP COMEND              ;* QUIT WITH '?' IF NG
RCOK    JMP CHANG0              ;* GO TO 'ME' IF OK
WHMESG  FCC 'REG: '
        FCB 4
RNAMES  FCC 'CABDXXYYUUP'       ;* REGISTER NAMES
 ENDIF

;* "FD" PROGRAMMED I/O DISK BOOT COMMAND
FLBOOT  LDS #SCRPAD             ;* RESET STACK
        LDA #$D0
        STA CMDREG              ;* FORCE FDC INTERRUPT
        CLR DLATCH              ;* DRIVE = 0
        LDB #3
FLWAIT  JSR DELAY
        DEC B                   ;* WAIT WAIT FOR MOTOR
        BNE FLWAIT
        LDA #$0F                ;* RESTORE, SLOW, LOAD, VERIFY
        BSR GIVFDC
FL1     LDB STAREG
        AND B #$01              ;* BUSY?
        BNE FL1                 ;* YES
        INC B
        STB SECREG
        BSR SHORTD
        LDA #$8C                ;* LOAD AND READ
        BSR GIVFDC
        LDX #$C000              ;* POINT TO MEM
        BRA FL3
FL2     AND B #$02              ;* DRQ?
        BEQ FL3
        LDA DATREG
        STA 0,X+
FL3     LDB STAREG
        BIT B #$01              ;* BUSY?
        BNE FL2
        BIT B #$2C              ;* CHECK FOR CRC ERROR OR LOST DATA
        BEQ FLDONE
        BRA FLBOOT              ;* REPEAT ON ERROR
FLDONE  JMP $C000               ;* GO TO EXECUTE
;* SHORT DELAY FOR BOOT
GIVFDC  STA CMDREG              ;* GIVE FDC COMMAND AND WAIT
SHORTD  LDB #20
SHORT1  DEC B
        BNE SHORT1
        RTS
;* "LO" COMMAND - LOAD MIKBUG FORMAT TAPE
LOAD    BSR RDON                ;* READER ON
LOAD3   JSR INEEE
        CMPA #'S
        BNE LOAD3               ;* 1ST CHAR NOT S
        JSR INEEE               ;* READ CHAR
        CMPA #'9
        BEQ LOAD21
        CMPA #'1
        BNE LOAD3               ;* 2ND CHAR NOT 1
        CLR B                   ;* ZERO CHECKSUM
        JSR BYTE                ;* READ BYTE
        PSHS A
        ADD B 0,S+              ;* ADD TO CHECKSUM
        SUBA #2
        PSHS B
        TFR A,B
        CLR A
        TFR D,Y
        PULS B                  ;* RESTORE CHKSUM
        JSR BADDR               ;* GET ADDRESS
        PSHS X
        ADD B 0,S+              ;* ADD TO CHECKSUM
        ADD B 0,S+
;* STORE DATA
LOAD11  JSR BYTE
        PSHS A
        ADD B 0,S+
        LEAY -1,Y               ;* DECR COUNT
        BEQ LOAD15              ;* ZERO BYTE COUNT?
        STA 0,X                 ;* STORE DATA
        CMPA 0,X                ;* DATA STORED?
        BNE LOAD19
        INX
        BRA LOAD11
LOAD15  COM B
        BEQ LOAD3
LOAD19  LDA #'?
        JSR OUTEEE
LOAD21  BRA RDOFF               ;* READER OFF AND RTS TO NXTCMD
FROMST  FCC '    FROM '
        FCB 4
;* "CL" CLEAR SCREEN COMMAND
CLEAR   LDX #CLRSTR
        JSR PDATA
        RTS
TOSTR   FCC '   TO '
        FCB 4
;* GENERAL PURPOSE DELAY ROUTINE
DELAY   LDX #$FFFF
DELAY1  DEX
        BNE DELAY1
        RTS
;* RDON - READER ON ROUTINE
RDON    LDA #$11                ;* ^Q RON CHAR
RPOUT   JSR OUTEEE
        BRA DELAY
;* RDOFF - READER OFF ROUTINE
RDOFF   LDA #$13                ;* TURN READER OFF
        BRA RPOUT
;*PNCHON - PUNCH ON ROUTINE
PNCHON  LDA #$12
        BRA RPOUT
;* PNCHOF - PUNCH OFF ROUTINE
PNCHOF  LDA #$14
        BRA RPOUT
MANYST  FCC 'HOW MANY BYTES? '
        FCB 4
CLRSTR  FCB $7E,$1C,$10,$10,4   ;* CLEAR SCREEN CHARACTERS
;* OUTPUT 4 HEX SPACE
OUT4HV  JSR OUT2H               ;* OUTPUT 4 HEX CHAR + SPACE
OU2HSV  JSR OUT2H               ;* OUTPUT 2 HEX CHAR + SPACE
        JMP OUTS                ;* VECTOR TO FC ROM
;* 'HD' HEX DUMP COMMAND
HEXDMP  JSR FROMTO
        LDD BEGA                ;* GET STARTING ADDRESS
        AND B #$F0              ;* ROUND DOWN TO NEXT 0
        TFR D,Y
HEX     JSR CRLF
        PSHS Y
        TFR S,X                 ;* POINT TO ST ADDR
        JSR OUT4HS              ;* PRINT IT
        PULS Y
        JSR OUTS                ;* EXTRA SPACE
        LDB #16                 ;* SET COUNTER TO 16
        TFR Y,X
HEX1    JSR OUT2HS              ;* PRINT NEXT BYTE
        DEX                     ;* BACKUP PTR
        CMPX ENDA               ;* LAST ADDRESS?
        BNE HEX2                ;* CONTINUE IF NOT
        RTS                     ;* OTHERWISE END
HEX2    INX                     ;* RESTORE PTR
        DEC B                   ;* DECREMENT COUNTER
        BNE HEX1                ;* CONTINUE LINE IF NOT FINISHED
        TFR X,Y
        BRA HEX             ;*GET READY FOR NEXT LINE
;* "PU" COMMAND - PUNCH MIKBUG FORMAT TAPE
PUNCH   JSR FROMTO              ;* GET BEGA AND ENDA
        LDX BEGA
PUNCH2  TFR X,Y
        BSR PNCHON
PUN11   LDD ENDA
        PSHS Y
        SUBD 0,S++
        CMPD #$20
        BCS PUN23
PUN22   LDB #$1F
PUN23   ADDB #4
        CLR A
        TFR D,U
        SUBB #3
        STB TEMP
;* PUNCH CR LF NULLS S1
        LDX #S1STR1
        JSR PDATA
        CLRB
;* PUNCH FRAME COUNT
        PSHS U
        TFR S,X
        LEAX 1,X
        BSR PUNT2               ;* PUNCH 2 HEX CHARACTERS
        LEAS 2,S
;* PUNCH ADDRESS
        PSHS Y
        TFR S,X
        BSR PUNT2
        BSR PUNT2
        LEAS 2,S
;* PUNCH DATA
        TFR Y,X
PUN32   BSR PUNT2               ;* PUNCH ONE BYTE
        DEC TEMP
        BNE PUN32
        TFR X,Y
        COMB
        PSHB
        TSX
        BSR PUNT2               ;* PUNCH CHECKSUM
        PULB                    ;* RESTORE STACK
        TFR Y,X
        DEX
        CMPX ENDA
        BNE PUN11
        JSR PNCHOF              ;* TURN OFF PUNCH
        RTS
;* PUNCH W HEX CHAR, UPDATE CHEKSUM
PUNT2   ADDB 0,X
        JMP OUT2H               ;* OUTPUT TWO HEX CHAR AND RTS
S9      FCB 'S,'9,4             ;* END OF TAPE
S1STR1  FCB $D,$A,$15,0,0,0,'S,'1,4 ;* PUNCH FORMAT
WITHST  FCC ' WITH? '
        FCB 4
WHATST  FCC 'FIND WHAT? '
        FCB 4
;* "EN" COMMAND - PUNCH END OF TAPE WITH S9
PNCHS9  JSR PNCHON
        LDX #S9
        JSR PDATA
POFC4   JMP PNCHOF ;* AND RETURN
;* FROMTO SUBR - INITIALIZE BEGA AND ENDA ADDRESSES
FROMTO  LDX #FROMST
        JSR PDATA               ;* PRINT "FROM "
        JSR INEEE               ;* GET CHARACTER
        CMP A #$0D              ;* IS IT A CR?
        LBEQ CRLF               ;* ON CR, DO CRLF AND RETURN
        SUB A #$30              ;* CONTINUE .. CHECK FOR DIGIT
        LBMI NXTCMD             ;* NOT HEX
        CMP A #$9
        BLE GOTONE
        CMP A #$11
        LBMI NXTCMD             ;* NOT HEX
        CMP A #$16
        LBGT NXTCMD             ;* NOT HEX
        SUB A #7                ;* CONVERT A-F TO NUMBER
GOTONE  ASL A                   ;* GOT FIRST DIGIT
        ASL A
        ASL A
        ASL A
        TFR A,B                 ;* TEMP SAVE IT
        JSR INHEX               ;* GET SECOND DIGIT
        ABA                     ;* COMBINE THEM
        STA BEGA                ;* STORE LEFT TWO DIGITS
        JSR BYTE                ;* GET NEXT TWO
        STA BEGA+1              ;* STORE RIGHT TWO AS FROM ADDRESS
        LDX #TOSTR
        JSR PDATA               ;* PRINT "TO "
        JSR BADDR               ;* GET TO ADDRESS
        STX ENDA                ;* STORE IT
        JMP OUTS
;* "FI" COMMAND - FIND 1, 2, OR 3 BYTES IN MEMORY
FIND    LDX #MANYST
        JSR PDATA               ;* ASK "HOW MANY BYTES"
        JSR INEEE               ;* GET NUMBER
        SUB A #$30              ;* CONVERT FROM ASCII
        LBEQ CRLF               ;* IF = 0
        LBMI CRLF               ;* IF LESS THAN 0
        CMP A #$3
        LBGT CRLF               ;* IF GREATER THAN 3
        STA FINDNO              ;* STORE NUMBER OF BYTES
        JSR OUTS
        LDX #WHATST
        JSR PDATA               ;* ASK "WHAT BYTES"
        LDB FINDNO              ;* GET NUMBER
        LDX #WHAT
FIENTR  PSHS B
        JSR BYTE                ;* ENTER A BYTE
        PULS B                  ;* RESTORE COUNTER
        STA 0,X                 ;* STORE IT
        INX
        DEC B
        BNE FIENTR              ;* ENTER MORE, IF NEEDED
        JSR FROMTO              ;* GET BEGA AND ENDA
        LDX BEGA                ;* GET READY TO LOOK
FIND1   LDB FINDNO              ;* MAIN FIND LOOP
        LDA 0,X                 ;* GET FIRST BYTE
        CMP A                   ;* WHAT
        BNE FIND4               ;* WRONG BYTE
        DEC B
        BEQ FIND2               ;* FOUND ONE CORRECT BYTE
        LDA 1,X                 ;* GET SECOND BYTE
        CMP A                   ;* WHAT+1
        BNE FIND4               ;* WRONG
        DEC B
        BEQ FIND2               ;* FOUND TWO CORRECT BYTES
        LDA 2,X                 ;* GET THIRD BYTE
        CMP A                   ;* WHAT+2
        BNE FIND4               ;* WRONG BYTE
FIND2   LBSR CRLF               ;* PRINT CRLF
        PSHS X                  ;* SAVE ADDR PTR
        LEAX 0,S                ;* POINT TO ADDRESS
        LBSR OUT4HS             ;* PRINT IT
        JSR OUTS                ;* ONE MORE SPACE
        LDX 0,S                 ;* RESTORE PTR
        LEAX -1,X               ;* BACKUP ONE BYTE
        LDB #4                  ;* READY TO PRINT FOUR BYTES
FIND3   JSR OUT2HS              ;* PRINT BYTE
        DEC B
        BNE FIND3               ;* PRINT FOUR BYTES
        PULS X                  ;* RESTORE INDEX REGISTER
FIND4   CMPX ENDA               ;* SEE IF DONE
        LBEQ CRLF               ;* YES
        INX                     ;* NO
        BRA FIND1               ;* KEEP LOOKING
;* "FM" COMMAND - FILL MEMORY WITH CONSTANT
FILL    JSR FROMTO              ;* GET FROM-TO ADDRESSES
        LDX #WITHST
        JSR PDATA               ;* ASK FOR DATA
        JSR BYTE
        LDX BEGA                ;* GET STARTING ADDRESS
        DEX
FILOOP  INX
        STA 0,X                 ;* STORE THE BYTE
        CMPX ENDA               ;* SEE IF DONE
        BNE FILOOP              ;* CONTINUE OF NO
        RTS QUIT                ;* WHEN DONE
;* SUM - MEMORY CHECKSUM
SUM     JSR FROMTO              ;* GET ADDRESS LIMITS
        LDX BEGA                ;* GET STARTING ADDRESS
        CLR A
        CLR B
SUMLP   ADD B 0,X               ;* ADD TO CHECKSUM
        ADC A #0                ;* ALSO ADD CARRY TO SECOND BYTE
        CMPX ENDA               ;* LAST ADDRESS?
        BEQ SUMDON              ;* YES
        INX                     ;* NO, SO INCREMENT AND
        BRA SUMLP
SUMDON  PSHS D
        LEAX 0,S                ;* POINT TO CHKSUM
        JSR OUT4HS              ;* OUTPUT CHECKSUM
        PULS D,PC               ;*AND RETURN
;* MTEST MEMORY TEST
MTEST   JSR FROMTO              ;* GET ADDRESS LIMITS
        LDX BEGA
LODREG  LDU 0,X                 ;* SAVE OLD CONTENTS
        LDA #1                  ;* STORE 1 IN MEMORY
        STA 0,X
        LDB 1,X                 ;* DISCHARGE BUS CAP
        CMP A 0,X               ;* WAS 1 WRITTEN?
        BNE ERRPNT
ROLP1   ASL A
        STA 0,X
        LDB 1,X                 ;* DISCH BUS
        CMP A 0,X
        BNE ERRPNT
        CMP A #$80              ;* SHIFT UNTIL 80
        BNE ROLP1
        BRA INCR1
ERRPNT  PSHS X,A
        JSR CRLF
        LEAX 1,S                ;* LOAD ADDRESS OF WRONG BYTE
        LBSR OUT4HS             ;* PRINT IT
        LEAX 0,S                ;* LOAD ADDRESS OF CORRECT BIT
        JSR OUT2HS              ;* OUTPUT WHAT SHOULD BE STORED
        LDX 1,S                 ;* LOAD ADDRESS OF WRONG BIT
        JSR OUT2HS              ;* OUTPUT WHAT WAS STORED
        PULS A,X                ;* LOAD ADDRESS OF WRONG BIT
INCR1   STU 0,X                 ;* RESTORE PREVIOUS
        CMPX ENDA               ;* COMPARE WITH END ADDRESS
        BEQ FINISH
        INX
        BRA LODREG              ;* DO NEXT BYTE IF NOT DONE
FINISH  LDA #'+ PRINT           ;* "+" WHEN DONE
        JMP OUTEEE              ;* AND RETURN WHEN DONE
;* 'DE' COMMAND - DESEMBLER DUMP
DESEMB  JSR FROMTO              ;* ASK FOR ADDRESSES
        LDX BEGA
        PSHS X,B,A              ;* MAKE ROOM
DES2    BSR PRNTOP              ;* GO TO PRINT CURRENT LINE
        LDD ENDA
        SUBD 2,S
        BCC DES2                ;* RETURN IF NEXT <= LAST
        PUL A,B,X
        RTS                     ;* OTHERWISE EXIT;*
;* PRNTOP - SUBR TO PRINT ADDRESS AND CURRENT INSTR
PRNTOP  JSR CRLF
        LEAX 4,S                ;* POINT TO NEXT ADDRESS
        JSR OUT4HS              ;* PRINT IT
        JSR OUTS
        CLR B
        CLR 2,S                 ;* PREFIX
        LDX 4,S                 ;* GET ADDRESS OF INSTR
        LDA 0,X                 ;* GET OPERATION CODE
PROP1   STA 3,S                 ;* SAVE OPCODE
        PSHS B
        JSR OUT2HS              ;* PRINT IT
        STX 5,S                 ;* SAVE ADDRESS
        PULS B
        LDA 3,S                 ;* GET OP CODE
        CMP A #$10
        BCS NOT10
        CMP A #$12
        BCC NOT10
        STA 2,S                 ;* STORE 10 OR 11 PREFIX
        LDA 0,X                 ;* GET NEXT CHAR
        CMP A #$30
        BCC PROP1               ;* IF NOT LONG BRANCH
        INC B
        BRA PROP1               ;* FOR LONG BRANCH
NOT10   AND A #$FC
        CMP A #$30
        BEQ INDEX               ;* INDEXED INSTR
        LSRA
        LSRA
        LSRA
        LSRA
        BEQ LENTH2              ;* OP 0X
        DEC A
        BEQ LOOK1               ;* 1X
        DEC A
        BEQ LENTH2              ;* 2X
        DEC A
        BEQ LOOK3               ;* 3X
        DEC A
        BEQ LENTH1              ;* 4X
        DEC A
        BEQ LENTH1              ;* 5X
        DEC A
        AND A #$03
        BEQ INDEX               ;* 6X,AX,EX
        DEC A
        BEQ LENTH3              ;* 7X,BX,FX
        DEC A
        BEQ LOOK8C              ;* 8X,CX
        BRA LENTH2              ;* 9X,DX
LOOK1   LDA 3,S                 ;* GET OP CODE
        AND A #$0F
        CMP A #$05
        BCS LENTH1
        CMP A #$09
        BCS LENTH3
        BEQ LENTH1
        CMP A #$0D
        BEQ LENTH1
        BRA LENTH2
LOOK3   LDA 3,S                 ;* GET OP CODE
        BIT A #$08
        BEQ LENTH2
        AND A #$03
        BEQ LENTH2
        BRA LENTH1
LOOK8C  LDA 3,S                 ;* GET OP CODE
        AND A #$0F
        CMP A #$03
        BEQ LENTH3
        AND A #$0D
        CMP A #$0C
        BEQ LENTH3
        BRA LENTH2
INDEX   LDA 0,X                 ;* GET POST-BYTE
        CMP A #$9F
        BEQ LENTH4
        AND A #$8B
        CMP A #$88
        BEQ LENTH3
        CMP A #$89
        BNE LENTH2
LENTH4  INC B                   ;* 4-BYTE
LENTH3  INC B                   ;* 3-BYTE
LENTH2  INC B                   ;* 2-BYTE
LENTH1  TST B                   ;* 1-BYTE
        BEQ POP3
        DEC B
        BEQ POP1
        DEC B
        BEQ POP4
        JSR OUT2HS
POP4    JSR OUT4HS              ;* PRINT 2 BYTES
        BRA POP2
POP1    JSR OUT2HS              ;* PRINT ONE BYTE
POP2    STX 4,S                 ;* INCREMENT NEXT ADDR
POP3    RTS
        FCC '(C) 1981 BY P. STARK'
;* END OF FIRST EPROM; BEGINNING OF SECOND
        ORG ROM+$0800
        FDB START               ;* VECTOR TO START
        FDB NXTCM1              ;* VECTOR TO NEXT COMMAND
        FDB INCH8               ;* VECTOR TO INPUT 8 BIT CHAR
        FDB INEEE               ;* VECTOR TO INPUT 7 BIT CHAR
        FDB INCHEK              ;* VECTOR TO CHECK FOR INPUT
        FDB OUTNOT              ;* VECTOR TO OUTPUT W/O CTRL-S
        FDB PDATA               ;* VECTOR TO OUTPUT STRING
        FDB CRLF                ;* VECTOR TO PRINT CR-LF
        FDB PSTRNG              ;* VECTOR TO CRLF AND PDATA
        FDB LRACVT              ;* CONVERT LRA ADDRESS
        JMP START               ;* VECTOR TO START
        FDB OUTEEE              ;* VECTOR TO OUTPUT WITH CTRL-S
;* 'RD' COMMAND - RETURN TO DOS
RETDOS  LDA A $CD00             ;* CHECK IF LOADED
        CMP A #$7E              ;* JUMP?
        LBNE COMEND             ;* IGNORE IF NOT
        JMP [RETADD]            ;* RETURN TO DOS IF OK
;* 'AI' COMMAND - ASCII INPUT ROUTINE
ASCIN   JSR FROMTO              ;* GET ADDRESS RANGE
        JSR CRLF
        LDX ENDA                ;* GET LAST EMPTY ADDRESS
        PSHS X
        LDX BEGA                ;* GET STARTING ADDRESS
        DEX
ASCI2   INX
        JSR INEEE               ;* GET NEXT CHARACTER
        STA 0,X                 ;* STORE IT
        LDB 1,X                 ;* DISCH BUS
        CMP A 0,X               ;* SEE IF IT STORED OK
        BNE ASCI3
        STX ENDA                ;* STORE ENDING ADDRESS
        CMPX 0,S                ;* CHECK IF RUN OUT OF MEMORY
        BNE ASCI2               ;* NO, SO GET MORE
ASCI3   LDX #ESTR               ;* MEM FULL OR BAD, SO..
        JSR PDATA               ;* PRINT ERROR
        BRA ASCI3               ;* GO TO REPEAT
ESTR    FCB ' ,'F,'U,'L,'L,' ,4
;* 'AO' COMMAND - ASCII OUTPUT ROUTINE
ASCOUT  JSR FROMTO              ;* GET ADDRESS RANGE
        JSR CRLF
        LDX BEGA                ;* GET STARTING ADDRESS
ASCO2   LDA 0,X                 ;* GET NEXT CHARACTER
        JSR OUTEEE              ;* OUTPUT IT
        CMPX ENDA               ;* SEE IF DONE
        BEQ ASCO3               ;* YES
        INX
        BRA ASCO2               ;* REPEAT IF NOT
ASCO3   RTS RETURN              ;* WHEN DONE
;* 'MO' COMMAND - MOVE MEMORY ROUTINE
OLDSTR  FCC 'ENTER OLD ADDRESSES:'
        FCB 4
NEWSTR  FCC 'ENTER NEW ADDRESS:  '
        FCB 4
MOVE    LDX #OLDSTR
        JSR PDATA               ;* ASK FOR OLD ADDRESSES
        JSR FROMTO
        JSR CRLF
        LDX #NEWSTR
        JSR PDATA               ;* ASK FOR NEW ADDRESS
        JSR BADDR
        STX NEWLOC      	;* SAVE
;* NOW CHECK FOR FORWARD MOVE OR BACKWARD MOVE
        LDD BEGA
        SUBD NEWLOC
        BCS BACK                ;* IF NEW>OLD
        BNE FORWRD
MEXIT   RTS NO                  ;* MOVE IF NEW=OLD
;* FORWARD MOVE
FORWRD  LDX BEGA
        LDY NEWLOC
FWD1    LDA 0,X+
        STA 0,Y+                ;* MOVE AND INCR
        CMPX ENDA               ;* END?
        BNE FWD1                ;* NO
        LDA 0,X
        STA 0,Y                 ;* MOVE LAST
        RTS
;* BACKWARD MOVE
BACK    LDD ENDA
        SUBD BEGA               ;* COMPUTE NEW END
        ADDD NEWLOC
        TFR D,Y
        LDX ENDA
        LDA 0,X                 ;* MOVE LAST
        STA 0,Y
BACK1   CMPX BEGA               ;* DONE?
        BNE BACK2               ;* NO
        RTS YES
BACK2   LDA 0,-X                ;* DECR AND MOVE
        STA 0,-Y
        BRA BACK1           ;* AND REPEAT
;* 'BR' COMMAND - SET/RESET UP TO FOUR BPS
BREAK   BSR BKNUM               ;* GET NUMBER OF DESIRED BP
        BSR BERASE              ;* GO ERASE OLD ONE
        LDX #NEWSTR             ;* PRINT "ENTER NEW ADDRESS: "
        JSR PDATA
        JSR BADDR               ;* GET ADDRESS
        STX 0,Y                 ;* STORE ADDRESS IN TABLE
        LDB 0,X                 ;* GET PRESENT OP CODE
        LDA #$3F                ;* GET SWI INSTR
        STA 0,X                 ;* SUBSTITUTE IT
        STB 2,Y                 ;* STORE DELETED OP CODE
        RTS                     ;* AND RETURN
;* ERASE PREVIOUS BP, IF ANY, AND RESTORE OP CODE
BERASE  LDB 2,X                 ;* GET OP CODE
        LDA 0,X                 ;* GET PART OF ADDRESS
        CMP A #$FF              ;* WAS THERE A BP?
        BEQ BEEXIT              ;* NO, EXIT
        LDX 0,X                 ;* YES, GET ADDRESS OF BREAK
        STB 0,X                 ;* RESTORE OP CODE
        LDA #$FF
        STA 0,Y                 ;* ERASE BP TABLE ENTRY
BEEXIT  RTS                     ;* AND RETURN
;* BKNUM ROUTINE - GET NO OF DESIRED BP & POINT
;* TO ITS LOCATION IN BKTAB TABLE
BNSTR   FCC ' NUMBER: '
 FCB 4
BKNUM   LDX #BNSTR
        JSR PDATA
        JSR INEEE               ;* GET BP NUMBER
        SUB A #$30              ;* CONVERT FROM ASCII
        LBMI NXTCMD             ;* IF NEGATIVE
        LBEQ NXTCMD             ;* IF ZERO
        CMP A #$4
        LBGT NXTCMD             ;* IF GREATER THAN 4
        TFR A,B
        JSR OUTS
        LDA #3
        MUL
        ADDD #BKTAB-3           ;* GET TABLE LOC
        TFR D,Y
        TFR D,X
        RTS                     ;* RETURN WHEN DONE
;* 'BP' COMMAND - PRINT BP LOCATIONS
BPRINT  LDB #'0                 ;* BP NUMBER IN ASCII
        LDX #BKTAB
BPR1    INC B
        CMP B #'5               ;* STOP AT 5 BPS
        BNE BPR2
        RTS                     ;* RETURN WHEN DONE
BPR2    JSR CRLF                ;* PRINT CR
        TBA                     ;* GET BP NUMBER
        JSR OUTEEE              ;* PRINT BP NUMBER
        LDA 0,X                 ;* GET BP ADDRESS
        CMP A #$FF              ;* IS THERE ONE?
        BNE BPR3                ;* YES, GO PRINT IT
        LEAX 3,X
        BRA BPR1                ;* AND REPEAT
BPR3    JSR OUTS                ;* PRINT SPACE
        JSR OUT4HS              ;* PRINT ADDRESS OF BP
        JSR OUT2HS              ;* PRINT OP CODE
        BRA BPR1                ;* AND REPEAT
;* BP RE-ENTRY POINT AFTER SWI IN MAIN PROGRAM
BKRETN  STS SP                  ;* SAVE USER STACK PTR
        LDX 10,S                ;* GET USER PC
        LEAX -1,X
        STX 10,S
;* NMI ABORT RE-ENTRY POINT
NMIRET  STS SP                  ;* SAVE USER STACK PTR
        LDS #MSTACK             ;* RESET TO MON STACK
;* 'RE' COMMAND - PRINT USER REGISTERS FROM STACK
REGIST  JSR CRLF
        LDX #REMSG
        JSR PDATA               ;* PRINT HEADER
        JSR CRLF
        LDX SP                  ;* POINT TO USER STACK
        LDB 0,X                 ;* GET CC REGISTER
        LDX #8                  ;* SET COUNTER
RELOOP  ASLB                    ;* MOVE NEXT BIT INTO CARRY
        LDA #$30
        ADC A #0                ;* CONVERT TO ASCII
        JSR OUTEEE              ;* PRINT IT
        DEX                     ;* BUMP COUNTER
        BNE RELOOP              ;* PRINT NEXT BIT
        JSR OUTS                ;* PRINT SPACE
        LDX SP                  ;* POINT TO USER STACK AGAIN
        INX                     ;* POINT TO A ACCUMULATOR
        JSR OUT2HS              ;* PRINT A
        JSR OUT2HS              ;* PRINT B
        JSR OUT2HS              ;* PRINT DP
        JSR OUT4HS              ;* PRINT X INDEX
        JSR OUT4HS              ;* PRINT Y INDEX
        JSR OUT4HS              ;* PRINT U
        JSR OUT4HS              ;* PRINT PC
        LDD SP
        ADDD #12                ;* RESTORE USER SP
        PSHS B,A                ;* TO VALUE IT HAD
        TFR S,X
        JSR OUT4HS
        LEAS 2,S                ;* FIX SP
        JMP NXTCMD              ;* AND RETURN
REMSG   FCC 'EFHINZVC A  B  DP  X    '
        FCC 'Y    U    PC   SP'
        FCB 4
;* 'CO' COMMAND - CONTINUE AFTER A BREAKPOINT
CONT    LDS SP                  ;* GET USER STACK POINTER
RTI     RTI                     ;* AND RETURN TO HIS PROGRAM
        NOP
STMSG   FCC 'START FROM ADDRESS: '
        FCB 4
;* 'ST' - START SINGLE-STEP COMMAND
STRTSS  LDX #STMSG              ;* ASK FOR STARTING ADDRESS
        JSR PDATA
        JSR BADDR               ;* GET IT
        STX USTACK-4            ;* SAVE IN USER STACK
        LDX #WARMST
        STX USTACK-2            ;* PUT MON STRT ADDR ON US STACK
        LDX #USTACK-14
        STX SP                  ;* SET CURR USER STACK JUST BELOW
        CLR 3,X                 ;* INIT DP REG TO 0
        LDA #$D0
        STA 0,X         ;* SET E,F,I BITS
;* 'SS' COMMAND - SINGLE STEP AFTER BREAKPOINT
STEP    LDA #$FF
        STA BRANCH              ;* ERASE PREV BR
        LDX SP                  ;* GET USER STACK POINTER
        LDY 10,X                ;* GET USER PC
        PSHS Y,B,A
        JSR PRNTOP              ;* PRINT ADDRESS AND INSTRUCTION
;* REPLACE NEXT INTRUCTION WITH SWI
        STX NEXT                ;* SAVE ADDRESS
        LDA 0,X                 ;* GET INSTRUCTION
        STA NEXT+2              ;* SAVE IT
        LDA #$3F                ;* GET SWI
        STA 0,X
        CMP A 0,X               ;* CHECK IT
        BNE NOGOOD              ;* ABORT IF ERROR
;* NEXT, SEE IF A BRANCH OR JUMP IS INVOLVED
        LDA 1,S                 ;* GET OP CODE
        CMP A #$17              ;* IS IT LBSR?
        LBEQ MEDBR
        CMP A #$16              ;* IS IT LBRA?
        LBEQ MEDBR
        CMP A #$20
        BCS NOBR                ;* NO BRANCH
        CMP A #$30
        LBCS YESBR              ;* YES
NOBR    CMP A #$39              ;* CHECK FOR RTS
        LBEQ RTSIN              ;* YES
        CMP A #$3B
        BEQ NOGOOD              ;* DON'T DO RTI
        CMP A #$3F
        BEQ NOGOOD              ;* DITTO FOR SWI
        CMP A #$13
        BEQ NOGOOD              ;* DITTO FOR SYNC
        CMP A #$6E
        LBEQ JINDEX             ;* OK FOR INDEXED JUMPS
        CMP A #$AD
        LBEQ JINDEX             ;* DITTO
        CMP A #$7E
        LBEQ JEXT               ;* OK FOR EXTENDED JUMPS
        CMP A #$BD
        LBEQ JEXT               ;* DITTO
        CMP A #$8D
        BEQ SHORTB              ;* BSR IS A BRANCH TOO
        CMP A #$0E
        LBEQ JDIREC             ;* DIRECT JMP
        CMP A #$9D
        BEQ JDIREC              ;* DIRECT JSR
        CMP A #$3C
        BEQ NOGOOD              ;* WAI
        CMP A #$35
        BEQ PULL                ;* PULS
        CMP A #$37
        BNE GOUSER              ;* IF NOT PULU
PULL    LDB 1,Y                 ;* GET POSTBYTE
        BPL GOUSER              ;* IF NOT PULL PC
;* REFUSE TO DO SOME INSTRUCTIONS
NOGOOD  LDX #NOSTR
        JSR PDATA               ;* PRINT "NO!"
        LDA NEXT+2
        STA [NEXT]              ;* RESTORE NEXT INSTR ON ERROR
        JMP NXTCMD
NOSTR   FCC 'NO!'
        FCB 4
;* FINALLY GO TO USER PROGRAM
GOUSER  LDX #SSRETN             ;* REDIRECT SWI RETURN
        STX SWI
        LDA #$D0                ;* SET E,F,I FLAGS
        TFR A,CC
        LDS SP                  ;* GET USER STACK
        RTI                     ;* GO TO USER
        NOP
;* RETURN POINT FROM SINGLE STEP
SSRETN  LDX #BKRETN             ;* RESTORE BREAK ADDRESS
        STX SWI
        LDA NEXT+2              ;* RESTORE NEXT OP CODE
        STA [NEXT]
        LDA BRANCH              ;* CHECK BRANCH ADDRESS
        CMP A #$FF
        BEQ NONE
        LDA BRANCH+2            ;* RESTORE IT
        STA [BRANCH]
NONE    JMP BKRETN              ;* STORE STACK PTR AND PRINT REG
;*
;* HANDLE EFFECTIVE ADDRESS OF BRANCH
YESBR   TST 0,S                 ;* TEST PREFIX
        BEQ SHORTB              ;* RANCH
LONGBR  LDD 2,Y                 ;* GET OFFSET
        BEQ NOGOOD
        BRA GOTOFS
MEDBR   LDD 1,Y                 ;* GET OFFSET
        BEQ NOGOOD
        BRA GOTOFS
SHORTB  LDB 1,Y                 ;* GET OFFSET
        BEQ NOGOOD              ;* WATCH OUT
        SEX EXTEND              ;* SIGN OF B
GOTOFS  PSHS X                  ;* PUSH NEXT ADDRESS
        ADDD 0,S++              ;* ADD TO OFFSET
        TFR D,X                 ;* BACK INTO X
GOTADD  STX BRANCH              ;* SAVE ADDRESS
        LDA 0,X                 ;* GET INSTRUCTION
        STA BRANCH+2            ;* SAVE IT
        LDA #$3F
        STA 0,X                 ;* SUBSTITUTE SWI
        CMP A 0,X               ;* CHECK THAT IT WENT IN
        BEQ GOUSER              ;* GO TO USER IF OK
        BRA NOGOOD              ;* IF IT DIDN'T STORE PROPERLY
;*
;* HANDLE DIRECT JUMPS
JDIREC  LDX SP
        LDA 3,X                 ;* GET MSB FROM DP
        LDB 1,Y                 ;* GET LSB FROM INSTRUCTION
        TFR D,X
        BRA GOTADD
;*
;* HANDLE EXTENDED JUMP ADDRESS
JEXT    LDX 1,Y                 ;* GET EXTENDED JUMP ADDRESS
        BRA GOTADD              ;* GO TAKE CARE OF IT
;*
;* HANDLE RTS INSTRUCTION
RTSIN   LDX SP                  ;* GET USER STACK POINTER
        LDX 12,X                ;* GET RET ADDR FROM USER'S STACK
        BRA GOTADD              ;* AND TREAT IT AS A JUMP
;*
;* HANDLE INDEXED JUMP
JINDEX  LDB 1,Y                 ;* GET POSTBYTE
        BPL OKINDX              ;* ALLOW 4-BIT OFFSET
        CMP B #$9F              ;* PLAIN INDIRECT?
        BEQ INDIR               ;* YES
        AND B #$8F
        CMP B #$84              ;* LOOK FOR 0,RR
        LBNE NOGOOD             ;* DON'T ALLOW OTHERS
OKINDX  LDB 1,Y                 ;* POSTBYTE AGAIN
        AND B #$60              ;* GET REGISTER BITS
        CMP B #$60              ;* IS IT SP?
        BNE NOTSIN              ;* NO
        LDD SP                  ;* YES, GET IT
        ADDD #12                ;* RESET TO USER VAL
        STD BRANCH
        BRA GOTREG
NOTSIN  LDX SP                  ;* POINT TO CC ON STACK
NOTSI2  SUB B #$20
        BMI GETREG              ;* POINT TO CORRECT
        INX
        INX
        BRA NOTSI2
GETREG  LDX 4,X                 ;* GET REGISTER CONTENTS
        STX BRANCH
GOTREG  LDB 1,Y                 ;* GET POSTBYTE
        AND B #$8F              ;* CHECK FOR ZERO OFFSET
        CMP B #$84
        BEQ ZEROIN              ;* ON ZERO OFFSET
        LDB 1,Y                 ;* POSTBYTE AGAIN
        AND B #$10              ;* IS OFFSET POSITIVE?
        LBNE NOGOOD             ;* NO, NEGATIVE
        LDB 1,Y                 ;* POSTBYTE AGAIN
        AND B #$0F              ;* GET 4-BIT POSITIVE OFFSET
        ADD B BRANCH+1
        STB BRANCH+1            ;* ADD THEM
        BCC ZEROIN
        INC BRANCH              ;* ADD CARRY IF NEEDED
ZEROIN  LDB 1,Y                 ;* POSTBYTE AGAIN
        LDX BRANCH              ;* LOAD ADDRESS
        BIT B #$10              ;* LOOK AT INDIRECT BIT
        LBEQ GOTADD             ;* IF DIRECT
        LDX 0,X                 ;* GET EFFECTIVE ADDRESS
        LBRA GOTADD             ;* AND CONTINUE
;* PLAIN INDIRECT (POSTBYTE 9F)
INDIR   LDX 2,Y                 ;* GET ADDRESS
        LDX 0,X                 ;* GET INDIRECT ADDR
        LBRA GOTADD
 IFC &A,MY
;* PORT D STEERING ROUTINE FOR CENTRONICS S1
PORTD   CMP A #$7F              ;* IS IT RUBOUT?
        BCC PORTDX
        CMP A #$0D              ;* IS IT CR?
        BEQ PORTDX              ;* YES, SO IGNORE IT
        PSHS B
        LDA B #$15              ;* SET ACIA TO 8 DATA 1 STOP
        STA B ACIA0B
PORTDC  LDA B ACIA0B            ;* CHECK STATUS
        ASR B
        ASR B
        BCC PORTDC              ;* WAIT IF BUSY
        STA A ACIA0B+1          ;* THEN PRINT
        PULS B
PORTDX  RTS                     ;* AND RETURN
 ENDIF
;* FA COMMAND - SWITCH PORT 1 OUTPUT TO/FROM FASTYPE
SWFAST  LDX #FASTYP             ;* POINT TO FASTYP ROUTINE
SWTEST  CMPX DVECTR             ;* SEE IF IT'S ALREADY ON
        BEQ SWOFF               ;* GO SWITCH OFF IF ON
SWITCH  STX DVECTR              ;* OTHERWISE TURN IT ON
SWRTS   RTS                     ;* AND RETURN
SWOFF   LDX #SWRTS              ;* IF ON, GET POINTER TO RTS
        STX DVECTR              ;* AND PUT THAT IN TO TURN OFF
        RTS                     ;* AND RETURN
;* FASTYPIST OUTPUT ROUTINE
FASTYP  CMP A #$0D
        BEQ FPRINT              ;* CR IS OK TO PRINT
        CMP A #$20
        BCS FDONE               ;* SKIP OTHER CONTROL CHAR
FPRINT  PSHS X,B                ;* SAVE INDEX AND B
        LDB STATUS
        AND B #$BF              ;* TURN OFF PORT 1
        STB STATUS
        LDX #$1000              ;* SET UP DELAY COUNTER
FDELAY  DEX
        BNE FDELAY              ;* WAIT
        LBSR OUTCHM             ;* PRINT CHAR
        CMP A #$0D              ;* WAS IT A CR
        BNE FGETX               ;* NO, SO RESTORE IX & EXIT
        LBSR DELAY
        LBSR DELAY
FGETX   PULS B,X                ;* GET INDEX AND B
FDONE   RTS
;* 'MC' - MEMORY COMPARISON ROUTINE
COMPAR  LDX #P1MSG
        JSR PDATA               ;* PRINT 'PROGRAM 1: '
        JSR FROMTO              ;* GET PROGRAM 1 START AND END ADDR.
        JSR CRLF
        LDX #P2MSG
        JSR PDATA               ;* PRINT 'PROGRAM 2: FROM '
        JSR BADDR               ;* GET PROGRAM 2 START ADDR.
        TFR X,U
        JSR CRLF
        LDY #4                  ;* INITIALIZE LINE COUNTER
        LDX BEGA
MCLOOP  LDA 0,X                 ;* GET NEXT BYTE FROM PGM 1
        LDB 0,U                 ;* GET NEXT BYTE FROM PROGRAM 2
        CBA                     ;* COMPARE THE TWO
        BEQ INCR                ;* IF SAME, GO TO INCREMENT
;* DIFFERENT BYTES, SO PRINT
        LEAY -1,Y               ;* DECREMENT LINE COUNTER
        BNE MCCONT              ;* IF ROOM ON LINE, CONTINUE
        LDY #3                  ;* OTHERWISE RESET
        JSR CRLF
MCCONT  JSR OUTS
        JSR OUTS                ;* PRINT TWO SPACES
        PSHS X
        TFR S,X
        JSR OUT4HS              ;* PRINT ADDRESS 1
        LDX 0,S
        JSR OUT2HS              ;* PRINT DATA 1
        PSHS U
        TFR S,X
        JSR OUT4HS              ;* PRINT ADDRESS 2
        LDX 0,S
        JSR OUT2HS              ;* PRINT DATA 2
        PULS U
        PULS X
INCR    CMPX ENDA
        BEQ MCDONE              ;* GO TO MCDONE IF MCDONE
        LEAX 1,X                ;* INCREMENT ADDRESSES
        LEAU 1,U
        BRA MCLOOP
MCDONE  RTS                     ;*FINISH UP
P1MSG   FCC 'PROGRAM 1: '
        FCB 4
P2MSG   FCC 'PROGRAM 2: FROM '
        FCB 4
;* 'LR' COMMAND - PRINT LRA EQUIVALENCE
LRPRNT  LBSR CRLF
        LDX #LOGMSG             ;* PRINT "LOG ADDR ="
        LBSR PDATA
        LDA #'0
LRP1    LBSR OUTEEE             ;* PRINT 0 THRU F
        LBSR OUTS               ;* SPACE
        INC A
        CMP A #$3A              ;* AT 10?
        BNE LRP2                ;* NO
        ADD A #$7               ;* YES, CHGE 10 TO A
LRP2    CMP A #$47              ;* DONE?
        BNE LRP1                ;* NO
        LBSR CRLF               ;* YES, START NEXT LINE
        LDX #REAMSG             ;* PRINT "REAL ADDR ="
        LBSR PDATA
        LDB #0                  ;*COUNTER AND POINTER
        LDX #LRATAB
LRP3    LDA B,X                 ;* GET LRA ENTRY
        COM A
        LBSR OUTHR              ;* PRINT RIGHT DIGIT
        LBSR OUTS
        INC B
        CMP B #$10              ;* DONE?
        BNE LRP3                ;* NO
        LBSR CRLF               ;* YES
        LDX #MEMMSG             ;* PRINT "RAM ="
        LBSR PDATA
        LDX #$0000
        LDB #14
LRP4    LBSR TSTRAM             ;* CHECK IF RAM
        BCC LRP5                ;* IF NOT
        LDA #'Y                 ;* PRINT Y IF RAM
        BRA LRP6
LRP5    LDA #$20                ;* PRINT SPACE IF NOT RAM
LRP6    LBSR OUTEEE
        LBSR OUTS
        LEAX $1000,X            ;* GO UP 4K
        DEC B
        BNE LRP4                ;* IF NOT DONE
        LBSR CRLF               ;* END WITH CRLF
        RTS
LOGMSG  FCC 'LOG. ADDR = '
        FCB 4
REAMSG  FCC 'REAL ADDR = '
        FCB 4
MEMMSG  FCC '      RAM = '
        FCB 4
;* 'AD' COMMAND - FORMATTED ASCII DUMP
ASCDMP  JSR FROMTO              ;* GET ADDRESSES
        LDD BEGA                ;* GET STARTING ADDRESS
        AND B #$F0              ;* ROUND DOWN TO NEXT 0
        TFR D,Y
ASC     JSR CRLF
        PSHS Y                  ;* POINT TO STRT ADDR
        TFR S,X
        JSR OUT4HS              ;* PRINT IT
        JSR OUTS                ;* PRINT EXTRA SPACE
        PULS Y
        LDB #16                 ;* SET COUNTER TO 16
ASC1    LDA 0,Y                 ;* GET BYTE
        LEAY -1,Y
        CMPY ENDA               ;* LAST ADDRESS?
        BNE ASC2                ;* NO
        RTS                     ;* YES
ASC2    LEAY 2,Y
        AND A #$7F              ;* MASK OFF PARITY BIT
        CMP A #$7E
        BCC ASC3                ;* SUBSTITUTE . FOR 7E AND 7F
        CMP A #$20
        BCC ASC4                ;* PRINT SPACE AND ABOVE
ASC3    LDA #'.                 ;*SUBSTITUTE . FOR ALL ELSE
ASC4    JSR OUTEEE              ;* PRINT IT
        DEC B                   ;* DECREMENT COUNTER
        BNE ASC1                ;* CONTINUE SAME LINE IF NOT DONE
        BRA ASC                 ;* OTHERWISE START NEW LINE
;* 'HA' COMMAND - COMBINED HEX AND ASCII DUMP
HEXASC  JSR FROMTO              ;* GET ADDRESSES
        LDD BEGA                ;* GET STARTING ADDRESS
        AND B #$F0              ;* ROUND DOWN TO NEXT 0
        TFR D,Y
HA1     JSR CRLF
        PSHS Y                  ;* POINT TO STR ADDR
        TFR S,X
        JSR OUT4HS              ;* PRINT IT
        JSR OUTS                ;* PRINT EXTRA SPACE
        PULS Y
        LDB #16                 ;* SET COUNTER TO 16
        TFR Y,X                 ;* POINT TO NEXT BYTE
HA2     JSR OUT2HS              ;* PRINT NEXT BYTE
        DEC B                   ;* DECREMENT COUNTER
        BNE HA2                 ;* CONTINUE LINE IF NOT FINISHED
        JSR OUTS                ;* PRINT ANOTHER SPACE
        LDB #16                 ;* SET COUNTER TO 16 AGAIN
HA3     LDA 0,Y                 ;* GET BYTE
        AND A #$7F              ;* MASK OFF PARITY BIT
        CMP A #$7E
        BCC HA4                 ;* SUBSTITUTE . FOR 7E AND 7F
        CMP A #$20
        BCC HA5                 ;* PRINT SPACE AND ABOVE
HA4     LDA #'. SUBSTITUTE . FOR ALL ELSE
HA5     JSR OUTEEE              ;* PRINT IT
        CMPY ENDA               ;* LAST ADDRESS?
        BNE HA6                 ;* NO;*
        RTS                     ;* YES
HA6     LEAY 1,Y
        DEC B                   ;* DECREMENT COUNTER
        BNE HA3                 ;* CONTINUE SAME LINE IF NOT DONE
        BRA HA1                 ;* OTHERWISE START NEW LINE
;* 'HE' - HELP COMMAND
HELP    JSR CRLF
        LDX #COMTAB
HLOOP1  LDA B #20               ;* SET COUNTER
HLOOP2  LDA A 0,X               ;* GET COMMAND
        JSR OUTEEE
        LDA 1,X
        JSR OUTEEE
        JSR OUTS
        LEAX 4,X                ;* GO TO NEXT
        CMPX #TABEND            ;* DONE?
        BNE HLOOP3              ;* NO
        RTS                     ;* YES
HLOOP3  DEC B                   ;* COUNTER
        BNE HLOOP2              ;* NO CR
        JSR CRLF                ;* CR
        BRA HLOOP1
;* LRACVT - CONVERT LOG TO PHYS ADDRESS
LRACVT  PSHS D,X
        TFR X,D                 ;* LOG ADDR
        LSRA
        LSRA
        LSRA
        LSRA                    ;* GET MSB
        LDX #LRATAB             ;* POINT TO TABLE
        LDB A,X                 ;* GET LRA ENTRY
        LSRB
        LSRB
        LSRB
        LSRB                    ;* GET FIELD NO
        STB 0,S                 ;* SAVE INTO STACK AS A
        LDB A,X                 ;* AGAIN LRA ENTRY
        ASLB
        ASLB
        ASLB
        ASLB                    ;* MOVE LEFT
        LDA 2,S                 ;* LOG ADDR HI BYTE
        ORA #$F0                ;* DELETE OLD MSB
        STB 2,S                 ;* TEMP SAVE
        EOR A 2,S               ;* COMBINE TWO MS DIGITS
        STA 2,S                 ;* SAVE AS MSB OF X
        PULS D,X,PC             ;* RESTORE AND RETURN
;* TSTRAM ROUTINE. TESTS FOR RAM IN 4K SEG
;* POINTED TO BY X, AND RETURNS C=1 IF
;* IT FINDS RAM.
TSTRAM  PSHS Y,U
        LDY #$A55A              ;* TEST BYTES
;* TEST 1ST 2K
        LDU 0,X                 ;* SAVE PREV
        STY 0,X                 ;* SAVE TEST
        LDY 0,X                 ;* READ IT BACK
        CMPY #$A55A             ;* CHECK IT
        BNE NOTRAM              ;* IF NO MATCH
        STU 0,X                 ;* RESTORE PREV
;* CHECK 2ND 2K
        LDU $0800,X
        STY $0800,X
        LDY $0800,X
        CMPY #$A55A
        BNE NOTRAM
        STU $0800,X
YESRAM  SEC                     ;* SET CARRY
        PULS Y,U,PC             ;* AND RETURN
NOTRAM  CLC                     ;* CLEAR CARRY
        PULS Y,U,PC             ;* AND RETURN
;* RESTART ROUTINE
RESTRT  CLR PTRINZ              ;* ERASE RESTRT FLAG
        LBRA HUMBUG             ;* AND START OVER
;* SWI3 TEST ROUTINE. USE SWI3 VECTOR IN RAM
;* IF SUPERVISOR CALL LOCATIONS STILL AT FFFF;
;* OTHERWISE USE THE NEXT BYTE TO VECTOR THROUGH
;* SUPERVISOR CALL TABLE TO ROUTINE
TSTSW3  TFR S,U                 ;* COPY SP INTO U
        LDX 10,U                ;* PC POINTS PAST SWI3
        LDB 0,X+                ;* NEXT BYTE INTO B, INC PC
        STX 10,U                ;* SAVE INCREMENTED PC
        CLR A
        ASLB                    ;* MULT BYTE (IN D) BY 2
        ROL A
        LDX SUPVEC              ;* GET SUP CALL VECTOR
        CMPX #$FFFF             ;* IS IT STILL FFFF?
        BEQ SWEXIT              ;* YES, EXIT VIA SWI3 VCTR
        LEAX D,X                ;* ADD SUPVEC TO OFFSET IN D
        CMPX SUPEND             ;* COMPARE WITH END
        BCC SWEXIT              ;* NORM EXIT IF TOO BIG
        PSHS X                  ;* PUT SVC ADDR ON STACK
        PULU CC,A,B,DP,X        ;* RESTORE REGISTERS
        JMP [0,S++]             ;* PULL SVC ADDR, FIX SP, JMP
SWEXIT  PULU CC,A,B,DP,X        ;* RESTORE REGISTERS
        LDU 2,U
        JMP [SWI3]              ;* AND USE NORMAL SWI3 VECTOR

;* RESET AND INTERRUPT VECTORS
RESVEC  JMP [RESRVD]
SWI2V   JMP [SWI2]
FIRQV   JMP [FIRQ]
IRQV    JMP [IRQ]
SWIV    JMP [SWI]
        FCC '(C) 1981 BY P. STARK'

;* HUMBUG-09 MAIN ENTRY ON POWER UP / RESET

        ORG    ROM+$0F00

;* FIRST, INITIALIZE DAT FOR 1:1 RAM CORRESPONDENCE

HUMBUG  LDX    #DAT             ;*      POINT TO DAT
        LDY    #LRATAB          ;*   POINT TO LRA TABLE
        LDA    #$02
        STA    $D,X             ;*      SET DAT SO LOG $D SAME AS PHYSICAL
        LDA    #$0F

SETDAT  STA    0,X+             ;*      PUT INTO DAT
        STA    0,Y+             ;*      AND MAYBE INTO LRA TABLE TOO
        DECA
        CMPA   #$01             ;*      IS I/O NEXT?
        BNE    SETDAT           ;*    NO, SO CONTINUE

;* NEXT INIT DAT FOR I/O AND ROM

        IFNC   &C,$8000
        LDD    #$F1F0           ;*vIF I/O IS AT PHYSICAL $E000
        ENDIF
        IFC    &C,$8000
        LDD    #$F7F0           ;* IF I/O IS AT PHYSICAL $8000
        ENDIF

        STD    0,X              ;* SET DAT FOR I/O AND ROM
        STD    0,Y              ;* AND MAYBE ALSO INTO LRA TABLE
        TFR    A,CC             ;* SET E,F,I FLAGS

;* NOW CHECK IF THERE'S A DAT

        STA    1,X              ;* TEMP MAKE LOG $F = LOG $E
        LDU    $E004
        CMPU   $F004            ;* CHECK IF $E AND $F ARE SAME
        BEQ    YESDAT           ;* THEN THERE IS A DAT
        STB    1,X              ;* IF NO DAT (OR COMP EX DAT) RESTORE DAT
        JMP    DONE             ;* SKIP DAT INITIALIZATION
YESDAT  STB    1,X              ;* RESTORE DAT FOR LOG $F = REAL $F

;* THERE IS A DAT.
;* NOW WE HAVE ROM AT $F000, I/O AT $E000,
;* AND 1:1 CORRESPONDENCE ON ALL LOWER MEMORY.
;* PROCEED TO FIND THE TOP 8K OF MEMORY
;* AND MAKE IT INTO $C000 AND $D000
;* CURRENTLY X->D IN DAT, Y->E IN LRATAB

        LEAS   -1,X             ;* POINT S TO D IN DAT
        LDB    #$01             ;* COMPLEMENTED $D (-1)
        LDX    #2               ;* USE X AS A COUNTER
        LDU    #$E000           ;* REALLY START CHECKING AT $D000
CLOOP   LEAU   -$1000,U         ;* GO DOWN 4K
        INCB                    ;* GO TO NEXT COMPLEMENTED DAT ENTRY
        COM    0,U              ;* COMPLEMENT MEMORY, IF ANY
        LDA    0,U              ;* GET COMPLEMENTED BYTE FROM RAM?
        COM    0,U              ;* CHANGE MEMORY BACK TO ORIGINAL
        ADDA   0,U              ;* ADD IT TO WHAT'S IN A, SHOULD GIVE $FF
        INCA                    ;* CHANGE POSSIBLE $FF TO 0
        BEQ    FDDRAM           ;* WE FOUND A RAM LOCATION IF 0
        BRA    CLOOP            ;* AND REPEAT; BOMB OUT IF <8K RAM EXISTS!
FDDRAM  STB    0,S              ;* PUT DAT ENTRY INTO DAT
        LEAS   -1,S             ;* AND MOVE STACK POINTER TO POINT TO NEXT
        STB    0,-Y             ;* AND ALSO INTO LRA
        DEX                     ;* DECREMENT COUNTER
        BNE    CLOOP            ;* REPEAT ONE MORE TIME FOR $C000 MEMORY
CLRRST  CLR    0,-Y             ;* CLEAR BEGINNING OF LRA
        CMPY   #LRATAB          ;* CHECK IF FINISHED
        BHS    CLRRST           ;* NO, CLEAR THE REST

        IFNC   &C,$8000
        LDD    #$F1F0           ;* IF I/O IS AT PHYSICAL $E000
        ENDIF
        IFC    &C,$8000
        LDD    #$F7F0           ;* IF I/O IS AT PHYSICAL $8000
        ENDIF

        STD    15,Y             ;* SET LRA TO KNOW ABOUT I/O AND ROM;*
        LDS    #MSTACK          ;* SET STACK POINTER

;* NOW MAKE ALL REMAINING RAM CONTIGUOUS
;* BEGINNING AT $0000

        PSHS   U                ;* SAVE $C AS HIGHEST (GETS LEFT ON UNTIL LDS)
        LDX    #0000            ;* START AT 0000
        LDY    #LRATAB          ;* POINT TO LRA TABLE
        LDB    #$0F             ;* COMPLEMENT OF $0
FLOOP   CMPX   0,S              ;* POINTING TO $C000 BLOCK?
        BEQ    FFOUND           ;* YES, FINISHED
        JSR    TSTRAM           ;* CHECK IF X POINTS TO RAM
        BCC    NORAM            ;* RETURNED C=0 ON NO RAM
        STB    0,Y+             ;* PUT ENTRY INTO LRA TABLE TO THIS RAM
NORAM   DECB                    ;* POINTER GOES UP BY ONE
        LEAX   $1000,X          ;* AND X UP BY 4K
        BRA    FLOOP            ;* AND REPEAT AGAIN

;* AT THIS POINT, Y POINTS TO NEXT (POSSIBLY EMPTY) LOCATION
;* IN LRA. FILL IN UNASSIGNED 4K SLOTS WITH NONEXISTENT MEMORY

FFOUND  CMPY   #LRATAB+$C       ;* FINISHED ASSIGNING ALL?
        BEQ    DATMOV           ;* YES, MOVE LRA INTO DAT
        LDB    #$0F             ;* COMPLEMENT OF $0
OLOOP   LDX    #LRATAB          ;* POINT TO LRA TABLE
ILOOP   CMPX   #LRATAB+$E       ;* AT END OF TABLE?
        BEQ    NTHERE           ;* THIS ENTRY IS NOT IN TABLE
        CMPB   0,X+             ;* IS B IN TABLE?
        BNE    ILOOP            ;* NO, LOOK AT NEXT PLACE IN TABLE
        DECB                    ;* YES, TRY THE NEXT LOWER ENTRY
        BRA    OLOOP            ;* LOOK THROUGH ENTIRE TABLE AGAIN
;* THIS ENTRY IS NOT IN TABLE, SO PUT IT IN
NTHERE  STB    0,Y+             ;* PUT INTO NEXT ENTRY IN TABLE
        BRA    FFOUND           ;* AND REPEAT IF NOT FINISHED

;* FINALLY, COPY LRA TABLE INTO DAT

DATMOV  LDX    #LRATAB          ;* POINT TO LRA TABLE
        LDY    #DAT             ;* AND TO DAT
        LDB    #12              ;* MOVE 12 BYTES (ALL ELSE IS ALREADY DONE)
DMLOOP  LDA    0,X+             ;* GET A BYTE
        STA    0,Y+             ;* PUT IT THERE
        DECB                    ;* DECREMENT COUNTER
        BNE    DMLOOP           ;* REPEAT UNTIL FINISHED

;* NOW CLEAR DP, AND INITIALIZE INTERRUPT VECTORS

DONE    LDS    #MSTACK          ;* INIT STACK POINTER
        CLRA
        TFR    A,DP             ;* INIT DP REG TO 0
        LDX    #RTI             ;* POINT TO AN RTI INSTRUCTION
        LDU    #RESRVD          ;* POINT TO RAM VECTORS
        LDB    #5               ;* FIVE VECTORS
RTILOO  STX    0,U++            ;* RES,SWI3,SWI2,FIRQ, AND IRQ
        DECB                    ;* DECREMENT COUNTER
        BNE    RTILOO
        LDX    #BKRETN
        STX    0,U++            ;* POINT SWI TO BREAKPOINT RETURN
        LDX    #$FFFF
        STX    0,U++            ;* ERASE SUPERVISOR CALL VECTOR
        STX    0,U++            ;* ERASE SUP CALL END

;* NOW CHECK WHETHER THIS IS INITIAL POWER UP

CHKINI  LDX    PTRINZ           ;* IS IT INITIALIZED?
        CMPX   #$1115
        LBEQ   START            ;* IF YES, JUST RESTART HUMBUG
        LDX    #BKTAB           ;* NO, ERASE BRK TABLE AND RESTART
        LDA    #$FF
        LDB    #12

;* ERASE BREAKPOINT TABLE

BKERAS  STA    0,X+             ;* ERASE BREAKPOINT TABLE
        DECB
        BNE    BKERAS           ;* REPEAT IF NOT FINISHED
        JMP    START            ;* AND GO TO COLDSTART

;* RESET AND INTERRUPT VECTORS

        ORG    ROM+$0FF0
        FDB    RESVEC           ;* RESERVED VECTOR
        FDB    TSTSW3           ;* GO TEST SWI3 FOR SUP CALL
        FDB    SWI2V            ;* SWI2 VECTOR
        FDB    FIRQV            ;* FIRQ VECTOR
        FDB    IRQV             ;* IRQ VECTOR
        FDB    SWIV             ;* SOFTWARE INTERRUPT
        FDB    NMIRET           ;* NMI VECTOR DIRECTLY TO NMI RETURN
        FDB    HUMBUG           ;* RESTART FOR RESET OR POWERUP

        END

; =[ Fini ]=====================================================================
