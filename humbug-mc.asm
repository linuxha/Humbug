; ===========================================================================
; This is the disassembly of the Peter Stark's Humbug+ monitor for the Tandy
; MC-10. While this was disassembled with the 6803 CPU option it does appear
; that this is basically the 6800 version of Humbug.
; ===========================================================================
; Most of Stark's Humbug ROM for the 6800 is described in great detail in 2
; Microcomputing articles: Thoughts13.pdf (Aug 80) and Thoughts14.pdf (Sept 80)
; Remeber that the MC10 Humbbug+ was loaded from Cassette and loading into
; RAM (@$7500) so this version needs to be adjust accordingly. Also the MC10
; has a 6801 process whis is a super set of the 6800 (a subset of the 6809).
; Some instructions will need to be adjusted to run properly on the 6800.
; Fortunately the 2 articles seem to have that covered. The articles mention
; an E0ROM (Exxx), E4ROM (E4xx) and FCROM (FCxx) 2708 EPROMS.
;
; IRQ - FFED
; SWI - FFED
; NMI - FFF2
; RST - FC00
;
; ===========================================================================
;
; |--------+------+--------------------------------------------------------+------------+------|
; | Name   | Addr | Desc                                                   | Notes      | Addr |
; |--------+------+--------------------------------------------------------+------------+------|
; | BADDR  | E047 | Input four hex digits                                  | Build Addr |
; | BYTE   | E055 | Input two hex digits in A                              |            |
; | OUTHL  | E067 | Output left BCD digit in A                             |            |
; | OUTHR  | E06B | Output right BCD digit in A                            |            |
; | OUTCH  | E075 | Points to OUTEEE                                       |            |
; | INCH   | E078 | Points to INEEE                                        |            |
; | PDATA1 | E07E | Print a text string pointed to be X                    |            |
; | INHEX  | E0AA | Input a hex digit into A                               |            |
; | OUT2CH | E0BF | Output two hex digits pointed to by X                  |            |
; | OUT4HS | E0C8 | Output four hex digits pointed to by X followed by SPC |            |
; | OUT2HS | E0CA | Output two hex digits pointed to by X followed by SPC  |            |
; | OUTS   | E0CC | Print a SPC                                            |            | FD6D |
; | START  | E0D0 | Start MIKBUG                                           | Cold start |
; | CONTRL | E0E3 | Restart MIKBUG                                         | Warm start |
; | INEEE  | E1AC | Input a 7 bit character from keyboard                  |            |
; | OUTEEE | E1D1 | Output a character to terminal                         |            |
; | INCH8  | E1F6 | Input an 8 bit character from keyboard                 |            |
; |--------+------+--------------------------------------------------------+------------|
;
; Aticle is messed up, even though the code seems to be everything it is
; very much partial. It's misisng all the 
;
; MIKBUG, $E100-$E2C3 - 451 bytes
;         $E000-$E184 - 388 bytes
; egrep 'BADDR|BYTE|OUTHL|OUTHR|OUTCH|INCH|PDATA1|INHEX|OUT2CH|OUT4HS|OUT2HS|OUTS|START|CONTRL|INEEE|OUTEEE|INCH8' mikbug.lst
; @E100
;  115/E144 : 8D 09                      BADDR	BSR	BYTE	;* READ 2 FRAMES
;  123/E14F : 8D 4F                      BYTE	BSR	INHEX	;* GET HEX CHAR
;  137/E15F : 44                         OUTHL	LSRA		;* OUT HEX LEFT BCD DIGIT
;  141/E163 : 84 0F                      OUTHR	ANDA	#$F	;* OUT HEX RIGHT BCD DIGIT
;  148/E16D : 7E E2 B6                   OUTCH	JMP	OUTEEE
;  149/E170 : 7E E2 91                   INCH	JMP	INEEE
;  154/E176 : A6 00                      PDATA1 LDAA    ,X
;  179/E1A0 : 8D CE                      INHEX	BSR	INCH
;  199/E1BE : 8D F5                      OUT4HS	BSR	OUT2H	;* OUTPUT 4 HEX CHAR + SPACE
;  200/E1C0 : 8D F3                      OUT2HS	BSR	OUT2H	;* OUTPUT 2 HEX CHAR + SPACE
;  203/E1C2 : 86 20                      OUTS	LDAA	#$20	;* SPACE
;  207/E1C6 : =$E1C6                     START	EQU	*
;  221/E1D8 : 8E 00 C2                   CONTRL	LDS	#STACK	;* SET CONTRL STACK POINTER
;  331/E291 :                            INEEE
;  347/E2B6 : 36                         OUTEEE	PSH	A
;
; @E000
;  115/E047 : 8D 0C                      BADDR	BSR	BYTE	;* READ 2 FRAMES
;  123/E055 : 8D 53                      BYTE	BSR	INHEX	;* GET HEX CHAR
;  137/E067 : 44                         OUTHL	LSRA		;* OUT HEX LEFT BCD DIGIT
;  141/E06B : 84 0F                      OUTHR	ANDA	#$F	;* OUT HEX RIGHT BCD DIGIT
;  148/E075 : 7E E1 A8                   OUTCH	JMP	OUTEEE
;  149/E078 : 7E E1 94                   INCH	JMP	INEEE
;  154/E07E : A6 00                      PDATA1	LDAA	,X
;  179/E0AA : 8D CC                      INHEX	BSR	INCH
;  199/E0C8 : 8D F5                      OUT4HS	BSR	OUT2H	;* OUTPUT 4 HEX CHAR + SPACE
;  200/E0CA : 8D F3                      OUT2HS	BSR	OUT2H	;* OUTPUT 2 HEX CHAR + SPACE
;  203/E0CC : 86 20                      OUTS	LDAA	#$20	;* SPACE
;  207/E0D0 : =$E0D0                     START	EQU	*
;  221/E0E3 : 8E 1F 42                   CONTRL	LDS	#STACK	;* SET CONTRL STACK POINTER
; *337/E194 :                            INEEE
; *353/E1A8 : 36                         OUTEEE	PSH	A
;
;
;    |-------+------+-----------+---+-------------------|
;    | Begin | End  | Hi-Nibble |   | Notes             |
;    |-------+------+-----------+---+-------------------|
;    | 0000  | 00FF |           |   | Immediate page    |
;    | 0100  | DFFF |           |   | User/Prog Ram     |
;    |       |      |           |   | OS RAM ???        |
;    |       |      |           |   |                   |
;    | E000  | E0FF |           |   | 256 bytes for I/O |
;    | E100  | FFFF |           |   | 2764 (8K) ROM     |
;    |       |      |           |   |                   |
;    |       |      |           |   |                   |
;    |       |      |           |   |                   |
;    | FFF8  | FFFF |           |   | Interrupt vectors |
;    |-------+------+-----------+---+-------------------|
;
;    |--------+------+-------|
;    | Humbug | Addr | Notes |
;    |--------+------+-------|
;    | IRQ    | FFED |       |
;    | SWI    | FFED |       |
;    | NMI    | FFF2 |       |
;    | RESET  | FC00 | RESET |
;    |--------+------+-------|
;
;    [SR]     <- [[SP] + 1],
;    [B]      <- [[SP] + 2],
;    [A]      <- [[SP] + 3],
;    [X(HI)]  <- [[SP] + 4],
;    [X(LO)]  <- [[SP] + 5],
;    [PC(HI)] <- [[SP] + 6],
;    [PC(LO)] <- [[SP] + 7],
;    [SP]     <- [SP] + 7
;
; ===========================================================================
;
;	Disassembled by:
;		DASMx object code disassembler
;		(c) Copyright 1996-2003   Conquest Consultants
;		Version 1.40 (Oct 18 2003)
;
;	File:		hb7500c.bin
;
;	Size:		2677 bytes
;	Checksum:	5886
;	CRC-32:		D130ACFC
;
;	Date:		Sun Feb 17 00:12:51 2019
;
;	CPU:		Motorola 6803 (6801/6803 family)
	MACEXP  off
;       CPU     6301            ; That's what asl has
        CPU     6800            ; That's what asl has
;
;
; ===========================================================================
        ;;
	macexp  on
        include "motorola.inc"          ; Macros for things like fcc,db, etc.
        include "humbug.inc"            ; This may need a lot of clean up
        include "MC6800.inc"
        ;;
        ;; $4000 - $41FF - Video RAM
        ;; $4200 - $4334 - OS Variables
        ;; $4235 - End of RAM - Program RAM
        ;; $4000 - $7FFF - 4K + 16K
        ;; $7500 - $7FFF - Humbug+ @7500
        ;;
        IFNDEF  NOCLEAR
CLS     EQU     $0C
        ELSE
CLS     EQU     $20
        ENDIF
        
; =[ MIKBUG RAM ]============================================================
        ;;*
        ;;* MIKBUG alsos uses the 128 byte scratchpag RAM @ $A000
        ORG     $A000           ;*

IRQ     RMB     2               ;* A000
BEGA    RMB     2               ;* A002
ENDA    RMB     2               ;* A004
NMI     RMB     2               ;* A006
SP      RMB     2               ;* A008
PORADD  RMB     2               ;* A00A
PORECH  RMB     1               ;* A00C
XHI	RMB     1               ;* A00D
XLO     RMB     1               ;* A00E
CKSUM   RMB     3               ;* A00F
;
SWIJMP  RMB     2               ;* A012
        RMB     52              ;* 0x36
PC      RMB     2               ;* A048
        RMB     53              ;* 
USTACK	RMB     1               ;* A07F

; $A07F - 6810 128 bytes

; =[ Humbug RAM ]============================================================
        ORG     $D000           ;*
; D000
P0STAT  RMB     1               ;* Port 0
; D001
P1STAT  RMB     1               ;* Port 1
; D002
VSTAT   RMB     1               ;*
; D003
BSTAT   RMB     1               ;* BSTAT? DSTAT?
; D004
PASTAT  RMB     1               ;* Pause flag
; D005
INEEXR  RMB     2               ;* In X Register
; D007
OUTEXR  RMB     2               ;* Out X Register
; D009
RETADD  RMB     2               ;*
; D00B
PTRINC  RMB     1               ;*
; D00C
KBDINZ  RMB     1               ;*
; D00D
PAUCTR  RMB     1               ;* Pause control
        RMB     18
; D020
USAVEX	RMB     2               ;* USAVEX was Save X
; D022
WHAT	RMB     3               ;*
; D025
FINDNO  RMB     3               ;*
; D028
POWUP   RMB     4               ;*
; D02C
SAVEX   RMB     2               ;* Hmm, 2nd one
; D02E
USERPC  RMB     2               ;*
; D030
NEXT    RMB     3               ;*
; D033
BRANCH	RMB     3               ;*
; D036
BKTAB   RMB     12              ;* Breakpoint able (4 bp - 3 bytes each)
; D042
NEWLOC  RMB     2               ;*
; D044
INSTR   RMB     2               ;*
; D046
COUNT	RMB	1               ;*
        RMB     56              ;*
; D07F
STACK   RMB     1               ;* Top of RAM (6810 @ $D000)


; ===========================================================================
        ;;* E0ROM Entry Vectors
        org     $E000
CINITV  jmp     CINIT           ;* Cold start initialization
;
; Not empty space (yikes)
;
        ORG     $E1AC           ;* Vector to FCROM
        ;;
CINITV0 jmp     CINIT           ;* E0ROM Cold start initialization
INIEEEV jmp     INEEE           ;* Vector to FCROM
        ;;* CINIT - Cold start initialization
CINIT   nop                     ;* None required for this ROM
        ;;* See if other ROMS require initialization
        ldaa    $E400           ;* Check E4ROM
        cmpa    #JMPINST        ;* Is there a jump?
        beq     CMORE4
        rts                     ;* No, return to FCROM
;;;
CMORE4  jmp     $E400           ;* Yes, Go initialize
CMORE8  jmp     $E800           ;* Yes, Go initialize second ROM

; ===========================================================================
        ;;* $E094 14 pg 179 R
COMNDV  jmp     COMAND          ;* Command Entry point
COMAND  psha                    ;* Save first character
        ldx     #COMTAB-4       ;* Set addr of command table
LOOKUP  inx
        inx
        inx
        inx
        cpx     #TABEND         ;* End of tablet
        bne     COMEND          ;* Yes
        cmpa    0,X             ;* No, check first character
        bne     LOOKUP          ;* Wrong
        cmpb    1,X             ;* Check second character
        bne     LOOKUP          ;* Wrong, skip to next
        ldx     2,X             ;* Get address if ok
        pula                    ;* Restore stack
        jsr     OUTS            ;* print a space
        jmp     0,X             ;* Jump to appropriate command routine
;;;* Command not found; See if other ROMs have commands
COMEND  ldaa    #$E406          ;* Check for next ROM
        cmpa    #JMPINST        ;* Is there a jump
        bne     COMMD4          ;*
        ldaa    $E806           ;* Check the ROM after that
        beq     COMMD8          ;*
        pula                    ;* No more ROMs; fix up stack
        rts                     ;* and return from FCROM

COMMD4  pula                    ;* Next ROM exists; Restore first character
        jmp     $E406           ;* Go to it
COMMD8  pula                    ;* Second ROM exists; Restore first character
        jmp     $E806           ;* Go to it
;
;;;* Command Table
;
COMTAB  FCC     'LO'            ;* Load MIKBUG tape
        FDB     LOAD            ;*
        FCC     'PU'            ;* Punch MIKBUG tape
        FDB     PUNCH           ;*
        FCC     'FB'            ;* Flex boot
        FDB     FLBOOT          ;*
        FCC     'EN'            ;* End of tape formatting
        FDB     PNCHS9          ;*
        FCC     'GO'            ;* Go to user program via A048/A049
        FDB     GOTO            ;*
        FCC     'CL'            ;* Clear screen
        FDB     CLEAR           ;*
        FCC     'FI'            ;* Find bytes command
        FDB     FIND            ;*
        FCC     'HD'            ;* Hex Dump routine
        FDB     HEXDMP          ;*
        FCC     'FM'            ;* Fill memory
        FDB     FILL            ;*
        FCC     'PB'            ;* Percom Disk DOS+PLUS
        FDB     MDOSPL          ;*
        FCC     'CS'            ;* Two-byte checksum
        FDB     SUM             ;*
        FCC     'MT'            ;* Memory Test
        FDB     ROBIT           ;*
        FCC     'PC'            ;* Print A048/A049
        FDB     PRNT40          ;* ($E276)
TABEND  EQU     *

; ===========================================================================
        ;;*
; ===========================================================================
        ;;* $E009 - 14 Pg 181R
        ORG     $E009           ;*
FROMTOV jmp     FROMTO          ;* From-To subroutine Entry

        org     $E270           ;*
;;;* FROMTO subroutine - Init BEGA and ENDA addesses
FROMTO  ldx     #FROMST         ;*
        jsr     PDATA           ;* Print "FROM "
        jsr     INEEE           ;* Get character
        cmpa    #$0D            ;* Is it a CR?
        bne     GETFI           ;* Continue if not
        jmp     CRLF            ;* On CR, do CRLF and return
SETFT   suba    #$30            ;* Continue ... check for digit
        bmi     SONOTS          ;* Not hex
        cmpa    #$09            ;*
        ble     GOTONE          ;*
        cmpa    #$11            ;*
        bmi     SONOTS          ;* Not hex
        cmpa    #$16            ;*
        bgt     SONOTS          ;* Not Hex
        suba    #$07            ;* Convert A-F to number
GOTONE  asla                    ;* Got first digit
        asla                    ;*
        asla                    ;*
        asla                    ;*
        tab                     ;* Temp save it
        jsr     INHEX           ;* Get second digit
        aba                     ;* combine them
        staa    BEGA            ;* Store left two digits $(A002)
        jsr     BYTE            ;* Get next two
        staa    BEGA+1          ;* Store right two as from address
        ldx     STOSTR          ;* ($E04E)
        jsr     PDATA           ;* Print "TO "
        jsr     BADDR           ;* Get to address
        stx     ENDA            ;* Store it ($A004)
        jmp     OUTS            ;*
SONOTS  ins                     ;* Invalid digit; Increment SP to bypass
        ins                     ;* ... the calling routine and return one level
        rts                     ;* ... above (to hotstart)

; ===========================================================================
        ;;* $E0D3 - 14 Pg 181R
        ORG     $E0D3
HEXDMP  jsr     FROMTO          ;*
        ldx     BEGA            ;* Save starting address
        stx     USAVEX          ;* Save duplicate
        bra     HEXCON          ;* And skip over next vector
;;;* Free to E0E2 (5)
        fcc     '\0\0\0\0\0'
;;;*    ORG     $E0E3
EOE3    jmp     WARMST          ;* Vector to FCROM
;;;* Continuation of hex dump
HEXCON  ldaa    USAVEX+1        ;*
        anda    #$F0            ;* Round down to next 0
        staa    USAVEX+1        ;*
HEX     jsr     CRLF            ;*
        ldx     #USAVEX         ;* Get location of starting ADDR
        jsr     OUT4HS          ;* Print it
        jsr     OUTS            ;* Extra space
        ldab    #16             ;* Set counter to 16 ($10)
        ldx     USAVEX          ;*
HEX1    jsr     OUT2HS          ;* Print next byte
        dex                     ;* backup pointer
        cpx     ENDA            ;* last address
        bne     HEX2            ;* continue if not
        rts                     ;* Otherwise end

HEX2    inx                     ;* Restore pointer
        decb                    ;* decremtn counter
        bne     HEX1            ;* continue line if not finished
        stx     USAVEX          ;* Save current pointer
        bra     HEX             ;* get ready for next line ($E110)

; ===========================================================================
        ;;* $E61E 14 Pg 187L
        ORG     $E61E
;;;* 'BR' Command - Set/Reset up yo four breakpoints
BREAK   bsr     BKNUM           ;* Set number of Desired breakpoint
        stx     SAVEX           ;* Save Address
        bsr     BERASE          ;* Go Erase old one
        ldx     #NEWSTR         ;* Print "Enter new address: "
        jsr     PDATA           ;*
        jsr     BADDR           ;* Get Address
        stx     NEWLOC          ;*
        ldab    0,X             ;* Set present Op code
        ldaa    #$3F            ;* Get SWI Instruction
        staa    0,X             ;* Substitute it
        ldx     SAVEX           ;* Set pointer to BRKTAB again
        ldaa    NEWLOC          ;*
        staa    0,X             ;* Store Address in Table
        ldaa    NEWLOC+1        ;*
        staa    1,X             ;*
        stab    2,X             ;* Store deleted Op code
        rts                     ;* and return

;;;* Erase previous breakpoint, if any, and restore Op code
BERASE  ldab    2,X             ;* Get Op code
        ldaa    0,X             ;* Get part of address
        cmpa    #$FF            ;* Was there a breakpoint?
        beq     BEEXIT          ;* No, Exit
        ldx     0,X             ;* Yes, get address of Break
        stab    0,X             ;* Restore Op code
        ldx     SAVEX           ;*
        staa    0,X             ;* Erase brealpoint table Entry
BEEXIT  rts                     ;* and return
;
;;;* BKNUM Routine - Get number of desired breakpoint and point
;;;* to its location in BKTAB table
:
BKSTR   FCC     ' Number: \4'   ;*
BKNUM   ldx     #BKSTR          ;*
        jsr     PDATA           ;*
        jsr     INEEE           ;* Get breakpoint number
        suba    #$30            ;* Convery from ASCII
        bmi     NGEXIT          ;* If Negative
        beq     NGEXIT          ;* if Zero
        cmpa    #$04            ;*
        bgt     NGEXIT          ;* If greater than 4
        psha                    ;*
        jsr     OUTS            ;*
        pula                    ;*
        ldx     #BKTAB          ;*
BKN1    dec     A               ;*
        beq     OKEXIT          ;* Exit when index points correctly
        inx
        inx                     ;* Bump index by 3
        inx                     ;*
        bra     BKN1            ;* and repeat
NGEXIT  ins                     ;* Fix stack to bypass calling routine on error
        ins                     ;*
OKEXIT  rts                     ;* return when done $(E68A)

; ===========================================================================
        ;;* $E6DF 14 Pg 188L
        ORG     $E6BF
BKRETN  sts     SP              ;* Save User stack pointer ($A008)
        tsx                     ;* Transfer to Index
        lds     #$D07F          ;* Reset to Monitor Stack
        tst     6,X             ;* Decrement USER PC to Point ... (???)
        bne     RONLY           ;* ... SWI, not past it
        dec     5,X             ;* decr left byte
RONLY   dec     4,X             ;* decr right byte
;;;* 'RE' command - Print user registers from stack
REGIST  jsr     CRLF            ;*
        ldx     SP              ;* Point to user stack
        ldab    1,X             ;* Get CC Register
        aslb                    ;*
        aslb                    ;* Ready for shifting into carry
        ldx     #$06            ;* Get Counter
RELOOP  aslb                    ;* Move next bit into carry
        ldaa    #$30            ;*
        adca    #$00            ;* Convery to ASCII
        jsr     OUTEEE          ;* Print it
        dex                     ;* Bump counter
        bne     RELOOP          ;* Print next bit
        jsr     OUTS            ;* Print space
        ldx     SP              ;* Point to User Stack again
        inx                     ;* Step past CC register
        inx                     ;* Point to B Accumulator
        jsr     OUT2HS          ;* Print B
        jsr     OUT2HS          ;* Print A
        jsr     OUT4HS          ;* Print Index
        jsr     OUT4HS          ;* Print PC
        ldaa    SP              ;*
        ldab    SP+1            ;* Get current USER Stack
        addb    #$07            ;*
        adca    #$00            ;* Change back to value it had in USER PGM
        staa    SAAVEX          ;* Point to it
        jsr     OUT4HS          ;* Print to it
        jmp     HOTST           ;* and return to FCROM $(E710)
	
; ===========================================================================
        ;;* $E713 14 Pg 188R
CONT    lds     SP              ;* Set user stack pointer
        rti                     ;* and return to User program ($E716)

; ===========================================================================
        ;;* $E717 14 Pg 189R
	ORG     $E717
;;;* 'SS' command - Single Step after breakpoint
STEP    ldx     SP              ;* Get user stack pointer
        ldx     4,X             ;* Get user PC
        stx     USERPC          ;* Save it
        stx     SAVEX           ;*
        jsr     PRNTOP          ;* Print address and instruction
;;;* Replace next instuction with SWI
        stx     NEXT            ;* Save address
        ldaa    0,X             ;* Get instruction
        staa    NEXT+2          ;* Save it
        ldaa    #$3F            ;* Get SWI
        staa    0,X             ;*
        cmpa    0,X             ;* Check it
        beq     OK1             ;* it stored OK
        bra     NOGOOD          ;* Abort if error
;;;* Next, See if a branch or jump is involved
OK1     ldaa    INSTR           ;* Get OP code
        cmpa    #$20            ;*
        bcs     NOBR            ;* No branch
        cmpa    #$30            ;*
        bcs     TYESBR          ;* Yes
NOBR    cmpa    #$39            ;*
        beq     NOTRTS          ;* No
        jmp     RTSIN           ;* Yes
NOTRTS  cmpa    $$3B            ;*
        beq     NOGOOD          ;* Don't do RTI
        cmpa    #$3F            ;*
        beq     NOGOOD          ;* Dito for SWI
        cmpa    #$6E            ;*
        bne     NOTJIN          ;*
JINV    jsr     JINDEX          ;* OK for index jumps
NOTJIN  cmpa    #$AD            ;*
        beq     JINV            ;* Ditto
        cmpa    #$7E            ;*
        beq     JEXT            ;* OK for extended jump
        cmpa    #$BD            ;*
        beq     JEXT            ;* Ditto
        cmpa    #$8D            ;*
        beq     YESBR           ;* BSR is a branch too
        cmpa    #$3E            ;*
        bne     NORMAL          ;* Ok if not WAI
;;; * Refuse to do some instuctions
NOGOOD  ldx     #NOSTR          ;*
        jsr     PDATA           ;* Print "NO!"
        ldx     NEXT            ;*
        ldaa    NEXT+2          ;*
        staa    0,X             ;* Retores next instr on error
        jmp     HOTST           ;*
NOSTR   FCC     'NO!\4'         ;*
;;;* Normal instructions are easy
NORMAL  ldaa    $%FF            ;* Erase ALT address Loc
        staa    BRANCH          ;*
GOUSER  ldx     #SSRETN         ;* Redirect SQI return
        stx     SWIJMP          ;*
        lds     SP              ;* Get user stack
        rti                     ;* Got to user ($E78F)

; ===========================================================================
        ;;* $E790 14 Pg 191L
	ORG     $E790
;;;* Return point from single step
SSRETN  ldx     #BKRETN         ;* Restore break address
        stx     SWIJMP          ;*
        ldx     NEXT            ;* Restore next OP code
        ldaa    NEXT+2          ;*
        staa    0,X             ;*
        ldaa    BRANCH          ;* Check branch address
        cmpa    #$FF            ;*
        beq     NONE            ;*
        ldx     BRANCH          ;* Restore it
        ldaa    BRANCH+2        ;*
        staa    0,X             ;*
NONE    jmp     BKRETN          ;* Store stack ptr and print registers
;;;* Handle Effective address of branch
YESBR   ldx     USERPC          ;*
        ldab    1,X             ;* Get offset
        beq     ZEROOF          ;*
        bmi     MINOFF          ;*
;;;* Plus offset
PLUSOF  inx                     ;* Add offset to instr address
        dec     B               ;*
        bne     PLUSOF          ;*
ZEROOF  inx                     ;* Point to next instr
        inx                     ;*
GOTADD  stx     BRANCH          ;* Save Address
        ldaa    0,X             ;* Get Instruction
        staa    BRANCH+2        ;* Save it
        ldaa    $$3F            ;*
        staa    0,X             ;* Substitute SWI
        cmpa    0,X             ;* Check that it went in
        beq     GOUSER          ;* Go to user if on
        bra     NOGOOD          ;* If it didn't store properly
;;;* Minus offset
MINOFF  dex                     ;* Subtract offset
        inc     B               ;* From instr address
        bne     MINOOF          ;*
        bra     ZEROOF          ;*
;;;*
;;;* Handle Extended jump address
;;;*
JEXT    ldx     USERPC          ;*
        ldx     1,X             ;* Get extended jump address
        bra     GOTADD          ;*
;;;*
;;;* Handle Index Jump
;;;*
JINDEX  ldx     USERPC          ;*
        ldab    1,X             ;* Get offset
        ldx     SP              ;*
        ldx     4,X             ;* Get user Index register
        dex                     ;*
        dex                     ;* Point to 2 bytes under
        tst     B               ;*
        beq     ZEROOF          ;* If offset is Zero
        bra     PLUSOF          ;* If offset is NonZero
;;;*
;;;*  Handle RTS Instruction
;;;*
RTSIN   ldx     SP              ;* Get user stack pointer
        ldx     0.X             ;* Get return address from user's stack
        bra GOTADD              ;* And treat it as a jump ($E7F4)

; ===========================================================================
        ;;* E305 14 Pg 183R
        ORG     $E305
FIND    ldx     #MANYST         ;* ($E0AB)
        jsr     PDATA           ;* Ask "How many btyes"
        jsr     INEEE           ;* Get a number
        suba    #$30            ;* Convert from ASCII
        beq     FIND5           ;* if = 0
        bmi     FIND5           ;* if < 0
        cmpa    #$03            ;*
        bgt     FIND5           ;* If > 3
        staa    FINDNO          ;* store number of bytes ($D025)
        jsr     OUTS            ;*
        ldx     #WHATST         ;* ($E1EA)
        jsr     PDATA           ;* Ask "What bytes"
        ldx     #WHAT           ;* ($D025)
FIENRT  pshb                    ;*
        jsr     BYTE            ;* Enter a byte
        pulb                    ;* Restore counter
        staa    0,X             ;* Store it
        inx                     ;*
        decb                    ;*
        bne     FIENTR          ;* Enter more, if needed
        jsr     FROMTO          ;* Get BEGA and ENDA
        ldx     BEGA            ;* Get ready to look
FIND1   ldab    FONDNO          ;* Main find loop ($D025)
        ldaa    0,X             ;* Get first byte
        cmpa    WHAT            ;*
        bne     FIND4           ;* Wrong byte
        decb                    ;*
        beq     FIND2           ;* Found one correct byte
        ldaa    1,X             ;* Get second byte
        cmpa    WHAT+1          ;*
        bne     FIND4           ;* Wrong
        decb                    ;*
        beq     FIND2           ;* Found two correct bytes
        ldaa    2,x             ;* Get tird byte
        cmpa    WHAT+2          ;*
        bne     FIND4           ;* Wrong byte
FIND2   stx     USAVEX          ;* Found correct bytes
        bsr     FIND5           ;* Print CRLF via vector at FIND5
        ldx     #USAVEX         ;* Point to address where found ($D020)
        jsr     OUT4HS          ;* Print it -
        jsr     OUTS            ;* One more space
        ldx     USAVEX          ;*
        dex                     ;* backup one byte
        ldab    #$04            ;* Ready to print 4 bytes
FIND3   jsr     OUT2HS          ;* Print byte
        decb                    ;*
        bne     FIND3           ;* Print four bytes
        ldx     USAVEX          ;* Restire X
FIND4   cpx     ENDA            ;* See if doine
        beq     FIND5           ;* Yes
        inx                     ;* No
        bra     FIND1           ;* Keep looking
FIND5   jmp     CRLF            ;* Do last CRLF nd return to FCROM when done ($E37E)
; ==============================================================================
        ;;* $E381 14 Pg 184R
;;;* "FN" Command - Fill memory with constant
        ORG     $E381
FILL    jsr     FROMTO          ;* Get From-To addresses
        ldx     #WITHST         ;* ($E1C5)
        jsr     PDATA           ;* Ask for data
        jsr     BYTE            ;*
        ldx     BEGA            ;* Get starting address
        dex                     ;*
FILOOP  inx                     ;*
        staa    0,X             ;* Store the byte
        cpx     ENDA            ;* See if done
        bne     FILOOP          ;* Contiune of No
        rts                     ;* Quit when Done ($E399)
; ==============================================================================
        ;;* $E39A 14 Pg 184R
        ORG     $E39A
;;;* SUM - Memory Checksum
SUM     jsr     FROMTO          ;* Get address limits
        ldx     BEGA            ;* Get start address
        clr     A               ;*
        clr     B               ;*
SUMLP   adda    0,X             ;* Add to checksum
        adca    #0              ;* Also add carry to second byte
        cpx     ENDA            ;* Last address?
        beq     SUMDON          ;* Yes
        inx                     ;* No, So increment and
        bra     SUMLP           ;*
DUMDON  staa    USAVEX          ;* Store SUM when done
        stab    USAVEX+1        ;*
        ldx     #USAVEX         ;* Point to Checksum
VEC4HS  jmp     OUT4HS          ;* Output checksum and return when done $(E3B7)

; ===========================================================================
	;;* $E525 14 Pg 184L
        ;;* E4ROM Entry Vectors
        org     $E525
;;;* 'AI' Command - ASCII Input routine
ASCIN   jsr     FROMTO          ;* Get address Range
        jsr     CRLF            ;*
        ldx     ENDA            ;* Get Last empty address
        stx     SAVEX           ;* Save it
        ldx     BEGA            ;* Get starting address
        dex
ASCIN2  inx                     ;*
        jsr     INEEE           ;* Get next character
        staa    0,X             ;* Store it
        cmpa    0,X             ;* See if it stored OK
        bne     ASCIN3          ;*
        stx     ENDA            ;* Store Ending address
        cpx     SAVEX           ;* Check if Run out of memory
        bne     ASCIN2          ;* No, so get more
ASCIN3  ldx     #ESTR           ;* Mem full or bad, so ...
        jsr     PDATA           ;* Print error
        bra     ASCIN3          ;* Go to repeat
;
ESTR    FCC     ' Error\4'      ;* ($E54F)
; ===========================================================================
	;;* $E556 14 Pg 184L
        ORG     $E556           ;*
;;;* 'AO' Command - ASCII Output routine
ASCOUT  jsr     FROMTO          ;* Get addres range
        jsr     CRLF            ;*
        ldx     BEGA            ;* Get starting address
ASCO2   ldaa    0,X             ;* Get next character
        jsr     OUTEEE          ;* Output it
        cpx     ENDA            ;* See if done
        beq     ASC03           ;* Yes
        inx                     ;*
        bra     ASCO2           ;* Repeat if not
ASC03   rts                     ;* Return when done
;
; Fill in here?
;
; ===========================================================================
	;;* $E56D 14 Pg 184R
; ===========================================================================
        ;;* E4ROM Entry Vectors
        ORG     $E56D
OLDSTR  FCC     'Enter old addresses:\4'
NEWSTR  FCC     'Enter new address: \4'
;
        ORG     $E497
MOVE    ldx     #OLDSTR         ;*
        jsr     PDATA           ;* Ask for old addresses
        jsr     FROMTO          ;*
        jsr     CRLF            ;*
        ldx     #NEWSTR         ;*
        jsr     PDATA           ;* Ask for new address
        jsr     BADDR           ;*
        stx     NEWLOC          ;* Save ($D042)
;;;* Now check fir firward move or backward move
        ldaa    BEGA            ;*
        suba    NEWLOC          ;*
        bcs     BACK            ;* if NEW > OLD
        bne     FORWRD          ;* IF <>
        ldaa    BEGA+1          ;* If =, Check the rest
        suba    NEWLOC+1        ;*
        bcs     BACK            ;* if NEW > OLD
        bne     FORWRD          ;*
NEXIT   rts                     ;* No move if NEW=OLD
;;;* Forward move
FORWRD  ldx     BEGA            ;*
        stx     SAVEX           ;* Save copy of Starting address
FWD1    ldx     SAVEX           ;*
        dex                     ;*
        cpx     ENBA            ;* Check for the end
        beq     NEXIT           ;* Exit if done
        inx                     ;*
        ldaa    0,X             ;* Get next byte
        inx                     ;* Bump FROM pointer
        stx     SAVEX           ;*
        ldx     NEWLOC          ;*
        staa    0,X             ;* Save byte
        inx                     ;* Bump TO pointer
        stx     NEWLOC          ;*
        bra     FWD1            ;* and repeat
;;;* Backward move
BACK    ldaa    ENDA            ;* Compute end of new aread
        ldab    ENDA+1          ;*
        subb    BEGA+1          ;*
        sbca    BEGA            ;* Length of old
        addb    NEWLOC+1        ;*
        adca    NEWLOC          ;*
        staa    NEWLOC          ;*
        stab    NEWLOC+1        ;* Store last loc of new
        ldx     ENDA            ;*
        stx     SAVEX           ;* Save copy of last loc
BACK1   ldx     SAVEX           ;*
        inx                     ;*
        cpx     BEGA            ;* Check for End
        beq     NEXIT           ;* Exit if done
        dex                     ;*
        ldaa    0,X             ;* Get next byte
        dex                     ;* Bump FROM point
	stx     SAVEX           ;*
        ldx     NEWLOC          ;*
        staa    0,X             ;* Save byte
        dex                     ;* Bump TO pointer
        stx     NEWLOC          ;*
        bra     BACK1           ;* and repeat ($E61C)


; ===========================================================================
        ;;* E4ROM Entry Vectors
        org     $E400
CINITV4 jmp     CINIT4          ;* Cold start initialization
;;* CINIT4 - Cold Start Initialization
;;* Check whether this is powerup or reset of system
CINIT4  ldx     #$1234          ;* Check power-up locations
        cmpx    POWUP           ;*
        bne     PUP             ;*
        cpx     POWUP+2         ;*
        beq     xRESET          ;*
;;;* Initial power up sequence
PUP     ldx     #BKRETM         ;* Initialize Breakpoint ISS addresses
        stx     SWIJMP          ;*
        ldx     #$1234          ;*
        stx     POWUP           ;*
        stx     POWUP+2         ;* Initialize POWUP flag
        ldaa    #$FF            ;*
        ldab    $$12            ;*
BKERAS  staa    0,X             ;* Erase Break points table
        inx                     ;*
        decb                    ;*
        bne     BKERAS          ;* Repeat if not finished
;;;* See if other ROMs require initialization
xRESET  ldaa    $E800           ;* Check next ROM
        cmpa    #JMPINST        ;* Is there a jump?
        beq     CMORE4b         ;*
        ldaa    $EC00           ;* Check the ROM after that
        cmpa    #JMPINST        ;* Is there a jump?
        beq     CMORE8b         ;*
        rts                     ;* No, return to FCROM
CMORE4b jmp     $E800           ;* Yes, go initialize
CMORE8b jmp     $EC00           ;* Yes, go initialize second ROM

; ===========================================================================
        ;;* $E4B6 14 Pg 186R
	ORG     $E4B6
;;;* 'DE' Command - Desembler dump
DESEMB  jsr     FROMTO          ;* Ask for addresses
        ldx     BEGA            ;*
        stx     SAVEX           ;*
DES2    jsr     PRNTOP          ;* Go to print current line
        ldaa    ENDA            ;* Subtract next FROM last
        ldab    ENDA+1          ;*
        subd    SAVEX+1         ;*
        sbca    SAVEX           ;*
        bcc     DES2            ;* Return if NEXT <= LAST
        rts                     ;* otherwise exit
;;;* PRNTOP - subroutine to print address and current instruption
PRNTOP  jsr     CRLF            ;*
        ldx     #SAVEX          ;* Get location of next address
        jsr     OUT4HS          ;* Print it
        jsr     OUTS            ;*
        ldx     SAVEX           ;* Get address of instruction
        ldaa    0,X             ;* Get operation code
        staa    INSTR           ;* Save it
        jsr     OUT2HS          ;* Print it
        stx     SAVEX           ;* Increment SAVEX
        clr     B               ;* Byte counter
        ldaa    INSTR           ;*
        cmpa    #$8C            ;* Analyze Op code for nu of bytes
        beq     LENTH3          ;*
        cmpa    #$8E            ;*
        beq     LENTH3          ;*
        cmpa    #$CE            ;*
        beq     LENTH3          ;*
        anda    #$F0            ;*
        cmpa    #$20            ;*
        beq     LENTH2          ;*
        cmpa    #$60            ;*
        beq     LENTH1          ;*
        anda    #$30            ;*
        cmpa    #$30            ;*
        bne     LENTH2          ;*
LENTH3  inc     B               ;* 3-Byte: 8C, 8E, CE, 7x, Bx, Fx
LENTH2  inc     B               ;* 2-Byte: 2x, 6x, 8x, 9x, Ax, Cx, Dx, Ex
LENTH1  inc     B               ;* 1-Byte: 1x, 3x, 4x, 5x
        nop                     ;*
        nop                     ;*
        beq     POP3            ;*
        dec     COUNT           ;* ($D046)
        beq     POP1            ;*
        jsr     OUT4HS          ;* Print 2 bytes
        bra     POP2            ;*
POP1    jsr     OUT2HS          ;* Print One byte
POP2    stx     SAVEX           ;* Increment next
POP3    RTS                     ;* ($E524)

; ===========================================================================
        ;;* $E68B 14 Pg 186R
        ORG     $E68B
;;;* 'BP' Command - Print breakpoint Locations
BPRINT  ldab    #'0'            ;* Breakpoint number in ASCII
        ldx     #BKTAB          ;*
        stx     SAVEX           ;*
BPR1    inc     B               ;*
        cmpb    #'5'            ;* Stop at 5 Breakpoints @FIXME: I think $5
        bne     BPR2            ;*
        rts                     ;* Return when done
BPR2    jsr     CRLF            ;* Print CR
        tba                     ;* Get BP number
        jsr     OUTEEE          ;* Print breakpoint number
        ldx     SAVEX           ;* Get its location in Table
        ldaa    0,X             ;* Get BP address
        cmpa    #$FF            ;* Is there one?
        bne     BOR3            ;* Yes, go print it
        inx                     ;*
        inx                     ;* No, update pointer
        inx                     ;*
        bra     BPR4            ;* And repeat
BPR3    jsr     OUTS            ;* Print space
        ldx     SAVEX           ;*
        jsr     OUT4HS          ;* Print address of breakpoint
        jsr     OUT2HS          ;* Print Op code
BPR4    stx     SAVEX           ;* Save BKTAB location of next
        bra     BPR1            ;* and repeat ($E6BD)

; ===========================================================================
        ;;* Jump Vectors
        ORG     $FC00
        ;; The 6800 Humbug is from the Kilobaud-Thoughts13.pdf
        ;;                           & Kilobaud-Thoughts14.pdf
;;;
HUMBUG:
COLDV	jmp	COLDST          ;* 1 - 
WARMV	jmp	WARMST          ;* 2
HOTV	jmp	HOTST           ;* 3
INEEEV	jmp	INEEE           ;* 4
OUTEEEV	jmp	OUTEEE          ;* 5
CRLFV	jmp	PCRLF           ;* 6
PDATAV	jmp	PDATA           ;* 7
INCH8V	jmp	INCH8           ;* 8
INHEXV	jmp	INHEX           ;* 9
BYTEV	jmp	BYTE            ;* 0
BADDRV	jmp	BADDR           ;* 1
OUT2HV	jmp	OUT2H           ;* 2
OUTHLV	jmp	OUTHL           ;* 3
OUTHRV	jmp	OUTHR           ;* 4
OUT2HSV	jmp	OUT2HS          ;* 5
OUT4HSV	jmp	OUT4HS          ;* 6
OUTSV	jmp	OUT4            ;* 7 ($FC30)  $FD6B ?
;
; ===========================================================================
        org     $FC33           ; From article
        ; 
        ;* Initialize the I/O Ports
COLDST: 
RESTART:lds	#STACK          ;* $4EA0 $7FA0
        ldx     #$8000          ;* ACIA/PIA ?
        ldaa    #$03            ;* Port 0 & 1 ACIA - Reset
        staa    0,X             ;* ACIA 0
        staa    4,X             ;* ACIA 1 (?)
        ldaa    #$0B            ;* 
        staa    0,X             ;* ACIA 0
        staa    4,X             ;* ACIA 1 (?)
        jsr     VINIT           ;* Initialize video
;;; See if other ROMs require COLD Start Initialization
        ldaa    $E000           ;* Check for E0ROM
        cmpa    #JMPINST        ;* Is there a Jump?
        bne     WARMST          ;* No
        jmp     $E000           ;* Yes, go to it ($FC4F)
;
; ===========================================================================
;
; $FC52 - 14 pg 178L
WARMST  lds     $D07F           ;* Set STACK pointer to Monitor Area
        clra
        staa    DSTAT           ;* Turn off D
        staa    P0OUT           ;* Turn off port 0 output
        staa    PASTAT          ;* Turn off pause function
        deca
        staa    P1STAT          ;* Turn on control port output
        staa    VSTAT           ;* Turn on video board output
        ldx     DWARMV          ;*
        stx     RETADD          ;* Initialize pause-return address
        ldaa    #$0F            ;* 15
        staa    KBINZ
        ldaa    #$11            ;* ACIA output initialize ($15 maybe?)
        staa    PRTINZ
        ldaa    #$13            ;* Turn reader off
        jsr     OUTCHR
        inca                    ;* Turn off punch
        jsr     OUTCHH
;;;* See if other ROMs require WARM start initialization
        ldaa    $E003           ;* Check E0ROM
        cmpa    #JMPINST        ;* Is there a jump?
        bne     HOTST           ;* No
        jsr     $E003           ;* Yes, Go to it
;;;* $FC94 14 Pg 179 L
;;;* HOST - Initialization complete, ready for command
HOTST   lds     #$D07F          ;* Reset stack pointer to monitor area
        clr     PORECH          ;* Turn on control port echo
        jsr     PCRLF           ;* Print CR/LF
        ldaa    #'*'            ;* Print prompts
        jsr     OUTEEE          ;*
        jsr     INEEE           ;* Get first command character
        psha                    ;* Save first character of command
        jsr     INEEE           ;* Get second command character
        tab                     ;* move second to B
        jsr     OUTS
        pula                    ;* Restore first commadn
        psha                    ;* Save it once more
;;;* Check command
        cmpa    #'J'            ;* Check for JU(mp)
        bne     NOTJU
        cmpb    #'U'
        bne     NOTJU
        jmp     JUMP            ;* Execute jump command
NOTJU   cmpa    #'M'            ;* Check for ME(mory change)
        bne     HOTEND
        cmpb    #'E'
        beq     CHANGE          ;* Execute change command
;;;* See if other ROMs have commands
HOTEND  ldaa    $E006           ;*
        cmpa    #JMPINST        ;* Is the a jump
        beq     GOJUMP          ;*
        bra     HOTST           ;* And look for more
GOJUMP  pula                    ;* Get first character
        jsr     $E006           ;* And jump to next ROM
GOHOT1  bra     HOTST           ;* Then do more commands

; ===========================================================================
        ;;* $FD6F 14 pg 180L
        ORG     $FD6F
JUMP    bsr     BADDR           ;* Get address
        lds     #USTACK         ;* USTACK, Init stack to user area
        jsr     0,X             ;* Jump to user program
        jmp     WARMST          ;* On RTS, return to warm start

; ===========================================================================
        ;;* $FD93 14 pg 180R
        ORG     $FD93
;;;* INEEE - Character input routine
INEEE   pshb                    ;* Save B
        stx     INEEXR          ;* $D005 Save registers
INRPT   bsr     INCH7           ;* Get input character
        cmpa    #$13            ;* Is it Crtl-S?
        beq     GOTCS           ;* Yes
        tst     P0RECH          ;* No, echo on?
        bne     INEXIT          ;* No, so exit
INEXIT  ldx     INEEXR          ;* Restore registers
        pulb                    ;*
        rts                     ;* And return

;;;*  Control-S detected, Set and interpret command
GOTCS   bsr     GETCMD          ;* Do command
        bra     INRPT           ;*

;;;* Subroutine to get and do command
GETCMD  ldaa    #$07            ;*
        jsr     OUTCMM          ;* OUTCHM? Echo Cntl-G on CTL Port
        bsr     INCH7           ;* Get seconf character of CMD
        cmpa    #'0'            ;* Port 0 command
        bne     NOT0            ;* No
        com     P0STAT          ;* Yes; Flip port 0 Status
        rts                     ;* and return

NOT0    cmpa    #'1'            ;* Port 1 command?
        bne     NOT1            ;* No
        com     P1STAT          ;* Yes; Flip port 1 status
        rts

NOT1    cmpa    #$'D'           ;* Port D Command?
        bne     NOTD            ;* No
        com     DSTAT           ;* Yes, flip port D Status
        rts

NOTD    cmpa    #'P'            ;* Pause command?
        bne     NOTP            ;* No
        com     PASTAT          ;* Yes; Flip pause status
        ldaa    #$0F            ;* (15 lines?)
        staa    PAUCTR          ;* Reset pause line CNTR
        rts

NOTP    cmpa    #$0D            ;* CR Command to quit?
        bne     NOTCR           ;* No
        pulb                    ;* Yes; Fix up stack
        pulb
QUIT    pulb                    ;* Restore B
        pula                    ;*
        pula                    ;* Fix stack some more
        ldx     RETADD          ;* Get return address
        jmp     0,X             ;* Add return
NOTCR   rts                     ;* Return without doing anything otherwise
;;;* Actual control port input routines
INCH7   bsr     INCH8           ;* Get 7-Bit characters
        anda    #$7F            ;* Mask out Parity
        rts                     ;*

INCH8   ldx     P0RADD          ;* Get 8 bit character
        ldaa    KBDINZ          ;* Configure ACIA
        staa    0,X             ;*
ACIAIN  ldaa    0,X             ;*
        asra                    ;*
        bcc     ACIAIN          ;* Wait for a character
        ldaa    1,X             ;* Get it
        rts                     ;* and return ($FDFC)
; ===========================================================================
        ;;* $FDFD 14 pg 181L
;;;* OUTEEE - Character output routine
OUTEEE  pshb                    ;* Save B
        stx     OUTEXR          ;* Save X
        psha                    ;* Save character
        ldx     P0RADD          ;*
        ldaa    0,X             ;* Check control port
        asra                    ;*
        bcc     NOTEST          ;* No
        ldaa    1,X             ;* Character; Get it
        anda    #$7F            ;* Mask out parity bit
        bsr     GETCMD          ;* Yes; Get command and do it
NOTEST  pula                    ;* Finished testing for command
;;;* Check for pause
        tst     PASTAT          ;* Pause stat on?
        beq     NOPAUS          ;* No
        cmpa    $#10            ;* Clear screen?
        bne     NOCLR           ;* No
        ldaa    #$0F            ;* Test; reset pause counter
        staa    PAUCTR          ;*
        bra     NOPAUSE         ;*
NOCLR   cmpa    #$0D            ;* CR?
        bne     NOPAUS          ;* Only pause at the end of the line
        dec     PAUCTR          ;* decr pause line cntr
        bne     NOPAUS          ;* and check it
        ldaa    #$0F            ;* Must pause, reset cntr
        staa    PAUCTR          ;*
        bsr     INCH7           ;* wait for restart char
        cmpa    #$0D            ;* Quit if it's a CR
        bne     PCONT           ;*
        jmp     QUIT            ;*
PCONT   ldaa    #$0D            ;* continue with a CR
NOPAUS  tst     P0STAT          ;* Print on port 0?
        beq     NOTPT0          ;* No
        bsr     OUTCH0          ;* yes
NOTPT0  tst     P1STAT          ;* Print on control port?
        beq     NOTPTH          ;* No
        bsr     OUTCHM          ;* Yes
NOTPTH  tst     VSTAT           ;* Output via video BOARD?
        beq     NOTVID          ;* No
        psha                    ;* Yes
        bsr     OUTCHV          ;* Output to video
        pula                    ;*
NOTVID  tst     DSTAT           ;* Print on D?
        beq     NOTDUR          ;* No
        jsr     OUTCMD          ;* Yes (jsr $EC0C)
BOTDUR  ldx     OUTEXR          ;* Reload X & B
        pulb                    ;*
        rts                     ;*
;;;* Output on Port 0
OUTCH0  ldx     #$8000          ;* Output to Port 0
        bra     OUTCHE          ;*
;;;* Output no control port
OUTCHM  ldx     P0RADD          ;*
OUTCHE  ldab    PTRINZ          ;* ACIA init
        stab    0,X             ;* Init for 8 bits, 2 stop bits
OUTH2   ldab    0,X             ;* Wait for ready
        asrb                    ;*
        asrb                    ;*
        bcc     OUTH2           ;*
        staa    1,X             ;* Print it
        rts                     ;* ($FE77)

HBUGSTR:fcc     "HUMBUG+(C) 1983 P. STARK\4" ; S447B:

; ===========================================================================
;
        ORG     $FFE0
IRQV    ldx     IRQ             ;* IRQ vector via A000
        jmp     0,X

SWIV    ldx     SWIJMP          ;* SWI vector via A012
        jmp     0,X

NMIV    ldx     NMI             ;* NMI vector via A006
        jmp     0,X

        ORG     $FFF8
        FDB     IRQV            ;* IRQ vector
        FDB     SWIV            ;* SWI vector
        FDB     NMIV            ;* NMI vector
        FDB     COLDV           ;* Reset vector
; ===========================================================================
;   STARTUP VECTORS $FFF8 -$FFFF
;
        ORG $FFF8
;
;        FDB IRQ                 ; FFF8 IRQ (no code)
;        FDB SWI                 ; FFFA SWI (no code)
;        FDB NMI                 ; FFFC NMI (no code)
;        FDB HUMBUG              ; FFFE Restart
;
; Version 1 has table of RAM locations at $A000H
        END
; ------------------------------------------------------------------------------
;* MEMORY MAP
;* Hex Address
;* 
;*        0000 Data Direction Register for keyboard lines
;*        0001 Data Direction Register for miscellaneous I/O
;*        0002 Keyboard Output Lines
;*        0003 Miscellaneous I/O Data Register
;*        0004 Not Used (not available?)
;*        0005 Not Used
;*        0006 Not Used
;*        0007 Not Used
;*        0008 Timer Control and Status Register
;*        0009 Counter (high byte)
;*        000A Counter (low byte)
;*        000B Output Compare Register (high byte)
;*        000C Output Compare Register (low byte)
;*        000D Input Capture Register (high byte)
;*        000E Input Capture Register (low byte)
;*        000F Port 3 Control and Status Register
;*        0010 Not Used
;*        0011 Not Used
;*        0012 Not Used
;*        0013 Not Used
;*        0014 RAM Control Register
;* 0015 - 007F Not Used
;* 0080 - 00FF RAM internal to the 6803
;* 0100 - 3FFF Not Used
;*
;* 4000 - 8FFF 16K RAM (4K - 20K used)
;* 4000 - 41FF - Video RAM
;* 4200 - 4334 - OS Variables
;* 4335 - End of RAM - Program RAM
;* 9000 - BFFF 16K I/O Slot (Keyboard and VDG control)
;* C000 - FFFF 16K ROM (only 8K used)
;*
;* MC10 default memory layout
;* 0000 - 3FFF 1st 16K
;* 4000 - 7FFF 2nd 16K
;* 8000 - BFFF 3rd 16K
;* C000 - FFFF 4th 16K (BASIC 8K ROM mirrored)
;*
;* MC10 with MCX-128
;*
;*
;* Humbug+
;* $4350 - Top of RAM ($7500 - base of Humbug+)
; ------------------------------------------------------------------------------

; =[ Fini ]==================================================================
;/* Local Variables: */
;/* mode:asm         */
;/* End:             */
