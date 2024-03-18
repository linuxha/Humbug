; ===========================================================================
; This is the disassembly of the Peter Stark's Humbug+ monitor for the Tandy
; MC-10. While this was disassembled with the 6803 CPU option it does appear
; that this is basically the 6800 version of Humbug but for the MC10.
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

        CPU     6800            ; That's what asl has
;
; This is Peter Stark's Humbug+ for the MC10. There are issues here with code in
; that it loads from tape and a large number of ld and st inst write over the
; loaded code ($7500-$7FFF = 2815 bytes). I'll use this code and Peter's Kilobaud
; articles to get something that compiles and runs properly on the 6802
; system I have, Then I'll clean up the MC10 code so we have proper working
; code there also.
;
;	$ srec_info hb7500d.s19
;	Format: Motorola S-Record
;	Execution Start Address: 00000000
;	Data:   7500 - 76EB
;	        76FB - 770A
;	        7717 - 7F6F
;	$
;
;	76EC - 76FA     ;* Free space $0E
;       770A - 7717     ;* Free space $0D
;       7F70 - 7FFF     ;* Free space $8F
;
;       Fix L7681 db $43, $42, $41, $58, $58, negb
pshx    MACRO
        stx     SAVEX
        ENDM

pulx    MACRO
        ldx     SAVEX
        ENDM

ldd     MACRO   arg
        ldaa    arg
        ldab    arg+1
        ENDM

std     MACRO   arg
        staa    arg
        stab    arg+1
        ENDM

abx    MACRO
	warning "Check this 6800 code (was abx)"

        stx     XHI             ; Save X to SAVEX or XHI
        addb    XLO             ; add B to X lo
        adca    XHI             ; add the CC to X hi
	ldx     XHI             ; retore X
        ENDM
;
;       RAM
;
;	X76E9:  db	$00, $00
;	X76EB:	db	$00
;	X76EC:	                        ; ror	
;	X76ED:  rmb     1
;	X76EE:  rmb     1
;	X76FB:	db	$00
;	X76FC:	db	$00
;	X76FD:	db	$00, $00
;	X76FF:	db	$00, $00        ; On init this gets clr'd (2Byte)
;	X7701:	db	$00             ; On init this gets clr'd (1Byte)
;	X7702:	db	$75, $7B        ; Humbug+ str addr
;	X7702           ;* Humbug not restart? Str already loaded?
;	X7702           ; !! jumps here (monitor reset)
;	X7704:	db      $0F             ; sei
;	X7705:
;	X7706:	db	$00
;	X7707:
;	X7708:  db      $70
;	X7709:
;	X770A:
;	X770B:	rmb     1               ; 770B - 7710
;	X770C:	rmb     1
;	X770D:  rmb     2
;	X770F:
;	X770F           ; POINT TO USER STACK AGAIN (
;	X770F           ; SAVE USER STACK PTR (
;	X7711:
;	X7711           ; GET BP ADDRESS
;	X7711:	stx	
;	X7711           ;* Tmp storage ?
;	X7712:  rmb     5
;
;	X7FFC
;	X7FFC   equ     $7FFC
;	X7FFE
;	X7FFE   equ     $7FFE
;
;*
;* These are the direct addresses, I need to fix these
;*
;
;
; ===========================================================================
        ;;
        include "motorola.inc"          ; Macros for things like fcc,db, etc.
        include "humbug.inc"            ; This may need a lot of clean up
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
; ===========================================================================
;
	org	$7300           ;
; =[ MIKBUG RAM ]============================================================
        ;;*
        ;;* MIKBUG alsos uses the 128 byte scratchpag RAM @ $A000
;        ORG     $A000           ;*
;*
;* Mixing MIKBU, Humbug & Humbug+ names
;*
IRQ     RMB     2               ;* A000
BEGA    RMB     2               ;* A002
ENDA    RMB     2               ;* A004
NMI     RMB     2               ;* A006
SP      RMB     2               ;* A008
P0RADD  RMB     2               ;* A00A
P0RECH  RMB     1               ;* A00C
BADDRH
XHI     RMB     1               ;* A00D BADDRH
BADDRL
XLO     RMB     1               ;* A00E BADDRL
CKSUM   RMB     3               ;* A00F
;
SWIJMP  RMB     2               ;* A012
        RMB     52              ;* 0x36
PC      RMB     2               ;* A048
        RMB     53              ;* 
STACK2
USTACK  RMB     1               ;* A07F

AMIDSIZ EQU     * - $7300

;*
;* Temporary, until I figure out what these really are
;* These fall inside the code area (ROM) so you shouldn't write there
;*
X76E9   RMB     2               ;* $76E9
X76EB   RMB     1               ;* $76EB
BRTMP
X76EC   RMB     1               ;* $76EC
X76ED   RMB     1               ;* $76ED
X76EE   RMB     13              ;* $76EE
X76FB   RMB     1               ;* $76FB
X76FC   RMB     1               ;* $76FC
X76FD   RMB     2               ;* $76FD
X76FF   RMB     1               ;* $76FF
X7701   RMB     1               ;* $7701
X7702   RMB     2               ;* $7702
X7704   RMB     1               ;* $7704
X7705   RMB     1               ;* $7705
X7706   RMB     1               ;* $7706
X7707   RMB     1               ;* $7707
X7708   RMB     1               ;* $7708
X7709   RMB     1               ;* $7709
X770A   RMB     1               ;* $770A
X770B   RMB     1               ;* $770B
X770C   RMB     1               ;* $770C
X770D   RMB     2               ;* $770D
X770F   RMB     2               ;* $770F
TMPSTR
X7711   RMB     1               ;* $7711
X7712   RMB     1               ;* $7712

DUMMY   RMB     2

X7644   RMB     2               ;* $7644
X75D8   RMB     2               ;* $75D8
X7801   RMB     2               ;* $7801
X792D   RMB     2               ;* $792D

X426A   RMB     2               ;* X426A <- MC10 memory
X4210   RMB     2               ;* X4210

;*
;* Not sure what to do about these yet
;*
X7FF6   RMB     2               ;* $7FF6
X7FFC   RMB     2               ;* $7FFC
X7FFE   RMB     1               ;* $7FFE

AMEMSIZ EQU     * - $7300

; $A07F - 6810 128 bytes
	org	$7400           ;
;
; =[ Humbug RAM ]============================================================
;        ORG     $D000           ;*
; D000
P0STAT  RMB     1               ;* Port 0
; D001
P1STAT  RMB     1               ;* Port 1
; D002
VSTAT   RMB     1               ;*
; D003
DSTAT   RMB     1               ;* BSTAT? DSTAT?
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
USAVEX  RMB     2               ;* USAVEX was Save X
; D022
WHAT    RMB     3               ;*
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
BRANCH  RMB     3               ;*
; D036
BKTAB   RMB     12              ;* Breakpoint able (4 bp - 3 bytes each)
; D042
NEWLOC  RMB     2               ;*
; D044
INSTR   RMB     2               ;*
; D046
COUNT   RMB     1               ;*
        RMB     56              ;*
; D07F
STACK   RMB     1               ;* Top of RAM (6810 @ $D000)
;
BMEMSIZ EQU     * - $7400

	org	$7500           ;
        ;; The 6800 Humbug is from the Kilobaud-Thoughts13.pdf
        ;;                           & Kilobaud-Thoughts14.pdf
        ;; But referenced HUMBUG09.TXT for some of this code.
        ;; 6800 has fewer instructions
        ;; 6803 is closer to the 6809 (no Y reg)
        ;;
        ;; lilbug has 12 vectors
        ;; humnug09, 13 vectors
        ;; humbug+, has 15 vectors
        warning "JMP Table needs review"
;                               ;* 6803        6809
MAIN:	jmp	RESTART         ;* L7533 -  1  RESTRT
	jmp	NXTCMD          ;* L7594 -  2  WARMST
	jmp	INEEE           ;* L7733 -  3  NXTCMD (NextCmd?)
	jmp	INCH7           ;* L7763 -  4  INEEE
	jmp	INHEX           ;* L76B8 -  5  BADDR
	jmp	BADDR           ;* L768A -  6  OUTEEE (CRLFV (L 768A))
	jmp	OUTEEE          ;* OUTEEE -  7  PDATA
	jmp	INCH8           ;* L77AE -  9  CRLF
	jmp	L77CD           ;* L77CD -  9  OUTS
	jmp	PDATA           ;* PDATA - 10  OUTHR
	jmp	OUTCH           ;* L7717 - 12  OUT2HS
	jmp	OUTS            ;* L76DA - 13  OUT4HS
	jmp	OUTHR           ;* L76A8 - 14
	jmp	OUT2HS          ;* L76D8 - 15
	jmp	OUT4HS          ;* L76D6 - 16
        jmp     OUTS            ;*
;       ;; ------------------------------------------------------------------
        ;; I think it is safe to assume that the above 6803 table is not
        ;; correct, it doesn't line up with the 6800 article or the 6809
        ;; vectors.
;       ;; ------------------------------------------------------------------
;
;752D:  dex                     ; $09     Odd nobody jumps here (???)
;	ror	0,X             ; $66 $00
ROMBEG
MONRST  clr	X7702           ; !! jumps here (monitor reset)
COLDST                          ; L 7533:
RESTART lds	#STACK          ;* $4EA0 $7FA0
        ;; Load the contents of SWIVC $7AC4 (SWI Vector?) into X
        ;; Store it into SWIADDR ($4210 - OS addr)
        ;; Add the JMP ($7E)
	ldx	SWIHDLR         ;* *$7AC4 SWIVC ?
	stx	SWIADDR         ;* SWIADDR ($4210)
	ldaa	#JMPINST        ;* #7E = jmp
	staa	SWIVEC          ;* Hard coded as 420F (also in hb4400)
        ;; 
	ldx	#MAIN           ;* $7500
	stx	EXECJP          ;* X421F
        ;; 
	ldx	#X76FF
	clr	0,X             ;* $76FF = 0
	clr	1,X             ;* $7700 = 0
	clr	2,X             ;* $7701 = 0 (L 754E:)
        ;; 
	ldaa	#CLS            ;* FF (FormFeed - Clears the scr)
	jsr	OUTEEE          ;* was PDATA (L 7769)
	ldx	#HBUGSTR        ;* $757B "HUMB..."
	cpx	X7702           ;* Humbug not restart? Str already loaded?
	beq	WARMST          ;* Yes? Then skip  (L 756A)
	ldx	#BKTAB          ;* Dst JMP table $45EF
        ;; 
	ldaa	#$FF            ;* Src JMP table $FF0C - There is no ldad #$FF0C
	ldab	#$0C            ;* $0C bytes
L7564:
FILL:   staa	0,X           ;* Fill 76EF -760D with $FF
	inx
	decb
	bne	FILL            ; (L 7564)
L756A:
;NXTCMD:                         ;
WARMST: lds	#STACK          ; ($ 7FA0)
	jsr	CRLF            ; (L 7717)
	ldx	#HBUGSTR
	stx	X7702
	jsr	PDATA           ; (L 7724)
	bra	NXTCMD          ; (L 7594)
;
HBUGSTR:fcc     "HUMBUG+(C) 1983 P. STARK\4" ; S447B:
;
        ;;
        ;; NXTCMD - return the 2 byte command
        ;; 
L7594:
NXTCMD: lds	#STACK          ; I think this is the real NXTCMD (L 7594 $7FA0)
	jsr	CRLF            ; L7717
	ldaa	#$0F
	staa	X7704
	ldaa	#$3A            ; ':'
	jsr	OUTEEE          ; (L 7769)
	jsr	INEEE           ; (L 7733) njc
	psha
	jsr     INEEE           ;
L75AB:
	tab
	jsr	OUTS            ; (L 76DA)
	pula
	ldx	#X7644
	stx	X76EC
	ldx	#X75D8
L75B9:
	inx
	inx
	inx
	inx
	cpx	X76EC
	beq	L75D5
	cmpa	0,X
	bne	L75B9
	cmpb	1,X
	bne	L75B9           ; (L 75B9)
	jsr	OUTS            ; (L 76DA)
	inx
	inx
	ldx	0,X
	jsr	0,X           ; INFO: index jump
	bra	NXTCMD          ; (L 7594)
;
L75D5:
	ldaa	#$3F            ; 3F = '?'
	jsr	OUTEEE          ; (L 7769)
	bra	NXTCMD          ; (L 7594)
;
;
;;; ============================================================================
;;;
;;; HUMBUG COMMANDS (Confirmed)
;;; AD AI AO AT BA BO BR CI CS DE EX FI FM HD HE JU MC ME MM MT RC RE SA SS ST !!
;;;
;;; LO - Load MIKBUG Tape
;;; PU - Punch/Save NIKBUG formatted tape
;;; EN - END OF TAPE FORMATTING (S9 - end punch tape)
;;; FD - REGULAR DISK BOOT (Flex)
;;; PD - Pericom Disk Boot
;;; GO - Go to user program using A048/A049
;;; CL - CLEAR SCREEN
;;; FI - Find one, two or three bytes in memory
;;; HD - Hexdump (From ... To ...)
;;; FM - Fill memory
;;; CS - Compute checksum
;;; MT - memory test
;;; PC - Print Contents of A048/A049 (Program Counter)
;;; PU - Punch tape
;;; DE - Desemble memory (display machine code, not opcodes)
;;; BP - Breakpoint printout
;;; BR - Breakpoint set or reset
;;; CO - Continue after BP
;;; RE - Register examine
;;; SS - Single Step
;;; AI - ASCII Input into memory
;;; AO - ASCII Output from memory
;;; MO - Move Memory contents
;;;
;;; AD - ASCII Dump
;;; AT - Analyze Tape
;;; BA - Change BAUD
;;; CS - Compute Checksum
;;; EX - Exit Return to BASIC
;;; HE - Help
;;; JU - Jump (like GO)
;;; MC - Memory Compare
;;; ME - Memory Exam/Change
;;; SA - Save to Cassette
;;; SS - Single Step
;;; ST - Start SS (if no Breakpoint)
;;; !! - Monitor Reset
;;; U1 - User 1 (not added yet)
;;; U2 - User 2 (not added yet)
;;; ============================================================================
COMTAB:
L75DC:
	FCC     "AD"            ; ASCII Dump
        FDB     ADINST          ; $7C85
        FCC     "AI"            ; ASCII Input
        FDB     AIINST          ; $7E77 - Last byte code, $4CFF ???
        FCC     "AO" 		; ASCII Output Oh not Zero
        FDB     AOINST          ; $7EA8
        FCC     "AT"            ; Analyze Tape
        FDB     ATINST          ; $7817
BASIC:               		;* L45EC/X45EC 0x45EC Not BASIC - Change BAUD
        FCC     "BA"            ; Change Baud
        FDB     BAINST          ; $77DA
        FCC     "BP"            ; Print Break points
        FDB     BPINST          ; $7A90
        FCC     "BR"            ;set/reset Breakpoints
        FDB     BRINST          ; $7A23
        FCC     "CO"            ; Continue (after a break)
        FDB     COINST          ; $7B67
        FCC     "CS"            ; Checksum
        FDB     CSINST		; $7E57
        FCC     "DE"            ; Desemble (not Disassemble, only bytes)
        FDB     DEINST          ; $79B4
L4504:  FCC     "EX"            ; Exit to BASIC
        FDB     EXINST          ; $79A9
        FCC     "FI"            ; Find 1, 2 or 3 bytes
        FDB     FIINST          ; $7DB5
        FCC     "FM"            ; Fill Memory
        FDB     FMINST          ; $7E3E
        FCC     "HD"            ; Hex Dump
        FDB     HDINST          ; $7C85
        FCC     "HE"            ; Help
        FDB     HEINST          ; $7F4A
        FCC     "JU"            ; Jump (actually JSR)
        FDB     JUINST          ; $76DE
        FCC     "MC"            ; Memory Compare
        FDB     MCINST          ; $7CFE
        FCC     "ME"            ; Memory Examine
        FDB     MEINST          ; $7644
        FCC     "MM"            ; Memory Move
        FDB     MMINST          ; $7ED5
        FCC     "MT"            ; Memory Test
        FDB     MTINST          ; $7D69
        FCC     "RC"            ; Register Change
        FDB     RCINST          ; $7B36
        FCC     "RE"            ; Register Examine
        FDB     REINST          ; $7AD6
        FCC     "SA"            ; CSAVEM to cassette
        FDB     SAINST          ; $78CC
        FCC     "SS"            ; Single Step (lots of limitations, careful)
        FDB     SSINST          ; $7B99
        FCC     "ST"            ; Start SS (if no Breakpoint)
        FDB     STINST          ; $7B81
;;; 
        FCC     "!!"            ; Monitor Reset
        FDB     MONRST          ; $7530
;;; 
	IFDEF   UNTRUE
        ;;
        ;; Extra User commands
        ;; 
        FCC     "U1"
        FDB     USER1           ;
        ;; 
        FCC     "U2"
        FDB     USER2           ;
        ;; 
        ENDIF
;;; 
TABEND  equ     *
        ;; 
        ;;* MEMORY EXAMINE AND CHANGE FUNCTION
        ;;
MEINST:
CHANGE	bsr	BADDR           ; (L 768A - $8D $44)
CHANGE0:jsr	CRLF            ; (L 7717 - L 7646:)
	ldx	#BADDRH         ; ($ 7709)
	jsr	OUT4HS          ; (L 76D6)
	ldx	BADDRH          ; (X 7709) njc
	jsr	OUT2HS          ; (L 76D8)
	jsr	OUTS            ; (L 76DA)
L7658:
CHANGE1:bsr	INEEEV          ; (L 76B5)
	cmpa	#$20            ; Space
	beq	CHANGE1         ; (L 7658)
	cmpa	#$0D            ; CR
	beq	NXTCMDV         ; (L 7687)
	cmpa	#$5E            ; ^ (up arrow?)
	bne	CHANGE2         ; (L 766D)
	dex
	dex
	stx	BADDRH          ; (X 7709)
	bra	CHANGE0         ; (L 7646)
;
L766D:
CHANGE2:stx	X7709
	cmpa	#$30            ; 0
	bcs	CHANGE0         ; (L 7646)
	cmpa	#$46            ; F
	bhi	CHANGE0         ; (L 7646)
	bsr	CVTHEX          ; (L 76BA)
	bsr	BYTE1           ; (L 769A) @FIXME?
	dex
	staa	0,X
	cmpa	0,X
	beq	CHANGE0         ; (L 7646)
	ldaa	#$3F            ; ?
	bsr	OUEXIT          ; (L 76B2)
L7687:                          ; not Decimal or Hex
NXTCMDV:jmp	NXTCMD          ; (L 7594)
;
CRLFV:  
L768A:
	;; 
        ;; BADDR - Build Address
        ;; 
BADDR:  bsr	BYTE            ; Get the Hi byte (L 7698 - L 768A:)
	staa	BADDRH          ; Save the Hi byte (X 7709)
	bsr	BYTE            ; (L 7698)
	staa	BADDRL          ; Save the Lo byte (X 770A)
	ldx	BADDRH          ; (X 7709)
	rts
;
L7698:
BYTE:   bsr	INHEX           ; (L 76B8)
L769A:
BYTE1:  asla
	asla
	asla
	asla
	tab
	bsr	INHEX           ; (L 76B8)
	aba
	tab
	rts
;
; * OUTHL AND R - OUTPUT ONE HEX DIGIT
L76A4:
OUTHL:  lsra                    ;* OUTPUT LEFT DIGIT ENTRY
	lsra
	lsra
	lsra
L76A8:
OUTHR:  anda	#$0F            ;* OUTPUT HEX RIGHT DIGIT ENTRY
	adda	#$30            ;*
	cmpa	#$39
	bls	OUEXIT          ;* (L 76B2)
	adda	#$07
;
OUEXIT: jmp	OUTEEE          ; (L 7769 - L 76B2:)
        ;; 
L76B5:
INEEEV:	jmp	INEEE           ; (L 7733)
;
        ;;
        ;;
        ;; 
L76B8:
	;; 
        ;; Input
        ;; 
INHEX:  bsr	INEEEV          ; (L 76B5)
CVTHEX:	suba	#$30            ; A - $30 (0 - 9 $30 - $39, A - F $41 - $46) (L 76BA:) @FIXME
	bmi	NXTCMDV         ; Less than '0' (L 7687) @FIXME
	cmpa	#$09
	ble	INEXIT          ; <= '9' (Decimal - L 76CC)
	cmpa	#$11
	bmi	NXTCMDV         ; < 'A' (not Decimal or Hex - L 7687)
	cmpa	#$16
	bgt	NXTCMDV         ; > 'F' (not Decimal or Hex - L 7687)
	suba	#$07
INEXIT: rts                     ; 0-9 A-F -> $00 - $0F (L 76CC:)
;
        ;;
        ;; OUT2H  - Output Reg X as a 2 char Hex value
        ;; 
OUT2H:	ldaa	0,X           ; (L 76CD:)
	bsr	L76A4
	ldaa	0,X
	inx
	bra	OUTHR           ; (L 76A8)
;
; -[ Print Bytes to Hex ]-------------------------------------------------------
;
        ;;
        ;; OUT4HS - Output a 4byte hex string
        ;; 
OUT4HS:	bsr	OUT2H           ; (L 76CD - L 76D6:)
;
        ;;
        ;; OUT2HS - Output a 2byte hex string
        ;; 
OUT2HS:	bsr	OUT2H           ; (L 76CD - L 76D8:)
        ;;
        ;; OUTS - Output a space
        ;; 
OUTS:	ldaa	#SPACE          ; (L 76DA:)
	bra	OUEXIT          ; (L 76B2 -> jmp OUTEEE (L 7769 ))
;
; -[ Jump ]---------------------------------------------------------------------
;
        ;;
        ;; JU - JUMP to user program
        ;; Reg X contains jump address ?
        ;; 
JUINST: bsr	BADDR           ; Get the address and put it in X (L 768A)
	lds	#USTACK         ; $7FFF
	jsr	0,X		; JUMP to the User program (INFO: index jump)
	jmp	WARMST          ; (L 756A)
; ------------------------------------------------------------------------------
; -[ Storage: 76E8 - 7716 = 2E ]------------------------------------------------
; ------------------------------------------------------------------------------
;L76E8:  db	$00
;X76E9:  db	$00, $00
;X76EB:	db	$00
;
;X76EC:	                        ; ror	X4400           ; 76 44 00
;BRTMP:  rmb     1
;X76ED:  rmb     1
;X76EE:  rmb     1
        ;;
        ;; CP JMP table from FF0C to here
        ;; 
        ;; 4 Addrs (12 bytes) Format: <addr> <op> (2Bytes + 1Byte)
        ;; 
;xBKTAB  rmb     3               ; Addr op (L 76EF:)
;        rmb     3
;        rmb     3
;        rmb     3
;
;X76FB:	db	$00
;X76FC:	db	$00
;X76FD:	db	$00, $00
;X76FF:	db	$00, $00        ; On init this gets clr'd (2Byte)
;X7701:	db	$00             ; On init this gets clr'd (1Byte)
;X7702:	db	$75, $7B        ; Humbug+ str addr
;
        ;; FROMTO ADDR?
;X7704:	db      $0F             ; sei
;
;X7705:
        ;; FROMTO ADDR?
;xBEGA   db	$75             ; Start Address
;X7706:	db	$00
        ;;
        ;; hinzvc b  a   x    pc   sp
        ;; 111111 01 7F FB00 0000 0006
	;; 
USRSTK: ;; FROMTO ADDR?
;X7707:
;xENDA   db      $7F             ; End Address
;X7708:  db      $70
        ;;
        ;; BADDR - Address of User program
        ;; 
;X7709:
;BADDRH: db      $75             ; User by BADDR for 1st Byte of addr
;X770A:
;BADDRL: db	$00             ; Used by BADDR for 2nd Byte of addr
;
;X770B:	rmb     1               ; 770B - 7710
;X770C:	rmb     1
;X770D:  rmb     2
;X770F:
;xSP:     rmb     2
        ;; 
        ;;
        ;; 
        IFDEF   UNTRUE
;X7711:	stx	XFF8D
;
;	byt	$02
;
;	bra	PDATA
	ELSEIF
;X7711:
;TMPSTR: rmb     1               ;* Temp storage?
;X7712:  rmb     5
        ENDIF
; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------
OUTCH:
CRLF:
L7717:	pshx
	ldx	#CRLFST         ; ($771F)
	bsr	PDATA
	pulx
	rts
;
L771F:
CRLFST: fcc     '\r\n\0\0\4'    ;* CR, LF, NUL, NUL, EOT
        ;;
        ;;* PDATA - PRINT DATA STRING
        ;;
L7724
PDATA   psha                    ; (L 7724:)
NXTCHR  ldaa	0,X             ; Get Char (L 7725:)
	cmpa	#$04            ; EOT, End?
	beq	PDEXIT          ; (L 7731)
	jsr	OUTEEE          ; Humbug says OUTEEE (L 7769)
	inx                     ; Point to the Next Char
	bra	NXTCHR          ; Loop (L 7725)
;
L7731:
PDEXIT: pula                    ; (L 7731:)
	rts
;
;* INEEE - CHARACTER INPUT ROUTINE
;* This looks like INEEE
;
L7733:
INEEE:  pshx                    ; Next
	pshb
L7735:
INRPT:  bsr	INCH7           ; Get input char (L 7763)
	cmpa	#CTRLS          ; Is it ^S?
	beq	GOTCS           ; Yes (L 7740)
	bsr	OUTEEE          ; Echo (L 7769)
	pulb
	pulx
	rts                     ; And return
;
        ;; Control S Detected, Get and Interpret command
L7740:
GOTCS:  bsr	GETCMD          ; Do command (L 7744)
	bra	INRPT           ; Next (L 7735)
;
L7744:
GETCMD: bsr	INCH7           ; (L 7763)
	cmpa	#$4F            ; Upper case Oh 'O'
	bne	NOTO            ; (L 774E) Not Oh?
	com	X76FF
	rts
;
L774E:
NOTO:   cmpa	#$50            ; P
	bne	NOTP            ; Not P? (L 775B)
	com	X7701
	ldaa	#$0F            ; ^O Ctrl-Oh
	staa	X7704
	rts
;
L775B:
NOTP:   cmpa	#$03            ; CTRL-C
	beq	GEEXIT          ; (L 7760)
	rts
L7760:
GEEXIT: jmp	WARMST          ; (L 756A)
;
        ;; Console In (Keyboard)
        ;; Read one byte from the current device and return it in ACCA
INCH7:  ;                        ; Was INEEE (I was wrong)
L7763:	jsr	CNSLIN           ; Wasn't in humbug.inc (LF865)
	anda	#$7F             ; Knock off the hi-bit
	rts

        ;; * PDATA - PRINT DATA STRING
        ;; * INEEE - CHARACTER INPUT ROUTINE

OUTEEE  pshx                    ;* pshs x,b,a Save Registers L 7769 (Not PDATA)
	pshb
	psha
        ;;
        ;; Humbug09 doesn't have this (I think that's okay)
        ;; 
	jsr	KEYIN           ; GET INPUT CHARACTER (LF883 - From humbug.inc)
	beq	NOTEST          ; (L 777A)
	anda	#$7F
	cmpa	#$13            ; IS IT CONTROL-S?
	bne	NOTEST          ; YES (L 777A)
	jsr	GETCMD          ; (L 7744)
	;;
        ;; Control-S handler
        ;; 
L777A:
NOTEST: pula                    ; FINISHED TESTING FOR COMMAND
	psha                    ; SAVE AGAIN
        ;;
        ;;  Humbug09 checks for pause here, I think we have that above
        ;; 
	ldab	X7701
	beq	L77A1
	cmpa	#$0C            ; ^L
	bne	L778C
	ldab	#$0F            ; ^O
	stab	X7704
	bra	L77A1
;
L778C:	cmpa	#$0D            ; CR
	bne	L77A1
	dec	X7704
	bne	L77A1
	ldaa	#$0F            ; ^O
	staa	X7704
	jsr	INCH7           ; (L 7763)
	cmpa	#$03            ; ^C
	beq	L7760
L77A1:	pula
	bsr	L77AE
	ldab	X76FF
	beq	L77AB
	bsr	L77CD
L77AB:	pulb
	pulx
	rts
;
INCH8:  
L77AE:	pshx
	psha
	cmpa	#$0C            ; ^L FF
	bne	L77C7
	ldx	#$4000
	stx	$4280
	ldaa	#$60            ; ` (backtick)
L77BC:	staa	0,X
	inx
	cpx	#$4200
	bne	L77BC
	pula
	pulx
	rts
;
L77C7:
	jsr	PUTCHR          ; LF9C6 - from humbug.inc
	pula
	pulx
	rts
;
L77CD:
        ;; * Send character in ACCA to serial port printer
	pshx
	pshb
	psha
	jmp	PRTSER          ; (L F9D0)
;
L77D3:
RATST   FCC     "RATE? \4"
;
L77DA:
BAINST: ldx	#RATST          ; L77D3
	jsr	PDATA
	jsr	L768A
	pshx
	pula
	pulb
	ldx	#X7801
L77E9:
	cmpa	0,X
	bne	L77F3
	ldx	1,X
	stx	LPTBTD          ; X4223 - Printer baud rate delay (118)
	rts
;
L77F3:
	inx
	inx
	inx
	tst	0,X
	bne	L77E9
	ldx	#NOST           ; $7C01
	jsr	PDATA
	rts
;
	nop
;
	byt	$02
;
	cpx	3,X
;
	byt	$00
;
	bitb	$0600
	adr	$1200
	rti
;
	bcc	L780F
L780F:
	aba
	asla
;
	byt	$00
;
	sev
	ldaa	$0000
;
	byt	$03, $00
;
	;; AT - Analyze Tape (???)
L7817:  
ATINST: ldaa	#CLS
	jsr	OUTEEE          ; (L 7769)
        ;;
        ;; Video $4000 - $41FF = 512 or 16x32 (Lines * Columns)
        ;; $20/line
        ;; 16x32 - $10x$20
        ;;
        ;; This writes directly to the screen
        ;; 
	ldx	#$4120          ; Video mem? ($ 4120) - Line 10 Col 1
	stx	CURPOS          ; Cursor Position (CRSPTR $ 4280)
	jsr	SYNLDR          ; LFF4E - from humbug.inc
	ldx	#$4020          ; Video mem? ($ 4020) - Line  2 Col 1 X
	jsr	LFEB0           ; LFE80 - rts so it comes right back (???)
	beq	L784E
L782D:
	ldx	#ERRST          ; $7EA1
	jsr	PDATA
	jmp	L78C3
;
L7836:
BASST   fcc     "BASIC: \4"     ;* 8 char
L783E
DATST   fcc     "DATA:  \4"     ;* 8 char
L7846:
BINST   fcc     "BIN: \4"
L784E:
	ldab	$4028           ;* in Video RAM (0001) 1=BASIC, 2=DATA, 3=BIN
	aslb                    ;* x2           (0010)
L7852:
	aslb                    ;* x4           (0100)
	aslb                    ;* x8           (1000)
	ldx	#BASST          ;* (L 7836)
	abx                     ;* X <- B + X
	jsr	PDATA           ; (L 7724)
	ldaa	#$04
	staa	$4028
	ldx	#$4020
	jsr	PDATA           ; (L 7724)
	jsr	CRLF            ; (L 7717)
	ldx	#$402D
L786C:
	jsr	OUT4HS          ; (L 76D6)
	ldx	$402B
	stx	X7705
	ldx	$402D
	dex
	stx	USRSTK          ; X7707
	ldaa	#$2C
	jsr	OUTEEE
	ldaa	$402A
	staa	BRTMP           ; (X 76EC)
	jsr	SYNLDR          ; LFF4E
L788A:
	ldx	#$4020
	stx	LDSIZE          ; LOAD address for ML file; SIZE of Basic/Array file ($ 426C)
	jsr	LFEB6           ;* Load any block from cassette into RAM
	bne	L782D
	tst	$4275
	bmi	L78A7
	ldd	USRSTK          ; X7707
	addb	$4276
	adca	#$00
	std	USRSTK          ; X7707
	bra	L788A
;
L78A7:
	ldx	#USRSTK         ; $7707
	jsr	OUT4HS          ; (L 76D6)
	ldaa	#$2C
	jsr	OUTEEE
	ldx	#X7705
	jsr	OUT4HS          ; (L 76D6)
	tst	BRTMP           ; (X 76EC)
	beq	L78C3
	ldx	#ASCST
	jsr	PDATA
L78C3:
	jmp	L7594
L78C6:
ASCST:  fcc     "ASCII\4"
        ;;
        ;;  SA - Save to cassette
L78CC:
SAINST: jsr	FROMTO          ; (L7957)
	ldx	#X792D
	jsr	PDATA
	jsr	L768A
	stx	X426A
L78DB:
	ldx	#NAMEST
	jsr	PDATA
	ldx	#CFNSTR         ; $4257
	ldab	#$08
L78E6:
	jsr	INEEE           ; (L 7733)
	cmpa	#$08
	bne	L78F5
	cmpb	#$08
	beq	L78E6
	dex
	incb
	bra	L78E6
;
L78F5:
	cmpa	#$0D
	beq	L7909
	cmpa	#$30
	bcs	L78DB
	cmpa	#$5A
	bgt	L78DB
	staa	0,X
	inx
	decb
	bne	L78E6
	bra	L7911
;
L7909:
	ldaa	#$20            ; Space?
L790B:
	staa	0,X
	inx
	decb
	bne	L790B
L7911:
	ldx	X7705
	stx	CASBEG          ; X426F
	stx	LDSIZE          ; X426C
	ldx	USRSTK          ; X7707
	inx
	stx	CASEND          ; X4271
	ldaa	#$02
	staa	CFTYPE          ; X4267
        ;;* Evaluate the optional filename argument for CSAVE then write the Name Block to cassette.
        ;;* Also writes 1/2 second of silence and the leader for the first data block.
        ;;* The ASCII and Gaps flags in the Name Block are both cleared.
        ;;* Other fields in the Name Block must be setup before calling.
	jsr	LFC8E           ; parse filename argument if given, evaluate filename and write Name Block
	jsr	LFC5D           ; point X to first data byte then set start address for block write
	rts
;
L792D:  fcc     "\r\n  START"
L7936:
ADDRST fcc     " ADDR: \4"
;
NAMEST
L793E:  fcc     "\r\n  NAME: \4"
L7949:
WITHST  fcc     "WITH? \4"
L7950:
WHATST  fcc     "WHAT? \4"
L7957:

FROMTO  ldx	#FROMST         ; Print From ($799D) @FIXME
	jsr	PDATA           ; (L 7724)
	jsr	INEEE           ; (L 7733)
	cmpa	#CR             ; $0D
L7962:
	bne	L7967
	jmp	CRLF            ; (L 7717)
;
        ;;
        ;; Convert a hex string to bin
        ;; $30 - $46 (0 - F)
        ;; to
        ;; $00 - $0F)
        ;; 
L7967:  suba	#$30
	bmi	NXTHEX           ; Not a Valid hex (less than '0' - L 799A)L799A
	cmpa	#$09
	ble	L7979           ; 0 >= A >= 9 (0 - 9)
	cmpa	#$11            ; 'A' - $30
	bmi	L799A
	cmpa	#$16            ; 'F' - $30
	bgt	L799A
	suba	#$07
L7979:
DIGIT:
GOTONE: asla                    ; @FIXME
	asla
	asla
	asla
	tab
	jsr	INHEX           ; (L 76B8)
	aba
	staa	X7705
	jsr	L7698
	staa	X7706
	ldx	#TOSTR          ; Print TO ($79A4)
	jsr	PDATA           ; (L 7724)
	jsr	INHEX           ; (L 768A)
	stx	USRSTK          ; (X 7707)
	jmp	OUTS            ; (L 76DA)
;
L799A:
NXTHEX: ins                     ; db      "1"
        ins                     ; db      "1"
        rts                     ; db      "9"
;
L799D:
FROMST  fcc     " FROM \4"      ; L799D:
L79A4:
TOSTR:  fcc     " TO \4"        ;L79A4:
;
L79A9:
EXINST: ldx	#MAIN           ; $7500 COLDST
	stx	EXECJP          ; X421F
	ldx	RESET           ; XFFFE
	jmp	0,X           ; INFO: index jump (EXECJP)
;
        ;; DE
L79B4:  
DEINST: jsr	FROMTO          ; (L 7957)
	ldx	X7705
	stx	X7711
L79BD:
	bsr	L79CE
	ldaa	USRSTK          ; X7707
	ldab	X7708
	subb	X7712
	sbca	X7711
	bcc	L79BD
	rts
;
L79CE:
	jsr	L7717
	ldx	#X7711
	jsr	OUT4HS          ; (L 76D6)
	jsr	OUTS            ; (L 76DA)
	ldx	X7711
	ldaa	0,X
	staa	X76FB
	jsr	OUT2HS          ; (L 76D8)
L79E5:
	stx	X7711
	clrb
	ldaa	X76FB
	anda	#$BF
	cmpa	#$83            ;
	beq	L7A0B
	anda	#$FD
	cmpa	#$8C            ;
	beq	L7A0B
	ldaa	X76FB
	anda	#$F0
	cmpa	#$20
	beq	L7A0C
	cmpa	#$60
	bcs	L7A0D
	anda	#$30
	cmpa	#$30
	bne	L7A0C
L7A0B:
	incb
L7A0C:
	incb
L7A0D:
	stab	X76FC
	beq	L7A22
	dec	X76FC
	beq	L7A1C
	jsr	OUT4HS          ; (L 76D6)
	bra	L7A1F
;
L7A1C:
	jsr	OUT2HS          ; (L 76D8)
L7A1F:
	stx	X7711
L7A22:
	rts
;
; ------------------------------------------------------------------------------
	;; 
	;; BR - Add a Break Point - SET/RESET UP TO FOUR BPS
        ;; n aaaa
        ;; aaaa    op
        ;; op -> bp#n 
L7A23:
BRINST: bsr	BKNUM           ; GET NUMBER OF DESIRED BP (L 7A6A)
	stx	TMPSTR          ; Save BP# to tmp (what are we saving?)
	bsr	BERASE          ; GO ERASE OLD ONE (L 7A4C)
	ldx	#ADDRST         ; ($7936)
	jsr	PDATA           ; Print " Number? "(L 7724)
	jsr	BADDR           ; GET ADDRESS (L 768A)
	stx	BRTMP           ; Save Addr to tmp (X 76EC)
	ldab	0,X             ; Get op @X
	ldaa	#SWIINST        ; GET SWI INSTR ($3F)
	staa	0,X             ; Stow SWI @X
	ldx	TMPSTR          ; Get BP# from tmp (X 7711)
	ldaa	BRTMP           ; Get hi(Addr) from tmp (X 76EC)
	staa	0,X
	ldaa	BRTMP+1         ; Get lo(Addr) from tmp (X 76ED) ldd perhaps?
	staa	1,X
	stab	2,X
	rts
;
L7A4C:
BERASE: ldab	2,X             ; Get OP
	ldaa	0,X             ; Get Part of Address
	cmpa	#$FF            ; Was there an Address?
	beq	BEEXIT          ; No, Exit (L 7A5F)
	ldx	0,X             ; Yes, Get Addr of Break
	stab	0,X             ; Restore OP
	ldx	TMPSTR          ; (X 7711)
	ldaa	#$FF            ;
	staa	0,X             ; Erase BP Table Entry
L7A5F:
BEEXIT: rts                     ; and Return
                                ;
        ;;
        ;; BKNUM routine - Get # of Desired BP & Point to its location in
        ;; BKTAB
        ;; 
L7A60:
NUMST   fcc     " NUMBER: \4"
;
L7A6A:
BKNUM:  ldx	#NUMST         ; ($7A60)
	jsr	PDATA           ; (L 7724)
	jsr	INEEE           ; GET BP# (L 7733)
	suba	#$30            ; CONVERT FROM ASCII
	bmi	BKNUM1          ; (L 7A8D)
	beq	BKNUM1          ; (L 7A8D)
	cmpa	#$04
	bgt	BKNUM1          ; IF Greater Than 4 (L 7A8D)
	psha
	jsr	OUTS            ; (L 76DA)
	pula
	ldx	#BKTAB          ;* ($76EF)
L7A85:
BKLOOP: deca
	beq	BKEXIT          ; (L 7A8F)
	inx
	inx
	inx
	bra	BKLOOP          ; (L 7A85)
;
L7A8D:
;NXTCMD:
BKNUM1: ins
	ins
L7A8F:
BKEXIT: rts
; ------------------------------------------------------------------------------
        ;; 
        ;; BP -
        ;; 
L7A90:
BPINST: ldab	#$30            ; BP Number in ASCII '0'
	ldx	#BKTAB          ; ($ 76EF)
	stx	X7711           ;* Tmp storage ?
L7A98:
BPR1:   incb
	cmpb	#$35            ;* Stop at 5 BPs (isn't it actual 4?)
	bne	BPR2            ;* No Display BP1 - BP4 (L 7A9E)
	rts                     ;* RETURN WHEN DONE
;
L7A9E:
BPR2:   jsr	CRLF            ; Print CR/LF (L 7717)
	tba                     ; GET BP NUMBER
	jsr	OUTEEE          ; PRINT BP NUMBER (L 7769)
	ldx	X7711           ; GET BP ADDRESS
	ldaa	0,X           ; GET BP ADDRESS
	cmpa	#$FF            ; IS THERE ONE?
	bne	BPR3            ; YES, GO PRINT IT (L 7AB3)
	inx
	inx
        inx
	bra	BPR4            ; AND REPEAT (L 7ABF)
;
L7AB3:
BPR3:   jsr	OUTS            ; PRINT SPACE (L 76DA)
	ldx	X7711
	jsr	OUT4HS          ;* PRINT ADDRESS OF BP (L 76D6)
	jsr	OUT2HS          ;* PRINT OP CODE (L 76D8)
L7ABF:
BPR4:   stx	X7711
	bra	BPR1            ;* Next BP (L 7A98)
; ------------------------------------------------------------------------------
;
SWIHDLR:
L7AC4:
	sts	USRSTK          ; SAVE USER STACK PTR (X770F)
	tsx                     ;
	tst	6,X
	bne	L7ACE
	dec	5,X
L7ACE:
	dec	6,X
	sts	X770F           ; SAVE USER STACK PTR (X770F)
	lds	#STACK          ; RESET TO MON STACK - ($7FA0)
        ;; 
        ;; * 'RE' COMMAND - PRINT USER REGISTERS FROM STACK
	;;
REINST:	jsr	CRLF            ; (L 7717 - L 7AD6:)
	ldx	#REMSG          ; $Register Message $7B1B
	jsr	PDATA           ; (L 7724)
	jsr	CRLF            ; (L 7717)
	ldx	SP              ; (X 770F)
	ldab	1,X             ; GET CC REGISTER
	ldx	#$0006          ; SET COUNTER
	aslb                    ; MOVE NEXT BIT INTO CARRY
	aslb                    ;
L7AEC:
RELOOP: 
	aslb                    ;
	ldaa	#$30            ;
	adca	#$00            ; CONVERT TO ASCII
	jsr	OUTEEE          ; PRINT IT (L 7769 - OUTEEE)
	dex                     ; BUMP COUNTER
	bne	RELOOP          ; PRINT NEXT BIT (L7AEC)
	jsr	OUTS            ; PRINT SPACE (L76DA)
	ldx	X770F           ; POINT TO USER STACK AGAIN (X770F)
	inx                     ; POINT TO A ACCUMULATOR
	inx
	jsr	OUT2HS          ; OUT2HS PRINT A (L76D8)
	jsr	OUT2HS          ; PRINT B
	jsr	OUT4HS          ; PRINT X INDEX (L76D6)
	jsr	OUT2HS          ; PRINT PC
	ldd	X770F           ; (X770F) Get the current USER stack
    IFNDEF ORIG
        ;;* Cam't make this a macro
        ;;* ASL can't break a #$0007 into # 00 07
        addb    #$07            ;*
        adca    #$00            ;* Change back to value it had in USER PGM
    ELSE
	addd	#$0007          ; RESTORE USER SP
    ENDIF
	pshb                    ; TO VALUE IT HAD
	psha                    ; TO VALUE IT HAD
	tsx                     ; 
	jsr	OUT2HS          ; OUT4HS PRINT SP
	pulx                    ; FIX SP
	jmp	NXTCMD          ; NXTCMD AND RETURN (L7594)
;
L7B1B:
REMSG:  fcc     "hinzvc b  a   x    pc   sp\4"
L7B36:
RCINST: ldx	#REGST          ;* Register change ($7B5B)
	jsr	PDATA
	jsr	INEEE           ; (L 7733)
	ldab	#$01
	ldx	#WTFST          ; ($7B61)
L7B44:
	cmpa	0,X
	beq	L7B51
L7B48:
	inx
	incb
	cmpb	#$07
L7B4C:
	bne	L7B44
L7B4E:
	jmp	L75D5
;
L7B51:
	ldx	USRSTK          ; (X 770F)
	abx                     ; X <- B + X Probably need inx and a ldx/stx SAVEX
	stx	BADDRH          ; (X 7709) XHI/XLO
	jmp	CHANGE0         ; (L 7646)
;
L7B5B
REGST   fcc     "REG: \4"
                                ;
        ;;
        ;; This needs some clean up, not sure what
        ;; this is for but nothing calls it
        ;; 
    IFNDEF ORIG
L7B61
WTFST   fcc     'CBXX@'         ;* ???
    ELSE
L7B61   coma                    ; db $43 C
	byt	$42, $41        ; db $42, $41 B A
	aslb                    ; db $58 X
L7B65	aslb                    ; db $58 X
	negb                    ; db $40 @
    ENDIF
L7B67
COINST: lds	USRSTK          ; X770F
	rti                     ; $3B
;
L7B6B:  nop
L7B6C:
SFRMST  fcc     "START FROM ADDRESS: \4"
;
L7B81:
STINST: ldx	#SFRMST         ; $7B6C
	jsr	PDATA
	jsr	L768A
	stx	X7FFC
	ldx	#WARMST         ; $756A
	stx	X7FFE
	ldx	#X7FF6          ; $7FF6
	stx	USRSTK          ; X770F
L7B99:  
SSINST: ldx	USRSTK          ; X770F
	ldx	6,X
	stx	X76FD
	stx	X7711
	jsr	L79CE
	stx	X76EC
	ldaa	0,X
	staa	X76EE
	ldaa	#$3F            ; ?
	staa	0,X
	cmpa	0,X
	bne	L7BF0
	ldaa	X76FB
	cmpa	#$20            ; Space?
	bcs	L7BC2
	cmpa	#$30            ; 0
	bcs	L7C34
L7BC2:
	cmpa	#$39            ; 9
	bne	L7BC9
	jmp	L7C7E
;
L7BC9:
	cmpa	#$3B            ; ';'
	beq	L7BF0
	cmpa	#$3F            ; '?'
	beq	L7BF0
	cmpa	#$6E            ; 'n'
	bne	L7BD8
L7BD5:
	jmp	L7C6D
;
L7BD8:
	cmpa	#$AD
	beq	L7BD5
	cmpa	#$7E
	beq	L7C5B
	cmpa	#$BD
	beq	L7C5B
	cmpa	#$8D
	beq	L7C34
	cmpa	#$9D
	beq	L7C62
	cmpa	#$3E
	bne	L7C05
L7BF0:
	ldx	#NOST           ; $7C01
	jsr	PDATA
	ldx	X76EC
	ldaa	X76EE
	staa	0,X
	jmp	L7594
;
L7C01:
NOST    fcc     "NO!\4"
L7C05:
	ldaa	#$FF
	staa	X76E9
L7C0A:
	ldx	#L7C14
	stx	$4210
	lds	USRSTK          ; X770F
	rti
; Is this L7C14?
L7C14	ldx	SWIHDLR         ; #$7AC4
	stx	X4210
	ldx	X76EC
	ldaa	X76EE
	staa	0,X
	ldaa	X76E9
	cmpa	#$FF
	beq	L7C31
	ldx	X76E9
	ldaa	X76EB
	staa	0,X
L7C31:
	jmp	SWIHDLR         ; L7AC4
;
L7C34:
	ldx	X76FD
	ldab	1,X
	beq	L7C41
	bmi	L7C55
L7C3D:
	inx
	decb
	bne	L7C3D
L7C41:
	inx
	inx
L7C43:
	stx	X76E9
	ldaa	0,X
	staa	X76EB
	ldaa	#$3F
	staa	0,X
	cmpa	0,X
	beq	L7C0A
	bra	L7BF0
;
L7C55:
	dex
	incb
	bne	L7C55
	bra	L7C41
;
L7C5B:
	ldx	X76FD
	ldx	1,X
	bra	L7C43
;
L7C62:
	ldx	X76FD
	ldab	1,X
	ldx	#$00
	abx
	bra	L7C43
;
L7C6D:
	ldx	X76FD
	ldab	1,X
	ldx	USRSTK          ; X770F
	ldx	4,X
	dex
	dex
	tstb
	beq	L7C41
	bra	L7C3D
;
L7C7E:
	ldx	USRSTK          ; X770F
	ldx	8,X
	bra	L7C43
                                ;
        ;; AD
        ;; HD
L7C85:
HDINST: 
ADINST: jsr	FROMTO          ;* GET ADDRESSES (L 7957)
	ldx	BEGA            ;* GET STARTING ADDRESS (X 7705)
	stx	X7711
	stx	X770B
	bra	L7C96
;
	staa	X7712
L7C96:
	jsr	CRLF            ; (L 7717)
	ldx	#X7711
	jsr	OUT4HS          ; (L 76D6)
	tst	OUTS            ; (X 76DA)
	ldab	#$04
	ldx	X7711
L7CA7:
	jsr	OUT2H           ; (L 76CD)
	decb
	bne	L7CA7
	stx	X7711
	jsr	OUTS            ; (L 76DA)
	ldab	#EOT            ; $04
	ldx	X7711
L7CB8:
	jsr	OUT2H           ; (L 76CD)
	decb
	bne	L7CB8
	stx	X7711
	bra	L7CCE
;
	ldaa	X770C
	anda	#$F0
	staa	X770C
	jsr	L7717
L7CCE:
	ldx	#X770B
	tst	OUT4HS          ; (X 76D6)
	jsr	OUTS            ; (L 76DA)
	ldab	#CTRLH          ; $08
	ldx	X770B
L7CDC:
	ldaa	0,X
	dex
	cpx	USRSTK          ; X7707
	bne	L7CE5
	rts
;
L7CE5:
	inx
	inx
	stx	X770B
	anda	#$7F
	cmpa	#$7F            ; DEL?
	beq	L7CF4
	cmpa	#$20            ; Space?
	bcc	L7CF6
L7CF4:
	ldaa	#$2E
L7CF6:
	jsr	OUTEEE
	decb
	bne	L7CDC
	bra	L7C96
;
MCINST: ldx	#COMPST         ;* $7D60
	jsr	PDATA
	jsr	FROMTO          ; L 7957
	jsr	CRLF            ; L7717
	ldx	X7705
	stx	X770B
	jsr	CRLF            ; L7717
	ldx	#WITHST
	jsr	PDATA
	jsr	L768A
	stx	X770D
	jsr	CRLF            ; (L 7717)
L7D22:
	ldx	X770B
	ldaa	0,X
	ldx	X770D
	ldab	0,X
	cba
	beq	L7D4A
	jsr	CRLF            ; (L 7717)
	ldx	#X770B
	jsr	OUT4HS          ; (L 76D6)
	ldx	X770B
	jsr	OUT2HS          ; (L 76D8)
	ldx	#X770D
	jsr	OUT4HS          ; (L 76D6)
	ldx	X770D
	jsr	OUT2HS          ; (L 76D8)
L7D4A:
	ldx	X770B
	inx
	stx	X770B
	cpx	USRSTK          ; X7707
	beq	L7D5F
	ldx	X770D
	inx
	stx	X770D
	bra	L7D22
L7D5F:
	rts
;
L7D60:
COMPST  fcc     "COMPARE:\4"
L7D69:
MTINST: jsr	FROMTO          ; (L7957)
	ldx	X7705
L7D6F:
	ldaa	0,X
	staa	X770B
	ldaa	#$01
	staa	0,X
	cmpa	0,X
	bne	L7D89
L7D7C:
	asla
	staa	0,X
	cmpa	0,X
	bne	L7D89
	cmpa	#$80
	bne	L7D7C
	bra	L7D9F
;
L7D89:
	pshx
	psha
	jsr	L7717
	tsx
	inx
	jsr	OUT4HS          ; (L 76D6)
	tsx
	jsr	OUT2HS          ; (L 76D8)
	tsx
	ldx	1,X
	jsr	OUT2HS          ; (L 76D8)
	pula
	pulx
L7D9F:
	ldaa	X770B
	staa	0,X
	cpx	USRSTK          ; X7707
	beq	L7DAC
	inx
	bra	L7D6F
;
L7DAC:
	ldx	#OKST           ; X7DB2
	jmp	PDATA
;
L7DB2
OKST	fcc     "OK\4"
L7DB5:
FIINST: ldx	#MANYST         ; X7E33
	jsr	PDATA
	jsr	INEEE           ; (L 7733)
	suba	#$30
	beq	L7E30
	bmi	L7E30
	cmpa	#$03
	bgt	L7E30
	staa	X76E9
	jsr	OUTS            ; (L 76DA)
	ldx	#WHATST         ; X7950
	jsr	PDATA
	ldab	X76E9
	ldx	#X76EC
L7DDA:
	pshb
	jsr	L7698
	pulb
	staa	0,X
	inx
	decb
	bne	L7DDA
	jsr	L7717
	jsr	FROMTO          ; (L7957)
	ldx	X7705
L7DEE:
	ldab	X76E9
	ldaa	0,X
	cmpa	X76EC
	bne	L7E28
	decb
	beq	L7E0C
	ldaa	1,X
	cmpa	X76ED
	bne	L7E28
	decb
	beq	L7E0C
	ldaa	2,X
	cmpa	X76EE
	bne	L7E28
L7E0C:
	stx	X7711
	bsr	L7E30
	ldx	#X7711
	bsr	L7E74
	jsr	OUTS            ; (L 76DA)
	ldx	X7711
	dex
	ldab	#$04
L7E1F:
	jsr	OUT2HS          ; (L 76D8)
	decb
	bne	L7E1F
	ldx	X7711
L7E28:
	cpx	USRSTK          ; X7707
	beq	L7E30
	inx
	bra	L7DEE
L7E30:
	jmp	L7717
;
L7E33
MANYST  fcc     "HOW MANY? \4"
	;; 
L7E3E:
FMINST: jsr	FROMTO          ; L7957
	ldx	#WITHST         ; $7949
	jsr	PDATA
	jsr	L7698
	ldx	X7705
	dex
L7E4E:
	inx
	staa	0,X
	cpx	USRSTK          ; X7707
	bne	L7E4E
	rts
;
	;; CS
L7E57:  
CSINST: jsr	FROMTO          ; L7957
	ldx	X7705
	clra
	clrb
L7E5F:
	addb	0,X
	adca	#$00
	cpx	USRSTK          ; X7707
	beq	L7E6B
	inx
	bra	L7E5F
;
L7E6B:
	staa	X7711
	stab	X7712
	ldx	#X7711
L7E74:
	jmp	OUT4HS          ; L76D6
;
	;; AI
L7E77:  
AIINST: jsr	FROMTO          ; L7957
	jsr	CRLF            ; L7717
	ldx	USRSTK          ; X7707
	stx	X7711
	ldx	X7705
	dex
L7E87:
	inx
	jsr	INEEE           ; (L 7733)
	staa	0,X
	cmpa	0,X
	bne	L7E99
	stx	USRSTK          ; X7707
	cpx	X7711
	bne	L7E87
L7E99:
	ldx	#ERRST          ; $7EA1
	jsr	PDATA
	bra	L7E99

ERRST   fcc     " ERROR\4"      ; L7EA1
	;;
	;; AO - ASCII OUTPUT ROUTINE
        ;;
L7EA8:
AOINST: jsr	FROMTO          ;* GET ADDRESS RANGE (L 7957)
	jsr	CRLF            ;* (L 7717)
	ldx	BEGA            ;* GET STARTING ADDRESS (X 7705)
L7EB1:
AOINST1:ldaa	0,X           ;* GET NEXT CHARACTER
	jsr	OUTEEE          ;* OUTPUT IT (L 7769)
	cpx	ENDA            ;* SEE IF DONE (USRSTK - X 7707)
	beq	AOINST1         ;* YES (L 7EBE)
	inx
	bra	AOEXIT          ;* REPEAT IF NOT (L 7EB1)
L7EBE:
AOEXIT: rts                     ;* WHEN DONE
;
OLDST   fcc     "OLD ADDR:\4"
NEWST   fcc     "NEW ADDR:\4"
;
MMINST
    IFNDEF ORIG
        warning "MOVE needs 6800 code tested"

MOVE    ldx     #OLDST          ;*
        jsr     PDATA           ;* Ask for old addresses
        jsr     FROMTO          ;*
        jsr     CRLF            ;*
        ldx     #NEWST          ;*
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
        cpx     ENDA            ;* Check for the end
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
    ELSE
MOVE    ldx	#OLDST
	jsr	PDATA           ; (L 7724)
	jsr	FROMTO          ; (L 7957)
	jsr	CRLF            ; (L 7717)
	ldx	#NEWST
	jsr	PDATA           ; (L 7724)
	jsr	BADDR           ; (L 768A)
	stx	NEWLOC          ; (X 770D)
; Now check for forward move or backward move
	ldx	X7705           ; humbug-mc.asm does this different, steal that
	stx	X770B
	cpx	X770D
	bcs	BWARD
	bne	FWARD
NEXIT	rts
; Forward move
FWARD	ldx	X770B
	cpx	USRSTK          ; X7707
	bhi	NEXIT
	ldaa	0,X             ;
FWD1	inx
	stx	X770B
	ldx	X770D           ;
	staa	0,X             ;
	inx                     ; Bump to pointer
FWD2	stx	NEWLOC          ; (X 770D)
	bra	FWARD           ; (L 7EFB)
;
BWARD	ldd	USRSTK          ; X7707
	subd	X7705           ;
	addd	X770D           ;
	std	X770D           ;
	ldx	USRSTK          ; X7707
	stx	X770B
BWD1	ldx	X770B
	cpx	X7705
	bcs	NEXIT
	ldaa	0,X
	dex
	stx	X770B
	ldx	X770D
	staa	0,X
	dex
	stx	X770D
	ldx	X7705
	bne	BWD1
	dex                     ; Bump to pointer
	cpx	X770B
	bne	BWD             ; (L 7F26)
	bra	NEXIT           ; (L 7EFA)
    ENDIF
;
        ;; * 'HE' - HELP COMMAND
HEINST: jsr	CRLF            ;* (L 7717)
	ldx	#COMTAB         ;* ($ 75DC)
L7F50:
HLOOP1: ldab	#$0A            ;* Set the counter (Items/line)
L7F52:
HLOOP2: ldaa	0,X             ;* Get the command (L1)
	jsr	OUTEEE          ;* (L 7769)
	ldaa	1,X             ;* (L2)
	jsr	OUTEEE          ;* (L 7769)
	jsr	OUTS            ;* (L 76DA)
        ;;
        ;; 'xx'   <- 2 letter command
        ;; XXINST <- 2 Byte address
        ;; 
	inx                     ;* Past the cmd
	inx                     ;*
	inx                     ;* Past the address
	inx                     ;*
	cpx	#TABEND         ;* DONE? ($ 7644)
	bne	HLOOP3          ;* NO (L 7F69)
	rts                     ;* YES
;
L7F69:
HLOOP3: decb
	bne	HLOOP2          ;* (L 7F52)
	jsr	CRLF            ;* (L 7717)
	bra	HLOOP1          ;* (L 7F50)
ROMEND  EQU     *
;
        ;;
        ;; @FIXME: This needs a proper clean up with rmb
        ;; 
;L7F71:  fcc	"\0\0\0\0"
;L7F75   equ     *
;        org     $7FA0
;STACK   equ     *
;X7FFC   equ     $7FFC
;X7FFE   equ     $7FFE
;        org     $7FFF
;USTACK  equ     *
;STACK2  equ     *

ROMSIZE EQU     ROMEND - ROMBEG

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
