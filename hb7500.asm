; ===========================================================================
; This is the disassembly of the Peter Stark's Humbug+ monitor for the Tandy
; MC-10. While this was disassembled with the 6803 CPU option it does appear
; that this is basically the 6800 version of Humbug but for the MC10.
; ===========================================================================
;	include "bitfuncs.inc"
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
	MACEXP  on

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
;	X7702:	db	$75, $7B        ; Humbug+ str addr (@addr of Humbug... string)
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
        include "MC6800.inc"
;        include "motorola.inc"          ; Macros for things like fcc,db, etc.
;        include "humbug.inc"            ; This may need a lot of clean up
	include "hb.inc"

SPACE   equ     $20
CR      equ     $0D
LF      equ     $0A
EOT     equ     $04
CTRLH   equ     $08
CTRLS   equ     $13

SWIINST equ     $3F

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
	MACEXP  on
;	org	$7300           ;
	org	ARAM            ;
ASTART  equ     *
; =[ MIKBUG RAM ]============================================================
        ;;*
        ;;* MIKBUG alsos uses the 128 byte scratchpag RAM @ $A000 (MC6810)
        ;;*
;*
;* Mixing MIKBUG, Humbug & Humbug+ names, @FIXME: clean this up to be mikbug vars only
;*
IOV                             ;* mikbug name
IRQ     RMB     2               ;* A000 X7702 IRQ Handler address (Don't put Humbug... str here)
BEGA    RMB     2               ;* A002 X7705
ENDA    RMB     2               ;* A004 X7707
NIO                             ;*  mikbug name
NMI     RMB     2               ;* A006    09 NMI Handler address
SP      RMB     2               ;* A008    0B & 0C 770C User stack pointer
CKSM
CKSUM   RMB     1               ;* A00A Someone uses CJSUM with 3 byytes
BYTECT  RMB     1               ;* A00B
;
BADDRH                          ;* Humbug name
XHI     RMB     1               ;* A00D BADDRH
BADDRL                          ;* Humbug name
XLO     RMB     1               ;* A00E BADDRL
;
TEMP    RMB     1               ;* A00F CHAR COUNT (INADD)
TW      RMB     2               ;* A010 TEMP
MCONT   RMB     1               ;* A012 TEMP
XTEMP   RMB     2               ;* A013 X-REG TEMP STORAGE
; 
        org     ARAM+$48
PC      RMB     2               ;* A048 Program Counter for the GO command
        RMB     46              ;* 
STACK   RMB     1               ;* A07F USRSTK USRSTK2 SP SPTR ? @FIXME

AMEMSIZ EQU     * - ASTART

; $A07F - 6810 128 bytes
;* -[ ARAM Fini ]---------------------------------------------------------------

	org	DRAM            ;
DSTART  equ     *
;
; =[ Humbug RAM ]============================================================
;        ORG     $D000           ;*
P0STAT  RMB     1               ;* (D000) Port 0
P1STAT  RMB     1               ;* (D001) Port 1
    ifdef VIDEO
VSTAT   RMB     1               ;* (D002)
    endif
DSTAT   RMB     1               ;* (D003) not used
PASTAT  RMB     1               ;* (D004) Pause flag
;
P0RADD  RMB     2               ;* A00A    0D
P0RECH  RMB     1               ;* A00C    0F
;
INEEXR  RMB     2               ;* (D005) In X Register
OUTEXR  RMB     2               ;* (D007) Out X Register
RETADD  RMB     2               ;* (D009) x
PTRINC  RMB     1               ;* (D00B) x
KBDINZ  RMB     1               ;* (D00C) x
PAUCTR  RMB     1               ;* (D00D) Pause control
        RMB     18
USAVEX  RMB     2               ;* (D020) USAVEX was Save X
WHAT    RMB     3               ;* (D022) x
FINDNO  RMB     3               ;* (D025) x
POWUP   RMB     4               ;* (D028) x thru D02B Power up flags
SAVEX   RMB     2               ;* (D02C) Hmm, 2nd one
USERPC  RMB     2               ;* (D02E) x
NEXT    RMB     3               ;* (D030) x
BRANCH  RMB     3               ;* (D033) x
BKTAB   RMB     12              ;* (D036) Breakpoint able (4 bp - 3 bytes each)
NEWLOC  RMB     2               ;* (D042) x
INSTR   RMB     2               ;* (D044) x
COUNT   RMB     1               ;* (D046) x
SWIJMP  RMB     2               ;* A012 x77
;
        RMB     56              ;* x
USTKPTR
HSTACK  RMB     1               ;* (D07F) Top of RAM (6810 @ $D000)
;        RMB     32
;USTKPTR RMB     2               ;* Now $73A0
;        RMB     32
	
NXTTMP  RMB     2
        
DMIDSIZ EQU     * - DSTART

    IFDEF       MC10
        ORG     $4000

;* $4xxx addresses
X4000   RMB     2
X4020   RMB     2
X4028   RMB     1
X402A   RMB     1
X402B   RMB     2
X402D   RMB     2
X4120   RMB     2
X4200   RMB     2
X4210   RMB     2
X4257   RMB     2 		;* CFNSTR - Cassette file name
X426A   RMB     2               ;* X426A <- MC10 memory
X4275   RMB     2
X4276   RMB     1
    ENDIF

UNKWN   RMB     12              ;* No idea yet

USR1    RMB     2               ;* njc, my stuff
USR2    RMB     2               ;* njc, my stuff
;*
;* Temporary, until I figure out what these really are
;* These fall inside the code area (ROM) so you shouldn't write there
;*
X76E9   RMB     2               ;* $76E9 now $73C2
X76EB   RMB     1               ;* $76EB now $73C4
BRTMP
X76EC   RMB     1               ;* $76EC now $73C5
X76ED   RMB     1               ;* $76ED
X76EE   RMB     13              ;* $76EE
X76FB   RMB     1               ;* $76FB
X76FC   RMB     1               ;* $76FC
X76FD   RMB     2               ;* $76FD

X76FF   RMB     2               ;* $76FF Some vect (puts MAIN here #7500)
X7701   RMB     1               ;* $7701

COLDSTF RMB     2               ;* Will contain the HBUGSTR addr after COLDST was X7702
X7704   RMB     1               ;* ???
;X7705   RMB     1               ;* $7705 (Begin?)
;X7706   RMB     1               ;* $7706
;USRSTK2                         ;        now $73E0
;X7707   RMB     1               ;* $7707 (End?)
;X7708   RMB     1               ;* $7708
;X7709   RMB     1               ;* $7709 BADDR BADDRH
;X770A   RMB     1               ;* $770A       BADDRL
X770B   RMB     1               ;* $770B XTEMP SP?
X770C   RMB     1               ;* $770C Used by AD for From, Nope it's USTACk
X770D   RMB     2               ;* $770D
;xSP
;xUSTACK                          ;*                                      <- USTACK ($73E0 now)
;xUSRSTK
xX770F   RMB     2               ;* $770F USRSTK (which is also elsewhere)
TMPSTR                          ;        now $73EA
X7711   RMB     1               ;* $7711
X7712   RMB     1               ;* $7712

DUMMY   RMB     2
;* -----------------------------------------------------------------------------
;*
;* Variables in the middle of the source code?
;*
X7644   RMB     2               ;* $7644
X75D8   RMB     2               ;* $75D8
X7801   RMB     2               ;* $7801
X792D   RMB     2               ;* $792D

;*
;* Not sure what to do about these yet
;*
X7FF6   RMB     2               ;* $7FF6 STSP
X7FFC   RMB     2               ;* $7FFC STFROM
X7FFE   RMB     2               ;* $7FFE something with ST SS start (loads #WARMST here)

DMEMSIZ EQU     * - DSTART

;* -[ DRAM Fini ]---------------------------------------------------------------

;* -----------------------------------------------------------------------------
  IF ROM = $E100
        ORG     $E000
        message "With FILL at $E000"
        ;; Fill up to FFD0 with FF
        dc.b [(*+(ROM-*))&ROM-*]$ff
  ENDIF
;* -----------------------------------------------------------------------------
;	org	$7500           ;
	org	ROM             ;
ROMBEG
        ;; The 6800 Humbug is from the Kilobaud-Thoughts13.pdf
        ;;                           & Kilobaud-Thoughts14.pdf
        ;; But referenced HUMBUG09.TXT for some of this code.
        ;; 6800 has fewer instructions
        ;; 6803 is closer to the 6809 (no Y reg)
        ;;
        ;; lilbug has 12 vectors
        ;; humnug09, 13 vectors
        ;; humbug+, has 15 vectors
        if       mompass=1
        warning "JMP Table needs review"
        endif
;                               ;* 6803         6809
MAIN:	jmp	RESTART         ;* L7533  -  1  RESTRT
	jmp	NXTCMD          ;* NXTCMD  -  2  WARMST
	jmp	INEEE           ;* L7733  -  3  NXTCMD (NextCmd?)
	jmp	INCH7           ;* L7763  -  4  INEEE
	jmp	INHEX           ;* L76B8  -  5  BADDR
	jmp	BADDR           ;* L768A  -  6  OUTEEE (CRLFV (L 768A))
	jmp	OUTEEE          ;* OUTEEE -  7  PDATA
	jmp	INCH8           ;* L77AE  -  9  CRLF
        ;*
        ;* This sends a character(? String?) to the serial printer
        ;*
	jmp	L77CD           ;* L77CD  -  9  OUTS  @FIXME:
	jmp	PDATA           ;* PDATA  - 10  OUTHR
	jmp	OUTCH           ;* L7717  - 12  OUT2HS
	jmp	OUTS            ;* L76DA  - 13  OUT4HS
	jmp	OUTHR           ;* L76A8  - 14
	jmp	OUT2HS          ;* L76D8  - 15
	jmp	OUT4HS          ;* L76D6  - 16
        jmp     OUTS            ;* njc added
;       ;; ------------------------------------------------------------------
        ;; I think it is safe to assume that the above 6803 table is not
        ;; correct, it doesn't line up with the 6800 article or the 6809
        ;; vectors.
;       ;; ------------------------------------------------------------------
;
COLDST  clr	COLDSTF         ; !! jumps here (monitor reset)(X7702)
RESTART lds	#STACK          ;* $4EA0 $7FA0 (L7533)
        ;; Load the contents of SWIVC $7AC4 (SWI Vector?) into X
        ;; Store it into SWIADDR ($4210 - OS addr)
        ;; Add the JMP ($7E)
        ;; SWIVEC  $420F = $7E   = jmp
        ;; SWIADDR $4210 = $7AC4 = $7AC4
;;;
;;; Setup the initial vectors
;;; IRQ
;;; SWI
;;; NMI
;;; 
	ldx	#SWIHDLR        ;* *$7AC4 SWIVC ?
	stx	SWIJMP          ;* 
    ifdef       MC10
	stx	SWIADDR         ;* SWIADDR ($4210)
	ldaa	#JMPINST        ;* #7E = jmp
	staa	SWIVEC          ;* Hard coded as 420F (also in hb4400)
        ;; 
	ldx	#MAIN           ;* $7500
	stx	EXECJP          ;* $421F (EQU @FIXME) J-> MAIN J-> RESTART
        ;; 
	ldx	#X76FF          ;* Local memory now @$7396 (Stack?)
	clr	0,X             ;* $76FF = 0
	clr	1,X             ;* $7700 = 0
	clr	2,X             ;* $7701 = 0 (L 754E:)
    endif
;;;
;;;* Need to init the U1 & U2 here
;;;
        ldx     #WARMST         ;* Set both to WARMST as default
        stx     USR1            ;*
        stx     USR2            ;*
;;;
;;;* Need to init the U1 & U2 here
;;;
;       ldx     #USTKPTR        ;*
        ldx     #STACK          ;*
        stx     SP              ;*
;;;
;;;* Need to init the ACIA here
;;;
;	ldaa	#CLS            ;* FF (FormFeed - Clears the scr)
;	jsr	OUTEEE          ;* was PDATA (L 7769)
	ldx	#HBUGSTR        ;* $757B "HUMB..."
	cpx	COLDSTF         ;* Humbug not restart? Str already loaded? (X7702)
	beq	WARMST          ;* Yes? Then skip  (L 756A)
;;;
;;;* Fill the BKTAB with FFFF (Break tables 4 breakpoints x 3 bytes)
;;; 
	ldx	#BKTAB          ;* Break Table (4x3 bytes)
	ldaa	#$FF            ;* Fill value
	ldab	#$0C            ;* $0C bytes (12 bytes)

FILL:   staa	0,X             ;* Fill 76EF -760D with $FF
	inx
	decb
	bne	FILL            ; (L 7564)
L756A:
WARMST: lds	#STACK          ; ($ 7FA0)
;*
;*	ACIA INITIALIZE
;*
	LDAA	#$03            ;* RESET ACIA CODE
	STAA	ACIACS
	NOP                     ;* Was 3 NOPs
	NOP                     ;* Was 3 NOPs
	NOP                     ;* Was 3 NOPs
	LDAA	#$15            ;* 8N1 NON-INTERRUPT
        STAA	ACIACS
;*
LOUT	LDAA	ACIACS
	ASRA
	ASRA
	BCC	LOUT
        ldaa    #'#'
        staa    ACIADA
	NOP                     ;* Was 3 NOPs
	NOP                     ;* Was 3 NOPs
	NOP                     ;* Was 3 NOPs
;*
        jsr     CLINST

	jsr	CRLF            ; (L 7717)
	ldx	#HBUGSTR
	stx	COLDSTF         ; (X7702)
        jsr	PDATA           ; (L 7724)
	;bra	NXTCMD          ; (L 7594)
        ;;
        ;; NXTCMD - return the 2 byte command
        ;; 
NXTCMD: lds	#STACK          ; I think this is the real NXTCMD (L 7594 $7FA0)
	jsr	CRLF            ; L7717

	ldaa	#$0F            ; What is this for??? Probably pause control PAUCTR (Missing)
;	staa	X7704           ; 
	staa	PAUCTR          ; 

	ldaa	#'*'            ; $3A = ':' $2A = '*' $23 = '#'
	jsr	OUTEEE          ; (L 7769)
	jsr	OUTS            ; Output a space
	jsr	INEEE           ; Get the first character aA 0110 0100
;*
;* Okay this is cheating! Only A-Z,a-z will work. no other characters
;*
        anda    #%11011111      ; Push to uppercase
	psha
	jsr     INEEE           ;
;*
;* Okay this is cheating! Only A-Z,a-z will work. no other characters
;*
NXT1    anda    #%11011111      ; L75AB: Push to uppercase
	tab
	jsr	OUTS            ; (L 76DA)
	pula
;	ldx	#TABEND-1       ; was X7644 now E255-1
;	stx	NXTTMP          ; (X76EC) now D084
;	stx	BKTAB+6         ; (X76EC) now D084
;*
;* @FIXME: Need to sub 4 so that it's at COMTAB when comparing
;*
;* COMTAB   = E1E1+4 E1E5
;* BKTAB[0] = E1E7          (D038)
;* BKTAB[4] = E203          (D03C)
;* BKTAB[6] = E25A          (D03E)
;* BKTAB    = E1E7 FFFF E203 E25A
;* E203 is "CO" COINST(why is it always E203 in last
;* BKTAB    = E1E7 FFFF E1EF E25A
	ldx	#COMTAB-4       ; was X75D8
;L75B9:
NXT2	inx                     ; A
	inx                     ; B
	inx                     ; addr
	inx                     ; X now point to the next 'xx' ldx 4,X?
;	cpx	NXTTMP          ; (X76EC) past TABEND? - [X(HI)] - [addr16], [X(LO)] - [addr16 + 1]
;	cpx	BKTAB+6         ; (X76EC) past TABEND? - [X(HI)] - [addr16], [X(LO)] - [addr16 + 1]
	cpx	#TABEND          ; (X76EC) past TABEND? - [X(HI)] - [addr16], [X(LO)] - [addr16 + 1]
	;beq	NXT3            ; L75D5 (Z == 1) ? {[PC]←[PC]+disp+2} - Branch if equal 0
        nop
	beq	NXT3            ; L75B9 (S⊻O == 0) ? {[PC]←[PC]+disp+2} - Branch if greater than or equal to zero
	cmpa	0,X
	bne	NXT2            ; L75B9

	cmpb	1,X
	bne	NXT2            ; L75B9
;* Match 
	jsr	OUTS            ; Yea, match! (L76DA) 
	inx                     ; Get past the  first char (H)
	inx                     ; Get past the second char (E)
	ldx	0,X             ; Get the address
	jsr	0,X             ; INFO: index jump to function@addr
	bra	NXTCMD          ; (L 7594)
; Not in the list               ; L75D5:
NXT3	ldaa	#'?'            ; 3F = '?'
	jsr	OUTEEE          ; (L 7769)
	bra	NXTCMD          ; (L 7594)
;
HBUGSTR fcc     "\rHUMBUG+(C) 1983 P. STARK" ; S447B:
VER     fcc     " 1.0.14\4"
;* -----------------------------------------------------------------------------
;
; Max # of commands in table: 64
;
;;; ============================================================================
;;;
;;; Implemented (currently)
;;; AD AI AO AT BA BP BR CO CS DE EX FI FM HD HE JU LO MC ME MM MT PU RC RE SA SS ST !!
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
COMTAB:                         ;* 75E6 - 764E (!! last command at 764C)
L75DC:
        FCC     "AD"            ; ASCII Dump
        FDB     ADINST          ; $7C85
        FCC     "AI"            ; ASCII Input
        FDB     AIINST          ; $7E77 - Last byte code, $4CFF ???
        FCC     "AO"            ; ASCII Output Oh not Zero
        FDB     AOINST          ; $7EA8
    IFDEF       MC10
        FCC     "AT"            ; Analyze Tape
        FDB     ATINST          ; $7817
    ENDIF
        FCC     "BA"            ; Change Baud
        FDB     BAINST          ; $77DA
        FCC     "BP"            ; Print Break points
        FDB     BPINST          ; $7A90
        FCC     "BR"            ; set/reset Breakpoints
        FDB     BRINST          ; $7A23
        FCC     "CO"            ; Continue (after a break)
        FDB     COINST          ; $7B67
        FCC     "CS"            ; Checksum
        FDB     CSINST          ; $7E57
        FCC     "DE"            ; Desemble (not Disassemble, only bytes)
        FDB     DEINST          ; $79B4
    IFDEF       MC10
L4504:  FCC     "EX"            ; Exit to BASIC
        FDB     EXINST          ; $79A9
    ENDIF
        FCC     "FI"            ; Find 1, 2 or 3 bytes
        FDB     FIINST          ; $7DB5
        FCC     "FM"            ; Fill Memory
        FDB     FMINST          ; $7E3E
        FCC     "GO"            ; 
        FDB     GOINST          ; 
        FCC     "HD"            ; Hex Dump
        FDB     HDINST          ; $7C85
        FCC     "HE"            ; Help
        FDB     HEINST          ; $7F4A
        FCC     "JU"            ; Jump (actually JSR)
        FDB     JUINST          ; $76DE
;
        FCC     "LO"
        FDB     LOAD            ; was COLDST
;
        FCC     "MC"            ; Memory Compare
        FDB     MCINST          ; $7CFE
        FCC     "ME"            ; Memory Examine
        FDB     MEINST          ; $7644
        FCC     "MM"            ; Memory Move
        FDB     MMINST          ; $7ED5
        FCC     "MT"            ; Memory Test
        FDB     MTINST          ; $7D69
;
        FCC     "PC"            ; Print PC (A048/9)
        FDB     PCINST
        FCC     "PU"
        FDB     PUNCH
;
        FCC     "RC"            ; Register Change
        FDB     RCINST          ; $7B36
        FCC     "RE"            ; Register Examine
        FDB     REINST          ; $7AD6
    IFDEF       MC10
        FCC     "SA"            ; CSAVEM to cassette
        FDB     SAINST          ; $78CC
    ENDIF
        FCC     "SS"            ; Single Step (lots of limitations, careful)
        FDB     SSINST          ; $7B99
        FCC     "ST"            ; Start SS (if no Breakpoint)
        FDB     STINST          ; $7B81
;;; 
        FCC     "XX"            ; Monitor Reset
        FDB     COLDST          ; $7530
;
        FCC     "CL"            ;* Clear the screen
        FDB     CLINST          ;*
        ;;
        ;; Extra User commands
        ;; 
        FCC     "UA"
        FDB     USR1V           ;
        ;; 
        FCC     "UB"
        FDB     USR2V           ;
        ;; 
;;; 
TABEND  equ     *
        ;; 
        ;;* MEMORY EXAMINE AND CHANGE FUNCTION
        ;;*
        ;;* CR exit the command
        ;;* ^ goes backward
        ;;* SPC does nothing
MEINST:
CHANGE	bsr	BADDR           ; Get the address (L 768A - $8D $44)
CHANGE0:jsr	CRLF            ; Output a CRLF   (L 7717 - L 7646:)
	ldx	#BADDRH         ; Hex Hi          ($ 7709)
	jsr	OUT4HS          ; Print it        (L 76D6)
	ldx	BADDRH          ; Hex Lo          (X 7709) njc
	jsr	OUT2HS          ; Print it        (L 76D8)
	jsr	OUTS            ; Output a SPC    (L 76DA)
L7658:
CHANGE1:bsr	INEEEV          ; Get input       (L 76B5)
	cmpa	#$20            ; Space
	beq	CHANGE2         ; Do nothing oops (L 7658)
	cmpa	#'+'            ; Space
	beq	CHANGE2         ; Do nothing oops (L 7658)
	cmpa	#'.'            ; Space
	beq	CHANGE2         ; Do nothing oops (L 7658)
	cmpa	#$0D            ; CR
	beq	NXTCMDV         ; exit command    (L 7687)
	cmpa	#'-'            ; ^ (up arrow)
	bne	CHANGE3         ; Make chage      (L 766D)
	dex                     ; Previous addr
        dex
CHANGE2	stx	BADDRH          ; (X 7709)
	bra	CHANGE0         ; (L 7646)
;
L766D:
CHANGE3 stx	BADDRH          ;* X7709
	cmpa	#'0'            ; 0
	bcs	CHANGE0         ; (L 7646)
	cmpa	#'F'            ; F
	bhi	CHANGE0         ; (L 7646)
	bsr	CVTHEX          ; (L 76BA)
	bsr	BYTE1           ; (L 769A) @FIXME?
	dex
	staa	0,X
	cmpa	0,X
	beq	CHANGE0         ; (L 7646)
	ldaa	#SWI            ; ? SWI = $3F
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
;L7698:
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
; Defined above
;NXTCMDV:jmp	NXTCMD          ; (L 7594)
;
        ;;
        ;; OUT2H  - Output Reg X as a 2 char Hex value
        ;; 
OUT2H   ldaa	0,X             ; (L 76CD)
	bsr	OUTHL           ; (L 76A4)
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
        ;; 
JUINST: bsr	BADDR           ; Get the address and put it in X (L 768A)
;	lds	#SP             ; $7FFF
	lds	SP              ; $7FFF
	jsr	0,X		; JUMP to the User program (INFO: index jump)
	jmp	WARMST          ; (L 756A)
; ------------------------------------------------------------------------------
;
; -[ Go ]-----------------------------------------------------------------------
;
        ;;
        ;; JU - JUMP to user program
        ;; 
GOINST: ;ldx     PC              ;
;	lds	#SP             ; $7FFF
	;lds	SP              ; $7FFF
	;jsr	0,X		; JUMP to the User program (INFO: index jump)
	jmp	WARMST          ; (L 756A)
;
; -[ PC ]-----------------------------------------------------------------------
;
        ;;
        ;; JU - JUMP to user program
        ;; 
PCINST: ;ldx     PC              ;
;	lds	#SP             ; $7FFF
	;lds	SP              ; $7FFF
	;jsr	0,X		; JUMP to the User program (INFO: index jump)
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
;COLDSTF db	$75, $7B        ; Humbug+ str addr (X7702)
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
;USRSTK: ;; FROMTO ADDR?
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
L7717:	;pshx
        stx     OUTEXR
	ldx	#CRLFST         ; ($771F)
	bsr	PDATA
	;pulx                    ;
        ldx     OUTEXR
	rts
;
L771F:
;CRLFST: fcc     '\r\n\0\0\4'    ;* CR, LF, NUL, NUL, EOT
CRLFST: fcc     '\n\r\4'        ;* CR, LF, NUL, NUL, EOT
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
; CR cancels
; 0  turns port 0 on/off
; 1  turns port 1 on/off
; P  turns pause on/off
; D  does the same for a user written port routine (?)
; ^S
; ^O
;
L7733:
INEEE:  ;pshx                    ; Next (savex it to SAVEX)
        stx     INEEXR
	pshb
L7735:
INRPT:  bsr	INCH7           ; Get input char (L 7763)
	cmpa	#CTRLS          ; Is it ^S?
	beq	GOTCS           ; Yes (L 7740)
	bsr	OUTEEE          ; Echo (L 7769)
	pulb
;	pulx
        ldx     INEEXR
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
	bne	NOTO            ; (L 774E) Not Oh? Oh No! ;-)
	com	X76FF
	rts
;
L774E:
NOTO:   cmpa	#'P'            ; $50 P
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
    IFDEF MC10
        ;; Console In (Keyboard)
        ;; Read one byte from the current device and return it in ACCA
INCH7:  ;                        ; Was INEEE (I was wrong)
L7763:	jsr	CNSLIN           ; Wasn't in humbug.inc (LF865)
	anda	#$7F             ; Knock off the hi-bit
	rts
    ELSE
;
;	SAVE X REGISTER
;SAV	STX	XTEMP
;	RTS
;
;
;	INPUT ONE CHAR INTO A-REGISTER
;INEEE
;	BSR	SAV
;IN1	LDAA	ACIACS
;	ASRA
;	BCC	IN1	;* RECEIVE NOT READY
;	LDAA	ACIADA	;* INPUT CHARACTER
;	ANDA	#$7F	;* RESET PARITY BIT
;	CMPA	#$7F
;	BEQ	IN1	;* IF RUBOUT, GET NEXT CHAR
;	BSR	OUTEEE
;	RTS
;
;
; This is for the ACIA new @WIP)
;
INCH7   stx     SAVEX           ; Save X
IN1     ldaa    ACIACS          ;
        asra                    ;
        bcc     IN1             ; Receiver not ready
        ldaa    ACIADA          ; Input the character
        anda    #$7F            ; Reset parity bit
        ;cmpa    #$7F            ; If rubout next char
        ;beq     IN1
        rts
    ENDIF
        ;; * PDATA - PRINT DATA STRING
        ;; * INEEE - CHARACTER INPUT ROUTINE

    IFDEF MC10
xOUTEEE  pshx                    ;* pshs x,b,a Save Registers L 7769 (Not PDATA)
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
	bsr	INCH8           ;* L77AE
	ldab	X76FF
	beq	L77AB
	bsr	L77CD           ;* cut out
L77AB:	pulb
	pulx
	rts
    ELSE
;*	OUTPUT ONE CHAR 
OUTEEE	psha
OUTEEE1	ldaa	ACIACS
	asra
	asra
	bcc	OUTEEE1
	pula
	staa 	ACIADA
	rts
; 
    ENDIF
;
INCH8
    IFDEF MC10
L77AE:	pshx                    ;#
	psha                    ;#
	cmpa	#$0C            ;# ^L FF
	bne	L77C7           ;#
	ldx	#X4000          ;# Video RAM
	stx	X4280           ;# OS Variable
	ldaa	#$60            ;#` (backtick)
L77BC:	staa	0,X             ;#
	inx                     ;#
	cpx	#X4200          ;# OS Variable
	bne	L77BC           ;#
	pula                    ;#
	pulx                    ;#
	rts                     ;#
;
L77C7:                          ;#
	jsr	PUTCHR          ;# LF9C6 - from humbug.inc
	pula                    ;#
	pulx                    ;#
	rts                     ;#
    ELSE
        ;; @WIP
;
; This is for the ACIA new @WIP)
;
        stx     XTEMP           ;* Save X
IN81    ldaa    ACIACS          ;*
        asra                    ;*
        bcc     IN81            ;* Receiver not ready
        ldaa    ACIADA          ;* Input the character
        ;cmpa    #$7F            ;* If rubout next char
        ;beq     IN1            ;*
        rts
    ENDIF
;
L77CD:
    IFDEF       MC10
        ;; * Send character in ACCA to serial port printer
	pshx
	pshb
	psha
	jmp	PRTSER          ; (L F9D0)
    ELSE
        jmp     WARMST
    endif
;
L77D3:
RATST   FCC     "RATE? \4"
;
L77DA:
BAINST: ldx	#RATST          ; L77D3
	jsr	PDATA           ;
	jsr	L768A           ;
    ifdef       MC10
	pshx                    ;
	pula                    ;
	pulb                    ;
	ldx	#X7801          ;
L77E9:                          ;
	cmpa	0,X             ;
	bne	L77F3           ;
	ldx	1,X             ;
	stx	LPTBTD          ; X4223 - Printer baud rate delay (118)
	rts                     ;
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
	nop                     ;
;                               ;
	byt	$02             ;
;                               ;
	cpx	3,X             ;
;                               ;
	byt	$00             ;
;                               ;
	bitb	$0600           ;
	adr	$1200           ;
	rti                     ;
;                               ;
	bcc	L780F           ;
L780F:                          ;
	aba                     ;
	asla                    ;
;                               ;
	byt	$00             ;
;                               ;
	sev                     ;
	ldaa	$0000           ;
;                               ;
	byt	$03, $00        ;
;
    else
        rts
    endif
;
;
;
    IFDEF       MC10
	;; AT - Analyze Tape (???)
L7817:                          ;
ATINST: ldaa	#CLS            ;
	jsr	OUTEEE          ; (L 7769)
        ;;
        ;; Video $4000 - $41FF = 512 or 16x32 (Lines * Columns)
        ;; $20/line
        ;; 16x32 - $10x$20
        ;;
        ;; This writes directly to the screen
        ;; 
	ldx	#X4120          ; Video mem? ($ 4120) - Line 10 Col 1
	stx	CURPOS          ; Cursor Position (CRSPTR $ 4280)
	jsr	SYNLDR          ; LFF4E - from humbug.inc
	ldx	#X4020          ; Video mem? ($ 4020) - Line  2 Col 1 X
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
	ldab	X4028           ;* in Video RAM (0001) 1=BASIC, 2=DATA, 3=BIN
	aslb                    ;* x2           (0010)
L7852:
	aslb                    ;* x4           (0100)
	aslb                    ;* x8           (1000)
	ldx	#BASST          ;* (L 7836)
	abx                     ;* X <- B + X
	jsr	PDATA           ; (L 7724)
	ldaa	#$04
	staa	X4028
	ldx	#X4020
	jsr	PDATA           ; (L 7724)
	jsr	CRLF            ; (L 7717)
	ldx	#X402D
L786C:
	jsr	OUT4HS          ; (L 76D6)
	ldx	X402B
	stx	BEGA            ;* X7705
	ldx	X402D
	dex
	stx	ENDA            ;* USRSTK2         ; X7707
	ldaa	#$2C
	jsr	OUTEEE
	ldaa	X402A
	staa	BRTMP           ; (X 76EC)
	jsr	SYNLDR          ; LFF4E
L788A:
	ldx	#X4020
	stx	LDSIZE          ; LOAD address for ML file; SIZE of Basic/Array file ($ 426C)
	jsr	LFEB6           ;* Load any block from cassette into RAM
	bne	L782D
	tst	X4275
	bmi	L78A7
	ldd	ENDA            ;* USRSTK2         ; X7707
	addb	X4276
	adca	#$00
	std	ENDA            ;* USRSTK2         ; X7707
	bra	L788A
;
L78A7:
;	ldx	#USRSTK2        ; $7707
	ldx	#ENDA           ; $7707
	jsr	OUT4HS          ; (L 76D6)
	ldaa	#$2C
	jsr	OUTEEE
	ldx	BEGA            ;* #X7705
	jsr	OUT4HS          ; (L 76D6)
	tst	BRTMP           ; (X 76EC)
	beq	L78C3
	ldx	#ASCST
	jsr	PDATA
L78C3:
	jmp	NXTCMD
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
	ldx	#CFNSTR         ; X4257
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
	ldx	BEGA            ;* X7705
	stx	CASBEG          ; X426F
	stx	LDSIZE          ; X426C
	ldx	ENDA            ;* USRSTK2         ; X7707
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
    ENDIF

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
L7957
;; 
;; Send out:
;;    'FROM '
;; Get 4 byte hex addr
;; load into BEGA
;; Send out:
;;    'TO '
;; Get 4 byte hex addr
;; load into ENDA
;; 
;* -[ From/To ]-----------------------------------------------------------------
FROMTO  ldx	#FROMST         ; Print From ($799D) @FIXME
	jsr	PDATA           ; (L 7724)
	jsr	INEEE           ; (L 7733)
	cmpa	#CR             ; $0D
;
FRTO1	bne	FRTO2
	jmp	CRLF            ; (L 7717)
;
        ;;
        ;; Convert a hex string to bin
        ;; $30 - $46 (0 - F)
        ;; to
        ;; $00 - $0F)
        ;; 
FRTO2   suba	#$30
	bmi	NXTHEX           ; Not a Valid hex (less than '0' - L 799A) L799A
	cmpa	#$09
	ble	GOTONE          ; 0 >= A >= 9 (0 - 9)
	cmpa	#$11            ; 'A' - $30
	bmi	NXTHEX
	cmpa	#$16            ; 'F' - $30
	bgt	NXTHEX
	suba	#$07
DIGIT 
GOTONE  asla                    ; @FIXME
	asla
	asla
	asla
	tab
	jsr	INHEX           ; (L 76B8)
	aba
    IFDEF ORIG
	staa	BEGA            ;* X7705           ; BEGIN+1
	jsr	BYTE
	staa	BEGA+1          ;* X7706           ; BEGIN
;
	ldx	#TOSTR          ; Print TO ($79A4)
	jsr	PDATA           ; (L 7724)
	jsr	INHEX           ; (L 768A)
	stx	ENDA            ;* USRSTK2         ; (X 7707) should be end?
    ELSE
	staa	BEGA            ; BEGIN+1 (was X7705)
	jsr	BYTE
	staa	BEGA+1          ; BEGIN   (was X7706)
;
;;;
;;;* This is where it breaks (B s 795B -> jsr OUTS
;;;* What happens is I get the 'TO ' I enter 1 and we hit the break
;;;* I need to have code that will pick up the 4 bytes
;;;
	ldx	#TOSTR          ; Print TO ($79A4)
	jsr	PDATA           ; (L 7724)
;;;
;;;*  INHEX?
;;;
;	jsr	INHEX           ; (L 768A)
	jsr	BADDR           ; (L 768A)
	stx	ENDA            ;* USRSTK2         ; (X 7707) should be end?
    ENDIF
	jmp	OUTS            ; (L 76DA)
;
NXTHEX  ins                     ; db      "1"
        ins                     ; db      "1"
        rts                     ; db      "9"
;
GOTTWO
;
FROMST  fcc     " FROM \4"      ; L799D:
TOSTR   fcc     " TO \4"        ;L79A4:

    IFDEF       MC10
;
;* -[ EX - Exit ]---------------------------------------------------------------
;
L79A9
EXINST  ldx	#MAIN           ; $7500 COLDST
	stx	EXECJP          ; X421F
	ldx	RESET           ; XFFFE
	jmp	0,X             ; INFO: index jump (EXECJP)
;
    ENDIF

        ;; DE
L79B4:  
DEINST: jsr	FROMTO          ; (L 7957)
	ldx	BEGA            ;* X7705
	stx	X7711
L79BD:
DE1	bsr	DE2             ;* L79CE
	ldaa	ENDA            ;* USRSTK2         ; X7707
	ldab	ENDA+1          ;* X7708
	subb	X7712           ; subd  X7711
	sbca	X7711
	bcc	DE1             ;* L79BD
	rts
;
L79CE:
DE2	jsr	CRLF            ;* L7717
	ldx	#X7711
	jsr	OUT4HS          ; (L 76D6)
	jsr	OUTS            ; (L 76DA)
	ldx	X7711
	ldaa	0,X
	staa	X76FB
	jsr	OUT2HS          ; (L 76D8)
L79E5:
DE3	stx	X7711
	clrb
	ldaa	X76FB
	anda	#$BF
	cmpa	#$83            ;
	beq	DE4             ;* L7A0B
	anda	#$FD
	cmpa	#$8C            ;
	beq	DE4             ;* L7A0B
	ldaa	X76FB
	anda	#$F0
	cmpa	#$20
	beq	DE5             ;* L7A0C
	cmpa	#$60
	bcs	DE6             ;* L7A0D
	anda	#$30
	cmpa	#$30
	bne	DE5             ;* L7A0C
L7A0B:
DE4	incb
L7A0C:
DE5	incb
L7A0D:
DE6	stab	X76FC
	beq	DEEXIT          ;* L7A22
	dec	X76FC
	beq	DE7             ;* L7A1C
	jsr	OUT4HS          ; (L 76D6)
	bra	DE8             ;* L7A1F
;
L7A1C:
DE7	jsr	OUT2HS          ; (L 76D8)
L7A1F:
DE8	stx	X7711
L7A22:
DEEXIT	rts
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
; -[ SWIHDLR ]------------------------------------------------------------------
;*
;* SWI Handler 
;*
;* Stack order (pushed)
;* 
;* [[SP]] ← [PC(LO)],
;* [[SP] - 1] ← [PC(HI)],
;* [[SP] - 2] ← [X(LO)],
;* [[SP] - 3] ← [X(HI)],
;* [[SP] - 4] ← [A],
;* [[SP] - 5] ← [B],
;* [[SP] - 6] ← [SR],
;* [SP] ← [SP] - 7,
;* [PC(HI)] ← [$FFFA],
;* [PC(LO)] ← [$FFFB]
;* 
    ifdef       OLD
L7AC4
SWIHDLR	sts	SP              ; SAVE USER STACK PTR (X770F)
	tsx                     ; X now points to the stack
	tst	6,X             ; Not sure what they're testing
	bne	SWIH1           ;* L7ACE
	dec	5,X

L7ACE
SWIH1	dec	6,X
	sts	SP              ;* SAVE USER STACK PTR (X770F)
	lds	#STACK          ; RESET TO MON STACK - ($7FA0)
    else
        ;*
        ;* There doesn't seem to be much of a handler, so I'm stealing
        ;* from other (mikbug) ROM monitors
        ;*
SWIHDLR
SFE	STS	SP	;* SAVE TARGET'S STACK POINTER
;*
;*	DECREMENT P-COUNTER
	TSX
	TST	6,X
	BNE	SFE1            ;* *+4
	DEC	5,X
SFE1	DEC	6,X
;*
;*	PRINT CONTENTS OF STACK
;*
PRINT	LDX	SP
	INX
	jsr	OUT2HS	;* CONDITION CODES
	jsr	OUT2HS	;* ACC-B
	jsr	OUT2HS	;* ACC-A
	jsr	OUT4HS	;* X-REG
	jsr	OUT4HS	;* P-COUNTER
	LDX	#SP
	jsr	OUT4HS	;* STACK POINTER
;C2	jmp	CONTRL
	jmp	NXTCMD
;
    endif
;* -[ REINST ]------------------------------------------------------------------
;*
;* REINST seems to working perfectly (not sure about SP)
;*
;* *SP = $73A0
;* 73A0 = 11 22 33 44 55 66 77
;*        CC A  B  X     PC
;*
;* SP doesn't appear to be part of that
;*
;* -----------------------------------------------------------------------------
        ;; 
        ;; * 'RE' COMMAND - PRINT USER REGISTERS FROM STACK
	;;
    ifndef      ORIG
REINST:	jsr	CRLF            ; (L 7717 - L 7AD6:)
	ldx	#REMSG          ; $Register Message $7B1B
	jsr	PDATA           ; (L 7724)
	jsr	CRLF            ; (L 7717)
	ldx	SP              ;* SP - (X 770F) SP SPTR USRSTK @FIXME now $73E8 (ptr to $73A0)
	ldab	1,X             ; GET CC REGISTER B = CC Reg
	ldx	#$0006          ; SET COUNTER
	aslb                    ; MOVE NEXT BIT INTO CARRY (11cvznih)
	aslb                    ; 
RELOOP: 
	aslb                    ; B<<3
	ldaa	#$30            ; Turn Carry bit into a '1' or '0'
	adca	#$00            ; CONVERT TO ASCII
	jsr	OUTEEE          ; PRINT IT (L 7769 - OUTEEE)
	dex                     ; BUMP COUNTER
	bne	RELOOP          ; PRINT NEXT BIT (L7AEC)
;*
	jsr	OUTS            ; PRINT SPACE (L76DA)
	ldx	SP              ;* USRSTK 		;* X770F           ; POINT TO USER STACK AGAIN (X770F)
	inx                     ; POINT TO A ACCUMULATOR
	inx
	jsr	OUT2HS          ; OUT2HS PRINT A (L76D8)
	jsr	OUT2HS          ; PRINT B
	jsr	OUT4HS          ; PRINT X INDEX (L76D6)
	jsr	OUT4HS          ; PRINT PC
;*
;* This is Wonky
;*
	ldd	SP              ;* (X770F) Get the current USER stack
    IFNDEF ORIG
        ;;* Cam't make this a macro
        ;;* ASL can't break a #$0007 into # 00 07
        addb    #$07            ;*
        adca    #$00            ;* Change back to value it had in USER PGM
    ELSE
	addd	#$0007          ; RESTORE USER SP
    ENDIF
;*
;* Is this kind of a fancy/sneeky pshx ?
;*
	pshb                    ; TO VALUE IT HAD
	psha                    ; TO VALUE IT HAD
	tsx                     ; 
	jsr	OUT4HS          ; OUT4HS PRINT SP
	;pulx                    ; FIX SP
	jmp	NXTCMD          ; NXTCMD AND RETURN (NXTCMD)

    else
REINST  bsr     PRREGS          ;*
	jmp	NXTCMD          ; NXTCMD AND RETURN (NXTCMD)
    endif

L7AEC
;* -----------------------------------------------------------------------------
;* 
;* Okay this is really messesd up. We're pointing to $FFFF
;* 
PRREGS	jsr	CRLF            ; (L 7717 - L 7AD6:)
	ldx	#REMSG          ; $Register Message $7B1B
	jsr	PDATA           ; (L 7724)
	jsr	CRLF            ; (L 7717)
;* 
;* Okay here's the problem. Our USTACK contains FFFF, we need it to pount to the
;* User Stack
;* 
	ldx	SP              ;* (X 770F) SP SPTR USRSTK @FIXME
;***
;*** This prints the bits of the CC Reg
;***
	ldab	1,X             ; GET CC REGISTER
	ldx	#$0006          ; SET COUNTER
	aslb                    ; Get rid of the top 2 bits (always 1)
	aslb                    ; MOVE NEXT BIT INTO CARRY
;* -----------------------------------------------------------------------------
;* 
RELOOP1	aslb                    ;                            ;
	ldaa	#$30            ;                            ;
	adca	#$00            ; CONVERT TO ASCII           ;
	jsr	OUTEEE          ; PRINT IT (L 7769 - OUTEEE) ;
	dex                     ; BUMP COUNTER               ;
	bne	RELOOP1         ; PRINT NEXT BIT (L7AEC)     ;
;
	jsr	OUTS            ; PRINT SPACE (L76DA)        ;
	ldx	SP              ;* POINT TO USER STACK AGAIN (X770F)
	inx                     ; POINT TO A ACCUMULATOR
	inx
	jsr	OUT2HS          ; PRINT A (L76D8)
	jsr	OUT2HS          ; PRINT B
	jsr	OUT4HS          ; PRINT X INDEX (L76D6)
	jsr	OUT4HS          ; PRINT PC
;*
;*
;*
	ldd	SP              ;* (X770F) Get the current USER stack ENDA
    IFNDEF ORIG
        ;;* Cam't make this a macro
        ;;* ASL can't break a #$0007 into # 00 07
        addb    #$07            ;*
        adca    #$00            ;* Change back to value it had in USER PGM
    ELSE
	addd	#$0007          ; RESTORE USER SP
    ENDIF
	std     SAVEX           ;
	pshb                    ; TO VALUE IT HAD
	psha                    ; TO VALUE IT HAD
	tsx                     ; 
	jsr	OUT4HS          ; OUT4HS PRINT SP
	pulx                    ; FIX SP
;*
;* I think I need to restore that stack, perhaps not ?
;*
	rts

L7B1B:
REMSG:  fcc     "hinzvc b  a  x    pc   sp\4"
;* -----------------------------------------------------------------------------
;*
;* 
L7B36:
;*
;*
;*
RCINST ;bsr     PRREGS          ;* I want the REGs to print here
        ldx	#REGST          ;* Register change ($7B5B)
	jsr	PDATA
	jsr	INEEE           ; (L 7733)
;*
	ldab	#$01
	ldx	#WTFST          ; ($7B61)
;L7B44                           ;
RC1	cmpa	0,X
	beq	RC5             ;* L7B51
;L7B48
RC2	inx
	incb
	cmpb	#$07
;L7B4C
RC3	bne	RC1             ;* L7B44
;L7B4E
RC4	jmp	NXT3            ;* L75D5
;
;L7B51
;* Need *SP in X
;* Need REG in B
;* Clear A ?
RC5	ldx	SP              ; (X 770F)
        nop
        clra
;
	abx                     ; X <- B + X (macro)
;
	stx	BADDRH          ; (X 7709) XHI/XLO
	jmp	CHANGE0         ; (L 7646)
;
REGST   fcc     "REG: \4"       ;* L7B5B
                                ;
        ;;
        ;; Registers, I've changed them but I'm not sure I'm correct
        ;;  Stack pull order
        ;; 
        ;; C - cc
        ;; B - B acc
        ;; A - A acc
        ;; X - X reg
        ;; P - PC reg
        ;; S - Stack pointer
        ;; 
WTFST   fcc     'CBAXPS'        ;* L7B61 db  $43. $42, $41, $58, $58 $40 = CBAXX@

;* -----------------------------------------------------------------------------
L7B67
COINST: lds	SP              ; X770F
	rti                     ; $3B
;
L7B6B:  nop
L7B6C:
SFRMST  fcc     "START FROM ADDRESS: \4"
;*
;* @FIXME: This needs major work! The Xxxx and Lxxx ops are many
;*
L7B81:
STINST: ldx	#SFRMST         ; $7B6C
	jsr	PDATA
	jsr	BADDR           ;* L768A
	stx	X7FFC
	ldx	#WARMST         ; $756A
	stx	X7FFE
	ldx	#X7FF6          ; $7FF6
	stx	SP              ; X770F
L7B99:  
SSINST: ldx	SP              ; X770F
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
	jmp	NXTCMD
;
L7C01:
NOST    fcc     "NO!\4"

;* -----------------------------------------------------------------------------
L7C05:
	ldaa	#$FF
	staa	X76E9
L7C0A:
	ldx	#L7C14          ;* Some kind of Interrupt handler
    IFDEF       MC10
	stx	X4210           ;*
    ENDIF
	lds	SP              ; X770F
	rti

;* -----------------------------------------------------------------------------
; Is this L7C14?
L7C14	ldx	SWIHDLR         ; #$7AC4
    IFDEF       MC10
	stx	X4210
    ENDIF
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
	ldx	SP              ; X770F
	ldx	4,X
	dex
	dex
	tstb
	beq	L7C41
	bra	L7C3D
;
L7C7E:
	ldx	SP              ; X770F
	ldx	8,X
	bra	L7C43
                                ;
        ;; HD
L7C85:
XDINST: 
HDINST: jsr	FROMTO          ;* GET ADDRESSES (L 7957)
	ldx	BEGA            ;* GET STARTING ADDRESS (X 7705)
	stx	SAVEX           ;* Art: SAVEX (X7711)
	stx	X770B           ;* Art: doesn't have this
	bra	HD1             ;* Art: has HEXCON ;* L7C96
;
	staa	SAVEX+1         ;* X7712
L7C96
HD1	jsr	CRLF            ; (L 7717)
	ldx	#SAVEX          ;* X7711
	jsr	OUT4HS          ; (L 76D6) Addr
	tst	OUTS            ; (X 76DA) Spaces
	ldab	#$04            ;* ???
	ldx	SAVEX           ;* X7711
L7CA7
HD2	jsr	OUT2H           ; (L 76CD)
	decb
	bne	HD2             ;* L7CA7
	stx	SAVEX           ;* X7711
	jsr	OUTS            ; (L 76DA)
	ldab	#EOT            ; $04
	ldx	SAVEX           ;* X7711
L7CB8:
HD3	jsr	OUT2H           ; (L 76CD)
	decb
	bne	HD3             ;* L7CB8
	stx	SAVEX           ;* X7711
	bra	HD4             ;* L7CCE
;
	ldaa	X770C
	anda	#$F0
	staa	X770C
	jsr	CRLF            ;* L7717 (also OUTCH ???)
L7CCE:
HD4	ldx	#X770B
	tst	OUT4HS          ; (X 76D6)
	jsr	OUTS            ; (L 76DA)
	ldab	#CTRLH          ; $08
	ldx	X770B
L7CDC:
HD5	ldaa	0,X
	dex
	cpx	ENDA            ;* USRSTK2         ; X7707
	bne	HD6             ;* L7CE5
	rts
;
L7CE5:
HD6	inx
	inx
	stx	X770B
	anda	#$7F
	cmpa	#$7F            ; DEL?
	beq	HD7             ;* L7CF4
	cmpa	#$20            ; Space?
	bcc	HD8             ;* L7CF6
L7CF4:
HD7	ldaa	#$2E            ;* $2E = dot
L7CF6:
HD8	jsr	OUTEEE
	decb
	bne	HD5             ;* L7CDC
	bra	HD1             ;* L7C96
        ;; AD

;* -[ AD - ASCII Dump ]---------------------------------------------------------
ADINST: jsr	FROMTO          ;* GET ADDRESSES (L 7957)
	ldx	BEGA            ;* GET STARTING ADDRESS (X 7705)
	stx	SAVEX           ;* Art: SAVEX (X7711)
	stx	X770B           ;* Art: doesn't have this
	bra	AD1             ;* Art: has HEXCON ;* L7C96
;
	staa	SAVEX+1         ;* X7712
;
AD1	jsr	CRLF            ; (L 7717)
	ldx	#SAVEX          ;* X7711
	jsr	OUT4HS          ; (L 76D6) Addr
	tst	OUTS            ; (X 76DA) Spaces
	ldab	#$08            ;* First half of the 16 (Hex)
	ldx	SAVEX           ;* X7711

AD2	jsr	OUT2H           ; (L 76CD)
	decb
	bne	AD2             ;* L7CA7
	stx	SAVEX           ;* X7711
	jsr	OUTS            ; (L 76DA)
	ldab	#$08            ;* Second half of the 16 (Hex)
	ldx	SAVEX           ;* X7711

AD3	jsr	OUT2H           ; (L 76CD)
	decb
	bne	AD3             ;* L7CB8
	stx	SAVEX           ;* X7711
	bra	AD4             ;* L7CCE
;
	ldaa	X770C
	anda	#$F0
	staa	X770C
	jsr	CRLF            ;* L7717 (also OUTCH ???)

AD4	ldx	#X770B
	tst	OUT4HS          ; (X 76D6)
	jsr	OUTS            ; (L 76DA)
	ldab	#16             ;* output 16 ASCII Chars
	ldx	X770B

AD5	ldaa	0,X
	dex
	cpx	ENDA            ;* USRSTK2         ; X7707
	bne	AD6             ;* L7CE5
	rts
;
AD6	inx
	inx
	stx	X770B
	anda	#$7F
	cmpa	#$7F            ; DEL?
	beq	AD7             ;* L7CF4
	cmpa	#$20            ; Space?
	bcc	AD8             ;* L7CF6

AD7	ldaa	#$2E            ;* $2E = dot

AD8	jsr	OUTEEE
	decb
	bne	AD5             ;* L7CDC
	bra	AD1             ;* L7C96
;* -----------------------------------------------------------------------------

; MCINST?
MCINST: ldx	#COMPST         ;* $7D60
	jsr	PDATA
	jsr	FROMTO          ; L 7957
	jsr	CRLF            ; L7717
	ldx	BEGA            ;* X7705
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
	cpx	ENDA            ;* USRSTK2         ; X7707
	beq	L7D5F
	ldx	X770D
	inx
	stx	X770D
	bra	L7D22
L7D5F
MCEXIT
	rts
;
L7D60:
COMPST  fcc     "COMPARE:\4"
L7D69:
MTINST: jsr	FROMTO          ; (L7957)
	ldx	BEGA            ;* X7705
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
;	cpx	USRSTK2         ; X7707
	cpx	ENDA            ; X7707
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
	jsr	BYTE
	pulb
	staa	0,X
	inx
	decb
	bne	L7DDA
	jsr	L7717
	jsr	FROMTO          ; (L7957)
	ldx	BEGA            ;* X7705
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
;	cpx	USRSTK2         ; X7707
	cpx	ENDA            ; X7707
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
	jsr	BYTE
	ldx	BEGA            ;* X7705
	dex
L7E4E:
	inx
	staa	0,X
	cpx	ENDA            ;* USRSTK2         ; X7707
	bne	L7E4E
	rts
;
;* -[ CS - Cheksum ]------------------------------------------------------------
	;; CS
L7E57:  
CSINST: jsr	FROMTO          ; L7957
	ldx	BEGA            ;* X7705
	clra
	clrb
L7E5F:
	addb	0,X
	adca	#$00
	cpx	ENDA            ;* USRSTK2         ; X7707 ENDA?
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
;* -[ S19 Checksum ]------------------------------------------------------------
;
; The check-sum is calculated by summing, in HEX, all of the 2-character bytes on
; the line starting wit the third and fourth characters (the length byte), and
; running up to the check-sum byte.
;
; The following example record:
;
; S1137AF00A0A0D0000000000000000000000000061
;   ^
; is decoded to show how the checksum value is calculated. The following example
; uses a dollar sign ($) to indicate a hexadecimal value (a Motorola convention):
;
;     Add: Add each byte $13 + $7A + $F0 + $0A + $0A + $0D + $00 + ... + $00 =
;          $019E sum.
;
;     Mask: Discard the most significant byte ($01) of the sum and retain the
;           least significant byte (LSB), which is $9E.
;
;     Complement: Compute the ones' complement of the LSB, which is $61.
;
; In the C programming language, the sum is converted into the checksum by:
;     0xFF - (sum & 0xFF)
;
;* -----------------------------------------------------------------------------

;* -----------------------------------------------------------------------------
	;; AI
L7E77:  
AIINST: jsr	FROMTO          ; L7957
	jsr	CRLF            ; L7717
	ldx	ENDA            ;* USRSTK2         ; X7707
	stx	X7711
	ldx	BEGA            ;* X7705
	dex
L7E87:
	inx
	jsr	INEEE           ; (L 7733)
	staa	0,X
	cmpa	0,X
	bne	L7E99
	stx	ENDA            ;* USRSTK2         ; X7707
	cpx	X7711
	bne	L7E87
        rts

L7E99:
	ldx	#ERRST          ; $7EA1
	jsr	PDATA
	rts
;	bra	L7E99

ERRST   fcc     " ERROR\4"      ; L7EA1
	;;
	;; AO - ASCII OUTPUT ROUTINE
        ;;
L7EA8:
AOINST: jsr	FROMTO          ;* GET ADDRESS RANGE (L 7957)
	jsr	CRLF            ;* (L 7717)
	ldx	BEGA            ;* GET STARTING ADDRESS (X 7705)
L7EB1:
AOINST1:ldaa	0,X             ;* GET NEXT CHARACTER
	jsr	OUTEEE          ;* OUTPUT IT (L 7769)
	inx                     ;*
	cpx	ENDA            ;* SEE IF DONE (USRSTK - X 7707)
	blt	AOINST1         ;* YES (L 7EBE)
	bra	AOEXIT          ;* REPEAT IF NOT (L 7EB1)
L7EBE:
AOEXIT: rts                     ;* WHEN DONE
;
OLDST   fcc     "OLD ADDR:\4"
NEWST   fcc     "NEW ADDR:\4"
;
MMINST
    IFNDEF ORIG
        if       mompass=1
        warning "MOVE needs 6800 code tested"
        endif

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
	ldx	BEGA            ;* X7705           ; humbug-mc.asm does this different, steal that
	stx	X770B
	cpx	X770D
	bcs	BWARD
	bne	FWARD
NEXIT	rts
; Forward move
FWARD	ldx	X770B
	cpx	ENDA            ;* USRSTK2         ; X7707
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
BWARD	ldd	ENDA            ; X7707
	subd	BEGA            ;* X7705           ;
	addd	X770D           ;
	std	X770D           ;
	ldx	ENDA            ; X7707
	stx	X770B
BWD1	ldx	X770B
	cpx	BEGA            ;* X7705
	bcs	NEXIT
	ldaa	0,X
	dex
	stx	X770B
	ldx	X770D
	staa	0,X
	dex
	stx	X770D
	ldx	BEGA            ;* X7705
	bne	BWD1
	dex                     ; Bump to pointer
	cpx	X770B
	bne	BWD             ; (L 7F26)
	bra	NEXIT           ; (L 7EFA)
    ENDIF
;
        ;; * 'HE' - HELP COMMAND
HEINST
HELP    jsr	CRLF            ;* (L 7717)
	ldx	#COMTAB         ;* ($ 75DC)
L7F50
    IFDEF ORIG
HLOOP1  ldab	#$0A            ;* Set the counter (Items/line)
    ELSE
HLOOP1  ldab    #(TABEND-COMTAB)/3
    ENDIF
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
	inx                     ;* Past the cmd      H
	inx                     ;*                   E
	inx                     ;* Past the address  $7F
	inx                     ;*                   $2C
	cpx	#TABEND         ;* DONE? ($ 7644)
	bne	HLOOP3          ;* NO (L 7F69)
	rts                     ;* YES
;
L7F69:
HLOOP3: decb
	bne	HLOOP2          ;* (L 7F52)
	jsr	CRLF            ;* (L 7717)
	bra	HLOOP1          ;* (L 7F50)
;
;
IRQV    ldx     #IRQ
        jmp     0,X
;
SWIV    ldx     SWIJMP          ;* SWI vector via A012
        jmp     0,X
;
NMIV    ldx     NMI             ;* NMI vector via A006
        jmp     0,X
;
USR1V   ldx     USR1            ;* 
        jmp     0,X
;
USR2V   ldx     USR2            ;* 
        jmp     0,X
;
;* -[ LO - LOAD ]---------------------------------------------------------------
;*
;*	L COMMAND
LOAD    JSR     CRLF
;*
;*	CHECK TYPE
LOAD3	JSR	INEEE           ;* Was INCH
	CMPA	#'S'
	BNE	LOAD3           ;* 1ST CHAR NOT (S)
	JSR	INEEE           ;* READ CHAR
	CMPA	#'9'
	BEQ	LDEXIT          ;* START ADDRESS
	CMPA	#'1'
	BNE	LOAD3           ;* 2ND CHAR NOT (1)
	CLR	CKSM            ;* ZERO CHECKSUM
	JSR	BYTE            ;* READ BYTE
	SUBA	#2
	STAA	BYTECT          ;* BYTE COUNT
;*
;*	BUILD ADDRESS
	JSR	BADDR
;*
;*	STORE DATA
LOAD11	JSR	BYTE
	DEC	BYTECT
	;BEQ	LOAD15          ;* ZERO BYTE COUNT
	BEQ	LOAD3           ;* ZERO BYTE COUNT
	STAA	0,X             ;* STORE DATA
	INX
	BRA	LOAD11
;*
;*	ZERO BYTE COUNT
LOAD15	INC	CKSM
	BEQ	LOAD3
LOAD19	LDAA	#'?'            ;* PRINT QUESTION MARK
	JSR	OUTCH
LDEXIT  nop
        rts                     ;* return control to NXTCMD
;LOAD21	JMP	NXTCMD          ;* CONTRL
;*

;
; Punch it! (WIP and not working)
;
; BEGA   - Starting address
; ENDA   - End address
; CKSM   - Checksum
; BYTECT - Byte count
;
; X      - Address
; A      - checksum
; B      - char count for line
;
; PDATA - String address in X, terminated with \4
; OUT2H - Output Reg X as a 2 char Hex value
;
PUINST
PUNCH   ldx     #PUNCHST        ;*
        jsr     PDATA           ;*
        jsr     FROMTO          ;*
        ldx     BEGA            ;* Get the starting address
        stx     USAVEX          ;*
        stx     X770B           ;* Not sure what this is but it's temp now XTEMP? SP?
        jsr     CRLF            ;*
;
        ldx     #S0ST           ;*
        jsr     PDATA           ;*
;*
;* S113xxxx\4
;
NXTADD  ldx     #S1ST           ;*
        jsr     PDATA           ;*
        ldaa    #$13            ;* Add count (to 0, so set count)
        adda    USAVEX          ;* Add Hi addr byte
        adda    USAVEX+1        ;* Add Lo addr byte
        ;;
        ;; Send address & incremment address by $10
        ;; and calculate the CKSM for the address
        ;;
        ldx     #USAVEX         ;* Get the Address
        psha
        jsr     OUT2H           ;* Output 4 Hex
        jsr     OUT2H           ;*
	pula
        nop
        ;;
        ;; Not we need to send 16 bytes as hex
        ;; And calculate the CKSM for all the data
        ;;
        ldab    #16             ;* Load rest of the count (count & addr hi+lo already done) 
NXTHX1  ldx     USAVEX          ;* Load the address
        adda    $00,X           ;* Add *addr (contents of) #FIXME
        psha                    ;* Save cksum (OUT2CH clobbers A)
        jsr     OUT2H           ;* Print *addr
        pula                    ;* Restore cksm
        stx     USAVEX          ;* Save it
        decb                    ;* Count down
        bne     NXTHX1          ;* until done with the line
        nop
        psha                    ;* Save cksm
        ;; Do maths - 0xFF - (sum & 0xFF) A = $FF, B = Cksm A = $FF - Cksm
        ldaa    #$FF            ;* A = $FF
        pulb                    ;* B = cksm
        sba                     ;* A = A($FF) - B(cksm)
        ;;
        ;; Output the CKSM as Hex
        ;;
        staa    WHAT            ;* Save the CheckSum (use WHAT a TEMP)
        ldx     #WHAT           ;* Load X the address of the Checksum
        jsr     OUT2H
        nop
        jsr     CRLF
        ;;
        ;; Check if we're done
        ;; 
;
        ldx     USAVEX          ;* [X(HI)] - data16(HI), [X(LO)] - data16(LO)
        cpx     ENDA
        bpl     PUEXIT          ;* Is USAVEX > ENDA ?
        bra     NXTADD          ;* No, then next addr
        nop                     ;* Yes, then we're done
;
PUEXIT  ldx     #S9ST
        jsr     PDATA           ;* Output a default S9 string
        rts                     ;*

PUNCHST FCC     "Punch \4"           ;*
S0ST    FCC     "S00B00005820\r\n\4" ;*
S1ST    FCC     "S113\4"             ;*
S9ST    FCC     "S9030000FC\r\n\4"   ;*

CLINST  ldaa    #CLS            ;* Ctrl-L
        jsr     OUTEEE          ;*
        rts

;* -----------------------------------------------------------------------------

ROMEND  EQU     *+8             ;* ROM length plus 4 Vectors

;* -----------------------------------------------------------------------------
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

        ORG     $FFF8

        FDB     IRQV            ;* IRQ vector
        FDB     SWIV            ;* SWI vector
        FDB     NMIV            ;* NMI vector
        FDB     COLDST          ;* Reset vector

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
;* X4350 - Top of RAM ($7500 - base of Humbug+)
; ------------------------------------------------------------------------------

    ifdef NADA
* ENTER FROM SOFTWARE INTERRUPT
SF0     NOP
SFE1    STS SP          SAVE TARGETS STACK POINTER
* DECREMENT P COUNTER
        TSX
        TST 6,X
        BNE *+4
        DEC 5,X
        DEC 6,X
* PRINT CONTENTS OF STACK.
PRINT   LDX #MCL
        JSR PDATA1
        LDX SP
        INX
        BSR OUT2HS      COND CODES
        BSR OUT2HS      ACC B
        BSR OUT2HS      ACC A
        BSR OUT4HS      IXR
        BSR OUT4HS      PGM COUNTER
        LDX #SP
        JSR OUT4HS      STACK POINTER
SWTCTL  LDX SWIJMP
        CPX #SF0
        BEQ CONTR1

CONTRL  LDS #STACK      SET CONTRL STACK POINTER
        LDX #CTLPOR     RESET TO CONTROL PORT
        STX PORADD
        CLR PORECH      TURN ECHO ON
        BSR SAVGET      GET PORT # AND TYPE
        BEQ POF1
        JSR PIAECH      SET PIA ECHO ON IF MP-C INTER
POF1    JSR PNCHOF      TURN PUNCH OFF
        JSR RDOFF       TURN READER OFF
CONTR1  LDX #MCLOFF
        JSR PDATA1      PRINT DATA STRING
        BSR INEEE       READ COMMAND CHARACTER
    endif
; =[ Fini ]==================================================================
