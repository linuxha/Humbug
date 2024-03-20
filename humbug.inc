;  ****************************************************************
;  * Radio Shack MC-10
;  * MICROCOLOR BASIC 1.0
;  *
;  *
;
;  ** Memory map at the end
;
;  **
;  **  Some equates
;  **
LBUFMX    equ       128                 ; maximum number of chars in line input buffer
STKBUF    equ       58                  ; minimum cushion size for the stack
NMSTRS    equ       3                   ; maximum number of entries on the string stack
	
;;; =[ EQU ]====================================================================
NUL	  equ   $00
SOH       equ   $01
EOT       equ   $04
BS        equ	$08                     ; backspace
CTRLH     equ   $08
CR        equ   $0A                     ; carriage return (ENTER)
LF        equ   $0D
NL        equ   $0D
CTRLQ	  equ   $11
CTRLS     equ   $13                     ; PAUSE control code (Shift-@)
PAUSE     equ   $13                     ; PAUSE control code (Shift-@)
SPACE     equ   $20                     ; space

PROMPT    equ   $3E                     ; '>'
MARK	  equ	$55                     ; 'U'
SYNC	  equ   $3C                     ; '<'

SWIINST   equ   $3F                     ; SWI Instruction $3F (used by Break Points)
JMPINST   equ   $7E                     ; JMP Instruction $7E

SWIADDR   equ   $4210           	; Part of the SWI Vector
;;; =[ Macro ]==================================================================
;
;
;  **
;  **  Error Numbers
;  **
NF_ERR    equ       0                   ; NEXT without FOR
SN_ERR    equ       2                   ; Syntax error
RG_ERR    equ       4                   ; RETURN without GOSUB
OD_ERR    equ       6                   ; Out of DATA
FC_ERR    equ       8                   ; Illegal Function Call
OV_ERR    equ       10                  ; Overflow
OM_ERR    equ       12                  ; Out of Memory
UL_ERR    equ       14                  ; Undefined Line number
BS_ERR    equ       16                  ; Bad Subscript
DD_ERR    equ       18                  ; Doubly Dimensioned array
D0_ERR    equ       20                  ; Division by Zero
ID_ERR    equ       22                  ; Illegal in Direct mode
TM_ERR    equ       24                  ; Type Mismatch
OS_ERR    equ       26                  ; Out of String space
LS_ERR    equ       28                  ; Length of String (too long)
ST_ERR    equ       30                  ; String formula Too complex
CN_ERR    equ       32                  ; Can't continue
IO_ERR    equ       34                  ; Input/Output error
FM_ERR    equ       36                  ; File Mode error
;
;
;  ** 
;  ** Opcodes used for skipping over instructions
;  ** 
SKP1      equ       $21                 ; Skip single byte instruction (BRN _)
SKP2      equ       $8C                 ; Skip two-byte instruction    (CPX #__)
SKP1LDA   equ       $86                 ; Skip single byte instruction (LDA #_)
SKP1LDB   equ       $C6                 ; Skip single byte instruction (LDB #_)
;
;
;
;  ****************************************************************
;  **   MEMORY MAP
;  ****************************************************************
;            org       $0000
;
;  *---------------------------------------------------------------
;  *  6803 Internals
;  *
P1DDR     equ	$0000	;       1                   ; Port 1 Data Direction Register
P2DDR     equ	$0001	;       1                   ; Port 2 DDR (5 bits)
P1DATA    equ	$0002	;       1                   ; Port 1 Data Register
P2DATA    equ	$0003	;       1                   ; Port 2 Data (5 bits)
        ;; 0004-0008 not availabe, 0006 is used by Humbug ???
TCSR      equ	$0008	;       1                   ; Timer Status and Control Register
TIMER     equ	$0009	;       2                   ; Free running counter
TOCR      equ	$000B	;       2                   ; Timer Output Compare Register
TICR      equ	$000D	;       2                   ; Timer Input Capture Register
SERMOD    equ	$0010	;       1                   ; Serial port Rate and Mode Control Register
SERCSR    equ	$0011	;       1                   ; Serial port Control and Status Registers
SERIN     equ	$0012	;       1                   ; Serial port Receiver Data Register
SEROUT    equ	$0013	;       1                   ; Serial port Transmit Data Register
RAMCR     equ	$0014	;       1                   ; RAM Control Register
;
;  *---------------------------------------------------------------
;  *  The address range of $0020 to $007F is not mapped
;  *  to anything by the stock hardware.
;  *
;0020 ;            rmb       96                  ; not available
;
;  *---------------------------------------------------------------
;  *  6803 On-Board RAM
;  *
CHARAC    equ	$0080	;       1                   ; termination character 1
ENDCHR    equ	$0081	;       1                   ; termination character 2
TMPLOC    equ	$0082	;       1                   ; scratch variable
DIMFLG    equ	$0083	;       1                   ; dimensioning flag (non-zero while in a DIM statement)
VALTYP    equ	$0084	;       1                   ; value type ($00 = numeric, $FF = string)
TMPFLG    equ	$0085	;       1                   ; temporary flag variable (used during Crunch and String Allocation)
ARYMOD    equ	$0086	;       1                   ; array mode flag to force ($01) or disable ($80) array lookups
INPFLG    equ	$0087	;       1                   ; flag to differentiate INPUT from READ
RELFLG    equ	$0088	;       1                   ; relational operator flags
TMPTR1    equ	$0089	;       2                   ; temporary pointer 1
TMPTR2    equ	$008B	;       2                   ; temporary pointer 2
FPA2      equ	$008D	;       4                   ; floating point accumulator #2 (mantissa only)
STKSAV    equ	$0091	;       2                   ; temporary storage for the stack pointer
;
;  **
;  ** Basic Program and Variable allocation pointers
;  **
TXTTAB    equ	$0093	;       2                   ; start of Basic program
VARTAB    equ	$0095	;       2                   ; start of simple variables
ARYTAB    equ	$0097	;       2                   ; start of array variables
ARYEND    equ	$0099	;       2                   ; end of array variables (first Free RAM address)
FRETOP    equ	$009B	;       2                   ; lower bounds of string space, upper bounds of stack
STRTAB    equ	$009D	;       2                   ; pointer to used string space (grows downward)
FRESPC    equ	$009F	;       2                   ; string space allocator returns a pointer here
MEMSIZ    equ	$00A1	;       2                   ; highest RAM address used by Basic (adjustable with CLEAR)
;
;  **
;  ** Basic Program execution info
;  **
BRKLIN    equ	$00A3	;       2                   ; line number where program execution stopped
BINVAL    equ	$00A5	;       2                   ; binary integer value (often used to hold a parsed line number)
OLDPTR    equ	$00A7	;       2                   ; address where program execution stopped (for use by CONT)
TINPTR    equ	$00A9	;       2                   ; temporary storage of parser pointer
DATLIN    equ	$00AB	;       2                   ; line number where search for DATA statements is taking place
DATPTR    equ	$00AD	;       2                   ; pointer to where a READ statement will start searching for a DATA statement
DATTMP    equ	$00AF	;       2                   ; temp storage for INPUT/READ parse pointer
VARNAM    equ	$00B1	;       2                   ; temp storage of variable name
VARADR    equ	$00B3	;       2                   ; temp storage of pointer to variable descriptor
VARDES    equ	$00B5	;       2                   ; pointer to variable's descriptor
OPSAVE    equ	$00B7	;       2                   ; saved pointer to entry in the operator dispatch table
RELOPS    equ	$00B9	;       1                   ; temporary relational operator flag
;
;  **
;  **  Floating Point Accumulators 3, 4 and 5 are
;  **  used mainly as scratch data storage.
;  **
;  FPA3      equ       *                            ; Packed Floating Point Accumulator #3 ($BA-$BE)
FP3EXP    equ	$00BA	;       1                   ; exponent of FPA3
DSTEND    equ	$00BB	;       2                   ; holds destination end address for upward block move
SRCEND    equ	$00BD	;       2                   ; holds source end address for upward block move
;
;  FPA4      equ       *                            ; Packed Floating Point Accumulator #4 ($BF-$C3)
;  DSTPTR    equ       *                            ; destination address for block moves (word)
FRACNT    equ	$00BF	;       1                   ; number of digits found on the right side of the decimal point
DECFLG    equ	$00C0	;       1                   ; decimal point flag during ASCII to binary number conversion
;  SRCPTR    equ       *                            ; source address for block moves
SCIEXP    equ	$00C1	;       1                   ; 8 bit exponent value when parsing scientific notation
EXPSGN    equ	$00C2	;       1                   ; sign of exponent when parsing scientific notation
;
;  FPA5      equ       *                            ; Packed Floating Point Accumulator #5 ($C4-$C8)
FP5EXP    equ	$00C4	;       1                   ; exponent and packed sign bit for FPA5
HISTRD    equ	$00C5	;       2                   ; descriptor address for 'highest' string during compaction
DESPTR    equ	$00C7	;       2
;
;  **
;  **  Floating Point Accumulator 0
;  **
FP0EXP    equ	$00C9	;       1                   ; exponent
FPA0      equ	$00CA	;       4                   ; mantissa
FP0SGN    equ	$00CE	;       1                   ; sign
COEFCT    equ	$00CF	;       1                   ; polynomial coefficient counter
STRDES    equ	$00D0	;       5                   ; temporary string descriptor
SFTSGN    equ	$00D5	;       1                   ; sign data shifted in during de-normalization
;
;  **
;  **  Floating Point Accumulator 1
;  **
FP1EXP    equ	$00D6	;       1                   ; exponent
FPA1      equ	$00D7	;       4                   ; mantissa
FP1SGN    equ	$00DB	;       1                   ; sign
RESSGN    equ	$00DC	;       1                   ; resulting sign for operation
FPSBYT    equ	$00DD	;       1                   ; sub-precision (fifth mantissa byte)
OUTPTR    equ	$00DE	;       2                   ; multi-purpose pointer (LIST, string, arrays, polynomial)
;
CURLIN    equ	$00E2	;       2                   ; current execution line number (-1 = not running)
;
;  **
;  **  I/O Device Info
;  **
DEVCFW    equ	$00E4	;       1                   ; current output device's tab width
DEVLCF    equ	$00E5	;       1                   ; current output device's last tab column
DEVPOS    equ	$00E6	;       1                   ; current line position on current output device
DEVWID    equ	$00E7	;       1                   ; current output device's line width
DEVNUM    equ	$00E8	;       1                   ; current I/O device number:  0 = Screen, -2 = Printer
EOFLAG    equ	$00E9	;       1                   ; EOF flag for use by CONSOLE IN hooks
;
RSTFLG    equ	$00EA	;       1                   ; warm start reset flag  ($55 permits warm start) 
;
;
;  *---------------------------------------------------------------
;  *  The front-end of the character parser subroutine is copied here
;  *  from ROM during initialization.  It needs to be in RAM because
;  *  it utilizes self-modifying code.  When entered at CHRGET the
;  *  routine advances the parse pointer and picks up the next character.
;  *  When entered at CHRGOT the routine re-loads the character got
;  *  by the most recent call to CHRGET.
;  *
CHRGET    equ	$00EB	;       3         *  inc   CHRPTR+1     ; increment LSB of parse location
CHRGOT    equ	$00F3	;       1         *  lda   >nnnn        ; load char from parse location (Extended)
CHRPTR    equ	$00F4	;       2         *                     ; current parse location
;
;
;  **
;  **  Unused Bytes in the Direct Page
;  **
;00F9 ;            rmb       7                      ; not used
;
;
;  *---------------------------------------------------------------
;  *  The address range of $0100 to $3FFF is not mapped
;  *  to anything by the stock hardware.
;  *
;0100 ;            rmb       $3F00                  ; not available
;
;
;  *---------------------------------------------------------------
;  *  The 4K built-in RAM begins here
;  *
VIDRAM    equ	$4000	;       512                 ; 512 bytes of video RAM
;
;
;  ***
;  ***  The system variables from SCIVEC to CLDRSZ ($4200 - $4230) are
;  ***  initialized during a cold start with data from ROM ($F7DF - $F80F).
;  ***
;
;  * Secondary Interrupt Vectors (all are initialized to RTI,$00,$00)
SCIVEC    equ	$4200	;       3                   ; SCI
TOFVEC    equ	$4203	;       3                   ; Timer Overflow
OCFVEC    equ	$4206	;       3                   ; Output Compare
ICFVEC    equ	$4209	;       3                   ; Input Capture
IRQVEC    equ	$420C	;       3                   ; IRQ1
SWIVEC    equ	$420F	;       3                   ; SWI
NMIVEC    equ	$4212	;       3                   ; NMI
;
USRVEC    equ	$4215	;       3                   ; USR function vector (JMP ILLFNC)
RVSEED    equ	$4218	;       4                   ; variable random number seed ($4FC75259)
CAPLOK    equ	$421C	;       1                   ; keyboard Caps-Lock state ($FF)
DEBDLY    equ	$421D	;       2                   ; keyboard debounce delay ($045E)
EXECJP    equ	$421F	;       2                   ; default EXEC address (ILLFNC)
WSTVEC    equ	$4221	;       2                   ; warm start vector (WMSTRT)
LPTBTD    equ	$4223	;       2                   ; printer baud rate delay (118)
LPTLND    equ	$4225	;       2                   ; printer carriage return delay (1)
LPTCFW    equ	$4227	;       1                   ; printer tab width (16)
LPTLCF    equ	$4228	;       1                   ; printer's last tab column (112)
LPTWID    equ	$4229	;       1                   ; printer line width (132)
LPTPOS    equ	$422A	;       1                   ; current printer line position (0)
BLNKTM    equ	$422B	;       1                   ; cursor blink timer (1)
;
;  * Cassette Timing
;
CYCPAR    equ	$422C	;       1                   ; cassette 1200/2400 Hz cycle partition (21)
CMAXTR    equ	$422D	;       1                   ; time limit for a valid cassette transition (26)
TRNPAR    equ	$422E	;       1                   ; cassette 1200/2400 Hz transition partition (11)
CLDRSZ    equ	$422F	;       2                   ; number of cassette leader bytes to write (128)
;
;  * Keyboard
;
KEYBUF    equ	$4231	;       8                   ; keyboard tracking state
KEYCOL    equ	$4239	;       1                   ; current keyboard column number (while polling)
CTRLKY    equ	$423A	;       1                   ; state of CONTROL key at last key-down
BRKSTA    equ	$423B	;       1                   ; tracking state for the BREAK key
;
PIXMSK    equ	$423C	;       1                   ; pixel mask for SET, RESET and POINT
SDSPTR    equ	$423D	;       2                   ; string descriptor stack pointer
TOPSTR    equ	$423F	;       2                   ; pointer to top entry on the string stack
STRSTK    equ	$4241	;       NMSTRS*5            ; space allocated for the string stack
TOPRAM    equ	$4250	;       2                   ; highest RAM address -1 ($4FFE with 4K)
RNDVAL    equ	$4252	;       4                   ; partial product from FP multiplication used as data for RND
;
;  * Cassette File Name Search String
CFNLEN    equ	$4256	;       1                   ; length of cassette file name search string
CFNSTR    equ	$4257	;       8                   ; cassette file name search string buffer
;
;  * Cassette File Name Block Buffer
;
NAMBLK    equ	$425F	;       8                   ; file name
CFTYPE    equ	$4267	;       1                   ; file type ($00 = Basic; $02 = ML; $04 = Array Data)
ASCFLG    equ	$4268	;       1                   ; ASCII file flag ($00 = Binary; $FF = ASCII)
GAPFLG    equ	$4269	;       1                   ; file gaps flag ($00 = continuous; $FF = gaps)
CFEXEC    equ	$426A	;       2                   ; EXEC addrress for ML file
LDSIZE    equ	$426C	;       2                   ; LOAD address for ML file; SIZE of Basic/Array file
;
;  * Cassette I/O
;
CLDFLG    equ	$426E	;       1                   ; set to $FF inside CLOAD to force a NEW upon error
CASBEG    equ	$426F	;       2                   ; start address for cassette file output
CASEND    equ	$4271	;       2                   ; end address for cassette file output
NOLOAD    equ	$4273	;       1                   ; SKIPF flag (prevents loading cassette data into RAM)
MYSTRY    equ	$4274	;       1                   ; mystery flag tested by cassette file scan routine 
BLKTYP    equ	$4275	;       1                   ; cassette file block type
BLKLEN    equ	$4276	;       1                   ; cassette file block length
BLKPTR    equ	$4278	;       2                   ; cassette file block pointer
BLKSUM    equ	$427A	;       1                   ; cassette file block checksum
CCTERR    equ	$427B	;       1                   ; cassette byte counter and error flag
CBITCT    equ	$427C	;       1                   ; cassette input bit/phase counter
CASTIM    equ	$427D	;       1                   ; cassette signal transition/cylce time
CASPOL    equ	$427E	;       1                   ; cassette input polarity (+/-96)
;
IKYCOD    equ	$427F	;       1                   ; key code discovered during BREAK check (for INKEY$)
CURPOS    equ   $4280   ;       2                   ;
CRSPTR    equ	$4280	;       2                   ; cursor Position
CRSCLR    equ	$4282	;       1                   ; cursor color
ENDFLG    equ	$4283	;       1                   ; STOP / END flag:  negative = STOP, positive = END
UNCRFL    equ	$4284	;       1                   ; flags byte used to UNCRUNCH a program line
;
;  * Extension Hooks
;
RVEC1     equ	$4285	;       3                   ; extension hook for CONSOLE IN
RVEC2     equ	$4288	;       3                   ; extension hook for CONSOLE OUT
RVEC3     equ	$428B	;       3                   ; extension hook to install Tab and Line settings for output device
RVEC4     equ	$428E	;       3                   ; extension hook for program initialization (NEW,RUN,CLEAR)
RVEC5     equ	$4291	;       3                   ; extension hook for line input
RVEC6     equ	$4294	;       3                   ; extension hook for expression terminal evaluator
RVEC7     equ	$4297	;       3                   ; extension hook for implementing ON ERR GOTO
RVEC8     equ	$429A	;       3                   ; extension hook for additional error messages
RVEC9     equ	$429D	;       3                   ; extension hook for ASCII to FP number conversion
RVEC10    equ	$42A0	;       3                   ; extension hook for command dispatcher
RVEC11    equ	$42A3	;       3                   ; extension hook for crunching keywords
RVEC12    equ	$42A6	;       3                   ; extension hook for uncrunching a token
RVEC13    equ	$42A9	;       3                   ; extension hook for polling the keyboard
RVEC14    equ	$42AC	;       3                   ; extension hook for interpreting CONTROL key values
;
LNKLSB    equ	$42AF	;       1                   ; $FF - ensures a non-zero link LSB during program line insertion
LINUMB    equ	$42B0	;       2                   ; the program line number being inserted or deleted
LINBUF    equ	$42B2	;       130                 ; line input buffer (128 characters plus two null terminators)
MOVSTR    equ	$4334	;       1                   ; first byte of buffer for a small string to be moved into string space
STRBUF    equ	$4335	;       17                  ; buffer for a small string (typically the output of Number-to-String)
;
;  PRGRAM    equ       *                   ; start of program space
PRGRAM    equ   $4335

PRTSER  equ     $F9D0 ; Serial out to the printer
; Humbug uses 4400
;                 * Jump table for public subroutines
POLCAT	equ	$FFDC ; F8 83           POLCAT    fdb       KEYIN               ; read keyboard
KEYIN   equ	$F883 ;
CHROUT	equ	$FFDE ; F9 C6           CHROUT    fdb       PUTCHR              ; console out
PUTCHR  equ	$F9C6 ;
RDCLDR	equ	$FFE0 ; FF 4E           RDCLDR    fdb       SYNLDR              ; read a cassette leader
SYNLDR  equ	$FF4E ;
BLKIN	equ	$FFE2 ; FE B9           BLKIN     fdb       GETBLK              ; read a block from cassette
GETBLK  equ	$FEB9 ;
BLKOUT	equ	$FFE4 ; FC C0           BLKOUT    fdb       PUTBLK              ; write a block to cassette 
PUTBLK  equ	$FCC0 ;
SNDOUT	equ	$FFE6 ; FF AB           SNDOUT    fdb       SNDGEN              ; generate sound through TV speaker
SNDGEN  equ	$FFAB ;
WRTLDR	equ	$FFE8 ; FC B7           WRTLDR    fdb       LDROUT              ; write cassette leader
LDROUT  equ	$FCB7 ;
IRETRN	equ	$FFEA ; EC E3           IRETRN    fdb       GIVABF              ; convert integer in ACCD to FP in FPA0
GIVABF  equ	$ECE3 ;
GETARG	equ	$FFEC ; EB C7           GETARG    fdb       INTCNV              ; convert FPA0 to an integer in ACCD
INTCNV  equ	$EBC7 ;
; 
;                 *
;                 * HD6303 NOTE:
;                 *  The HD6303 has 1 additional vector at $FFEE which is used for
;                 *  Illegal Instruction and Illegal Address trapping.  The MC-10 ROM
;                 *  stores the address of FPA0 ($00C9) in this location, but never
;                 *  references it.
;                 *
; 
;                 * CPU Vectors
;FFEE ; 00 C9                     fdb       FP0EXP              ; address of FPA0
;FP0EXP      equ  $00C9
;FFF0 ; 42 00                     fdb       SCIVEC              ; serial comm interrupt    (maskable)
;SCIVEC      equ  $4200
;FFF2 ; 42 03                     fdb       TOFVEC              ; timer overflow interrupt (maskable)
;TOFVEC      equ  $4203
;FFF4 ; 42 06                     fdb       OCFVEC              ; output compare interrupt (maskable)
;OCFVEC      equ  $4206
;FFF6 ; 42 09                     fdb       ICFVEC              ; input capture interrupt  (maskable)
;ICFVEC      equ  $4209
;FFF8 ; 42 0C                     fdb       IRQVEC              ; external IRQ             (maskable)
;IRQVEC      equ  $420C
;FFFA ; 42 0F                     fdb       SWIVEC              ; software interrupt
;SWIVEC      equ  $420F
;FFFC ; 42 12                     fdb       NMIVEC              ; non-maskable interrupt
;NMIVEC      equ  $4212
;FFFE ; F7 2E                     fdb       CPURES              ; CPU reset
RESET       equ  $F72E
CPURES      equ  $F72E
; 
;                           end
; 
;*
;* CONSOLE IN
;* Read one byte from the current device and return it in ACCA.  The only device supported
;* by MicroColor Basic is the keyboard.  Other devices can be implemented through the
;* RVEC1 extension hook.  Such extensions can also set EOFLAG ($009E) to indicate an
;* End-Of-File condition.
;* 
CNSLIN      equ  $F865          ;* Read one byte from the current device and return it in ACCA
OUTSER      equ  $F9D0          ;* Send character in ACCA to serial port printer
LFC8E       equ  $FC8E
LFC5D	    equ  $FC5D
LFEB0       equ  $FEB0
LFEB6       equ  $FEB6
LFF4E       equ  $FF4E

; ------------------------------------------------------------------------------
;* MEMORY MAP
;* Hex Address
;* C000 - FFFF 16K ROM (only 8K used)
;* 9000 - BFFF 16K I/O Slot (Keyboard and VDG control)
;* 4000 - 8FFF 16K RAM (4K - 20K used)
;* 0100 - 3FFF Not Used
;* 0080 - 00FF RAM internal to the 6803
;* 0015 - 007F Not Used
;* 0014 RAM Control Register
;* 0013 Not Used
;* 0012 Not Used
;* 0011 Not Used
;* 0010 Not Used
;* 000F Port 3 Control and Status Register
;* 000E Input Capture Register (low byte)
;* 000D Input Capture Register (high byte)
;* 000C Output Compare Register (low byte)
;* 000B Output Compare Register (high byte)
;* 000A Counter (low byte)
;* 0009 Counter (high byte)
;* 0008 Timer Control and Status Register
;* 0007 Not Used
;* 0006 Not Used
;* 0005 Not Used
;* 0004 Not Used
;* 0003 Miscellaneous I/O Data Register
;* 0002 Keyboard Output Lines
;* 0001 Data Direction Register for miscellaneous I/O
;* 0000 Data Direction Register for keyboard lines
;* 
;* $0000 - $00FF        6803 internal RAM
;* $0100 - $3FFF        Not used
;* $4000 - $41FF	Screen RAM
;* $4200 - $4FFF	System and User RAM
;* $5000 - $5FFF        Not used
;* $6000 - $6FFF        Phatom RAM
;* $7000 - $7FFF        Not used
;* $8000		VDG and Sound
;* $8001 - $BFFF        4095 Latch Phantoms
;* $C000 - $DFFF        BASIC ROM
;* $E000 - $FFEF        Phatom ROM
;* $FFF0 - $FFFF        System vectors
; ------------------------------------------------------------------------------
;/* Local Variables: */
;/* mode:asm         */
;/* End:             */
