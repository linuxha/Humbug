        ifndef   MC6800_INC    ; avoid multiple inclusion
MC6800_INC      equ      1

        save
        ;listing off   ; no listing over this file

        if       mompass=1
          message "MC6800 Compatibility Macros (C) 2024 Neil Cherry"
        endif

printf1 MACRO   op
        if       mompass=2
          message "\{op}"
        endif
        ENDM

;* 
;* Macros for the 6800 to make it easier to back port 6801/6809 code
;* 
;puld    MACRO
;        pula                    ; Xhi
;        pulb                    ; Xlo
;        endm
;
;pshd    MACRO
;        pshb                    ; Xlo
;        psha                    ; Xhi
;        endm
;
;ldd     MACRO   addr
;        nop
;        ldaa    addr            ; Xhi
;        ldab    addr+1          ; Xlo
;        endm
;
;std     MACRO   addr
;        staa    addr            ; Xhi
;        stab    addr+1          ; Xlo
;        endm
;
;* -----------------------------------------------------------------------------
;* Currently working on this
;* Need a way to transfer X to B:A
;* then pshd
	;; stx     $40,sp	; I could ins
        ;; stx     $4050,sp
        ;; ins
        ;; des
        ;;
        ;; Xhi:Xlo
        ;;   A:B
        ;;    D 
        ;; 15   0
        ;; 
        ;; ldx	#1234
        ;; stx	#$8000
	;; addr 80 = HiByte $12 ; Xhi (A)
        ;; addr 81 = LoByte $34 ; xLo (B)
; @FIXME: backwards?
pushx   MACRO                   ;m Actually gets X on the stack
        stx     SAVEX           ;m save X to tmp
        ldaa    SAVEX           ;m Xhi
        ldab    SAVEX+1         ;m Xlo
        pshd
        endm

pshx    MACRO                   ;m Save X to memory
        stx     SAVEX           ;m
        ENDM

;* Currently working on this
;* Need a way to transfer B:A to X
;* then puld @FIXME: backwards?
pullx   MACRO                   ;m Actually gets X off the stack
        ;pulb                    ;m
        ;pula                    ;m This is also puld
        puld
        staa    SAVEX           ;m Xhi
        stab    SAVEX+1         ;m Xlo
        endm

pulx    MACRO                   ;m Restores X from memory
        ldx     SAVEX           ;m
        ENDM
        ;; * Oops backwards
Xpshd    MACRO                   ;m Pushes A+B onto the stack
        psha                    ;m SP-2
        pshb                    ;m SP-1
        ENDM
; Oops backwards
Xpuld    MACRO                   ;m Pulls B=A from the stack
        pulb                    ;m
        pula                    ;m
        ENDM

pshd    MACRO                   ;m Pushes A+B onto the stack
        pshb                    ;m SP-2
        psha                    ;m SP-1
        ENDM

puld    MACRO                   ;m Pulls B=A from the stack
        pula                    ;m
        pulb                    ;m
        ENDM

;*
;* See subd for proper immediate handling
;*
ldd     MACRO   arg
_SARG     set     "ARG"
          if      substr(_SARG,0,1)='#'                 ; immediate?
_SARG       set     substr(_SARG,1,strlen(_SARG)-1)
_A          set     VAL(_SARG)
_Hi         set     _A/255                              ; I could use HI()/LO() ...
_Lo         set     ((_A&255)<<8)/255                   ; ... but need 'include bitfunc.inc'
            ldaa    #_Hi
            ldab    #_Lo
          else                ; absolute?
            ldaa    arg
            ldab    arg+1
          endif
        ENDM

;*
;* See subd for proper immediate handling
;*
std     MACRO   arg
_SARG     set     "ARG"
          if      substr(_SARG,0,1)='#'                 ; immediate?
_SARG       set     substr(_SARG,1,strlen(_SARG)-1)
_A          set     VAL(_SARG)
_Hi         set     _A/255                              ; I could use HI()/LO() ...
_Lo         set     ((_A&255)<<8)/255                   ; ... but need 'include bitfunc.inc'
            staa    #_Hi
            stab    #_Lo
          else                ; absolute?
            staa    arg
            stab    arg+1
          endif
        ENDM

;* X <- B + X
abx     MACRO
        if       mompass=1
	warning "Check this 6800 code (was abx)"
        endif

        ;pshb                   ;m Not needed
        psha
        clrb                    ;m Needs to be cleared, we're only adding B

        stx     SAVEX           ;m Save X to SAVEX or XHI
        addb    SAVEX+1         ;m add X lo to B
        adca    SAVEX           ;m add the X hi + CC to A
	std     SAVEX           ;m retore X
        ldx     SAVEX           ;m

	pula
        ;pulb                   ;m Not needed

        ENDM

;
; what we want is:
;       addd    #$0007
; to become:
;        addb    #$07           ;*
;        adca    #$00           ;*
; and
;        subd    #$0007
; to become:
;        subb    #$07           ;*
;        sdca    #$00           ;* Change back to value it had in USER PGM
;
; D <- D + M:M+1
addd    MACRO   ARG
_SARG   set     "ARG"
        if      substr(_SARG,0,1)='#'    ; immediate?
_SARG    set     substr(_SARG,1,strlen(_SARG)-1)
_A       set     VAL(_SARG)
_H       set     _A/255
_L       set     ((_A&255)<<8)/255
         addb    #_H
         adca    #_L
        elseif                ; absolute?
         addb    ARG+1
         adca    ARG
        endif
        ENDM

;
;         0  1  2
;0040    12 34 01 01 01 01 01 01 01 01 01 01 01 01 01 01  .4..............
;        |  \
;        |   --> Addr+1 = $41 - Contains lo byte ($34)
;        \-----> Addr   = $40 - Contains hi byte (#12)
;
; So if you push a 16 bit reg ($1234) onto the stack you get
;
;     SP   = $34
;     SP-1 = $12 (will look like $1234 in memory M:M+1)
;     And the SP will be:
;     SP <- SP - 2
; 
; So you pull A, then B
; 
; A = $12
; B = $34 (D = $1234)
; SP <- SP + 2
; 
;* D <- D - M:M+1
subd    MACRO   ARG
_SARG     set     "ARG"
          if      substr(_SARG,0,1)='#'                 ; immediate?
_SARG       set     substr(_SARG,1,strlen(_SARG)-1)
_A          set     VAL(_SARG)
_Hi         set     _A/255                              ; I could use HI()/LO() ...
_Lo         set     ((_A&255)<<8)/255                   ; ... but need 'include bitfunc.inc'
            subb    #_Hi
            sbca    #_Lo
          else                ; absolute?
            subb    ARG+1
            sbca    ARG
          endif
        endm 
;       xsubd   $0007
;       xsubd   #$0007

;* -[ Fini ]--------------------------------------------------------------------

; These didn't work as intended but had interesting side effects
;zsubd   MACRO   addr
;	;message 'Arg is \{arg}' 	; 1
;        ;ZLO     set     '{arg}'/255         ; 2
;        ;ZHi     set     (arg>>8)&255    ; 3
;	;message "Message"                ; 4
;        ;lda     addr
;        fcb      strstr('{addr}', '$')
;        fcb      strlen('{addr}')
;        fcb      substr('{addr}', 3, strlen('{addr}') - 1)
;        zlo     equ     substr('{addr}', 3, strlen('{addr}') - 1)
;        ENDM
;
;ysubd   macro   addr
;        suba    #('{addr}' & $FF)
;        sbcb    #('{addr}' >> 8)
;        endm
;
;wsubd   macro   addr
;        suba    <addr
;        sbcb    >addr
;        sbcb    substr('{addr}', 3, strlen('{addr}') - 1)
;        endm
;
;vsubd   macro   ARG
;_SARG   set     "ARG"
;        if      substr(_SARG,0,1)='#'   ; immediate?
;_SARG    set     substr(_SARG,1,strlen(_SARG)-1)
;_A       set     VAL(_SARG)
;_H       set     _A/255
;_L       set     ((_A&255)<<8)/255
;         suba    #_Hi
;         suba    #_Lo
;        else                            ; absolute?
;         suba    ARG+1
;         sbcb    ARG
;        endif
;        endm

; BHS is an unofficial equivalent of BCC. It stands for Branch if Higher or Same.
bhs     macro   op
        bcc     op              ;* Branch Higher or Same
        endm

; Branch Greater than, equal
bge     macro   op
        printf1 "BGE Macro not built yet"
        bcc     op
        endm

blo     macro   op
        bcs     op              ;* Branch Lower
        endm

; Branch Greater than, equal
;bge     macro   op
;        printf1 "BGE Macro not built yet"
;        bcc     op
;        endm
;
; Branch Greater than, equal
;bgt     macro   op
;        printf1 "BGT Macro not built yet"
;        bcc     op
;        endm

; Branch Greater than, equal
;ble     macro   op
;        printf1 "BLE Macro not built yet"
;        bcc     op
;        endm

; Branch Greater than, equal
;blt     macro   op
;        printf1 "BLT Macro not built yet"
;        bcc     op
;        endm

        printf1 "***\n*** Built the OpCode struct\n***"

        dottedstructs   on

OpCode  STRUCT
txt     rmb     3               ;* 3 byte string
addr    rmb     2               ;* function handler
OpCode  ENDSTRUCT

;/* Local Variables: */
;/* mode:asm         */
;/* End:             */

        restore                 ; allow listing again

        endif                   ; MC6800_INC
