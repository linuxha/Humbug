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
pshx    macro
        stx     tmpX            ; save X to tmp
        ldaa    tmpX            ; Xhi
        ldab    tmpX+1          ; Xlo
        pshd
        endm

;* Currently working on this
;* Need a way to transfer B:A to X
;* then puld
pulx    macro
        puld
        staa    tmpX            ; Xhi
        stab    tmpX+1          ; Xlo
        endm

puld    macro
        pula                    ; Xhi
        pulb                    ; Xlo
        endm

pshd    macro
        pshb                    ; Xlo
        psha                    ; Xhi
        endm

ldd     macro   addr
        nop
        ldaa    addr            ; Xhi
        ldab    addr+1          ; Xlo
        endm

std     macro   addr
        staa    addr            ; Xhi
        stab    addr+1          ; Xlo
        endm

;* X <- B + X
abx     macro
        nop
        endm

;* D <- D + M:M+1
;addd    macro op
;        nop
;        warning "addd - Need to create the 6801 macros for saving X and D acc" ;
;        endm

;* D <- D - M:M+1
subd    macro
        nop
        endm

;      error   "Need to create the 6801 macros for saving X and D acc" ;
;      warning "Need to create the 6801 macros for saving X and D acc" ;
;/* Local Variables: */
;/* mode:asm         */
;/* End:             */

