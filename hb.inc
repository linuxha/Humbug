        ifndef   HB_INC    ; avoid multiple inclusion
HB_INC  equ      1
        save

        if       mompass=1
          message "Humbug IO equates (C) 2024 Neil Cherry"
        endif
;;;
;;;
;;; 

SWI	EQU	$3F
JMP	EQU	$7E

    IFDEF _SIM
;;;
;;; Setup for the simulator
;;;
        if       mompass=1
          message "***"
          message "*** Built for the sim6800"
          message "***"
        endif
;
ACIACS  EQU     $F000           ;V3 has $F000, $F001
ACIADA  EQU     $F001
;
RSTART  EQU     $E100           ; IO at $E000, ROM starts at $E100
    ELSE
;
    IFDEF _E000
;;;
;;; Adjusted for my MP-02
;;; 
        if       mompass=1
          message "***"
          message "*** Built for the MP-02"
          message "***"
        endif
;;; 
ACIACS  EQU     $E000           ;V1 has $8000, $8001
ACIADA  EQU     $E001
;
PIAD1A  EQU     $E004           ;V2 has PIA support
PIAS1A  EQU     PIAD1A+1
PIAD1B  EQU     PIAD1A+2
PIAS1B  EQU     PIAD1A+3
;
PIAD2A  EQU     $E008
PIAS2A  EQU     PIAD2A+1
PIAD2B  EQU     PIAD2A+2
PIAS2B  EQU     PIAD2A+3
;;;
;;; Adjusted for my MP-02
;;; 
RSTART  EQU     $E100           ; IO at $E000, ROM starts at $E100
    ELSE
;;;
;;; Original (the way I got it)
;;; 
        if       mompass=1
          message "***"
          message "*** Built for the Original/SWTPC"
          message "***"
        endif
;;; 
ACIACS  EQU     $8018           ;V1 has $8000, $8001
ACIADA  EQU     $8019
;
PIAD1A  EQU     $8000           ;V2 has PIA support
PIAS1A  EQU     $8001
PIAD1B  EQU     $8002
PIAS1B  EQU     $8003
;
PIAD2A  EQU     $8008
PIAS2A  EQU     $8009
PIAD2B  EQU     $800A
PIAS2B  EQU     $800B
;;;
;;; Original (the way I got it)
;;; 
RSTART  EQU     $C000

    ENDIF                       ; _E100 or default
    ENDIF                       ; _SIM
        restore
    ENDIF                       ; HB_INC
; =[ Fini ]==================================================================
;/* Local Variables: */
;/* mode:asm         */
;/* End:             */
