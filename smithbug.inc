; ===========================================================================
        cpu 6800
        ;;
        include "motorola.inc"          ; Macros for things like fcc,db, etc.
        include "MC6800.inc"
; ===========================================================================
    IFDEF _E000
UPRAM   EQU     $DF00           ; TOP OF USER MEMORY
    ELSE
    IFDEF _SIM
UPRAM   EQU     $DF00           ; TOP OF USER MEMORY
    ENDIF
    ELSE
UPRAM   EQU     $7F00           ; TOP OF USER MEMORY
    ENDIF
;
        ORG     UPRAM           ; V1 has ORG at $A000H
;
MSTART  EQU     *
;
BUFFER  RMB 2                   ; V2: Buffer to protect system scratch ???
IOV     RMB 2                   ; Vector -here and below same as V1
BEGA    RMB 2
ENDA    RMB 2
NIO     RMB 2                   ; Vector
SP      RMB 2
ACIAT   RMB 1
ECHO    RMB 1
XHI     RMB 1
XLOW    RMB 1
TEMP    RMB 1
TW      RMB 2
TFLAG   RMB 1
XTEMP   RMB 2
BKFLG   RMB 1
SWIPTR  RMB 45                  ; Vector
STACK   RMB 8
PRINTR  RMB 3
BFLAG   RMB 1
MFLAG   RMB 1
XFLAG   RMB 1
BITE2   RMB 1
BITE3   RMB 15
TSTACK  RMB 1
OPSAVE  RMB 1
PB1     RMB 1
PB2     RMB 1
PB3     RMB 1
BYTECT  RMB 1
PC1     RMB 1
PC2     RMB 1
BPOINT  RMB 3
BKFLG2  RMB 1
MCONT   RMB 1
; V2 S-record data area
TEMPX1  RMB 2                   ; S-Load Temporary X register
ADDRESS RMB 1                   ; Build 16 bit address byte 1
;
ADDRESS1        RMB 1           ; Build 16 bit address byte 2
BYTESTORE       RMB 1           ; Save Byte Count
TEMPA           RMB 1           ; Save A Register
;
USERV   RMB 2                   ; User Vector (njc)
;;; 
MFINI   EQU *

RAMSIZE	EQU MFINI - MSTART
;;;
;;;
;;; 
    IFDEF _SIM
;;;
;;; Setup for the simulator
;;;
        message "***"
        message "*** Built for the sim6800"
        message "***"
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
        message "***"
        message "*** Built for the MP-02"
        message "***"
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
        message "***"
        message "*** Built for the Original/SWTPC"
        message "***"
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

; =[ Fini ]==================================================================
;/* Local Variables: */
;/* mode:asm         */
;/* End:             */
