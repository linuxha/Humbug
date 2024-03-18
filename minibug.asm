        MACEXP  off
        CPU     6800            ; That's what asl has
        include "asl.inc"       ;

       NAM    MINIB
;* MINI-BUG
;* COPYWRITE 1973, MOTOROLA INC
;* REV 004 (USED WITH MIKBUG)
;
;ACIACS EQU    @176364   ACIA CONTROL/STATUS ($FCF4-F5)
ACIACS EQU    $FCF4             ;* ACIA CONTROL/STATUS ($FCF$
ACIADA EQU    ACIACS+1
       ORG    $FE00             ;* FE00-FEFF
;* MINIB
;* INPUT ONE CHAR INTO A-REGISTER
INCH   LDAA   ACIACS
       ASRA 
       BCC    INCH              ;* RECEIVE NOT READY
       LDAA   ACIADA            ;* INPUT CHARACTER
       ANDA   #$7F              ;* RESET PARITY BIT
       CMPA   #$7F
       BEQ    INCH              ;* RUBOUT; IGNORE
       JMP    OUTCH             ;* ECHO CHAR

;* INPUT HEX CHAR
INHEX  BSR    INCH
       CMPA   #$30
       BMI    C1                ;* NOT HEX
       CMPA   #$39
       BLE    IN1HG
       CMPA   #$41
       BMI    C1                ;* NOT HEX
       CMPA   #$46
       BGT    C1                ;* NOT HEX
       SUBA   #7
IN1HG  RTS

LOAD   LDAA   #$D1              ;* TURN READER ON
       STAA   ACIACS
       LDAA   #@21
       BSR    OUTCH

LOAD3  BSR    INCH
       CMPA   #'S'
       BNE    LOAD3             ;* 1ST CHAR NOT (S)
       BSR    INCH
       CMPA   #'9'
       BEQ    LOAD21
       CMPA   #'1'
       BNE    LOAD3             ;* 2ND CHAR NOT (1)
       CLR    CKSM              ;* ZERO CHECKSUM
       BSR    BYTE              ;* READ BYTE
       SUBA   #2
       STAA   BYTECT            ;* BYTE COUNT
;* BUILD ADDRESS
       BSR    BADDR
;* STORE DATA
LOAD11 BSR    BYTE
       DEC    BYTECT
       BEQ    LOAD15            ;* ZERO BYTE COUNT
       STAA   0,X               ;* STORE DATA
       INX
       BRA    LOAD11

LOAD15 INC    CKSM
       BEQ    LOAD3
LOAD19 LDAA   #'?'               ;* PRINT QUESTION MARK
       BSR    OUTCH
LOAD21 LDAA   #$B1               ;* TURN READER OFF
       STAA   ACIACS
       LDAA   #@23
       BSR    OUTCH
C1     JMP    CONTRL

;* BUILD ADDRESS
BADDR  BSR    BYTE               ;* READ 2 FRAMES
       STAA   XHI
       BSR    BYTE
       STAA   XLOW
       LDX    XHI               ;* (X) ADDRESS WE BUILT
       RTS

;* INPUT BYTE (TWO FRAMES)
BYTE   BSR    INHEX             ;* GET HEX CHAR
       ASLA 
       ASLA 
       ASLA 
       ASLA 
       TAB
       BSR    INHEX
       ANDA   #$0F              ;* MASK TO 4 BITS
       ABA
       TAB
       ADDB   CKSM
       STAB   CKSM
       RTS

;* CHANGE MEMORY (M AAAA DD NN)
CHANGE BSR    BADDR             ;* BUILD ADDRESS
       BSR    OUTS              ;* PRINT SPACE
       BSR    OUT2HS
       BSR    BYTE
       DEX
       STAA   0,X
       CMPA   0,X
       BNE    LOAD19            ;* MEMORY DID NOT CHANGE
       BRA    CONTRL

OUTHL  LSRA                     ;* OUT HEX LEFT BCD DIGIT
       LSRA 
       LSRA 
       LSRA 

OUTHR  ANDA   #$F               ;* OUT HEX RIGHT BCD DIGIT
       ADDA   #$30
       CMPA   #$39
       BLS    OUTCH
       ADDA   #$7

;* OUTPUT ONE CHAR
OUTCH  PSHB                     ;* SAVE B-REG
OUTC1  LDAB   ACIACS
       ASRB 
       ASRB 
       BCC    OUTC1             ;* XMIT NOT READY
       STAA   ACIADA            ;* OUTPUT CHARACTER
       PULB 
       RTS

OUT2H  LDAA   0,X               ;* OUTPUT 2 HEX CHAR
       BSR    OUTHL             ;* OUT LEFT HEX CHAR
       LDAA   0,X
       BSR    OUTHR             ;* OUT RIGHT HEX VHAR
       INX
       RTS

OUT2HS BSR    OUT2H             ;* OUTPUT 2 HEX CHAR + SPACE
OUTS   LDAA   #$20              ;* SPACE
       BRA    OUTCH             ;* (BSR & RTS)

     
;* PRINT CONTENTS OF STACK
PRINT  TSX
       STX    SP                ;* SAVE STACK POINTER
       LDAB   #9
PRINT2 BSR    OUT2HS            ;* OUT 2 HEX & SPCACE
       DECB 
       BNE    PRINT2

;* ENTER POWER ON SEQUENCE
START  EQU    *
;* INZ ACIA
       LDAA   #$B1              ;* SET SYSTEM PARAMETERS
       STAA   ACIACS

CONTRL LDS    #STACK            ;* SET STACK POINTER
       LDAA   #$D               ;* CARRIAGE RETURN
       BSR    OUTCH
       LDAA   #$A               ;* LINE FEED
       BSR    OUTCH

       JSR    INCH              ;* READ CHARACTER
       TAB
       BSR    OUTS              ;* PRINT SPACE
       CMPB   #'L'
       BNE    *+5
       JMP    LOAD
       CMPB   #'M'
       BEQ    CHANGE
       CMPB   #'P'
       BEQ    PRINT             ;* STACK
       CMPB   #'G'
       BNE    CONTRL
       RTI                      ;* GO


       ORG    $FF00
       RMB    40
STACK  RMB    1                 ;* STACK POINTER
;* REGISTERS FOR GO
       RMB    1                 ;* CONDITION CODES
       RMB    1                 ;* B ACCUMULATOR
       RMB    1                 ;* A
       RMB    1                 ;* X-HIGH
       RMB    1                 ;* X-LOW
       RMB    1                 ;* P-HIGH
       RMB    1                 ;* P-LOW
SP     RMB    1                 ;* S-HIGH
       RMB    1                 ;* S-LOW
;* END REGISTERS FOR GO
CKSM   RMB    1                 ;* CHECKSUM
BYTECT RMB    1                 ;* BYTE COUNT
XHI    RMB    1                 ;* XREG HIGH
XLOW   RMB    1                 ;* XREG LOW
       END
; python3 ../RT68mx/filter.py smithbug.lst| jq '{INCH,INEEE,INHEX }'
; python3 ../RT68mx/filter.py minibug.lst| jq '{BADDR,BYTE,OUTHL,OUTHR,OUTCH,INCH,PDATA1,INHEX,OUT2CH,OUT4HS,OUT2HS,OUTS,START,CONTRL,INEEE,OUTEEE,INCH8}'
