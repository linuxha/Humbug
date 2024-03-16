;
opt             macro
                endm

nam             macro
                endm

noopt           macro
                endm

; This works but you need ttl "xyz abc" 
ttl             macro a
                TITLE a
                endm

spc             macro
                endm

list            macro
                listing on
                endm

nolist          macro
                listing off
                endm

page            macro
                newpage
                endm

PULD            macro
                PULA	; Pull the last thing off the stack
                PULB
                endm

PSHD            macro
                PSHB
                PSHA	; Pus the last thing on the stack
                endm

; ------------------------------------------------------------------------------
;
NULL    EQU $00                 ;
CTRL_A  EQU $01                 ;
CTRL_D  EQU $04
CTRL_Q  EQU $11                 ; XOFF
CTRL_S  EQU $13                 ; XON
CTRL_U  EQU $15                 ; NAK
;
LF      EQU $0A                 ; ASL allows \n
CR      EQU $0D                 ; ASL allows \r
SPC     EQU $20                 ;

;/* Local Variables: */
;/* mode:asm         */
;/* End:             */
