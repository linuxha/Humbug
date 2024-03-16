;
; Motorola 68xx/Hitachi 63xx
; ADR	BYT	DC[.<size>]	DFS	DS[.<size>]
; FCB	FCC	FDB		PADDING	RMB
;
list            macro
                  listing on
                endm

nolist          macro
                  listing off
                endm

db              macro   op
                  ;byt   (op)+1
                  byt   ALLARGS
                endm

Xfcb             macro   args
                  byt   ALLARGS
                endm

; Modified to allow
; label fcc "string"
; or
; label fcc "string", $04, ...
Xfcc             macro   args
                  byt   ALLARGS
                endm

dw              macro   op
                  adr   (op)
                endm

Xfdb             macro   op
                  adr   (op)
                endm

fcw             macro   op
                  adr   (op)
                endm

ds              macro   op
                  dfs   (op)
                endm

Xrmb            macro	op
                ds.b    (op)
                endm

Zrmb             macro	op
                  rept  op
                    db  0
                  endm
                endm

dd              macro   op
                  adr   (op)>>16,(op)&$ffff
                endm

fcd             macro   op
                  adr   (op)>>16,(op)&$ffff
                endm

page            macro
                  newpage
                endm

opt             macro
                endm

noopt           macro
                endm

