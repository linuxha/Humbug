# Makefile for the Motorola 6801/6803 lilbug monitor
# ncherry@linuxha.com 2023/01/04

all:	humbug-mc.s19

# ------------------------------------------------------------------------------
#

humbug: humbug-mc.s19

humbug-mc.s19: humbug-mc.p
	p2hex +5 -F Moto -r \$$-\$$ humbug-mc.p humbug-mc.s19
	ls
	srec_info humbug-mc.s19

humbug-mc.lst: humbug-mc.asm humbug.inc motorola.inc
	asl -i . -L humbug-mc.asm

humbug-mc.p: humbug-mc.asm humbug.inc asl.inc  humbug.inc motorola.inc
	asl -i . -D NOMIKBUG -L humbug-mc.asm

# ------------------------------------------------------------------------------
#
clean:
	rm -f *.lst *.p foo bar *~ *.bin *.hex *.s19 dstfile.srec *.srec
	echo Done

#

.PHONY: humbug

# -[ Fini ]---------------------------------------------------------------------
