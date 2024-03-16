# Makefile for the Motorola 6801/6803 lilbug monitor
# ncherry@linuxha.com 2023/01/04

#all:	mikbug.s19 rt68mx.s19	
all:	humbug00.s19 smithbug.s19 rt68mx.s19 mikbug.s19 minibug.s19

# ------------------------------------------------------------------------------
#

humbug: humbug00.s19

humbug00.s19: humbug00.p
	p2hex +5 -F Moto -r \$$-\$$ humbug00.p humbug00.s19
	ls
	srec_info humbug00.s19

humbug00.lst: humbug00.asm humbug.inc motorola.inc
	asl -i . -L humbug00.asm

humbug00.p: humbug00.asm humbug.inc asl.inc  humbug.inc motorola.inc
	asl -i . -D NOMIKBUG -L humbug00.asm

# ------------------------------------------------------------------------------
#
smithbug: smithbug.s19

smithbug.s19: smithbug.p
	p2hex +5 -F Moto -r \$$-\$$ smithbug.p smithbug.s19
	ls
	srec_info smithbug.s19

smithbug.lst: smithbug.asm smithbug.inc motorola.inc
	asl -i . -D _E000 -L smithbug.asm

smithbug.p: smithbug.asm asl.inc smithbug.inc motorola.inc
	asl -i . -D _E000 -L smithbug.asm

# ------------------------------------------------------------------------------
#
rt68mx: rt68mx.s19

rt68mx.s19: rt68mx.p
	p2hex +5 -F Moto -r \$$-\$$ rt68mx.p rt68mx.s19
	ls
	srec_info rt68mx.s19

rt68mx.lst: rt68mx.asm rt68mx.inc
	asl -i . -L rt68mx.asm

rt68mx.p: rt68mx.asm rt68mx.inc asl.inc
	asl -i . -D NOMIKBUG -L rt68mx.asm

# ------------------------------------------------------------------------------
#
mikbug: mikbug.s19

mikbug.s19: mikbug.p
	p2hex +5 -F Moto -r \$$-\$$ mikbug.p mikbug.s19
	ls
	srec_info mikbug.s19

mikbug.lst: mikbug.asm asl.inc
	asl -i .  -L mikbug.asm

mikbug.p: mikbug.asm ascii.inc
	asl -i . -L mikbug.asm

# ------------------------------------------------------------------------------
#
minibug: minibug.s19

minibug.s19: minibug.p
	p2hex +5 -F Moto -r \$$-\$$ minibug.p minibug.s19
	ls
	srec_info minibug.s19

minibug.lst: minibug.asm asl.inc
	asl -i .  -L minibug.asm

minibug.p: minibug.asm ascii.inc
	asl -i . -L minibug.asm

# ------------------------------------------------------------------------------
#
clean:
	rm -f *.lst *.p foo bar *~ *.bin *.hex *.s19 dstfile.srec *.srec
	echo Done

# # Assemble and convert to s19
# asl -i . -D DEF9600 -L lilbug.asm
# p2hex +5 -F Moto -r \$-\$ lilbug.p lilbug.s19
# # Not sure if the is the best way but it does work
# # Fill E000-F7FF with FF and append lilbug.s19
# srec_cat '(' -generate 0xE000 0xF800 --constant 0xFF ')' ~/lilbug.s19 -o dstfile.srec
# # convert the s19 to binary @ 0000
# srec_cat  dstfile.srec -offset -0xE000 -o lilbug.bin -binary
# # Use 2864A works for SEEQ and AT28C64-15 Note this automatically erases the chip
# minipro -p 28C64A -w lilbug.bin -y

burn: lilbug.s19
	srec_cat '(' -generate 0xE000 0xF800 --constant 0xFF ')' lilbug.s19 -o lilbug.srec
	srec_cat  lilbug.srec -offset -0xE000 -o lilbug.bin -binary
	@# We don't need sudo as we've fixed the device permission in the udev files
	minipro -p 28C64A -w lilbug.bin -y

# Okay this works
burn2: smithbug.s19
	(echo "S0030000FC" ; cat smithbug.s19 | python3 ./begin.py S113E100) >smithb.srec
	@#srec_cat '(' -generate 0xE000 0xF800 --constant 0xFF ')' lilbug.s19 -o lilbug.srec
	srec_cat  smithb.srec -offset -0xE000 -o smithbug.bin -binary
	@# We don't need sudo as we've fixed the device permission in the udev files
	@#minipro -p 28C64A -w lilbug.bin -y

# AM28C64A@DIP28
# AT28C64 <- Bad
# AT28C256

.PHONY: humbug smithbug rt68mx mikbug minibug

# -[ Fini ]---------------------------------------------------------------------
