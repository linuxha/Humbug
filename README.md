# Humbug

Peter Stark's Humbug monitor for the Motorola 68xx. Copied from the
Kilobaud/Micromputing articles of July, August and September of 1980.
Copies can be found in the docs directory.

And copied from a disassembly of the Tandy MC10 Humbug+ corrupted tape
cassette image (hb7500).

And a copy of the 6809 Humbug source. This is a little harder to use
as the 6800 has a subset of the instructions, a subset of the addressing,
a subset of the Registers and many techniques can't be used with the
6800.

If anyone has a copy of the Humbug ROMs for the 6800, please share! We
can disassemble them and finally get a working set for the 6800, the 6801,
the 6809 & 68K family.

# Table of Contents

- [Humbug](#humbug)
- [Table of Contents](#table-of-contents)
- [Description](#description)
- [History](#history)
- [Documentation](#documentation)
- [Files](#files)
- [Notes](#notes)
- [Sources](#sources)
- [Credits](#credits)

# Description

The usual, trying capture all the ROM bugs for the Motorola boards. This
one is one of those and I beleive it to be a very popular ROM bug. Peter
Stark wrote this for the Motorola 6800, 6802, 6803, 6809 and 68k. I've
found the original articles, the cassette tapes for the Tandy MC10 (6801)
the Tandy CoCo (6809) and the PT68K-4 (68K).

While the articles give plenty of detail they are not complete and seem
to be made up of parts of different packages. Parts appear to be missing
so it's very confusing. In addition to the articles I have a mostly working
MC10 cassette version. This is really an odd image as it seems to overwrite
itself. I think it is a damaged version. But between the two and the Tandy
CoCo version (which I hope is intact) I hope to build a working Humbug ROM
for the 6800, 6802 & 6801.

Right now this is just a mess of files. It now assembles but is a mess. I've
had to resort to abusing all the memory I can use as I whittle away at the
wayward RAM usage. I also need to keep an eye on any code that is unintentionally
being self modifiying code. But progress is being made on both subjects.

# History

March 30, 2024

I'm now at the point where I can't do much more testing in the simulator. So
I broke out the [memSIM2](https://github.com/nils-eilers/memSIM2). The USB to
ROM interfaces make it much quicker to update code. Now things get weird. I've
tested Mikbug, Smithbug and Humbug. Mikbug works, very limitted but works. Now
when I start working on the RT68MX code I think I'll need the breakpoints and
a few other niceties. So I've tested with Smithbug and it mostly works. Breakpoint
setting gos off into lala-land. Will look inot that. Now Humbug just doesn't want
to work. It boots, spits out a hello message, and accepts the AD command (and
address input). No other commands work. Odd part is that it works fine in the
simulator. I must be messing up the X register as it traverses the table.

March 27, 2024

I think I finally fixed the RC (register change). Turns out I had problems
with my abx macro. I've patched it up but I still need more testing on that
macro. The reason I say I think is that I'm not yet ready to test with user
code running in the monitor. Still have a few more things to work on like
step (ST/SS) and go (GO).

March 22, 2024

I'm overly pleased with myself. I couldn't find a PUnch routine so I
wrote my own. While it's only a few bytes long it's all mine. I did
get bit by bsr/jsr to various routines. They clobbered the registers I
was using. Still working on getting all the code working but I'm now
working on the SS, JU and BR. I think BR is working but JU is wonky.

March 21, 2024

Fixed up AI (ASCII Input) and AO (ASCII Output)

March 20, 2024

Fixed up AD (ASCII Dump) so it display 16 octets at a time. HD only does 8 at a
time. I've stolen LO (Load S19 file) from Mikbug. I've added PU (Punch S19) but
this is a work in progress.Found that SAVEX is being used by something while
testing PU. Switched to USAVEX. Should expect that as the 6800 doesn't have a
pshx/pulx. Some more of the varaibles are getting comments. Hope to remove
redundancies soon.

March 19, 2024

Code assembles and some functions work. The code is still questionable at this time.

Started March 9, 2024

Current status is that the code assembles but that doesn't mean it works. This code
is a mash of the 6801/MC10 code which has a keyboard and video output. The 6800 needs to deal with a serial port (9600 8,N,1). Additionally the RAM usage is a mess. There's variables in $7300, variables in $7400. The variables in $7400 should be okay. But I stuffed the mystery variable from the MC10 (remember the code overwrote itself) at the end of $7300. I know these variables are either the starting $7300 or $7400 variables. Also the current hb7500.asm is writing to $4XXX which is OS RAM on the MC10. So the related code needs to be fixed.

# Documentation

Anything that has been checked (X) is running okay but not heavily tested. Everthing else either hasn't been tested or is broken. Assume broken.

- [X] AD - ASCII Dump
- [X] AI - ASCII Input
- [X] AO - ASMTAPE1CII Output (Oh not Zero)
- [ ] AT - Analyze Tape (MC10)
- [ ] BA - Change Baud
- [X] BP - Print Break points
- [X] BR - set/reset Breakpoints
- [ ] CO - Continue (after a break)
- [X] CS - Checksum
- [X] DE - Desemble (not Disassemble, only bytes)
- [ ] EX - Exit to BASIC (MC10)
- [X] FI - Find 1, 2 or 3 bytes
- [X] FM - Fill Memory
- [X] HD - Hex Dump
- [X] HE - Help
- [X] JU - Jump (actually JSR)
- [X] LO - LOAD S1
- [X] MC - Memory Compare
- [X] ME - Memory Examine/Edit
- [X] MM - Memory Move
- [ ] MT - Memory Test
- [X] PU - Punch S1
- [X] RC - Register Change
- [X] RE - Register Examine
- [ ] SA - CSAVEM to cassette
- [ ] SS - Single Step (lots of limitations, careful)
- [ ] ST - Start SS (if no Breakpoint)
- [X] !! - Monitor Reset (Cold start)
- [X] U1 - User 1
- [X] U2 - User 2
- [ ] FL - Boot Flex (not written)
- [ ] OS - Boot other OS (not written)

# Files

| Filename            | description                                                |
|---------------------|------------------------------------------------------------|
| MC6800.inc          | Macros - do not use                                        |
| Makefile            | Makefile to build ROM                                      |
| README.md           | This file                                                  |
| asl.inc             | asl.inc - macros, might not need                           |
| begin.py            | Python code to filter a file until the 'begin string'      |
| filter.py           | Python code to filter a symbol table                       |
| hb7500.asm          | WIP, 6800 version of Peter Stark's MC10 Humbug ROM monitor |
| hb7500.lst          | ASL file listing                                           |
| hb7500d.asm         | Peter Stark's MC10 Humbug+ ROM monitor (bin mostly works)  |
| hb7500d.lst         | ASL file listing                                           |
| hb7500d.s19         | ASL S19 dumo of MC10 Humbug+ ROM monitor                   |
| mikbug.asm          | Motorla MIKBUG ROM monitor                                 |
| minibug.asm         |                                                            |
| minibug.lst         |                                                            |
| minipro-help.txt    | Minipro, for the TL866+ help file                          |
| minipro-list.txt    | Minipro EPROM list                                         |
| miniprohex-help.txt | miniprohex, for the TL866+ help file                       |
| motorola.inc        |                                                            |
| s0.sh               | Script to generate S0 header                               |
| smithbug.inc        |                                                            |

# Notes

This is a Work In Progress, the base files (humbug-mc.asm & hb7500d.asm) are incomplete
files. I am managing to put the files together into a workable file. At the moment the
humbug-mc.asm still contains a few 6801 op code that I need to rework with 6800 code.
Replacing the adx OP code has been interesting but I think I have the equivalent code
from Peter's articles.

The MC6800.inc contains a few macros I'm experimenting with. Do be careful with them as
I haven't documented the flags and the macros don't work with all addressing modes.

I will clean this up when done. I am trying to do that as I go along. I seem to be adding
to the mess. ;-)

# Sources

- Peter Stark (RIP), original author
- 

# Credits

- Peter Stark (RIP), original author

