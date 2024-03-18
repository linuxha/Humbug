# Humbug

Peter Stark's Humbug monitor for the Motorola 68xx. Copied from the
Kilobaud/Micromputing articles of July, August and September of 1980.

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

Right now this is just a mess of files. It won't compile yet.

# History

Started March 9, 2024

Current status is that the code assembles but that doesn't mean it works. This code
is a mash of the 6801/MC10 code which has a keyboard and video output. The 6800 needs to deal with a serial port (9600 8,N,1). Additionally the RAM usage is a mess. There's variables in $7300, variables in $7400. The variables in $7400 should be okay. But I stuffed the mystery variable from the MC10 (remember the code overwrote itself) at the end of $7300. I know these variables are either the starting $7300 or $7400 variables. Also the current hb7500.asm is writing to $4XXX which is OS RAM on the MC10. So the related code needs to be fixed.

# Documentation

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
| hb7500d.asm         | Peter Stark's MC10 Humbug+ ROM monitor (bin mostly works)      |
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

# Sources

- Peter Stark (RIP), original author
- 

# Credits

- Peter Stark (RIP), original author

