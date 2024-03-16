#!/usr/bin/python3

# Got help from Co-pilot but it was only 50% helpful
# a lot of the time the code was wrong

# Read from standard input
# and write to standard output after matching the begin pattern
# continue until the end pattern or end of file is reached
import sys

# read begin and end patterns from the command line
if len(sys.argv) < 2:
    sys.stderr.write("Usage: %s begin_pattern end_pattern\n" % sys.argv[0])
    sys.exit(1)
#

begin_pattern = sys.argv[1]

if len(sys.argv) == 3:
    end_pattern = sys.argv[2]
else:
    end_pattern = "\0" # Can't use None, can't use "" either
#

for line in sys.stdin:
    # check if the line contains the begin_pattern
    if begin_pattern in line: #print("# Begin") print(line) break
        print(line.strip())
        break
    #
#

for line in sys.stdin:
    if end_pattern in line:
        print(line.strip())
        break
    #
    print(line.strip())
#
