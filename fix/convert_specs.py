#!/usr/bin/python
# -*- Mode: python -*-
# convert_specs.py --- read fix specs
# Copyright (C) 2018  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Saturday, March 24, 2018
# Version: 1.0
# Modified Time-stamp: <2018-04-02 17:44:17 dharms>
# Modified by: Dan Harms
# Keywords: python fix
import csv
import sys
import os

if len(sys.argv) < 2:
    print "!!! ", sys.argv[0], "needs an input file."
    quit()

infilename = sys.argv[1]
file, ext = os.path.splitext(infilename)
outfilename = file + '-out' + '.hash'

print "Converting", infilename, "to", outfilename

with open(infilename, 'rb') as infile:
    rdr = csv.reader(infile, delimiter='\t')
    with open(outfilename, 'wb') as outfile:
        outfile.write("#s(hash-table size 500 test eql\n")
        outfile.write(" rehash-size 1.5 rehash-threshold 0.8 data (\n")
        cols = []
        desc = ""
        i = 0
        for row in rdr:
            # print "For", i, "Looking at", row
            if i == 0:
                if row:
                    cols.append(row[0])
                    cols.append(row[1])
                    cols.append(row[2])
                    i = i + 1
                else:
                    break
            elif i == 1:
                if row:
                    desc += row[0]
                else:
                    cols.append(desc)
                    i = i + 1
            elif i == 2:
                if row:
                    cols.append(row[0])
                    desc = cols[3].replace('"', '\\"')
                    outfile.write("%s \"%s [%s] %s (%s)\"\n" % (cols[0],
                                                                cols[1],
                                                                cols[2],
                                                                desc,
                                                                cols[4]))
                    cols = []
                    desc = ""
                    i = 0

        outfile.write("\n))\n")

# code ends here
