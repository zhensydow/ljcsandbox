#!/usr/bin/env python

import sys

def loadFile( filename ):
    f = open( filename, 'r')
    dats = []
    for line in f:
        row = []
        for word in line.split():
            row.append( float( word ) )
            
        dats.append( row )

    return dats

def printTab( dats ):
    for row in dats:
        for val in row:            
            print val,
        print ""

def main():
    if len( sys.argv ) != 2:
        print ('usage: {0:s} <file>'.format( sys.argv[0] ))
        sys.exit(2)

    a = loadFile( sys.argv[1] )
    printTab( a )

if __name__ == "__main__":
    main()
