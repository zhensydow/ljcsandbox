#!/usr/bin/env python
"""Example program for load a Table from a file."""
import sys


def load_file(filename):
    """Load a file and returns it as a table."""
    hfile = open(filename, 'r')
    dats = []
    for line in hfile:
        row = []
        for word in line.split():
            row.append(float(word))

        dats.append(row)

    return dats


def print_tab(dats):
    """Prints formatted table."""
    for row in dats:
        for val in row:
            print val,
        print ""


def main():
    """Main function."""
    if len(sys.argv) != 2:
        print 'usage: {0:s} <file>'.format(sys.argv[0])
        sys.exit(2)

    table = load_file(sys.argv[1])
    print_tab(table)


if __name__ == "__main__":
    main()
