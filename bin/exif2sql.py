#!/usr/bin/env python
# -*- mode:python; tab-width:4; indent-tabs-mode:nil; py-indent-offset:4 -*-
##

"""
    module_name_or_program_name
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Brief description
"""

def main():
    line = sys.stdin.readline()
    line_no = 0
    while line:
        line_no += 1
        try:
            name,val = line.strip().split("\t")

        except Exception, e:
            sys.stderr.write("ERROR on line %d (ignored): %s" % (line_no,e))
        line = sys.stdin.readline()

if __name__ == '__main__':
    main()
