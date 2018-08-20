#!/usr/bin/env python2.7

import sys
print_lines = False

for line in sys.stdin:
    if line.strip().endswith("*/"):
        sys.exit(0);
    if print_lines:
        print(line[3:-1])
    elif line.strip().startswith("/*"):
        print_lines = True
