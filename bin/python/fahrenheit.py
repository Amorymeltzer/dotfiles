#!/usr/bin/env python2

import string, sys

# If no arguments were given, print a helpful message
if len(sys.argv)==1:
    print 'Usage: fahrenheit temp1 temp2 ...'
    sys.exit(0)

# Loop over the arguments
for i in sys.argv[1:]:
    try:
        celsius=float(string.atoi(i))
    except string.atoi_error:
	print repr(i), "not a numeric value"
    else:
	fahrenheit=(celsius*9.0/5.0)+32
	print '%i\260C = %i\260F' % (int(celsius), int(fahrenheit+.5))
