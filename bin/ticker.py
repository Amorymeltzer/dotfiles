#!/usr/bin/env python
# WIP ticker replacement python to colorize, columnize, etc.
# Heavily based off bitbar gfinance plugin:
# https://github.com/matryer/bitbar-plugins/blob/master/Finance/gfinance.5m.py

import urllib2
import json
import sys

if len(sys.argv) <= 1:
    sys.exit(1)


# Stocks can be provided as symbol (AAPL) or exchange:symbol (NASDAQ:AAPL)
stocks=sys.argv[1:]

# Nothing fancy
class colors:
    GREEN = '\033[32m'
    RED = '\033[31m'
    ENDC = '\033[0m'

quote = ""
for i in stocks:
    url = "http://finance.google.com/finance/info?client=ig&q=" + i

    try:
        u = urllib2.urlopen(url)
    except Exception:
        print "Ticker lookup of %s failed" % i
        continue

    quote = u.read()
    obj = json.loads(quote[4:-1])

    for ticker in obj:
        if float(ticker["c"]) < 0:
            color = colors.RED
        else:
            color = colors.GREEN

        print color + "\t".join([ticker["t"], ticker["l"], ticker["c"], ticker["cp"]]) + colors.ENDC
