#!/usr/bin/env python
# WIP ticker replacement python to colorize, columnize, etc.
# Heavily based off bitbar gfinance plugin:
# https://github.com/matryer/bitbar-plugins/blob/master/Finance/gfinance.5m.py

import urllib2
import json
import time                     # why?!

import sys
if len(sys.argv) <= 1:
    sys.exit(1)
else:
    print "hi"


#Stocks can be provided with just the symbol (AAPL) or exchange:symbol (NASDAQ:AAPL)
#stocks={"MSFT","AAPL","GOOGL","AMZN","ONDK"}
stocks=sys.argv[1:]

quote = ""
for i in stocks:
    quote = i + ',' + quote

#  url = "http://finance.google.com/finance/info?client=ig&q=" + quote
    url = "http://finance.google.com/finance/info?client=ig&q=" + i

    try:
        u = urllib2.urlopen(url)
    except Exception:
        print "Ticker lookup of %s failed" % i
        continue
    #sys.exit(1)

    quote = u.read()
    obj = json.loads(quote[4:-1])

    for ticker in obj:
        print "{} {} {} | color=".format(ticker["t"], ticker["l"], ticker["c"]), "red" if float(ticker["c"]) < 0 else "green"
