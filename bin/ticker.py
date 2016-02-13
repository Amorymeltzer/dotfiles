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


class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

#Stocks can be provided with just the symbol (AAPL) or exchange:symbol (NASDAQ:AAPL)
#stocks={"MSFT","AAPL","GOOGL","AMZN","ONDK"}
stocks=sys.argv[1:]

from termcolor import colored
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
        if float(ticker["c"]) < 0:
            color = 'red'
        else:
            color = 'green'
        #print "{} {} {} | color=".format(ticker["t"], ticker["l"], ticker["c"]), "red" if float(ticker["c"]) < 0 else "green"
        print color
        print "{} {} {}".format(ticker["t"], ticker["l"], ticker["c"])
        print colored ('"{} {} {}".format(ticker["t"], ticker["l"], ticker["c"])', color)
        print bcolors.OKGREEN + "Green" + bcolors.OKBLUE + "Blue" + bcolors.ENDC
