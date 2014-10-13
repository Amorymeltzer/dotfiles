#!/usr/bin/env bash

quote=`echo $1`;
#curl -s "http://download.finance.yahoo.com/d/quotes.csv?s=$quote&f=l1";
curl -s "http://finance.yahoo.com/q/hp?s=$quote+Historical+Prices" | perl -ne 'print "$1\n" if /tabledata1.*?tabledata1.*?tabledata1.*?tabledata1.*?tabledata1.*?align\=\"right\"\>(\d+,?\d*\.\d\d)/;'
