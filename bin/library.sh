#!/usr/bin/env bash

BOOK=$@
BOOK=${BOOK// /+}
echo ${BOOK}

#curl -s "http://harvest.lib.ucdavis.edu/F/SLGG85EXRRE4ARJV2PPDULX9QA862ICB9C3EG6YEIL8JAX36MN-38908?func=find-b&find_code=WRD&request=${BOOK}&local_base=ucd01pub"

#curl -s "http://harvest.lib.ucdavis.edu/F/SLGG85EXRRE4ARJV2PPDULX9QA862ICB9C3EG6YEIL8JAX36MN-38908?func=find-b&find_code=WRD&request=${BOOK}&local_base=ucd01pub"

curl -s "http://harvest.lib.ucdavis.edu/F/NMP6U9NIBGRR81S3MQNPTPT4B71JL8L7NQD9LHUNJRH27CKVC5-00772?func=find-b&find_code=WRD&request=${BOOK}&local_base=ucd01pub" | grep Records | perl -pe 's/<a href.*<\/a> \n//g' | perl -pe 's/ \(maximum.*\)/ records found/g' | perl -pe 's/^.*of\W*//'

curl -s "http://harvest.lib.ucdavis.edu/F/NMP6U9NIBGRR81S3MQNPTPT4B71JL8L7NQD9LHUNJRH27CKVC5-00772?func=find-b&find_code=WRD&request=${BOOK}&local_base=ucd01pub" | grep -B 3 "width=\"5%\" valign=top><A" | sed 's/\<A\ HREF\=.*harvest.*\sub_library\=//g' | sed 's/\<BR\>//g' | sed 's/\<br\>//g' | sed 's/\<td\ .*valign=top\>//g' | sed 's/\&nbsp\;/ /g' | sed -E 's/\ ?\<\/A\>//g' | sed -E 's/\<.?noscript\>//g' | perl -pe 's/<\/td> \n//g' | sed -E 's/^ +//g' | sed 's/--//g' | perl -pe '$_ = "\033[0;34m$_\033[0;32m" if($. % 2)'
