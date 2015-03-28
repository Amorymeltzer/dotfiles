#!/usr/bin/env bash

FILES="^DJI ^IXIC ^GSPC ^TNX"

# Only show investments if after market close or weekend
# Based on DST, correct using Eastern time??
if ((`date -u '+%H'` < 13)) || ((`date -u '+%u'` > 5)); then
    FILES="FSPNX FBGKX FXSTX FSKAX FXAIX FOSKX FLBAX $FILES"
fi

for ticker in $FILES
do
    finance $ticker
done
