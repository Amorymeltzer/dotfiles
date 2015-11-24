#!/usr/bin/env bash

FILES="^DJI ^IXIC ^GSPC ^NYA ^TNX"

# Only show investments if after market close or weekend
# Based on DST, correct using Eastern time??
if ((`date -u '+%H'` < 13)) || ((`date -u '+%u'` > 5)); then
    FILES="FGCKX FDIKX CCPIX VSCPX VGSNX SSO VOOG QLD QQQ VFFVX RDITX $FILES"
fi
for ticker in $FILES
do
    finance $ticker
done
