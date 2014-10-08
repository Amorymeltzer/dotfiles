#!/usr/bin/env bash

FILES="^DJI ^IXIC ^GSPC ^TNX"

# Only show investments if after market close or weekend
# Based on DST, correct using Eastern time??
if ((`date -u '+%H'` < 13)) || ((`date -u '+%u'` > 5)); then
    #    FILES+=" VFTNX VSCIX VGSNX FBGKX FXSIX FSKTX"
    FILES="VFTNX VSCPX VGSNX FSKTX FXSIX FBGKX $FILES"
fi

for ticker in $FILES
do
    stockmarket $ticker
done
