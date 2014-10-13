#!/usr/bin/env bash

i=1 # day
j=1 # month
k=1 # day of week

while [ $i -lt 13 ];
do
    while [ $j -lt 32 ];
    do
	while [ $k -lt 8 ];
	do
	    echo -ne "m: $i d: $j w: $k "
	    perl bus.pl -d $i $j $k
	    k=$(expr $k + 1)
	done
	j=$(expr $j + 1)
	k=1
    done
    i=$(expr $i + 1)
    j=1
done
