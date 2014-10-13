#!/bin/sh #-x

for year in {2009..2012} #Files renamed to YYYY since bash 3.2 can't easily handle leading zeros in loops
do
    for month in jan feb mar apr may jun jul aug sep oct nov dec
    do
	new=$(($year+1))
#	echo "old $month$year New $month$new"
#	mv $month$year.txt $month$new.txt
	perl sysopHindex.pl hIndex_files/$month$year.txt hIndex_files/$month$new.txt
    done
done
