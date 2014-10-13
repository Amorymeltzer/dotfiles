#!/usr/bin/env bash
# December 2013
# Figure out how much I would earn from my bitcoin "investing"

coins=$(cut -f 3 -d ',' ~/coinbase.csv | tail -n +2 | paste -sd+ - | bc -l)
spent=$(cut -f 6 -d ',' ~/coinbase.csv | tail -n +2 | paste -sd+ - | bc -l)

avg=$(echo -e "$spent/$coins" | bc -l)

echo -ne "coins\tspent\tavg"
if [ $1 ]; then
    echo -e "\tact\trate"
    else
    echo
fi

printf '%.3f\t%.3f\t%.3f' $coins $spent $avg

if [ $1 ]; then
    actual=$(echo -e "$1*.99-.15" | bc -l)
    rate=$(echo -e "100*($actual/$avg)-100" | bc -l)
    printf '\t%.3f\t%.3f' $actual $rate
    echo "%"
    else
    echo
fi
