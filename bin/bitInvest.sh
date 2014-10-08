#!/usr/bin/env bash
# bitInvest.sh by Amory Meltzer
# For a given buy and sell price, figure out what I earn

buy=$(echo -e "$1*1.01+.15" | bc -l)

even=$(echo -e "($buy+.15)/.99" | bc -l)
one=$(echo -e "((1.01*$buy)+.15)/.99" | bc -l)
five=$(echo -e "((1.05*$buy)+.15)/.99" | bc -l)

buy=$(printf '%.2f' $buy)

even=$(printf '%.2f' $even)
one=$(printf '%.2f' $one)
five=$(printf '%.2f' $five)


echo -e "Bought:\t$buy"

if [ $2 ]; then
    sel=$(echo -e "$2*.99-.15" | bc -l)
    per=$(echo -e "100*$sel/$buy-100" | bc -l)
    dol=$(echo -e "$sel-$buy" | bc -l)
    sel=$(printf '%.2f' $sel)
    per=$(printf '%+.2f' $per)
    dol=$(printf '%+.2f' $dol)

    # Theoretical return on 25 USD
    bit=$(echo -e "((25-.15)/1.01)/$1" | bc -l)
    bit=$(echo -e "($bit*$2*.99-.15)-25" | bc -l)
    bit=$(printf '%+.2f' $bit)
    # 50
    bits=$(echo -e "((50-.15)/1.01)/$1" | bc -l)
    bits=$(echo -e "($bits*$2*.99-.15)-50" | bc -l)
    bits=$(printf '%+.2f' $bits)
    # 100
    bitx=$(echo -e "((100-.15)/1.01)/$1" | bc -l)
    bitx=$(echo -e "($bitx*$2*.99-.15)-100" | bc -l)
    bitx=$(printf '%+.2f' $bitx)

    echo -e "Sold:\t$sel\nUSD:\t$dol\$\nGain:\t$per%"
fi

echo -e "\n0% at:\t$even\n1% at:\t$one\n5% at:\t$five"

if [ $2 ]; then
    echo -e "\nROI\n25:\t$bit\$\t\n50:\t$bits\$\n100:\t$bitx\$"

    if [ $3 ]; then
	# User-defined
	bitu=$(echo -e "(($3-.15)/1.01)/$1" | bc -l)
	bitu=$(echo -e "($bitu*$2*.99-.15)-$3" | bc -l)
	bitu=$(printf '%+.2f' $bitu)
	echo -e "$3:\t$bitu\$"
    fi
fi
