#!/usr/bin/env bash
# randTimer.sh by Amory Meltzer
# Set off a timer randomly


if [[ $# -gt 1 ]]; then
    low=$1
    high=$2
elif [[ $# -gt 0 ]]; then
    echo "Please provide two numbers"
    exit
else
    low=10
    high=60
fi

while true; do
    range=$(echo "$high - $low" | bc)
    time=$(echo $RANDOM % "$range" + $low | bc)
    minutes=$(echo "$time"*60 | bc)
    sleep "$minutes"

    echo -n"$(date) "
    echo -e "$time minutes have elapsed\007"
    sleep .25
    echo -en "\007"
    sleep .25
    echo -en "\007"
done
