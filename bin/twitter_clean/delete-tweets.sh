#!/usr/bin/env bash
# delete-tweets.sh by Amory Meltzer
# Delete tweets made before a given date

limit='4200'

twip='@'$1
if [[ -z "$twip" ]]; then
    echo "You must provide a username"
fi

ago=$2
if [[ -z "$ago" ]]; then
    echo "You must provide a valid relative reference to a date in the past that date can parse, e.g. 6m"
    exit 1
fi

if [[ -n "$3" ]]; then
    limit=$3
fi

# Very simple/basic date handling
ago=$(date -v -"$ago" +'%Y%m%d')
if [[ -z "$ago" ]]; then
    echo "Not a valid relative reference"
    exit 1
fi
now=$(date +'%Y%m%d')
if [ "$now" -lt "$ago" ]; then
    echo "The reference must be to a past time"
    exit 1
fi


# Get tweets
readarray -t tweets < <(t timeline $twip --csv --number $limit)
# Remove header
tweets=( "${tweets[@]:1}" )
# Some dumbness around newlines I'm too lazy to deal with
for tweet in "${tweets[@]}"; do
    id="${tweet%%','*}"

    trim="${tweet#*','}" # Lop off id

    date="${trim%%' '*}" # Only consider days
    date="${date//-}" # Lose the hyphens

    if [ "$date" -lt "$ago" ]; then
	t delete status "$id"
    fi
done
