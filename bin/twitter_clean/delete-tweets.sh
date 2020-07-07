#!/usr/bin/env bash
# delete-tweets.sh by Amory Meltzer
# Delete tweets made before a given date

limit='4200'

function get_help {
    cat <<END_HELP

Usage: $(basename $0) -u @username -o <date-valid relative date> [-l number] [-f]

  -u		Username, including the leading @.  Required.
  -o		A relative date reference, such as 7m or 42y.  Required.
  -l		How many to tweets to search.  Optional
  -f		Delete favorites instead.
  -h		this help
END_HELP
}

while getopts 'u:o:l:hf' opt; do
    case $opt in
	u) twip=$OPTARG;;
	o) ago=$OPTARG;;
	l) limit=$OPTARG;;
	f) favs='1';;
	h) get_help $0
	   exit 0;;
    esac
done


if [[ -z "$twip" ]]; then
    echo "You must provide a username"
    exit 1
fi

if [[ -z "$ago" ]]; then
    echo "You must provide a valid relative reference to a date in the past that date can parse, e.g. 6m"
    exit 1
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
	if [[ -z $favs ]]; then
	    t delete favorite "$id"
	else
	    t delete status "$id"
	fi
    fi
done
