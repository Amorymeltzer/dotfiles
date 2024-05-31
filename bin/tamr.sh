#!/usr/bin/env bash
# tamr by Amory Meltzer

url="https://grizzlybulls.com/api/models"

signal=$(curl -s "$url" | jq -r '.[] | select(.shortName == "TA-MR Basic") | .signal')

echo "$signal"
