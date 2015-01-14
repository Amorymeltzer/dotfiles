#!/usr/bin/env bash
# tab_cleanup.sh by Amory Meltzer
# Encourage me to cleanup my beowser tabs, one a day

# 14 Jan 2015 - 181 tabs
today=$(date +%-j)		# %- removes padding 0 in day of year %j
tabs=$((181+14-$today))

printf %s "$(date +"%d %b %Y - ")"
echo $tabs tabs required
