#!/usr/bin/env bash
# basic_strategy.sh by Amory Meltzer
# Wrapper around basic_strategy.pl for columnization

# ${#arrayname[@]} gives the length of the array
# ${#arrayname[n]} gives the length of nth element

perl ~/Documents/perl/sandbox/basic_strategy.pl | awk '{printf "%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n", $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11;}' 
