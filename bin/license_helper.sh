#!/usr/bin/env bash
# license_helper.sh by Amory Meltzer
# Script to help automate license updating of hombrew-cask casks

grep -i "license :unknown" /usr/local/Library/Taps/caskroom/homebrew-cask/Casks/*.rb | column -t | cut -f 9 -d '/' |cut -f 1 -d ':' | sed 's/.rb$//g'
