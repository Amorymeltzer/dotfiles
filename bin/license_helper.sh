#!/usr/bin/env bash
# license_helper.sh by Amory Meltzer
# Script to help automate license updating of hombrew-cask casks

cd /usr/local/Library/Taps/caskroom/homebrew-cask/Casks/

cask=$(grep -i "license :unknown" *.rb|cut -f 1 -d ':'|sed 's/.rb$//g'|shuf|head -n 1)

echo "Looking up homepage for $cask"
sleep 1
brew cask home $cask
