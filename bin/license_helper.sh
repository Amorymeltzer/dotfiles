#!/usr/bin/env bash
# license_helper.sh by Amory Meltzer
# Script to help automate license updating of hombrew-cask casks
## Use dedicated list to save time?
## Ask first?

cd /usr/local/Library/Taps/caskroom/homebrew-cask/Casks/

cask=$(grep -i "license :unknown" *.rb|cut -f 1 -d ':'|sed 's/.rb$//g'|shuf|head -n 1)

brew cask cat $cask
brew cask home $cask
