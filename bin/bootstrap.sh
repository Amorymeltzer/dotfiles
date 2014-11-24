#!/usr/bin/env bash
# bootstrap.sh by Amory Meltzer
# Bootstrap installation of home environment


# Download dotfiles repo
## Use either curl or wget depending
# Extract, move, source
mv ../../dotfiles/* ~/
# Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# Casks - require perms?
# Use cask to install macports
# Proceed...
