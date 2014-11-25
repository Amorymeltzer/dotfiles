#!/usr/bin/env bash
# bootstrap.sh by Amory Meltzer
# Bootstrap installation of home environment
# Start by git clone dotfiles repo, then run this


# Extract, move, source
mv ../../dotfiles/* ~/
# Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# Casks - require perms?
# Use cask to install macports
# Proceed...
