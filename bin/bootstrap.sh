#!/usr/bin/env bash
# bootstrap.sh by Amory Meltzer
# Bootstrap installation of home environment
# Start by git clone dotfiles repo, then run this


# Extract, move, source
rsync -avxz . ~
source ~/.bash_profile
# Homebrew and Cask
# Check for Homebrew
if test ! $(which brew)
then
    echo "  Installing Homebrew for you."
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
# Use cask to install macports
brew cask install macports
# Install other casks, ports, brews...
# Proceed...
