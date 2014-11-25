#!/usr/bin/env bash
# bootstrap.sh by Amory Meltzer
# Bootstrap installation of home environment
# Start by git clone dotfiles repo, then run this


# Extract, move, source
rsync -avxz . ~
source ~/.bash_profile
# Homebrew
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
# Casks - require perms?
brew install caskroom/cask/brew-cask
# Use cask to install macports
brew cask install macports
# Install other casks, ports, brews...
# Proceed...
