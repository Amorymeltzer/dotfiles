#!/usr/bin/env bash

rsync -avxz /Volumes/SANDISK/mobilePerl/.bashrc ~/.bashrc
rsync -avxz /Volumes/SANDISK/mobilePerl/.bash_profile ~/.bash_profile
rsync -avxz /Volumes/SANDISK/mobilePerl/.bash_history ~/.bash_history
rsync -avxz /Volumes/SANDISK/mobilePerl/.inputrc ~/.inputrc
rsync -avxz /Volumes/SANDISK/mobilePerl/.perlcriticrc ~/.perlcriticrc
rsync -avxz /Volumes/SANDISK/mobilePerl/.perltidyrc ~/.perltidyrc
rsync -avxz /Volumes/SANDISK/mobilePerl/.tidyrc ~/.tidyrc
rsync -avxz /Volumes/SANDISK/mobilePerl/.gitconfig ~/.gitconfig
rysnc -avxz /Volumes/SANDISK/mobilePerl/.z ~/.z
rysnc -avxz /Volumes/SANDISK/mobilePerl/.z.sh ~/.z.sh
rsync -avxz /Volumes/SANDISK/mobilePerl/.colordiffrc ~/.colordiffrc
rsync -avxz /Volumes/SANDISK/mobilePerl/.wgetrc ~/.wgetrc
rsync -avxz /Volumes/SANDISK/mobilePerl/.curlrc ~/.curlrc
rsync -avxz /Volumes/SANDISK/mobilePerl/.hushlogin ~/.hushlogin
rsync -avxz /Volumes/SANDISK/mobilePerl/.emacs ~/.emacs
rsync -avxz /Volumes/SANDISK/mobilePerl/.emacs.d/ ~/.emacs.d
rsync -avxz /Volumes/SANDISK/mobilePerl/perl ~/Documents/
rsync -avxz /Volumes/SANDISK/mobilePerl/git ~/Documents/
rsync -avxz /Volumes/SANDISK/mobilePerl/bin ~/
rsync -avxz /Volumes/SANDISK/mobilePerl/.completions.d/ ~/

rsync -avxz /Volumes/SANDISK/mobilePerl/removePerl.sh ~/bin/removePerl.sh
