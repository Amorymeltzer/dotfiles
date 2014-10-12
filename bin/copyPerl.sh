#!//usr/bin/env bash

rsync -avxz ~/.bashrc /Volumes/SANDISK/mobilePerl/.bashrc
rsync -avxz ~/.bash_profile /Volumes/SANDISK/mobilePerl/.bash_profile
rsync -avxz ~/.bash_history /Volumes/SANDISK/mobilePerl/.bash_history
rsync -avxz ~/.inputrc /Volumes/SANDISK/mobilePerl/.inputrc
rsync -avxz ~/.perlcriticrc /Volumes/SANDISK/mobilePerl/.perlcriticrc
rsync -avxz ~/.perltidyrc /Volumes/SANDISK/mobilePerl/.perltidyrc
rsync -avxz ~/.gitconfig /Volumes/SANDISK/mobilePerl/.gitconfig
rsync -avxz ~/.z /Volumes/SANDISK/mobilePerl/.z
rsync -avxz ~/.z.sh /Volumes/SANDISK/mobilePerl/.z.sh
rsync -avxz ~/.colordiffrc /Volumes/SANDISK/mobilePerl/.colordiffrc
rsync -avxz ~/.wgetrc /Volumes/SANDISK/mobilePerl/.wgetrc
rsync -avxz ~/.curlrc /Volumes/SANDISK/mobilePerl/.curlrc
rsync -avxz ~/.hushlogin /Volumes/SANDISK/mobilePerl/.hushlogin
rsync -avxz ~/diary /Volumes/SANDISK/mobilePerl/diary
rsync -avxz ~/.emacs /Volumes/SANDISK/mobilePerl/.emacs
rsync -avxz ~/.emacs.d/ /Volumes/SANDISK/mobilePerl/.emacs.d
rsync -avxz ~/Documents/perl/ /Volumes/SANDISK/mobilePerl/perl
rsync -avxz ~/bin/ /Volumes/SANDISK/mobilePerl/bin --exclude-from=/Volumes/SANDISK/mobilePerl/exclude
rsync -avxz ~/.completions.d/ /Volumes/SANDISK/mobilePerl/.completions.d

rsync -avxz /opt/local/bin/pianobar /Volumes/SANDISK/mobilePerl/bin/pianobar
