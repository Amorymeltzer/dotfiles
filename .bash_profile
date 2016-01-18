# Path ------------------------------------------------------------
# Add python execs, not sure if this is the best but so be it
export PATH="/opt/local/Library/Frameworks/Python.framework/Versions/Current/bin:$PATH"
# Add perl execs; again, not sure if this is best
export PATH="/opt/local/libexec/perl5.22/sitebin:$PATH"
# add ~/bin, MacPorts, then Homebrew (duplicates /usr/local/bin) FIXME TODO
export PATH="$HOME/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:$PATH"
#export PATH=~/bin/msdir:$PATH  # add ms to command line
#export PATH=~/Documents/perl/sandbox:$PATH # add some perl

# if [ -d ~/bin ]; then
#     export PATH=~/bin:$PATH  # add bin folder to the path, if I've got it.
# fi

# Add unloved perl modules
export MANPATH="/opt/local/share/perl5.20/siteman:/opt/local/share/perl5.20/man:$MANPATH"

# emacs > vim
export EDITOR="emacs"
# Ignore sequential duplicates in history
export HISTCONTROL=ignoreboth
# Up history size
export HISTSIZE=500000
export HISTFILESIZE=$HISTSIZE
# Display timestamp
# export HISTTIMEFORMAT='[%D %H:%M] '
export HISTTIMEFORMAT='[%F %T] '
export HISTIGNORE='ls:la:ll:lal:lla:l1:ltr:lsr:lasr:lt:l:cdl:pwd:* --help:* -h:btc:clear:exit:logout:history*:h:pg'
#  export HISTIGNORE='clear:exit:logout:h:profileme:btc'


# Append all history to same file, don't overwrite
# Still doesn't get it perfectly unified across logins
shopt -s histappend
# Attempt to save all lines of multi-line commands in one line
shopt -s cmdhist
# Autofix transposed/missing/extra characters in cd.  Not too useful but hey
shopt -s cdspell
# Try to correct directory names when tab-completing
shopt -s dirspell
# Type 'dir' instead of 'cd dir'
shopt -s autocd
# Use ** for recursive glob
shopt -s globstar
# Check window size after each command, update values of lines/columns
shopt -s checkwinsize
# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob
# Do not autocomplete when accidentally pressing Tab on an empty line. (It takes
# forever and yields "Display all 15 gazillion possibilites?")
shopt -s no_empty_cmd_completion;
# If a command contains an invalid history operation, let me correct it
shopt -s histreedit;


# Load in .bashrc -------------------------------------------------
source ~/.bashrc

# Welcome Messsage --------------------------------------------------
echo -ne "Welcome to${Color_Green}" `hostname -s` "${Color_zOff}on" `sw_vers -productName`
echo -e "${Color_Green}" `sw_vers -productVersion` "${Color_zOff}("`uname -m`")"
if [[ `command -v fortune` && $UID != '0' && $- == *i* && $TERM != 'dumb' ]]; then
    echo -ne "${Color_White}"; fortune -s; echo -ne "${Color_zOff}" # only short ones
fi
echo -ne "${Color_Magenta}`uname -sr` up" ; uptime | awk -F'(  |up)' '{print $3" "$4}'
echo -ne "${Color_Cyan}" ; today

# Default designed to always take up same space without leading zero for day
# Meaning dates before the 10th get two spaces, that bugs me
# First line ("last login...") gone with .hushlogin
echo -ne "${Color_Red}Local time: ${Color_zOff}" ; date +'%a %b %d %H:%M:%S %Z %Y'
