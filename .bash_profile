# Path ------------------------------------------------------------
# MacPorts on OS X, add ~/bin
export PATH="/opt/local/bin:/opt/local/sbin:$HOME/bin:$PATH"
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
export HISTSIZE=50000
export HISTFILESIZE=$HISTSIZE
# Display timestamp
# export HISTTIMEFORMAT='[%D %H:%M] '
export HISTTIMEFORMAT='[%F %T] '
export HISTIGNORE='ls:la:ll:lal:lla:l1:ltr:lsr:lasr:lt:l:cdl:btc:clear:exit:logout:history*:h:pg'
#  export HISTIGNORE='clear:exit:logout:h:profileme:btc'


# Append all history to same file
# Still doesn't get it perfectly unified across logins
shopt -s histappend
# Attempt to save all lines of multi-line commands in one line
shopt -s cmdhist
# Autofix transposed/missing/extra characters in cd.  Not too useful but hey
shopt -s cdspell
# Try to correct directory names when tab-completing
# Not in this bash version
# shopt -s dirspell
# Type 'dir' instead of 'cd dir'
# Not in this bash version
# shopt -s autocd
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
echo -ne "Welcome to ${Color_Green}$ITSME"; echo -e "${Color_Off} on" `sw_vers -productName` `sw_vers -productVersion` "("`uname -m`")"
#echo -e "${COLOR_RED}`bash --version`"
if [[ `command -v fortune` && $UID != '0' && $- == *i* && $TERM != 'dumb' ]]; then
    echo -ne "${Color_White}"; fortune -s; echo -ne "${Color_Off}" # only short ones
fi
echo -ne "${Color_Magenta}`uname -sr` up" ; uptime | awk -F'(  |up)' '{print $3" "$4}'
#echo -ne "${Color_Blue}" ; weather | head -n 2 | tail -n 1

# Default designed to always take up same space without leading zero for day
# Meaning dates before the 10th get two spaces, that bugs me
# First line ("last login...") gone with .hushlogin
#echo -ne "${Color_Red}Local time: " ; date
echo -ne "${Color_Red}Local time: " ; date +'%a %b %d %H:%M:%S %Z %Y'
