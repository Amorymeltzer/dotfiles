## Notes
# /usr/share/doc/bash
# man fold
# man sysctl
# cp file{,.bk} to copy to backup

# Used in prompt, set here rather than .bash_profile
# to avoid weirdness when calling bash from within bash
ITSME=$(hostname -s)

# I see this around, seems good?  ;;;;;; ##### FIXME TODO
# if not running interactively, don't do anything
if [ -z "$PS1" ]; then
    return
fi

# Localization
export LC_ALL=C

# Colors ----------------------------------------------------------
export TERM=xterm-color
# Change for different colors, to magenta
#  export GREP_OPTIONS='--color=auto' GREP_COLOR='0;35'
alias grep='grep --color=auto '
export GREP_COLOR='0;35'
export CLICOLOR=1

# ls colors, see: http://www.linux-sxs.org/housekeeping/lscolors.html
#export LS_COLORS='di=1:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:*.rb=90'  #LS_COLORS is not supported by the default ls command in OS-X
#export LS_COLORS="ExGxBxDxCxEgEdxbxgxcxd"
#export LS_COLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:';

alias ls='ls -FGh'  # Folder symbols, colors, human-readable sizes
alias l='ls'
alias ll='ls -l'
alias la='ls -A'
alias lal='ls -lA'
alias lla='lal'
alias l1='ls -1'
alias lt='ls -lt'
alias lr='ll -R'
alias ltr='ls -ltr'
alias latr='ltr -A'
alias lat='lt -A'
alias lss='ls -lS'
alias lsr='ls -lrS'
alias lasr='lsr -A'
alias las='lss -A'
alias lass='las'
# Note: flags get passed to grep, not ls
alias ld='ls -l | grep "^d"'
alias lad='ls -Al | grep "^d"'
alias lda='lad'

# Colorized, recursive ls-like tree
alias treed='tree -aC -I ".git" --dirsfirst "$@" | less -FRNX'
alias tree='tree -Csuh'

# Git stuff
# Alias hub as git for github https://github.com/github/hub
alias git='hub'
alias g='git'
alias gb='git b '
alias gcb='git copy-branch-name'
alias gco='git co '
alias gg='git go '
alias gs='git s'
alias ga='git add '
alias gaa='git aa '
alias gaas='git aas'
alias gas='git aas'
alias gd='git d'
alias gdc='git dc'
alias gcm='git cm '
alias gl='git l1 -5'
alias gl1='git l1'

# Setup some colors to use later in interactive shell or scripts
# ~/bin/colordump and termcolors give some clues about these
# 0; = regular
# 1; = bold
# 4; = underline
# 5; = blinking
# 7; = highlighted

export Color_zOff="\033[0m"      # No color, stupid stupid hax for sorting
export Color_zInvisible="\033[8m" # Invisible text

# Regular Colors
export Color_Black="\033[0;30m"        # Black
export Color_Red="\033[0;31m"          # Red
export Color_Green="\033[0;32m"        # Green
export Color_Yellow="\033[0;33m"       # Yellow
export Color_Blue="\033[0;34m"         # Blue
export Color_Magenta="\033[0;35m"      # Magenta
export Color_Cyan="\033[0;36m"         # Cyan
export Color_White="\033[0;37m"        # White

# Bold
export Color_Black_Bold="\033[1;30m"       # Black
export Color_Red_Bold="\033[1;31m"         # Red
export Color_Green_Bold="\033[1;32m"       # Green
export Color_Yellow_Bold="\033[1;33m"      # Yellow
export Color_Blue_Bold="\033[1;34m"        # Blue
export Color_Magenta_Bold="\033[1;35m"     # Magenta
export Color_Cyan_Bold="\033[1;36m"        # Cyan
export Color_White_Bold="\033[1;37m"       # White

# Underline
export Color_Black_Underline="\033[4;30m"       # Black
export Color_Red_Underline="\033[4;31m"         # Red
export Color_Green_Underline="\033[4;32m"       # Green
export Color_Yellow_Underline="\033[4;33m"      # Yellow
export Color_Blue_Underline="\033[4;34m"        # Blue
export Color_Magenta_Underline="\033[4;35m"     # Magenta
export Color_Cyan_Underline="\033[4;36m"        # Cyan
export Color_White_Underline="\033[4;37m"       # White

# # Blinking
# export Color_Black_Blink="\033[5;30m"       # Black
# export Color_Red_Blink="\033[5;31m"         # Red
# export Color_Green_Blink="\033[5;32m"       # Green
# export Color_Yellow_Blink="\033[5;33m"      # Yellow
# export Color_Blue_Blink="\033[5;34m"        # Blue
# export Color_Magenta_Blink="\033[5;35m"     # Magenta
# export Color_Cyan_Blink="\033[5;36m"        # Cyan
# export Color_White_Blink="\033[5;37m"       # White

# # Reverse
# export Color_Black_Reverse="\033[7;30m"       # Black
# export Color_Red_Reverse="\033[7;31m"         # Red
# export Color_Green_Reverse="\033[7;32m"       # Green
# export Color_Yellow_Reverse="\033[7;33m"      # Yellow
# export Color_Blue_Reverse="\033[7;34m"        # Blue
# export Color_Magenta_Reverse="\033[7;35m"     # Magenta
# export Color_Cyan_Reverse="\033[7;36m"        # Cyan
# export Color_White_Reverse="\033[7;37m"       # White

# Background
# 'z' prefix merely for sorting
export Color_Black_zBackground="\033[40m"       # Black
export Color_Red_zBackground="\033[41m"         # Red
export Color_Green_zBackground="\033[42m"       # Green
export Color_Yellow_zBackground="\033[43m"      # Yellow
export Color_Blue_zBackground="\033[44m"        # Blue
export Color_Magenta_zBackground="\033[45m"     # Magenta
export Color_Cyan_zBackground="\033[46m"        # Cyan
export Color_White_zBackground="\033[47m"       # White

# High Intensty
export Color_Black_Intense="\033[0;90m"       # Black
export Color_Red_Intense="\033[0;91m"         # Red
export Color_Green_Intense="\033[0;92m"       # Green
export Color_Yellow_Intense="\033[0;93m"      # Yellow
export Color_Blue_Intense="\033[0;94m"        # Blue
export Color_Magenta_Intense="\033[0;95m"     # Magenta
export Color_Cyan_Intense="\033[0;96m"        # Cyan
export Color_White_Intense="\033[0;97m"       # White

# Bold High Intensty
# No difference to bold if bright colors used for bold in Terminal settings
export Color_Black_Bold_Intense="\033[1;90m"      # Black
export Color_Red_Bold_Intense="\033[1;91m"        # Red
export Color_Green_Bold_Intense="\033[1;92m"      # Green
export Color_Yellow_Bold_Intense="\033[1;93m"     # Yellow
export Color_Blue_Bold_Intense="\033[1;94m"       # Blue
export Color_Magenta_Bold_Intense="\033[1;95m"    # Magenta
export Color_Cyan_Bold_Intense="\033[1;96m"       # Cyan
export Color_White_Bold_Intense="\033[1;97m"      # White

# # High Intensty backgrounds
# export Color_Black_zBackground_Intense="\033[0;100m"   # Black
# export Color_Red_zBackground_Intense="\033[0;101m"     # Red
# export Color_Green_zBackground_Intense="\033[0;102m"   # Green
# export Color_Yellow_zBackground_Intense="\033[0;103m"  # Yellow
# export Color_Blue_zBackground_Intense="\033[0;104m"    # Blue
# export Color_Magenta_zBackground_Intense="\033[0;105m" # Magenta
# export Color_Cyan_zBackground_Intense="\033[0;106m"    # Cyan
# export Color_White_zBackground_Intense="\033[0;107m"   # White


alias colorslist="set | egrep '^Color_\w*' | sort"
# Lists colors in their actual color, on one line
alias colors="echo -e \`colorslist | sed 's/\(.*\)=\(.*\)/\2 \1/'\`"

function colordump() {
    # One column
    # for i in {0..255} ; do
    #	printf "\x1b[38;5;${i}mcolour${i}\n"
    # done

    # Six columns
    for i in {0..255} ; do
	declare -i a="$i % 6";
	if [ $a == '0' ]; then
	    printf "\x1b[38;5;${i}mcolour${i}\n";
	else
	    printf "\x1b[38;5;${i}mcolour${i} ";
	fi
    done
    echo
}

# more is less
alias more='less '
# Set less as default manpager, screen won't clear after quitting man
# -F, quit-if-one-screen, if it fits then print it and quit it
# -X, no-init, don't clear screen first
# -i, ignore-case
# -R, RAW-CONTROL-CHARS, show ANSI color escapes but not other escapes
# -g, highlight search, slightly faster?
# -M, display currently viewed lines
# -w, highlight first new unread line
# -N, display line numbers.  Not used.
# -z[n], page scroll n lines instead of one page.  Not used.

# Doesn't display percentage until whole document has gone through.  Type Gg
# once there to jump to bottom then to the top.  Ill-advised for large files
export MANPAGER="less -FXiRgMw";
export LESS="-FXiRgMw";

# Use lesspipe.sh (look inside archives) just in case
if [ -x /opt/local/bin/lesspipe.sh ]; then
    export LESSOPEN='| /opt/local/bin/lesspipe.sh %s'
fi

# Give man pages some color.  Can't use color names?
export LESS_TERMCAP_mb=$'\033[0;37m' # section titles
export LESS_TERMCAP_md=$'\033[0;34m' # bold header
export LESS_TERMCAP_me=$'\033[0m'    # end bold
export LESS_TERMCAP_so=$'\033[0;31m' # begin standout-mode - info box
export LESS_TERMCAP_se=$'\033[0m'    # end standout-mode
export LESS_TERMCAP_ue=$'\033[0m'    # end underline
export LESS_TERMCAP_us=$'\033[4;35m' # begin underline

# Open the manual page for the last command you executed.
function lman {
    set -- $(fc -nl -1);
    while [ $# -gt 0 -a '(' "sudo" = "$1" -o "-" = "${1:0:1}" ')' ]; do
	shift;
    done;
    cmd="$(basename "$1")";
    man "$cmd" || help "$cmd";
}
alias lastman='lman '

# Misc -------------------------------------------------------------
# bash readline settings
# note: bind used instead of sticking these in .inputrc
# ignore case
bind "set completion-ignore-case on"
# show list automatically, without double tab
bind "set show-all-if-ambiguous on"
# similar to above?
bind "set show-all-if-unmodified on"
# if long names are similar, use ...
bind "set completion-prefix-display-length 2"
# show all autocomplete results at once???
bind "set page-completions off"
# ask if sure for >125 items intead of 100
bind "set completion-query-items 125"
# only if start with .
bind "set match-hidden-files off"
# extra info when completing e.g. ls -F???
bind "set visible-stats on"
# automatically add trailing slash when autocomplete symlinks to directories
bind "set mark-symlinked-directories on"
# don't expand ~ to home dir when completing.  Off is usually the default anyway
bind "set expand-tilde off"
# try to keep the cursor position when moving through  history
bind "set history-preserve-point on"


### Prompts ----------------------------------------------------------
#export PS1="${Color_Cyan}\t \#_\u:\w> ${Color_zOff}"  # Primary prompt with user and path
#export PS1="${Color_Cyan}\u@\h${Color_Green} \w> ${Color_zOff}"  # Primary prompt with user, host, and path
#export PS1="${Color_Cyan}\u${Color_White_Bold_Intense}@${Color_Cyan}\h${Color_White_Bold_Intense}>${Color_Green}\w${Color_White_Bold_Intense}\$ ${Color_zOff}"

# Smiley (cat?) face prompts, three kinds
#export PS1="\`if [ \$? = 0 ]; then echo \e[33\;40m\\\^\\\_\\\^\e[0m; else echo \e[36\;40m\\\-\e[0m\\\_\e[36\;40m\\\-\e[0m; fi\` \u \w:\h)"
#export PS1="\033[0;32m\u@\h\033[0m:\033[0;36m\A\033[0m:\`if [ \$? = 0 ]; then echo '\033[1;30m^_^\033[0m'; else echo '\033[0;33m>_>\033[0m' ; fi\`:\033[1;34m\w\033[0m>\033[0m "
#export PS1="\`if [ \$? = 0 ]; then echo \[\e[34m\]^_^\[\e[0m\]; else echo \[\e[31m\]O_O\[\e[0m\]; fi\`[\u@\h:\w]\\$ "

# Display name in red if error
#export PROMPT_COMMAND='if (($? > 0)); then export PS1="${Color_Cyan}\t \#_${Color_Red_Bold_Intense}\u${Color_Cyan}:\w> ${Color_zOff}"; else export PS1="${Color_Cyan}\t \#_\u:\w> ${Color_zOff}"; fi'

# Neater, no errors?
fill="--- " # Why the space?

function battery_charge {
    echo `battery.py` 2>/dev/null
}

# Reset color for command output, invoked every time before command is executed
# Seems unnecessary right now
#    trap 'echo -ne "\033[00m"' DEBUG

function prompt_command {

    # Catch previous errors
    # Before everything else so that the return value isn't valid
    if (($? > 0)); then
	ERRORS=1
    else
	ERRORS=0
    fi

    #    if [[ $ITSME == $(hostname -s) ]]; then
    # Deal with stupid Durandal != durandal shit at work
    if [ `echo $ITSME | tr [:upper:] [:lower:]` == `hostname -s | tr [:upper:] [:lower:]` ]; then
	CNX=${Color_Red} # Connected on local machine
    else
	CNX=${Color_Green} # Connected on remote machine (ssh, presumably)
    fi

    # Test user type
    # Doesn't auto work with root ;;;;;; ##### FIXME TODO
    # see also `id -un` instead(?) og `logname`
    if [[ $UID -eq 0 ]]; then
	SUD=${Color_Red}           # User is root
    elif [[ ${USER} != $(logname) ]]; then
	SUD=${Color_Red_zBackground} # User is not login user
    else
	SUD=${Color_Green} # User is normal (mostly)
    fi

    NCPU=$(sysctl -n hw.ncpu) # Number of CPUs
    SLOAD=$(( 100*${NCPU} ))  # Small load
    MLOAD=$(( 200*${NCPU} ))  # Medium load
    XLOAD=$(( 400*${NCPU} ))  # Large load

    # System load of the current host, as percentage (40 instead of 0.40)
    function load()
    {
	local SYSLOAD=$(uptime | cut -d ":" -f 4- | sed s/,//g | cut -f 2 -d " " | tr -d '.')
	echo $((10#$SYSLOAD))
    }

    # Return a color indicating system load.
    function load_color()
    {
	local SYSLOAD=$(load)
	if [ ${SYSLOAD} -gt ${XLOAD} ]; then
	    echo -en ${Color_Red_zBackground}${Color_Red_Bold_Intense}
	elif [ ${SYSLOAD} -gt ${MLOAD} ]; then
	    echo -en ${Color_Red_Bold_Intense}
	elif [ ${SYSLOAD} -gt ${SLOAD} ]; then
	    echo -en ${Color_Magenta_Intense}
	else
	    echo -en ${Color_zOff}
	fi
    }

    # Return a color according to running/suspended jobs.
    function job_color()
    {
	if [ $(jobs -s | wc -l) -gt "0" ]; then
	    echo -en ${Color_Red_Bold_Intense}
	elif [ $(jobs -r | wc -l) -gt "0" ] ; then
	    echo -en ${Color_Cyan}
	fi
    }

    #   PS1="${Color_Black}"'$fill \t\n'"${Color_Cyan}"'${debian_chroot:+($debian_chroot)}\u@\h:\w\$'"${Color_zOff} "

    psbegin="\[$Color_Black\]"'$fill'"\n\[$Color_Cyan\]┌─"
    #psmiddle="\h\[$Color_Cyan\]]-[\[$(load_color)\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$Color_Yellow\]\w\[$Color_Cyan\]]\n\[$Color_Cyan\]└─"
    psmiddle="\h\[$Color_Cyan\]]-[\[$(load_color)\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$Color_Yellow\]$(gitprompt.py)\[$Color_Cyan\]]-[\[$Color_Yellow\]\w\[$Color_Cyan\]]\n\[$Color_Cyan\]└─"

    if ((${ERRORS} > 0)); then
	#	export PS1="${Color_Cyan}\t \#_${Color_Red_Bold_Intense}\u${Color_Cyan}:\w> ${Color_Cyan}";
	#	export PS1="${Color_Black}"'$fill \t\n'"${Color_Cyan}\t \#_${Color_Red_Bold_Intense}\u${Color_Cyan}:\w> ${Color_zOff}"
	#	export PS1="${Color_Black}"'$fill\n'"${Color_Cyan}\t \#_${Color_Red}\u${Color_Cyan}:\w> ${Color_zOff}"
	#	export PS1="\[$Color_Black\]"'$fill'"\n\[$Color_Cyan\]┌─[\[$Color_Red_Intense\]\u\[$Color_Blue\]@\[$Color_Red_Intense\]\h\[$Color_Cyan\]]-[\[$Color_zOff\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$Color_Yellow\]\w\[$Color_Cyan\]]\n\[$Color_Cyan\]└─[\[$Color_Red_Intense\]\$\[$Color_Cyan\]]\[$Color_Red_Bold_Intense\]->\[$Color_zOff\] "

	#  export PS1="\[$Color_Black\]"'$fill'"\n\[$Color_Cyan\]┌─[\[$Color_Red_Intense\]\u\[$Color_Blue\]@\[$Color_Red_Intense\]\h\[$Color_Cyan\]]-[\[$(load_color)\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$Color_Yellow\]\w\[$Color_Cyan\]]\n\[$Color_Cyan\]└─[\[$Color_Red_Intense\]\$\[$Color_Cyan\]]\[$(job_color)\]->\[$Color_zOff\] "
	#  PS1+="[\[$Color_Red_Intense\]\u\[$Color_Blue\]@\[$Color_Red_Intense\]\h\[$Color_Cyan\]]-[\[$(load_color)\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$Color_Yellow\]\w\[$Color_Cyan\]]\n\[$Color_Cyan\]└─[\[$Color_Red_Intense\]\$\[$Color_Cyan\]]\[$(job_color)\]->\[$Color_zOff\] "
	#  PS1="$psbegin[\[$Color_Red_Intense\]\u\[$Color_Blue\]@\[$Color_Red_Intense\]$psmiddle[\[$Color_Red_Intense\]\$\[$Color_Cyan\]]\[$(job_color)\]->\[$Color_zOff\] "
	PS1="$psbegin[\[$Color_Red_Intense\]\u\[$Color_Blue\]@\[$Color_Red_Intense\]$psmiddle[\[$Color_Red_Intense\]\$"
    else
	#	export PS1="${Color_Cyan}\t \#_\u:\w> ${Color_zOff}";
	#	export PS1="${Color_Black}"'$fill \t\n'"${Color_Cyan}\t \#_\u:\w> ${Color_zOff}"
	#	export PS1="${Color_Black}"'$fill\n'"${Color_Cyan}\t \#_\u:\w> ${Color_zOff}"

	#	export PS1="\n\[$Color_Cyan\]┌─[\[$Color_Green\]\u\[$Color_Blue\]@\[$Color_Red\]\h\[$Color_Cyan\]]-[\[$Color_zOff\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$Color_Yellow\]\w\[$Color_Cyan\]]\n\[$Color_Cyan\]└─[\[$Color_Magenta\]\$\[$Color_Cyan\]]->\[$Color_zOff\] "
	#	export PS1="\[$Color_Black\]"'$fill'"\n\[$Color_Cyan\]┌─[\[$Color_Green\]\u\[$Color_Blue\]@\[$Color_Red\]\h\[$Color_Cyan\]]-[\[$Color_zOff\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$Color_Yellow\]\w\[$Color_Cyan\]]\n\[$Color_Cyan\]└─[\[$Color_Magenta\]\#\[$Color_Cyan\]]->\[$Color_zOff\] "

	#  PS1+="[\[${SUD}\]\u\[$Color_Blue\]@\[${CNX}\]\h\[$Color_Cyan\]]-[\[$(load_color)\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$Color_Yellow\]\w\[$Color_Cyan\]]\n\[$Color_Cyan\]└─[\[$Color_Magenta\]\#\[$Color_Cyan\]]\[$(job_color)\]->\[$Color_zOff\] "
	#  PS1="$psbegin[\[${SUD}\]\u\[$Color_Blue\]@\[${CNX}\]$psmiddle[\[$Color_Magenta\]\#\[$Color_Cyan\]]\[$(job_color)\]->\[$Color_zOff\] "
	PS1="$psbegin[\[${SUD}\]\u\[$Color_Blue\]@\[${CNX}\]$psmiddle[\[$Color_Magenta\]\#"
    fi

    psend="\[$Color_Cyan\]]\[$(job_color)\]->\[$Color_zOff\] "
    PS1+=$psend
    export PS1

    history -a # All terminal windows go to same history

    # create a $fill of all screen width minus the time string and a space:
    # let fillsize=${COLUMNS} # fullscreen
    # let fillsize=${COLUMNS}-9 # fit the date in
    # Room for the date and battery charge via battery.py
    let fillsize=${COLUMNS}-11
    fill=""

    while [ "$fillsize" -gt "0" ]
    do
        fill="-${fill}" # fill with underscores to work on
        let fillsize=${fillsize}-1
    done
    fill="${fill}$(battery_charge)"
}


export PS2="\[$Color_Cyan\]→\[$Color_zOff\] "    # Secondary prompt, multiline commands
export PS3='#? '   # Tertiary prompt, select menus
export PS4='+ '    # Quaternary prompt, ???

PROMPT_COMMAND=prompt_command

## Sourcin'
# Advanced bash completion (http://www.caliban.org/bash/index.shtml#completion)
# Defaults, pip, and gem completion
# (https://github.com/revans/bash-it/blob/master/completion)
# Most of the rest symlinked from /opt/local/share/bash-completion/completions
# based on what I had installed
for file in ~/.completions.d/*; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file";
done;
unset file;

# Git aliases
complete -F _git hub
complete -F _git g

# whois, etc. auto-completion based on entries in known_hosts.
if [[ -e ~/.ssh/known_hosts ]]; then
    # This completely supersedes the above-sourced ssh.completion file, need
    # to stop that from happening. ;;;;;; ##### FIXME TODO
    complete -o default -W "$(cat ~/.ssh/known_hosts | sed 's/[, ].*//' | sort | uniq | grep -v '[0-9]')" whois nslookup nmap
fi

# networksetup completion
complete -o default -W "$(networksetup -printcommands | grep -Ee "-.+?\b" -o | grep -v delete | grep -v rofile)" networksetup;

# z, the awesome helper for moving around to popular directories
. ~/.z.sh

# Emacs ido-like for bash CD, not perfect
# if [ -f ~/bin/bash-ido ]; then
#     . ~/bin/bash-ido
# fi

# Function to run upon exit of shell
function _exit()
{
    echo -e "${Color_Red_Bold_Intense}Thanks for playing${Color_zOff}"
}
trap _exit EXIT

# ;;;;;; ##### FIXME TODO
# Make CPAN always select the default option
export PERL_MM_USE_DEFAULT=1
# Make perl -d automatically use NYTProf
export PERL5DB='use Devel::NYTProf'
# Define nytprofhtml, see also dprofpp
# alias nytprofhtml='~/.cpan/build/Devel-NYTProf-5.04-4mP05e/bin/nytprofhtml'
# Access Perl::Critic documentation
function explain_perlcritic() {
    #  perldoc -oman Perl::Critic::Policy::"$1"
    perldoc Perl::Critic::Policy::"$1"
}
# ;;;;;; ##### FIXME TODO
_explain_perlcritic()
{
    COMPREPLY=()
    cur=${COMP_WORDS[COMP_CWORD]^^} # Uppercase the result

    COMPREPLY=( $( compgen -W "$(\ls -1 /opt/local/share/perl5.20/siteman/man3/Perl\:\:Critic\:\:Policy\:\:* | sed 's/^.*man3\/Perl::Critic::Policy:://g' | sed 's/\.3pm$//g')" -- $cur ) )

    return 0
}
complete -F _explain_perlcritic explain_perlcritic

# Tell tidy to use a config file if it's there
if command_exists tidy; then
    export HTML_TIDY=~/.tidyrc
fi


# Make customization easier
alias bashrc='emacs ~/.bashrc'
alias rc='emacs ~/.bashrc'
alias eb='emacs ~/.bashrc'
alias bashprofile='emacs ~/.bash_profile'
alias bp='emacs ~/.bash_profile'
alias ep='emacs ~/.bash_profile'
#  alias reload="exec $SHELL -l"
alias reload='. ~/.bashrc'
alias ee='emacs ~/.emacs'
alias gitconfig='emacs ~/.gitconfig'
alias eg='emacs ~/.gitconfig'
function gitignore() {
    ignore=".gitignore"
    if [[ ! -f $ignore ]]; then
	ignore="~/.global-gitignore"
    elif [[ -n $1 && $1 = "g" ]]; then
	ignore="~/.global-gitignore"
    fi
    emacs $ignore
}

# Navigation -------------------------------------------------------
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~="cd ~"
alias -- -='cd -' # go back a directory, -- means end of options
alias -- --='cd -' # go back a directory

# Always be able to cd here
#export CDPATH=".:~:~/Documents/perl"

# Shortcuts
alias dt="cd ~/dotfiles"
alias dr="cd ~/Dropbox"
alias de="cd ~/Desktop"
alias dg="cd ~/Documents/git"
alias dga="cd ~/Documents/git/amorymeltzer.github.io"
alias dgg="dga"
alias dgh="cd ~/Documents/git/homebrew"
alias dgc="cd ~/Documents/git/brew-cask"
alias dgb="dgc"
alias dgcc="cd ~/Documents/git/brew-cask/Casks"
alias dp="cd ~/Documents/perl"
alias dps="cd ~/Documents/perl/sandbox"
alias dpk="cd ~/Documents/perl/ksp"
alias dpw="cd ~/Documents/perl/website"
alias wb="cd ~/Documents/perl/website"
alias eds="cd ~/.emacs.d/site-lisp/"
alias bin="cd ~/bin"
alias m='more'
#alias c='cat'
alias j="jobs"
alias h='history 15'
alias cl='clear'
alias cls='clear'
alias em='emacs '
alias e='$EDITOR '
alias count='wc -l '

# http://linux.die.net/man/1/pygmentize
# Colorized cat
alias cot='pygmentize -O bg=light -g '

# o with no args opens current directory, otherwise opens the given location
function o() {
    if [ $# -eq 0 ]; then
	open .
    else
	open "$@"
    fi
}

# cd and ls together
function cdl() {
    cd "$1" ; ls ;
}

# Run perl without going there, in case it needs local files
# Not sure this works or is even useful
function peek() {
    (cd "$1" && perl "$2");
}

# shorthand for perlfunc
function pf() {
    if [ $# -eq 0 ]; then
	perldoc perlfunc
    else
	perldoc -f "$@"
    fi
}
alias perlcheat='man perlcheat'

# Enable aliases to be sudo'ed
# http://www.gnu.org/software/bash/manual/bashref.html#Aliases says: "If the
# last character of the alias value is a space or tab character, then the next
# command word following the alias is also checked for alias expansion."
#  alias sudo='sudo '
# Hook lock-quote scripts into sudo.  Not ideal but fits the right profile:
# uses sudo, not overly common but often enough to change moderately frequently
# -E preserves environment, eg colors
alias sudo='sudo ~/Documents/git/lockquote/lock-quote && sudo -E '
# sudo aliases
alias fuckyou,='sudo '

# Prompt before overwrite, be vocal about it
alias mv='mv -vi' # add -f to override, or \ before command
alias cp='cp -Rvi' # recursive if folder, the ending / makes a difference
# alias rm='rm -i' # Too annoying, perhaps?
alias rm='rm -v'
# Move the given file(s) to the Trash.
# Alias rm to this instead???
function trash() {
    mv $1 ~/.Trash
}
# Make intermediate directories and be verbose about it
alias mkdir='mkdir -pv'
# Deleting intermediate directories not as logical
# alias rmdir='rmdir -p'

# mkdir then cd
function mkcd() {
    if [ ! -n "$1" ]; then
	echo "Enter a directory name"
    elif [ -d $1 ]; then
	echo "$1 already exists"
	cd $1
    else
	mkdir $1 && cd $1
    fi
}
alias mkd='mkcd '

# Make a directory and move a file into it
function mkmv() {
    if [[ $# -ne 2 ]]; then
	echo "Usage: mkmv <file> <directory>"
    elif [ -d $2 ]; then
	echo "$1 already exists"
    else
	mkdir "$2"
	mv "$1" "$2"
    fi
}


# Human-readable values, and a total for du
alias df='df -h'
# Also ignore stupid things that require permissions
alias du='du -hc -I .cpan -I .config'
# Depth of 1, the minimum
alias dud='du -d 1'

# Better format for uptime
alias utime="uptime | egrep -o -e 'up [0-9]*.*[0-9]* user[s]?' | tr 'u' 'U'"

# Base 10 instead of base 2, up here to "beat" the alias below
function diskusage() {
    df -H "`pwd`" | awk 'NR==2 { print "Used " $3 " of " $2 ", " $4 " (" $5 ") remaining" }'
}
# Displays user owned processes status.
function psu {
    ps -U "${1:-$USER}" -o 'pid,%cpu,%mem,command'
}

# Top by memory, not in delta
alias topm='top -o vsize'
# Top always sorted by cpu, in delta mode
alias top='top -d -o cpu'
function memstat {
    vm_stat | cut -d ":" -f 2 | tr -d '. ' | tr '\n' ' ' | awk '{printf("U:%.1fG|F:%.1fG\n", (($3 + $4 + $6) * 4)/(1024*1024), (($2+$5) * 4)/(1024*1024))}'
}
# See memory_pressure for Mavericks-style-ish output
# Tweak for Mavericks-style info, color ;;;;;; ##### FIXME TODO

# Show five most recently modified files.
alias last_modified='ls -t $* 2> /dev/null | head -n 5 '

# growlnotify: add after to show when done
# Maybe other options...
# Just use terminal-notifier...
# Also checkout boxcar
alias growl='growlnotify -t Terminal -m "Done" && tput bel' # Red badge? and popping only in 10.7+

alias beep='tput bel'

# Enhanced WHOIS
# Busted as of mid-2014
# alias whois='whois -h whois-servers.net'

# Use colordiff if it exists
if [ -a /opt/local/bin/colordiff ]; then
    alias diff='colordiff ';
    # Make wdiff colorful.  Consdier cwdiff? ;;;;;; ##### FIXME TODO
    if [ -a /opt/local/bin/wdiff ]; then
	function wcolordiff() {
	    wdiff -n "$@" | colordiff
	}
	alias diffc='wcolordiff '
    fi
fi
# But for real?  Just use dwdiff: git-like giff
# Use -3 option to only show changes
alias dwdiff='dwdiff -sc '

# grep prints line number if piped, kinda sorta breaks other things
# http://unix.stackexchange.com/a/25549/43935
grepp() {
    if [[ -t 1 ]]; then
	command grep -n "$@"
    else
	command grep "$@"
    fi
}

# Case and context in grep
alias grepi='grep -i'
alias grepc='grep -C 3'
alias grepic='grepc -i'
alias grepC='grep -C 10'
alias grepiC='grepC -i'

# Applications
#ls | open -f # pipe ls, open in default application (probably texteditor)
alias reveal='open -R '
alias preview='open -a preview '
alias finder='open -a Finder '
alias textedit='open -a TextEdit '
alias safari="open -a safari"
alias firefox="open -a firefox"
alias chrome="open -a google\ chrome"

# Macports
alias pecho='port echo '
alias psync='sudo port sync '
alias pself='sudo port selfupdate '
alias pall='sudo port selfupdate && port echo outdated && sudo port upgrade outdated'
alias pout='port echo outdated '
alias pug='sudo port upgrade outdated '
alias puo='pug '
alias pleaves='port echo leaves '
alias pclean='sudo port clean -v --all installed'
alias pactive='port echo active '
alias pinactive='port echo inactive '
alias pinfo='port info '
alias psearch='port search '
function pmoreinfo() {
    psearch $1 | grep @ | cut -f 1 -d ' ' | while read prt; do pinfo $prt; done;
}
alias pin='sudo port install '
alias pun='sudo port uninstall '
# Macports changelog
# https://trac.macports.org/browser/contrib/port-whatsnew/port-whatsnew
alias whatsnew='port echo outdated | cut -f 1 -d" " | xargs -n 1 ~/bin/port-whatsnew.sh'

# Homebrew/Cask
# Make homebrew verbose by defaults
# export HOMEBREW_VERBOSE=1
# Symlink in /Applications
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
alias ball='brew update && brew outdated && brew upgrade'
alias bclean='brew cleanup -n && brew cleanup && brew cleanup -s && brew cask cleanup'
alias bsearch='brew search'
alias binfo='brew info'
alias blist='brew list'
alias bdoctor='brew doctor'
# Homebrew-cask
alias cask='brew cask'
alias csearch='brew cask search'
alias cinfo='brew cask info'
alias clist='brew cask list'
alias cdoctor='brew cask doctor'

# Quickly open and make a new perl file executable and with headers
function newperl() {
    if [ -a $1 ]; then
	echo -e "$1 already exists";
    else
	echo -e "#!/usr/bin/env perl\n# $1 by Amory Meltzer\n# \n\nuse strict;\nuse warnings;\nuse diagnostics;\n" > $1 ; chmod 755 $1 ; emacs +3:3 $1 ;
    fi
}
alias nperl='newperl'

# Same for bash
function newbash() {
    if [ -a $1 ]; then
	echo -e "$1 already exists";
    else
	echo -e "#!/usr/bin/env bash\n# $1 by Amory Meltzer\n# " > $1 ; chmod 755 $1 ; emacs +3:3 $1 ;
    fi
}
alias nbash='newbash'

alias python3='python3.4 '
# Completion weird, but give some of it to 3.4?  ;;;;;; ##### FIXME TODO
complete -F _python python3.4

# Recompile all elisp files, with proper warnings/output
function recompile_emacs() {
    emacs -batch --eval '(byte-recompile-directory "~/.emacs.d/" 0)'
}

alias keys="more ~/.ssh/id_rsa.pub | pbcopy | echo '=> Public key copied to pasteboard.'"

function durandal() {
    ssh Amory@Durandal.local
}
function leela() {
    ssh Amory@Leela.local
}
function amory() {
    ssh iggatucdavis@amorymeltzer.org
}
function wedding() {
    ssh amoryandjarra@amoryandjarra.com
}
function twinmos() {
    diskutil umount /Volumes/TWINMOS
}
function sandisk() {
    diskutil umount /Volumes/SANDISK
}
alias unmount='diskutil umount '
alias eject='unmount '
# Eject all drives, kind of slow?
function  ejectall() {
    find /Volumes -maxdepth 1 -not -user root -a -not -name '.*' -print0 | xargs -0 umount
}

# Control iTunes ---------------------------------------------------
alias next='itunes next'
alias previous='itunes prev'
alias play='itunes play'
alias pause='itunes pause'

# Track who is listening to your iTunes music
alias whotunes='lsof -r 2 -n -P -F n -c iTunes -a -i TCP@Durandal.local:3689'

# Stuff I never use but can't delete because of http://xkcd.com/530/
alias stfu="osascript -e 'set volume output muted true'"
alias pumpitup="osascript -e 'set volume 7'"
alias hax="growlnotify -a 'Activity Monitor' 'System error' -m 'WTF R U DOIN'"

# Volume control, potentially using osxutils
if [ ! -x /opt/local/bin/setvolume ]; then
    function setvolume() {
	if [ ! $1 ]; then
	    echo "setvolume <0-100>"
	elif [ $1 -lt 0 -o $1 -gt 100 ]; then
	    echo "setvolume <0-100>"
	else
	    val=$1*7/100
	    osascript -e "set volume $val"
	fi
    }
fi
alias quartervolume='setvolume 25'
alias middlevolume='setvolume 50'
alias threequartervolume='setvolume 75'
alias maxvolume='setvolume 100'
alias mutevolume='setvolume 0'

# Seriously though, just use pianobar/pandora
alias pandora='pianobar'
# or mplayer
alias mplayer='mplayer -msgcolor'


# Show most used commands, fixed by doing $4 instead of $2, from:
# http://lifehacker.com/software/how-to/turbocharge-your-terminal-274317.php
alias profileme="history | awk '{print \$4}' | awk 'BEGIN{FS=\"|\"}{print \$1}' | sort | uniq -c | sort -n | tail -n 20 | sort -nr"

alias which='type -a'

# Pretty-print PATH
alias path='echo -e ${PATH//:/\\n}'
alias path-man="manpath | tr ':' '\n'"

# Matrix-esque screensaver-esque; man tr for different options
alias matrix='tr -c "[:print:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=lcase,unblock | GREP_COLOR="1;32" grep --color "[^ ]"'
# Star Wars
# C-] to exit
alias starwars='telnet towel.blinkenlights.nl'

# Screensaver as wallpaper, ctrl-c or cmd-. to quit
alias screensaverToWallpaper="/System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background"
# Set a custom wallpaper image. `DefaultDesktop.jpg` is already a symlink, and
# all wallpapers are in `/Library/Desktop Pictures/`. The default is
# `Wave.jpg`.
#rm -rf ~/Library/Application Support/Dock/desktoppicture.db
#sudo rm -rf /System/Library/CoreServices/DefaultDesktop.jpg
#sudo ln -s /path/to/your/image /System/Library/CoreServices/DefaultDesktop.jpg

# Lock the screen (when going AFK)
# Better as just starting screensaver
alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"

# Stopwatch
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'
alias stopwatch='timer'

# Get week number
alias weeknum='date +%V'

# Quickly check connection by pinging google
alias pg='ping -c 1 google.com'
function down4me()
{
    curl -s "http://www.downforeveryoneorjustme.com/$1" | sed "/just you/!d;s/<[^>]*>//g" | sed -e 's/  //g' | sed -e 's/http.*#x2F;/  /g'
}


# Combine all IP shit to give local and external, only if appropriate
# ######### ;;;;;;;;;; FIXME TODO
# Get my local IP
#alias ip="ifconfig | grep -E '(192|10)'"
#alias ip="ifconfig | grep -E '192'"
#alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
#alias ip="ifconfig -a | grep -o 'inet6\? \(\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\)\|[a-fA-F0-9:]\+\)' | sed -e 's/inet6* //'"
#alias ip='curl -s icanhazip.com'
# function myip()
# {
#     MY_IP=$(ifconfig en1 | awk '/inet/ { print $2 } ' | sed -e s/addr://)
#     echo ${MY_IP:-"Not connected"}
# }
function ip()
{
    iplist=$(ifconfig -a | perl -nle'/inet (?:addr:)?(\d+\.\d+\.\d+\.\d+)/ && print $1')

    if [ "$1" ]; then
	if [ "$(echo $iplist | grep -w $1)" ]; then
	    echo $1
	fi
    else
	echo $iplist
    fi
    # Ugly hacky fix
    curl -s icanhazip.com
}
alias myip='ip'
# Only have a local if on wifi?  Maybe option this to change depending on wifi V ethernet???
# ;;;;;; ##### FIXME TODO
alias localip='ipconfig getifaddr en0'
alias ips="ifconfig -a | grep -o 'inet6\? \(\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\)\|[a-fA-F0-9:]\+\)' | sed -e 's/inet6* //'"
alias ipz='dig +short myip.opendns.com @resolver1.opendns.com'

function ssid()
{
    ssid=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport -I | grep " SSID" | sed "s/.*: //")

    if [ "$1" ]; then
	if [ "$(echo $ssid | grep -w $1)" ]; then
	    echo $1
	fi
    else
	echo $ssid
    fi
}


# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump='sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\"'
# Which processes are listening on ports
alias listen='lsof -iTCP -sTCP:LISTEN -P "$@" '

alias quicklook="qlmanage -p "

alias pbc='pbcopy '
alias pbp='pbpaste '
# Trim new lines and copy to clipboard
alias copy="tr -d '\n' | pbcopy"

alias uniqsort='sort $* | uniq -u | sort'
# quicksort in three lines from http://git.io/UzwyWQ
qsort()
{
    local L=""; local G=""; [ $# -eq 1 ] && echo $1 && return;
    P=$1; shift; for i in $@; do [ $i -lt $P ] && L="$L $i" || G="$G $i"; done
    [ -z "$L" ] || L=`qsort $L`; [ -z "$G" ] || G=`qsort $G`; echo "$L $P $G"
}

# Get rid of pesky .DS_Store files, recursively
# alias dscleanup='find . -type f -name "*.DS_Store" -ls delete"
alias dscleanup='find . -type f -name "*.DS_Store" -print0 | xargs -0 rm -v'
# Clean up LaunchServices to remove duplicates in the "Open With" menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
# Flush Directory Service cache
alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"

# OS X has no `md5sum`, so use `md5` as a fallback
command -v md5sum > /dev/null || alias md5sum="md5"
# OS X has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum > /dev/null || alias sha1sum="shasum"
# OS X has no `sha256sum`, so use `shasum` as a fallback
command -v sha256sum > /dev/null || alias sha256sum="shasum -a 256 "
# OS X has no `sha512sum`, so use `shasum` as a fallback
command -v sha512sum > /dev/null || alias sha512sum="shasum -a 512 "

# Canonical hex dump; some systems have this symlinked
command -v hd > /dev/null || alias hd="hexdump -C"

# URL-encode strings
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

# Make tripperX use the logfile
alias tripperx="tripperx -l"

# ROT13-encode text. Works for decoding too
alias rot13='tr a-zA-Z n-za-mN-ZA-M'
# ROT47
alias rot47='tr !-~ P-~!-O'

# Convert to lowercase.
function lc {
    if [ $# -eq 0 ]; then
	tr '[:upper:]' '[:lower:]';
    else
	tr '[:upper:]' '[:lower:]' <<< "$@";
    fi;
}

# Convert to uppercase.
function uc {
    if [ $# -eq 0 ]; then
	tr '[:lower:]' '[:upper:]';
    else
	tr '[:lower:]' '[:upper:]' <<< "$@";
    fi;
}

# Backup file
function backup-file()
{
    local filename=$@

    for i in $filename
    do
	cp $i $i.bak
    done
}
# Backup file with timestamp
function backup-file-with-timestamp()
{
    local filename=$@
    local filetime=$(date +%Y%m%d_%H%M%S)

    for i in $filename
    do
	cp ${i} ${i}_${filetime}
    done
}

# PlistBuddy alias, because sometimes `defaults` just doesn't cut it
alias plistbuddy="/usr/libexec/PlistBuddy"

# Hide/show all desktop icons (useful when presenting)
alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"
alias showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"
alias desktophide="hidedesktop"
alias desktopshow="showdesktop"
# Show/hide hidden files in Finder
alias showhidden="defaults write com.apple.Finder AppleShowAllFiles -bool true && killall Finder"
alias hidehidden="defaults write com.apple.Finder AppleShowAllFiles -bool false && killall Finder"

# Merge PDF files
function mergepdf() {
    if [ ! $1 ]; then
	echo "Usage: mergepdf -o output.pdf input{1,2,3}.pdf";
    else
	/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py $@;
    fi
}

# Change working directory to the top-most Finder window location
function cdfinder() {
    cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')"
}
alias cdf='cdfinder '


## Couple of functions jacked from bash-it
## https://github.com/revans/bash-it
# Pick random line from a file
pickfrom ()
{
    local file=$1
    [ -z "$file" ] && reference $FUNCNAME && return
    length=$(cat $file | wc -l)
    n=$(expr $RANDOM \* $length \/ 32768 + 1)
    head -n $n $file | tail -1
}
# Generate random password from dictionary words
# Passed number gives length
pass ()
{
    local i pass length=${1:-4}
    pass=$(echo $(for i in $(eval echo "{1..$length}"); do pickfrom /usr/share/dict/words; done))
    echo "With spaces (easier to memorize): $pass"
    echo "Without (use this as the pass): $(echo $pass | tr -d ' ')"
}


# Stock prices, use ~/bin/finance script for historical or current indices
function stockmarket() {
    for quote in $@;
    do
	printf "$quote\t";
	curl -s "http://download.finance.yahoo.com/d/quotes.csv?s=$quote&f=l1";
    done
}

# Calculate netBenefits stuff, uses ~/bin/ticker.sh, which just curls the website to get end-of-day quote
alias netbenefits="perl ~/Documents/perl/sandbox/netBenefits.pl"

function btc() {
    five=$(curl -s 'https://coinbase.com/api/v1/currencies/exchange_rates' | perl -ne 'print "$1" if /btc_to_usd\":\"(.*?)\",/;';)
    tail -n 1 ~/btc.csv;
    echo -e "$(date -u -v-8H +'%y-%m-%d %H:%M:%S')\t$five";
    echo -e "$(date -u -v-8H +'%y-%m-%d %H:%M:%S')\t$five" >> ~/btc.csv;
}
alias btn='btc'
function bitcoin() {
    five=$(curl -s 'https://coinbase.com/api/v1/currencies/exchange_rates' | perl -ne 'print "$1" if /btc_to_usd\":\"(.*?)\",/;';)
    echo -e "BTC: $five";
}


# Get coordinates
function findlocation() {
    place=`echo $* | sed 's/ /%20/g'`
    curl -s "http://maps.googleapis.com/maps/api/geocode/json?address=$place&sensor=false" | grep -A 2 -e "location\"" -e "formatted_address" | grep -e "formatted" -e "lat" -e "lng" | sed -e 's/^ *//' -e 's/"//g' -e 's/formatted_address/Full address/g' -e 's/,$//g' -e 's/^.*{//g';
}
alias getcoordinates='findlocation'

# Get the weather
function weather() {
    if [ ! $1 ]; then
	zip="95618";
	#	zip=$(curl -s api.hostip.info/get_html.php?ip=$(curl -s icanhazip.com) | sed -e'1d;3d' -e's/C.*: \(.*\)/\1/' -e's/ /%20/g' -e"s/'/%27/g" -e"s/-/%2d/g")
    else
	zip=$1;
    fi
    echo "Forecast for $zip";
    curl -s "http://api.wunderground.com/auto/wui/geo/ForecastXML/index.xml?query=$zip" | perl -ne '/<title>([^<]+)/&&printf "%s: ",$1;/<fcttext>([^<]+)/&&print $1,"\n"';
    echo
    # Needs some fixing, tweaking ####### FIXME TODO #######
    # Odd spacing, degree symbol, visual appearance
    # Maybe use curl instead?
    lynx -dump "http://www.weather.com/weather/print/$zip" | sed -n '/%$/s/\[.*\]//p' | sed -n 's/ \%/\%/p' | sed 's/^             //g';
}

function sunrise()
{
    if [ ! $1 -o $1 = "Davis" ]; then
	# Uses WOEID, which is some ol' bullshit
	# Davis
	loc=2389646;
	echo Davis
    elif [ $1 = "NYC" ]; then
	# NYC
	loc=2459115;
	echo NYC
    elif [ $1 = "Stow" ]; then
	# Stow, MA
	loc=2500649;
	echo Stow
    else
	echo "sunrise <NYC|Stow|Davis>"
	return
    fi
    curl -s http://weather.yahooapis.com/forecastrss?w=$loc | grep astronomy | awk -F\" '{print $2 "\n" $4;}'
}

# Print the given text in the center of the screen.
function center {
    width=$(tput cols);
    str="$@";
    len=${#str};
    [ $len -ge $width ] && echo "$str" && return;
    for ((i = 0; i < $(((($width - $len)) / 2)); i++)); do
	echo -n " ";
    done;
    echo "$str";
}

function newtab() {
    osascript 2>/dev/null <<EOF
    tell application "System Events"
      tell process "Terminal" to keystroke "t" using command down
    end
    tell application "Terminal"
      activate
      do script with command " cd \"$PWD\"; $*" in window 1
    end tell
EOF
}

##############################################################################
# Things I will never, ever use from https://github.com/mathiasbynens/dotfiles
# Escape UTF-8 characters into their 3-byte format
function escape()
{
    printf "\\\x%s" $(printf "$@" | xxd -p -c1 -u)
    # print a newline unless we're piping the output to another program
    if [ -t 1 ]; then
	echo # newline
    fi
}

# Get a character's Unicode code point
function codepoint()
{
    perl -e "use utf8; print sprintf('U+%04X', ord(\"$@\"))"
    # print a newline unless we're piping the output to another program
    if [ -t 1 ]; then
	echo # newline
    fi
}

# Show all the names (CNs and SANs) listed in the SSL certificate
# for a given domain
function getcertnames() {
    if [ -z "${1}" ]; then
	echo "ERROR: No domain specified."
	return 1
    fi

    local domain="${1}"
    echo "Testing ${domain}..."
    echo # newline

    local tmp=$(echo -e "GET / HTTP/1.0\nEOT" \
		       | openssl s_client -connect "${domain}:443" 2>&1);

    if [[ "${tmp}" = *"-----BEGIN CERTIFICATE-----"* ]]; then
	local certText=$(echo "${tmp}" \
				| openssl x509 -text -certopt "no_header, no_serial, no_version, \
			no_signame, no_validity, no_issuer, no_pubkey, no_sigdump, no_aux");
	echo "Common Name:"
	echo # newline
	echo "${certText}" | grep "Subject:" | sed -e "s/^.*CN=//";
	echo # newline
	echo "Subject Alternative Name(s):"
	echo # newline
	echo "${certText}" | grep -A 1 "Subject Alternative Name:" \
	    | sed -e "2s/DNS://g" -e "s/ //g" | tr "," "\n" | tail -n +2
	return 0
    else
	echo "ERROR: Certificate not found.";
	return 1
    fi
}

# Manually remove a downloaded app or file from the quarantine
function unquarantine() {
    for attribute in com.apple.metadata:kMDItemDownloadedDate com.apple.metadata:kMDItemWhereFroms com.apple.quarantine; do
	xattr -r -d "$attribute" "$@"
    done
}
##############################################################################

# Print members of the House of Representatives
##### BROKEN TODO FIXME #####
#function housemembers() { curl -s "http://www.house.gov/representatives/" | sed -e :a -e 's/<[^>]*>//g;/</N;//ba' | perl -nle 's/^\t\t(.*$)/ $1/ and print;' ;}
function housemembers() { curl -s "http://www.house.gov/representatives/" | sed -e :a -e 's/<[^>]*>//g;/</N;//ba' | perl -nle 's/^\t\t(.*$)/ $1/ and print;' ;}

# Monitor file live with tail
function monitor() {
    tail -f $1 | while read line; do printf "$(date '+%F %T')\t$line\n"; done;
}

# Check unread count, shell script to option username?
function gmail() {
    curl -u $1 -s "https://mail.google.com/mail/feed/atom" | perl -ne 'print "\t" if /<name>/; print "$2\n" if /<(title|name)>(.*)<\/\1>/;'
}
function priority() {
    curl -u $1 -s "https://mail.google.com/mail/feed/atom/important" | perl -ne 'print "\t" if /<name>/; print "$2\n" if /<(title|name)>(.*)<\/\1>/;'
}

# Check available macs on UCD campus
function available() {
    # curl -s "clm.ucdavis.edu/rooms/available/" | perl -ne 'print "$2\t$3\n" if /(roomcolumn)\">(\d*\s\w*)<\/.*\/>(\d*\sMac)/;'
    curl -s "clm.ucdavis.edu/rooms/available/" | perl -ne 'print "$2\t$3\n" if /(roomcolumn)\">(\d*\s\w*).*?(\d*\sMac)/;'
}

# /. headlines, turn into shell script to option output?
########## Broken after update FIXME TODO ########
function slashdot() {
    curl -s "http://rss.slashdot.org/Slashdot/slashdot" | perl -ne 'print "\t" if /<name>/; print "$2\n" if /<item><(title|name)>(.*)<\/\1>/;'
}

# Test how fast the machine is, 32GB
function writetest() {
    dd if=/dev/zero of=/dev/null bs=1048576 count=32768;
}

# Find a file with a pattern in name:
# Excluding a bunch of useless locations
function ff() { find . -path './.cpan' -prune -o -path './.config' -prune -o -path './Library' -prune -o -path './.fseventsd' -prune -o -path './.Spotlight-V100' -prune -o -path './.Trashes' -prune -o -type f -iname '*'$*'*' -ls; }

# Remove empty directories under and including <path>s.
alias prunedirs='find "$@" -type d -empty -depth | xargs rmdir'

# Count files in each sub-directory
# Make script to option count versus list?????????????
# Use tree, may be faster?
function filecount() {
    for dir in */ ; do echo -n "$dir " ; find "$dir" | wc -l ; done;
}

# Determine size of a file or total size of a directory
function fs() {
    if du -b /dev/null > /dev/null 2>&1; then
	local arg=-sbh
    else
	local arg=-sh
    fi
    if [[ -n "$@" ]]; then
	du $arg -- "$@"
    else
	du $arg .[^.]* *
    fi
}
alias filesize='fs'

# Repeat n times command.
function repeat()
{
    local i max
    max=$1; shift;
    for ((i=1; i <= max ; i++)); do  # --> C-like syntax
	eval "$@";
    done
}

# Binary clock
function binaryclock() {
    perl -e 'for(;;){@d=split("",`date +%H%M%S`);print"\r";for(0..5){printf"%.4b ",$d[$_]}sleep 1}'
}

# Summary of enWiki article, perl bit accounts for some UTF-8 unicode stuff
# https://dgl.cx/wikipedia-dns
function wiki() {
    dig +short txt $1.wp.dg.cx | perl -pe's/\\(\d{1,3})/chr $1/eg'
}

# All images
function yotsuba() {
    curl $1 | grep -i "File<a href" | awk -F '<a href="' '{print $4}' | awk -F '" ' '{print $1}' | xargs wget
}

# Simple calculator
# function calc() {
#     local result=""
#     result="$(printf "scale=10;$*\n" | bc --mathlib | tr -d '\\\n')"
#     #                       └─ default (when `--mathlib` is used) is 20
#     #
#     if [[ "$result" == *.* ]]; then
#	# improve the output for decimal numbers
#	printf "$result" |
#	    sed -e 's/^\./0./'        `# add "0" for cases like ".5"` \
#		-e 's/^-\./-0./'      `# add "0" for cases like "-.5"`\
#		-e 's/0*$//;s/\.$//'   # remove trailing zeros
#     else
#	printf "$result"
#     fi
#     printf "\n"
# }

# Using Rscript allows more complex constructions, wrap () in quotes
# http://www.compbiome.com/2010/06/r-command-line-calculator-using-rscript.html
alias calc='Rscript -e "eval( parse( text=commandArgs( TRUE ) ) )"'

function =() {
    calc $@
}

alias sumup="perl -MList::Util=sum -alne 'push @S,@F; END { print sum @S }'"

# 39 digits each
alias pi="perl -Mbignum=PI -le 'print PI'"
alias E="perl -Mbignum=e -le 'print e'"

# Extract most types of compressed files
function extract {
    echo Extracting $1 ...
    if [ -f $1 ] ; then
	case $1 in
	    *.tar.bz2)tar xjf $1;;
	    *.tar.gz)tar xzf $1;;
	    *.tar.Z)tar xzf $1;;
	    *.bz2)bunzip2 $1;;
	    *.rar)rar x $1;;
	    *.gz) gunzip $1;;
	    *.jar)unzip $1;;
	    *.tar)tar xf $1;;
	    *.tbz2)tar xjf $1;;
	    *.tgz)tar xzf $1;;
	    *.zip)unzip $1;;
	    *.Z)uncompress $1;;
	    *.7z)7z x $1;;
	    *)echo "'$1' cannot be extracted via extract()" ;;
	esac
    else
	echo "'$1' is not a valid file"
    fi
}

# Create a .tar.gz archive, using `zopfli`, `pigz` or `gzip` for compression
function targz() {
    local tmpFile="${@%/}.tar";
    tar -cvf "${tmpFile}" --exclude=".DS_Store" "${@}" || return 1;

    size=$(
	stat -f"%z" "${tmpFile}" 2> /dev/null; # OS X `stat`
	stat -c"%s" "${tmpFile}" 2> /dev/null # GNU `stat`
	);

    local cmd="";
    if (( size < 52428800 )) && hash zopfli 2> /dev/null; then
	# the .tar file is smaller than 50 MB and Zopfli is available; use it
	cmd="zopfli";
    else
	if hash pigz 2> /dev/null; then
	    cmd="pigz";
	else
	    cmd="gzip";
	fi;
    fi;

    echo "Compressing .tar using \`${cmd}\`…";
    "${cmd}" -v "${tmpFile}" || return 1;
    [ -f "${tmpFile}" ] && rm "${tmpFile}";
    echo "${tmpFile}.gz created successfully.";
}

# Compare original and gzipped file size
function gz() {
    local origsize=$(wc -c < "$1")
    local gzipsize=$(gzip -c "$1" | wc -c)
    # Percent of total
    local ratio=$(echo "$gzipsize * 100/ $origsize" | bc -l)
    # Percent reduction
    #    local ratio=$(echo "100 - ($gzipsize * 100/ $origsize)" | bc -l)
    printf "orig: %d bytes\n" "$origsize"
    # printf "gzip: %d bytes (%2.2f%%)\n" "$gzipsize" "$ratio"
    printf "gzip: %d bytes, %2.2f%% of original\n" "$gzipsize" "$ratio"
}


#### Dictionary stuff
# wordnet
dictionary () { curl dict://dict.org/d:${1}:wn; }
alias define='dictionary'
alias d='dictionary'
# Moby thesaurus
thesaurus () { curl dict://dict.org/d:${1}:moby-thes; }
alias synonym='thesaurus'
# vera acronyms
acronym () { curl dict://dict.org/d:${1}:vera; }
# jargon files
jargon () { curl dict://dict.org/d:${1}:jargon; }
# foldoc
# free-online dictionary of computing
foldoc () { curl dict://dict.org/d:${1}:foldoc; }
# urbandict
urban() { word=`echo $* | sed 's/ /%20/g'`; curl -s http://www.urbandictionary.com/define.php?term=$word | iconv | grep -A 4 -m 1 "'meaning'>" | tail -n 4 | sed '2,3d'; }

# Add note to Notes.app (OS X 10.8+)
# Usage: `note 'title' 'body'` or `echo 'body' | note`
# Title is optional
function note() {
    local title
    local body
    if [ -t 0 ]; then
	title="$1"
	body="$2"
    else
	title=$(cat)
    fi
    #    osascript -e 'tell app "iTunes" to '"$*"
    osascript>/dev/null \<\<END
    tell application "Notes"
    tell account "iCloud"
    tell folder "Notes"
    make new note with properties {name:"$title", body:"$title" & "<br><br>" & "$body"}
    end tell
    end tell
    end tell
    END
}

# Add reminder to Reminders.app (OS X 10.8+)
# Usage: `remind 'foo'` or `echo 'foo' | remind`
function remind() {
    local text
    if [ -t 0 ]; then
	text="$1" # argument
    else
	text=$(cat) # pipe
    fi
    osascript >/dev/null \<\<END
    tell application "Reminders"
    tell the default list
    make new reminder with properties {name:"$text"}
    end tell
    end tell
    END
}


function bashtips() {
    echo "
DIRECTORIES
-----------
pushd tmp   Push tmp && cd tmp
popd        Pop && cd
GLOBBING AND OUTPUT SUBSTITUTION
--------------------------------
ls a[b-dx]e Globs abe, ace, ade, axe
ls a{c,bl}e Globs ace, able
\$(ls)      \`ls\` (but nestable!)
HISTORY MANIPULATION
--------------------
!!        Last command
!?foo     Last command containing \`foo'
^foo^bar^ Last command containing \`foo', but substitute \`bar'
!!:0      Last command word
!!:^      Last command's first argument
!\$       Last command's last argument
!!:*      Last command's arguments
!!:x-y    Arguments x to y of last command
C-s       search forwards in history
C-r       search backwards in history
LINE EDITING
------------
M-d     kill to end of word
C-w     kill to beginning of word
C-k     kill to end of line
C-u     kill to beginning of line
M-r     revert all modifications to current line
C-]     search forwards in line
M-C-]   search backwards in line
C-t     transpose characters
M-t     transpose words
M-u     uppercase word
M-l     lowercase word
M-c     capitalize word
COMPLETION
----------
M-/     complete filename
M-~     complete user name
M-@     complete host name
M-\$    complete variable name
M-!     complete command name
M-^     complete history
IF-THEN TEST OPERATORS
______________________
-e      file exists
-a      file exists, deprecated
-f      file is a regular file
-s      file is not zero size
-d      file is a directory
-h      file is a symbolic link
-L      file is a symbolic link
-r      file has read permissiong
-w      file has write permission
-x      file has execute permission
-nt     file is newer than file2
-ot     file is older than file2
-eq, -ne, -gt, -ge, -lt, -le"
}




###### holiday greeting
# from Jonathan's .bashrc file (by ~71KR117)
# http://dotshare.it/dots/516/
# get current day (Month-Day Format)
day=$(date +"%B%e")
# get current year (for new years greeting)
year=$(date +"%Y")
# make sure the holiday greeting is displayed (if any)
hol=1
# if it is New Year's Day
if [ "$day" = "January 1" ]
then
    holgreet="${Color_Magenta_Intense}Happy ${Color_Red_Intense}New ${Color_Blue_Intense}Year!${Color_zOff} Have a good $year."
    # if it is Groundhog Day
elif [ "$day" = "February 2" ]
then
    holgreet="Happy Groundhog Day"
    # if it is Valentine's Day
elif [ "$day" = "February 14" ]
then
    holgreet="Happy ${Color_Magenta}Valentine's Day${Color_zOff}"
    # if it is Independance Day
elif [ "$day" = "July 4" ]
then
    holgreet="Happy ${Color_Red_Intense}Fourth ${Color_White_Intense}of ${Color_Blue_Intense}July${Color_zOff}"
    # if it is my birthday
elif [ "$day" = "August 12" ]
then
    holgreet="Happy Birthday!"
    # it is Halloween
elif [ "$day" = "October 31" ]
then
    holgreet="${Color_Red_Bold}Happy Halloween.${Color_zOff}"
    # if it is Christmas Eve
elif [ "$day" = "December 24" ]
then
    holgreet="Happy ${Color_Green_Intense}Christmas ${Color_Red}Eve${Color_zOff}"
    # if it is Christmas
elif [ "$day" = "December 25" ]
then
    holgreet="${Color_Green_Intense}Merry ${Color_Red}Christmas${Color_zOff}!"
    # if it is New Year's Eve
elif [ "$day" = "December 31" ]
then
    holgreet="Happy New Year's Eve!"
else
    hol=0
fi
# display holiday greeting
if [ "$hol" = "1" ]
then
    echo -e $holgreet
    # elif [ "$hol" = "0" ]
    # then
    #   randomvarthatsomehowimportant=0
fi
