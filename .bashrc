## Notes
# /usr/share/doc/bash
# man fold
# man sysctl

# if not running interactively, don't do anything
# I see this around, seems good?  ;;;;;; ##### FIXME TODO
if [ -z "$PS1" ]; then
    return
fi

# Localization
export LC_ALL=C
# export LC_ALL=en_US.UTF-8

# Colors ----------------------------------------------------------
export TERM=xterm-256color
# Change for different colors, to magenta
#  export GREP_OPTIONS='--color=auto' GREP_COLOR='0;35'
alias grep='grep --color=auto '
alias egrep='egrep --color=auto '
export GREP_COLOR='0;35'

# ls colors
if [[ $OSTYPE == darwin* ]]; then
    export CLICOLOR=1
elif [[ $OSTYPE == linux* ]]; then
    alias ls='ls --color=auto'
    # ls colors, see: http://www.linux-sxs.org/housekeeping/lscolors.html
    # Should probably make this match OSX FIXME TODO
    export LSCOLORS='di=1:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:*.rb=90'
    # export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"
    # export LSCOLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:';
fi

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
alias l.='ls -dAFh .[^.]*'	# Dotfiles only
# Note: flags get passed to grep, not ls
alias ld='ls -l | grep "^d" --color=never'
alias lad='ls -Al | grep "^d" --color=never'
alias lda='lad'

# Colorized, recursive ls-like tree
alias treed='tree -aC -I ".git" --dirsfirst "$@" | less -FRNX'
alias tree='tree -Csuh'

# Preserve environment
alias sudo='sudo -E '

### Git stuff
# Should fix git signing when ssh'd, no issues locally?
export GPG_TTY=$(tty)
# Alias hub as git for github https://github.com/github/hub
if [[ -f `command -v hub` ]] ; then
    alias git='hub'
fi
# Quick
function g {
    local ref=$(git rev-parse --is-inside-work-tree 2> /dev/null)
    if [[ $ref ]]; then
	if [[ $# > 0 ]]; then
            git "$@"
	else
	    git lr5
	    git status --short --branch
	fi
    else
	case "$1" in
	    scan|'help'|h|browse|grab|config|cfg|version|notifications) git "$@";;
	    *) echo "Not a git repository"
	esac
    fi
}

alias gb='g branch'
alias gcb='g copy-branch-name'
alias gco='g co'
alias gg='g go'
alias gs='g s'
alias s='g s'
alias ga='g add'
alias gaa='g aa'
alias gaas='g aas'
alias gas='g aas'
alias gac='g aac'
alias gd='g d'
alias gdc='g dc'
alias gicd='g icd'
alias gicdc='g icdc'
alias gsi='g si'
alias gis='g si'
alias gsic='g sic'
alias gisc='g sic'
alias gdn='g dn'
alias gds='g ds'
alias gdsc='g dsc'
alias gdcs='g dsc'
alias gcm='g cm'
alias gl='g l5'
alias gl1='g l'
alias gfu='g lf1'
alias gld='g ld5'
alias glr='g lr5'
alias glg='g lg'
alias glm='g lm'
alias gllm='g llm'
alias gls='g ls'
alias greh='g reh'

# Setup some colors to use later in interactive shell or scripts
# ~/bin/colordump and termcolors give some clues about these
# 0; = regular
# 1; = bold
# 4; = underline
# 5; = blinking
# 7; = highlighted

export Color_zOff="\033[0m"	  # No color, stupid stupid hax for sorting
export Color_zBold="\033[1m"	  # Bold text
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

# # Underline
# export Color_Black_Underline="\033[4;30m"       # Black
# export Color_Red_Underline="\033[4;31m"         # Red
# export Color_Green_Underline="\033[4;32m"       # Green
# export Color_Yellow_Underline="\033[4;33m"      # Yellow
# export Color_Blue_Underline="\033[4;34m"        # Blue
# export Color_Magenta_Underline="\033[4;35m"     # Magenta
# export Color_Cyan_Underline="\033[4;36m"        # Cyan
# export Color_White_Underline="\033[4;37m"       # White

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


# See also 256-colors, colordump, colors_and_formatting, & tput_colors
# Really need to sort all this color crap out, maybe time for iterm 2 FIXME TODO
alias colorslist="set | egrep '^Color_\w*' | sort"
# Lists colors in their actual color, on one line
alias colors="echo -e \`colorslist | sed 's/\(.*\)=\(.*\)/\2 \1/'\`"


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

# more is less
alias more='less'

# Open the manual page for the last command you executed.
function lastman {
    set -- $(fc -nl -1);
    while [ $# -gt 0 -a '(' "sudo" = "$1" -o "-" = "${1:0:1}" ')' ]; do
	shift;
    done;
    local cmd="$(basename "$1")";
    man "$cmd" || help "$cmd";
}
alias lman='lastman'

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
# be more intellgigent about autocompleting in the middle of word
bind "set skip-completed-text on"
# change tab to cycle through matches
# bind 'TAB: menu-complete'
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
# Treat hyphens and underscores as equivalent
bind "set completion-map-case on"

### Prompts ----------------------------------------------------------
#export PS1="${Color_Cyan}\u${Color_White_Bold_Intense}@${Color_Cyan}\h${Color_White_Bold_Intense}>${Color_Green}\w${Color_White_Bold_Intense}\$ ${Color_zOff}"
# Smiley (cat?) face prompts, three kinds
#export PS1="\`if [ \$? = 0 ]; then echo \e[33\;40m\\\^\\\_\\\^\e[0m; else echo \e[36\;40m\\\-\e[0m\\\_\e[36\;40m\\\-\e[0m; fi\` \u \w:\h)"
#export PS1="\033[0;32m\u@\h\033[0m:\033[0;36m\A\033[0m:\`if [ \$? = 0 ]; then echo '\033[1;30m^_^\033[0m'; else echo '\033[0;33m>_>\033[0m' ; fi\`:\033[1;34m\w\033[0m>\033[0m "
#export PS1="\`if [ \$? = 0 ]; then echo \[\e[34m\]^_^\[\e[0m\]; else echo \[\e[31m\]O_O\[\e[0m\]; fi\`[\u@\h:\w]\\$ "

# Reset color for command output, invoked every time before command is executed
# Seems unnecessary right now
#    trap 'echo -ne "\033[00m"' DEBUG


### Prompt functions
# Return a color indicating system load
# Basically, if load average is greater than the number of CPUs, then
# executions will be delayed/queued.  This just assigns colors to that.
function _load_color() {
    if [[ $OSTYPE == darwin* ]]; then
	local NCPU=$(sysctl -n hw.ncpu)
	local SYSLOAD=$(sysctl -n vm.loadavg | cut -f 2 -d ' ')
    else
	local NCPU=$(nproc)
	local SYSLOAD=$(uptime | cut -d ":" -f 4- | sed s/,//g | cut -f 2 -d " ")
    fi
    # Remove decimal, essentially treating it as a percentage (40 instead of
    # 0.40) since bash can't do math with floating points
    SYSLOAD=$(tr -d '.' <<< "$SYSLOAD")

    if [ ${SYSLOAD} -lt $((100*${NCPU})) ]; then
	echo -en ${Color_Yellow} # Normal load
    elif [ ${SYSLOAD} -lt $((200*${NCPU})) ]; then
	echo -en ${Color_Magenta_Intense} # Small load
    elif [ ${SYSLOAD} -lt $((400*${NCPU})) ]; then
	echo -en ${Color_Red_Bold_Intense} # Medium load
    else
	echo -en ${Color_Red_zBackground}${Color_Red_Bold_Intense} # Large load
    fi
}

# Hostname when connected via SSH
function _cnx() {
    if [[ $SSH_TTY || $INSTANCEPROJECT ]]; then
	echo -en "${Color_Blue_Intense}@${Color_Red}\h"
    fi
}

# Test user
# id -un, logname, $USER: not necessarily the same!
function _uid() {
    if [[ $USER != $USER_NAME ]]; then # Only if not meeeee
	local color
	case $USER in
	    root) color="${Color_Red}";; # User is root
	    "$(logname)") color="${Color_Green}";; # User is normal (mostly)
	    *) color="${Color_Red_zBackground}";; # User is not login user
	esac
	echo -en "$color\u"
    fi
}

# Return a color according to running/suspended jobs.
function _job_color() {
    if [ $(jobs -s | wc -l) -gt "0" ]; then
	echo -en ${Color_Red_Bold_Intense}
    elif [ $(jobs -r | wc -l) -gt "0" ]; then
	echo -en ${Color_Yellow_Intense}
    fi
}

###### holiday greeting
# from Jonathan's .bashrc file (by ~71KR117)
# http://dotshare.it/dots/516/
function holiday_greeting() {
    case $(date +"%B %d") in
	"January 01")
	    # get current year (for new years greeting)
	    local year=$(date +"%Y")
	    holgreet="${Color_Magenta_Intense}Happy ${Color_Red_Intense}New ${Color_Blue_Intense}Year!${Color_zOff} Have a great $year.";;
	"February 02") holgreet="Happy Groundhog Day!";;
	"February 14") holgreet="Happy ${Color_Magenta}Valentine's Day!${Color_zOff}";;
	"July 04") holgreet="Happy ${Color_Red_Intense}Fourth ${Color_White_Intense}of ${Color_Blue_Intense}July!${Color_zOff}";;
	"August 12") holgreet="Happy Birthday!";;
	"October 31") holgreet="${Color_Red_Bold}Happy Halloween!${Color_zOff}";;
	"December 24") holgreet="Happy ${Color_Green_Intense}Christmas ${Color_Red}Eve!${Color_zOff}";;
	"December 25") holgreet="${Color_Green_Intense}Merry ${Color_Red}Christmas!${Color_zOff}!";;
	"December 31") holgreet="Happy New Year's Eve!";;
	*) exit 0;;
    esac
    echo -e "\n$holgreet"
}

### Actual prompt
# Should try and incorporate PS0 from bash 4.4 somewhere... FIXME TODO
function prompt_command {
    # Catch previous errors before everything else so that the return value
    # isn't valid
    if (($? > 0)); then
	ERRORS=1
    else
	ERRORS=0
    fi

    # PS1... ASSEMBLE!
    PS1="\[$Color_Black\]"'$fill'"\n\[$Color_Cyan\]┌─"

    psuser="$(_uid)$(_cnx)"
    if [[ -n "$psuser" ]]; then
	PS1+="[$psuser\[$Color_Cyan\]]-"
    fi

    PS1+="[\[$(_load_color)\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$Color_Yellow\]\w\[$Color_Cyan\]]"

    gitprompt="$(gitprompt.sh)"
    if [[ -n "$gitprompt" ]]; then
	PS1+="-[\[$Color_Yellow\]$gitprompt\[$Color_Cyan\]]"
    fi
    PS1+="$(holiday_greeting)\n\[$Color_Cyan\]└─["

    if ((${ERRORS} > 0)); then
	PS1+="\[$Color_Red_Intense\]\$"
    else
	PS1+="\[$Color_Magenta\]\#"
    fi

    PS1+="\[$Color_Cyan\]]\[$(_job_color)\]->\[$Color_zOff\] "
    export PS1

    # create a $fill of all screen width minus the time string and a space:
    let fillsize=${COLUMNS}	# fullscreen
    # room for battery charge plus the color control codes, not if sshing
    if [[ ! $SSH_TTY && ! $INSTANCEPROJECT && $(which battery) ]]; then
	battery=$(battery -a 2>/dev/null | tr -d ' ')
	let fillsize=${fillsize}-${#battery}+12
    fi

    if [[ $(which hr) ]]; then
	fill="$(COLUMNS=$fillsize hr -)${battery}"
    else
	fill=""
	while [ "$fillsize" -gt "0" ]
	do
            fill="-${fill}"
            let fillsize=${fillsize}-1
	done
	fill="${fill}${battery}"
    fi

    # Save and reload history after each command, send all windows go to same file
    history -a; history -c; history -r
}

export PS2="\[$Color_Cyan\]→\[$Color_zOff\] " # Secondary prompt, multiline commands
export PS3='#? '			      # Tertiary prompt, select menus
export PS4='+ '				      # Quaternary prompt, ???

PROMPT_COMMAND=prompt_command

# Function to run upon exit of shell
function _exit()
{
    echo -e "${Color_Red_Bold_Intense}Thanks for playing${Color_zOff}"
}
trap _exit EXIT


## Sourcin'
## Throw all potential sources into an array, check, then source
# Some personal/private stuff, used in various places bash and lisp
# USER_NAME, NAME, EMAIL
# WIKI_USERNAME, WIKI_EMAIL_ADDRESS, TOOLFORGE_USERNAME
sources=("$HOME/.config/bash/priv-env.sh")
# Homebrew completion directory, here before macports since I generally use
# the latter.  Confident glob probably fine given the -f -r checks below.
sources+=(/usr/local/etc/bash_completion.d/*)
# Advanced bash completion https://github.com/scop/bash-completion
# Standard location, in turn sources /usr/share/bash-completion/bash_completion
sources+=("/etc/bash_completion")
# Macports, in turn sources /opt/local/share/bash-completion/bash_completion
sources+=("/opt/local/etc/bash_completion")
# Supplement the above with some missing items (pip, gem), some mac-specific
# ones (defaults, eject), and some personal pecadilloes
sources+=(~/.completions.d/*)
# Source all the (readable) things (files)!
for file in "${sources[@]}"; do
    [ -f "$file" ] && [ -r "$file" ] && source "$file";
done;
unset file;


# whois, etc. auto-completion based on entries in known_hosts.
# [[ -e "$HOME/.ssh/config" ]] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh
if [[ -e ~/.ssh/known_hosts ]]; then
    # This completely supersedes the above-sourced ssh.completion file, need
    # to stop that from happening. ;;;;;; ##### FIXME TODO
    complete -o default -W "$(cat ~/.ssh/known_hosts | sed 's/[, ].*//' | sort | uniq | grep -v '[0-9]')" whois nslookup nmap
fi
# Complete for g alias
if type __git_complete &> /dev/null; then
    __git_complete g __git_main
fi
# networksetup completion
if [[ $OSTYPE == darwin* ]]; then
    complete -o default -W "$(networksetup -printcommands | grep -Ee "-.+?\b" -o | grep -v delete | grep -v rofile)" networksetup;
fi

if [[ -f `command -v pip` ]]; then
    # https://snarky.ca/why-you-should-use-python-m-pip/
    alias pip='python -m pip '

    # Only run pip if virtualenv activated
    # export PIP_REQUIRE_VIRTUALENV=true
fi

# z, the awesome helper for moving around to popular directories
# Installed via macports: https://github.com/rupa/z
if [[ -e /opt/local/etc/profile.d/z.sh ]]; then
    export _Z_MAX_SCORE=13000	# Up from 9000, entries persist longer
    . /opt/local/etc/profile.d/z.sh
fi


# Make perl -d automatically use NYTProf.  See also dprofpp
# Needs to be dependent on existing FIXME TODO
export PERL5DB='use Devel::NYTProf'

# Access Perl::Critic documentation
if [[ -f `command -v perlcritic` ]]; then
    function explain_perlcritic() {
	perldoc Perl::Critic::Policy::"$1"
    }
    _explain_perlcritic()		# ;;;;;; ##### FIXME TODO
    {
	local cur="${COMP_WORDS[COMP_CWORD]}"
	local list="$(\ls /opt/local/share/perl$PERL5/siteman/man3/Perl\:\:Critic\:\:Policy\:\:*)"
	local clean="$(echo -n "${list}" | sed 's/^.*man3\/Perl::Critic::Policy:://g' | sed 's/\.3pm$//g')"

	COMPREPLY=($(compgen -W "$clean" -- "$cur"))
    }
    complete -F _explain_perlcritic explain_perlcritic
fi
# shorthand for perlfunc
function pf() {
    if [ $# -eq 0 ]; then
	perldoc perlfunc
    else
	perldoc -f "$@"
    fi
}
function pq() {
    perldoc -q "$@"
}
alias perlsecret='man perlsecret'
alias perlcheat='man perlcheat'

# Tell tidy to use a config file if it's there
if [[ -f `command -v tidy` ]]; then
    export HTML_TIDY=~/.tidyrc
fi

# Emacs stuff.  Makes which/type confused, but order is important
# emacs daemon/emacsclient
alias emd='\emacs --daemon '
# Get server status
function eserver-status() {
    if [[ `ps -Af | grep "emacs --daemon"` ]]; then
	echo "Emacs server running"
    else
	echo "Emacs server not running"
    fi
}
alias killemacs-server="\emacsclient -e '(kill-emacs)'"
alias kemacs-server='killemacs-server'
alias ke='kemacs-server'
alias kef="\emacsclient -e '(server-force-delete)'"
alias emacsclient='\emacsclient -cqu '
alias emacs='emacsclient '
# Recompile all elisp files, with proper warnings/output
function recompile_emacs() {
    \emacs -batch --eval '(byte-recompile-directory "~/.emacs.d/" 0)'
}
alias ii=recompile_emacs

alias e="$VISUAL "
# Make customization easier
alias bashrc='$VISUAL ~/.bashrc'
alias rc='$VISUAL ~/.bashrc'
alias eb='$VISUAL ~/.bashrc'
alias bashprofile='$VISUAL ~/.bash_profile'
alias bp='$VISUAL ~/.bash_profile'
alias ep='$VISUAL ~/.bash_profile'
alias ee='$VISUAL ~/.emacs'
alias eg='$VISUAL ~/.gitconfig'
alias gitconfig='$VISUAL ~/.gitconfig'
function gitignore() {
    local ignore=".gitignore"
    if [[ ! -f $ignore ]]; then
	ignore="~/.global-gitignore"
    elif [[ -n $1 && $1 = "g" ]]; then
	ignore="~/.global-gitignore"
    fi
    $VISUAL $ignore
}

# Javascript alias, can also just use node
alias jsc='/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc '

#  alias reload="exec $SHELL -l"
alias reload='. ~/.bashrc'

# Navigation -------------------------------------------------------
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~="cd ~"
alias -- --='cd -'		# go back a directory, -- means end of options
alias -- -='cd -'
alias b='cd -'

# Shortcuts
alias bin="cd ~/bin"
alias dt="cd ~/dotfiles"
alias dr="cd ~/Dropbox"
alias de="cd ~/Desktop"
alias dg="cd $GIT_PERS_DIR"
alias dgt="cd $GIT_PERS_DIR/twinkle@azatoth"
alias dgm="cd $GIT_PERS_DIR/twinkle@azatoth/modules"
alias dgj="dgm"
alias dgs="cd $GIT_PERS_DIR/mls"
alias dgw="cd $GIT_PERS_DIR/mediawiki"
alias dp="cd $PERL_PERS_DIR"
alias dps="cd $PERL_PERS_DIR/sandbox"
alias dpk="cd $PERL_PERS_DIR/ksp"
alias dpw="cd $PERL_PERS_DIR/wiki"
alias dpc="cd $PERL_PERS_DIR/wiki/crathighlighter"
alias dws="cd $PERL_PERS_DIR/wiki/sysopIndex"
alias dwu="cd $PERL_PERS_DIR/wiki/userScripts"
alias drk="cd ~/Documents/R/kinship/"
alias eds="cd ~/.emacs.d/site-lisp/"
alias m='more'
#alias c='cat'
alias j="jobs"
alias h='history 15'
alias hig='history | grep -i'
alias cl='clear'
alias cls='clear'
alias count='wc -l'
alias linecount='count'

# http://linux.die.net/man/1/pygmentize
# Colorized cat
alias cot='pygmentize -O bg=light -g'

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

# Get current lockscreen quote
alias getquote='defaults read /Library/Preferences/com.apple.loginwindow.plist LoginwindowText'

# thefuck https://github.com/nvbn/thefuck
if which thefuck > /dev/null; then
    eval "$(thefuck --alias)"
fi

# Prompt before overwrite, be vocal about it
alias mv='mv -vi' # add -f to override, or \ before command
alias cp='cp -Rvi' # recursive if folder, the ending / makes a difference
# alias rm='rm -i' # Too annoying, perhaps?
alias rm='rm -v'
# Move the given file(s) to the Trash. Favor osxutils https://github.com/vasi/osxutils
# Alias rm to this instead???
if [[ ! -f `command -v trash` ]]; then
    function trash() {
	mv $1 ~/.Trash
    }
fi
function emptyalltrashes() {
    sudo rm -rfv /Volumes/*/.Trashes;
    sudo rm -rfv ~/.Trash;
}

# Make intermediate directories and be verbose about it
alias mkdir='mkdir -pv'
# Deleting intermediate directories not as logical?
alias rmdir='rmdir -p'
alias rd='rmdir'

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
alias mkd='mkcd'

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
# Find and kill processes by name.  Be careful!
killsearch() {
    ps ax | grep "$1" | grep -v grep | cut -f1 -d ' ' | xargs kill
}


# Top by memory, not in delta
alias topm='top -o vsize'
# Top always sorted by cpu, in delta mode
alias top='top -d -o cpu'
# See memory_pressure for Mavericks-style-ish output
# Use brainbarf for Mavericks-style info, color
function memstat {
    vm_stat | cut -d ":" -f 2 | tr -d '. ' | tr '\n' ' ' | awk '{printf("U:%.1fG|F:%.1fG\n", (($3 + $4 + $6) * 4)/(1024*1024), (($2+$5) * 4)/(1024*1024))}'
}

# Show five most recently modified files.
alias last-modified='ls -t $* 2> /dev/null | head -n 5'

# growlnotify: add after to show when done
# Maybe other options...
# Just use terminal-notifier...
# Also checkout boxcar, notify-send
alias growl='growlnotify -t Terminal -m "Done" && tput bel' # Red badge? and popping only in 10.7+
alias beep='tput bel'

# Enhanced WHOIS
# Busted as of mid-2014
# alias whois='whois -h whois-servers.net'

# Use colordiff if it exists
if [[ -f `command -v colordiff` ]]; then
    alias diff='colordiff';
fi
if [[ -f `command -v cwdiff` ]]; then
    alias wcolordiff='cwdiff'
elif [[ -f `command -v wdiff` ]]; then
    # What is this?  Need to explain FIXME TODO
    alias wcolordiff="wdiff -n -w $'\033[30;41m' -x $'\033[0m' -y $'\033[30;42m' -z $'\033[0m'"
fi
alias diffc='wcolordiff'
alias diffw='wcolordiff'
# But for real?  Just use dwdiff: git-like giff
# Use -3 option to only show changes
alias dwdiff='dwdiff -sc'

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

# Same for ripgrep
if [[ -f `command -v rg` ]]; then
    export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

    # -i largely meaningless with -S/--smart-case in the ripgreprc
    alias rgi='rg -i'
    alias rgc='rg -C 3'
    alias rgic='rgc -i'
    alias rgC='rg -C 10'
    alias rgiC='rgC -i'
fi

# Applications
#ls | open -f # pipe ls, open in default application (probably texteditor)
alias reveal='open -R'
alias preview='open -a preview'
alias finder='open -a Finder'
alias textedit='open -a TextEdit'
alias te='textedit '
alias safari='open -a safari'
alias firefox='open -a firefox'
alias chrome='open -a google\ chrome'
alias vlc='open -a vlc'
alias excel='open -a microsoft\ excel'

# Macports
if [[ -f `command -v port` ]]; then
    alias pecho='port echo'
    alias psync='sudo port sync'
    alias pself='sudo port selfupdate'
    alias pall='sudo port selfupdate && port outdated && sudo port upgrade outdated'
    alias pout='port outdated'
    alias pug='sudo port upgrade outdated'
    alias puo='pug'
    alias pleaves='port echo leaves'
    alias pclean='sudo port clean -v --all installed'
    alias pcleanleaves='port echo leaves >> ~/port_leaves_log.txt && sudo port uninstall leaves'
    alias pactive='port echo active'
    alias pinactive='port echo inactive'
    alias pinfo='port info'
    alias psearch='port search'
    alias pin='sudo port install'
    alias pun='sudo port uninstall'

    function pmoreinfo() {
	psearch $1 | grep @ | cut -f 1 -d ' ' | while read prt; do pinfo $prt; done;
    }

    # Macports changelog
    # https://trac.macports.org/browser/contrib/port-whatsnew/port-whatsnew
    alias whatsnew='port echo outdated | cut -f 1 -d" " | xargs -n 1 ~/bin/port-whatsnew.sh'
fi

# Homebrew/Cask
if [[ -f `command -v brew` ]]; then
    brew_repo=$(brew --repo)
    alias dgh="cd $brew_repo"
    alias dgc="cd $brew_repo/Library/Taps/homebrew/homebrew-cask"

    # Make homebrew verbose by defaults
    # export HOMEBREW_VERBOSE=1
    # Don't build from source
    export HOMEBREW_NO_BOTTLE_SOURCE_FALLBACK=1
    alias ball='brew update ; brew outdated ; brew upgrade'
    alias bclean='brew cleanup ; brew cleanup -s'
    alias bsearch='brew search'
    alias bs='bsearch '
    alias binfo='brew info'
    alias blist='brew list'
    alias bdoctor='brew doctor'

    # This could just be `brew deps --installed --formula` and would be *much* faster,
    # but the blue coloring is nice?  Maybe stupid
    function homebrew-deps() {
	brew list --formula | while read formula; do echo -en "${Color_Blue_Bold}$formula ->${Color_zOff}"; brew deps --formula --installed $formula | awk '{printf(" %s ", $0)}'; echo ""; done
    }
    function homebrew-dependents() {
	brew list --formula | while read formula; do echo -en "${Color_Blue_Bold}$formula ->${Color_zOff}"; brew uses --formula --installed $formula | awk '{printf(" %s ", $0)}'; echo ""; done
    }

    # Homebrew-cask
    # Symlink in /Applications
    export HOMEBREW_CASK_OPTS="--appdir=/Applications --qlplugindir=/Library/Quicklook"
fi

# bundle exec alias
alias be='bundle exec'
alias bej='be jekyll serve --watch'
alias jes='jekyll serve --watch'

# Quickly open and make a new file executable; emacs will add headers and move
# point to the appropriate location if there's a well-defined skeleton
function newscript() {
    if [ $# -eq 0 ]; then
	echo "No arguments provided";
    else
	if [ -a $1 ]; then
	    echo "$1 already exists, opening";
	else
	    touch $1
	    chmod 755 $1
	fi
	$VISUAL $1
    fi
}
# Deprecated
function newperl() {
    echo "Use newscript instead!"
    sleep 1
    newscript $1
}
alias newbash="newperl "

alias d="diary "

alias keys="more ~/.ssh/id_rsa.pub | pbcopy | echo '=> Public key copied to pasteboard.'"

# Start an HTTP server from a directory, optionally specifying the port
function server() {
    local port="${1:-8000}"
    sleep 1 && open "http://localhost:${port}/" &
    python -m SimpleHTTPServer "$port"
}

# Functions to start/stop mysql server (installed via dmg, not macports/homebrew)
# Names are awful for completion
function mysqlstart() {
    unset TMPDIR	 # Not sure why but this is apparently quite necessary
    if [[ -n `ls /usr/local/mysql/data/*.pid 2>/dev/null` ]]; then
	echo "mysql server is already running"
	if [ -e "/usr/local/mysql/data/mysqld.local.pid" ]; then
	    echo "Use system preferences to turn it off"
	else
	    echo "Use mysqlstop to turn it off"
	fi
	return 1
    fi
    sudo /usr/local/mysql/support-files/mysql.server start
}
function mysqlstop() {
    if [[ ! -n `ls /usr/local/mysql/data/*.pid 2>/dev/null` ]]; then
	echo "mysql server isn't running"
	echo "Use mysqlstart to turn it on"
	return 1
    fi
    if [[ -e "/usr/local/mysql/data/mysqld.local.pid" ]]; then
	echo "mysql server is running, but use system preferences to turn it off"
	return 1
    fi
    sudo /usr/local/mysql/support-files/mysql.server stop
}
function mysqlstatus() {
    if [[ -n `ls /usr/local/mysql/data/*.pid 2>/dev/null` ]]; then
	echo -n "Running"
	if [ -e "/usr/local/mysql/data/mysqld.local.pid" ]; then
	    echo " via system preferences"
	else
	    echo " via command line"
	fi
    else
	echo "Not running"
    fi
}

# https://transfer.sh function
# Modified slightly
function transfer() {
    if [ $# -eq 0 ]; then
	echo "Usage: transfer /tmp/test.md"
	echo "cat /tmp/test.md | transfer test.md"
	return 1
    fi

    local basefile tmp
    if [ `tty -s` ]; then
	basefile=$(basename "$1" | sed -e 's/[^a-zA-Z0-9._-]/-/g')
	tmp=$(curl --progress-bar --upload-file "$1" "https://transfer.sh/$basefile")
    else
	tmp=$(curl --progress-bar --upload-file "$1" "https://transfer.sh/$1")
    fi
    echo -n $tmp | pbcopy
    echo $tmp
}

function twinmos() {
    diskutil umount /Volumes/TWINMOS
}
function sandisk() {
    diskutil umount /Volumes/SANDISK
}
alias unmount='diskutil umount'
alias eject='unmount'
# Eject all drives, kind of slow?
function ejectall() {
    find /Volumes -maxdepth 1 -not -user root -a -not -name '.*' -print0 | xargs -0 umount
}

# Control iTunes ---------------------------------------------------
alias next='itunes next'
alias previous='itunes prev'
alias play='itunes play'
alias pause='itunes pause'

# Track who is listening to your iTunes music
alias whotunes='lsof -r 2 -n -P -F n -c iTunes -a -i TCP@Durandal.local:3689'

# Volume control, potentially using osxutils
if [ ! -x /opt/local/bin/setvolume ]; then
    function setvolume() {
	if [ ! $1 ]; then
	    echo "setvolume <0-100>"
	elif [ $1 -lt 0 -o $1 -gt 100 ]; then
	    echo "setvolume <0-100>"
	else
	    local val=$1*7/100
	    osascript -e "set volume $val"
	fi
    }
fi
alias mutevolume='setvolume 0'
alias quartervolume='setvolume 25'
alias middlevolume='setvolume 50'
alias threequartervolume='setvolume 75'
alias maxvolume='setvolume 100'

# Stuff I never use but can't delete because of http://xkcd.com/530/
alias stfu='mutevolume'
alias pumpitup='maxvolume'
alias haxx="growlnotify -a 'Activity Monitor' 'System error' -m 'WTF R U DOIN'"

# Seriously though, just use pianobar/pandora
alias pianobar='pianokeys && pianobar' # Use media keys with pianobar
alias piano='pianobar'		       # Allow autocomplete to work
alias pandora='pianobar'
# old habits die hard
alias mplayer='mpv'


# Show most used commands, fixed by doing $4 instead of $2, from:
# http://lifehacker.com/software/how-to/turbocharge-your-terminal-274317.php
alias profileme="history | awk '{print \$4}' | awk 'BEGIN{FS=\"|\"}{print \$1}' | sort | uniq -c | sort -n | tail -n 20 | sort -nr"

# Lack of space important!
alias which='type -a 2>/dev/null'

# Pretty-print PATH
alias path='echo -e ${PATH//:/\\n}'
alias pretty-manpath="manpath | tr ':' '\n'"

# Matrix-esque screensaver-esque; man tr for different options
alias matrix='tr -c "[:print:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=lcase,unblock | GREP_COLOR="1;32" grep --color "[^ ]"'
# Star Wars
function starwars()
{
    echo "Loading Star Wars..."
    echo "Use C-] to exit"
    sleep 1
    telnet towel.blinkenlights.nl
}
# Lightcycle https://github.com/zachlatta/sshtron
function lightcycle()
{
    echo "Loading Tron Lightcycle..."
    echo "Use WASD to play, ctrl-C to quit"
    sleep 1
    ssh sshtron.zachlatta.com
}
alias tron='lightcycle'
# Hackers script https://twitter.com/paultag/status/644160355409752065
# 2>&1 | less -S for chunks
alias zerocool='nc z.ero.cool 1337'

# Lock the screen (when going AFK)
alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"
# Better as just starting screensaver
alias screensaver='open -a ScreenSaverEngine'
# Screensaver as wallpaper, ctrl-c or cmd-. to quit
alias screensaverToWallpaper="/System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine -background"
# Stolen from @cowboy - https://github.com/cowboy/dotfiles/commit/28a3fd898f93c602080e3c3112b0f04854b66f22
function lock-screen()
{
    sleep 0.5

    if [[ "$(ioreg -c AppleSmartBattery | grep '"ExternalConnected" = Yes')" ]]; then
	# Plugged in: start screensaver.
	open /System/Library/Frameworks/ScreenSaver.framework/Versions/A/Resources/ScreenSaverEngine.app
    else
	# Battery power: go to lock menu.
	/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend
    fi
}

# Stopwatch
alias timer='echo "Timer started. Stop with Ctrl-D." && date && time cat && date'
alias stopwatch='timer'

# Get week number
alias weeknum='date +%V'

# See also .icalBuddy.plist
alias events='icalBuddy -sd -t -li 7 eventsFrom:today to:today+5'

# Quickly check connection by pinging google
alias pg='ping -c 1 google.com'
function down4me()		# FIXME TODO
{
    curl -s "http://www.downforeveryoneorjustme.com/$1" | sed "/just you/!d;s/<[^>]*>//g" | sed -e 's/  //g' | sed -e 's/http.*#x2F;/  /g'
}
alias downforeveryoneorjustme='down4me'


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
    local iplist=$(ifconfig -a | perl -nle'/inet (?:addr:)?(\d+\.\d+\.\d+\.\d+)/ && print $1')

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
# $OSTYPE == darwin*
alias localip='ipconfig getifaddr en0'
alias ips="ifconfig -a | grep -o 'inet6\? \(\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\)\|[a-fA-F0-9:]\+\)' | sed -e 's/inet6* //'"
alias ipz='dig +short myip.opendns.com @resolver1.opendns.com'
alias ipaddr="ifconfig -a | grep 'inet' | grep 'broadcast' | awk '{ print $2 }'"
alias ipinfo='http -b ipinfo.io/json'

# $OSTYPE == darwin* FIXME TODO
function ssid()
{
    local ssid=$(/System/Library/PrivateFrameworks/Apple80211.framework/Versions/A/Resources/airport -I | grep " SSID" | sed "s/.*: //")

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
alias eaves='lsof -iTCP -sTCP:LISTEN -P "$@"'

# Change mac address https://jezenthomas.com/free-internet-on-trains/
function remac {
    sudo /System/Library/PrivateFrameworks/Apple80211.framework/Resources/airport -z
    sudo ifconfig en0 ether $(openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//')
    sudo networksetup -detectnewhardware
    echo $(ifconfig en0 | grep ether)
}

for method in GET HEAD POST PUT DELETE TRACE OPTIONS; do
    alias "$method"="lwp-request -m '$method'"
done

alias quicklook="qlmanage -p 2> /dev/null"
alias ql="quicklook"

alias pbc='pbcopy'
alias pbp='pbpaste'
# Trim new lines and copy to clipboard
alias clipboard="tr -d '\n' | pbcopy"

function uniqsort() {
    sort $1 | uniq | sort
}
# quicksort in three lines from http://git.io/UzwyWQ
qsort()
{
    local L=""; local G=""; [ $# -eq 1 ] && echo $1 && return;
    P=$1; shift; for i in $@; do [ $i -lt $P ] && L="$L $i" || G="$G $i"; done
    [ -z "$L" ] || L=`qsort $L`; [ -z "$G" ] || G=`qsort $G`; echo "$L $P $G"
}

# Get rid of pesky .DS_Store files, recursively
alias dscleanup='find . -type f -name "*.DS_Store" -print0 | xargs -0 rm -v'
# Clean up LaunchServices to remove duplicates in the "Open With" menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
# Flush Directory Service cache
alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"

# Alias for playball a la nba-watch
# https://www.npmjs.com/package/playball
# https://github.com/chentsulin/watch-nba
if [[ -f `command -v playball` ]]; then
    alias watch-mlb='playball';
fi

# OS X has no md5sum, so use md5 as a fallback
command -v md5sum > /dev/null || alias md5sum="md5"
# OS X has no sha1sum, so use shasum as a fallback
command -v sha1sum > /dev/null || alias sha1sum="shasum"
# OS X has no sha256sum, so use shasum as a fallback
command -v sha256sum > /dev/null || alias sha256sum="shasum -a 256"
# OS X has no sha512sum, so use shasum as a fallback
command -v sha512sum > /dev/null || alias sha512sum="shasum -a 512"

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
alias whisper='lc'

# Convert to uppercase.
function uc {
    if [ $# -eq 0 ]; then
	tr '[:lower:]' '[:upper:]';
    else
	tr '[:lower:]' '[:upper:]' <<< "$@";
    fi;
}
alias shout='uc'

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
alias cdf='cdfinder'


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
# Same, but from /dev/urandom
randpass ()
{
    local length=$1
    if [[ -z $length ]]; then
	length=8
    fi
    echo $(cat /dev/urandom | env LC_CTYPE=C tr -cd "[:alnum:]" | head -c $length)
    echo $(cat /dev/urandom | env LC_CTYPE=C tr -cd "[:graph:]" | head -c $length)
}

# Dashboard stock prices, use ticker for price, stockclose for last close
function marketupdate() {
    local FILES="SSO QLD VOOG QQQ .DJI .IXIC .INX INDEXNYSEGIS:NYA TNX"

    # Only show investments if after market close or weekend
    # Based on DST, correct using Eastern time??
    if ((`date -u '+%H'` < 13)) || ((`date -u '+%u'` > 5)); then
	FILES="FGCKX FDIKX VFTNX VSCPX VGSNX VTSMX VFFVX RDITX $FILES"
    fi

    ticker $FILES | column -t
}
alias mu='marketupdate'
alias stockmarket='ticker'
alias inflation="perl $PERL_PERS_DIR/sandbox/inflation.pl"

# Robinhood dashboard
# https://github.com/bcwik9/robinhood-on-rails
function robinhood() {
    (cd $GIT_PERS_DIR/robinhood-on-rails@bcwik9/ ; exec bundle exec rails server & sleep 1 & browser http://localhost:3000/)
}
# Shell
# https://github.com/anilshanbhag/RobinhoodShell
function robinhood-shell() {
    (cd $GIT_PERS_DIR/RobinhoodShell@anilshanbhag ; ./shell.py)
}

# Update crathighlighter
function crathighlighter() {
    (cd $PERL_PERS_DIR/wiki/crathighlighter/ ; perl cratHighlighterSubpages.pl "$@")
}
# Check twinkle
function twinkleCheck() {
    (cd $PERL_PERS_DIR/wiki/twinkle/ ; perl twinkleCheck.pl $@)
}
# Easy
alias toolforge="ssh -i ~/.ssh/id_rsa_toolforge $TOOLFORGE_USERNAME@login.toolforge.org"

# Get the weather
function weather() {
    local where
    if [ $1 ]; then
	where=$1;
    fi
    if [[ $where =~ help ]]; then
	curl wttr.in/:help
    else
	forecast=$(curl -sH "Accept-Language: ${LANG%_*}" wttr.in/"$where")
	if [[ -n "${forecast}" ]]; then
	    echo "$forecast"
	else
	    echo "Unable to forecast weather"
	fi
    fi
}

# From https://gist.github.com/komasaru/9635884
# Busted, replace with https://www.aviationweather.gov/metar FIXME TODO
function metar()
{
    if [[ ! "$1" =~ [0-9A-Z]{4} ]]; then
	echo "Please enter an appropriate METAR code"
	echo "Suggestions: KEDU KSMF KSAC KNYC KBOS KSFO"
	return
    fi
    # Convert to all caps
    local code=$(echo -n $1|tr '[a-z]' '[A-Z]')

    local URL="http://weather.noaa.gov/pub/data/observations/metar/decoded/"
    wget -q -O - "${URL}${code}.TXT" 2>/dev/null
}

function thisforthat {
    curl "http://itsthisforthat.com/api.php?text"
    if [ -t 1 ]; then
	echo
    fi
}
alias sobasically='thisforthat'

function dad {
    curl https://icanhazdadjoke.com/
    echo
}

# Print the given text in the center of the screen.
function center {
    local width=$(tput cols);
    local str="$@";
    local len=${#str};
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

# Create a data URL from a file
function dataurl() {
    local mimeType=$(file -b --mime-type "$1");
    if [[ $mimeType == text/* ]]; then
	mimeType="${mimeType};charset=utf-8";
    fi
    echo "data:${mimeType};base64,$(openssl base64 -in "$1" | tr -d '\n')";
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

# Monitor file live with tail
function monitor() {
    tail -f $1 | while read line; do printf "$(date '+%F %T')\t$line\n"; done;
}

# /. headlines, turn into shell script to option output?
# Not perfect but good enough
function slashdot() {
    curl -s "http://rss.slashdot.org/Slashdot/slashdot" | perl -ne 'print "\t" if /<name>/; print "$2\n" if /<(title|name)>(.*)<\/\1>/;' | tail -n +3
}
# HN mainpage stories
function hackernews() {
    curl -s "https://news.ycombinator.com/news" | perl -ne 'print "$1\n" if /class=\"storylink\">(.*)<\/a><span class=\"sitebit comhead\">/;'
}

# Test how fast the machine is, 32GB
function writetest() {
    dd if=/dev/zero of=/dev/null bs=1048576 count=32768;
}

# Find a file with a pattern in name, excluding a bunch of useless locations
function ff() {
    find . -path './.cpan' -prune -o - path './.git' -prune -o -path './.config' -prune -o -path './Library' -prune -o -path './.fseventsd' -prune -o -path './.Spotlight-V100' -prune -o -path './.Trashes' -prune -o -type f -iname '*'$*'*' -ls;
}

# Remove empty directories under and including <path>s.
alias prunedirs='find "$@" -type d -empty -depth | xargs rmdir'

# Count files in each sub-directory
# Make script to option count versus list?????????????
# Use tree, may be faster?
function filecount() {
    for dir in */ ; do echo -n "$dir " ; find "$dir" | wc -l ; done;
}

# Determine size of a file or total size of a directory
function filesize() {
    if du -b /dev/null > /dev/null 2>&1; then
	local arg=-sbh
    else
	local arg=-sh
    fi
    if [[ -n "$@" ]]; then
	du $arg -- "$@"
    else
	du $arg .[^.]* ./*
    fi
}
alias fs='filesize'

# Pipe to map, then give command to invoke on every line from stdin
# from @nvie https://coderwall.com/p/4tkkpq
# ls **/*.sh | map dirname
alias map="xargs -n1 "

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

######## Scripts originally by @exogen
function aac {
    # Get best audio, convert it to AAC, and save it to the current directory.
    youtube-dl --default-search=ytsearch: \
	       --restrict-filenames \
	       --format=bestaudio \
	       --extract-audio \
	       --audio-format=aac \
	       --audio-quality=1 "$*"
}
function mp3 {
    # Get best audio, convert it to MP3, and save it to the current directory.
    youtube-dl --default-search=ytsearch: \
	       --restrict-filenames \
	       --format=bestaudio \
	       --extract-audio \
	       --audio-format=mp3 \
	       --audio-quality=1 "$*"
}
function listen-youtube {
    # Skip DASH manifest for speed purposes. This might actually disable being
    # able to specify things like 'bestaudio' as the requested format, but try
    # anyway. Use "$*" so that quoting the requested song isn't necessary.
    youtube-dl --default-search=ytsearch: \
	       --youtube-skip-dash-manifest \
	       --output="${TMPDIR:-/tmp}/%(title)s-%(id)s.%(ext)s" \
	       --restrict-filenames \
	       --format=bestaudio \
	       --exec=mpv "$*"
}
########

# Generate family tree without polutting current directory
function family()
{
    Rscript ~/Documents/R/kinship/family_tree.R
    rm Rplots.pdf
}

# Using Rscript allows more complex constructions, wrap () in quotes
# http://www.compbiome.com/2010/06/r-command-line-calculator-using-rscript.html
alias calc='Rscript -e "eval( parse( text=commandArgs( TRUE ) ) )"'

function =() {
    calc $@
}

# Calculate factors
# https://twitter.com/climagic/status/550355281415503872
function factors {
    local num=$1;
    seq $(($num/2)) | awk '"'$num'"%$0==0'
}

alias sumup="perl -MList::Util=sum -alne 'push @S,@F; END { print sum @S }'"

# 39 digits each
alias pi="perl -Mbignum=PI -le 'print PI'"
alias E="perl -Mbignum=e -le 'print e'"
function exp {
    perl -Mbignum=bexp -le "print bexp($1,39)"
}

# Extract most types of compressed files
function extract {
    echo Extracting $1 ...
    if [ -f $1 ] ; then
	case $1 in
	    *.tar.bz2)tar xjf $1;;
	    *.tar.gz)tar xzf $1;;
	    *.tar.Z)tar xzf $1;;
	    *.bz2)bunzip2 $1;;
	    *.rar)unrar x $1;;
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

    local size=$(
	stat -f"%z" "${tmpFile}" 2> /dev/null; # OS X `stat`
	stat -c"%s" "${tmpFile}" 2> /dev/null; # GNU `stat`
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

    echo "Compressing .tar ($((size / 1000)) kB) using \`${cmd}\`…";
    "${cmd}" -v "${tmpFile}" || return 1;
    [ -f "${tmpFile}" ] && rm "${tmpFile}";

    zippedSize=$(
	stat -f"%z" "${tmpFile}.gz" 2> /dev/null; # OS X `stat`
	stat -c"%s" "${tmpFile}.gz" 2> /dev/null; # GNU `stat`
	      );

    echo "${tmpFile}.gz ($((zippedSize / 1000)) kB) created successfully.";
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
alias dict='dictionary'
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
urban() { word=`echo $* | sed 's/ /%20/g'`; curl -s http://api.urbandictionary.com/v0/define?term=$word | jq -reM .list[0].definition; }

# Add note to Notes.app (OS X 10.8+)
# Usage: `note 'title' 'body'` or `echo 'body' | note`
# Title is optional
# FIXME TODO
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
# FIXME TODO
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
pushd tmp    Push tmp && cd tmp
popd         Pop && cd
GLOBBING AND OUTPUT SUBSTITUTION
--------------------------------
ls a[b-dx]e  Globs abe, ace, ade, axe
ls a{c,bl}e  Globs ace, able
\$(ls)        \`ls\` (but nestable!)
HISTORY MANIPULATION
--------------------
!!           last command
!?foo        last command containing \`foo'
^foo^bar^    last command containing \`foo', but substitute \`bar'
!!:0         last command word
!!:^         last command's first argument
!\$          last command's last argument
!!:*         last command's arguments
!!:x-y       arguments x to y of last command
C-s          search forwards in history
C-r          search backwards in history
LINE EDITING
------------
M-d          kill to end of word
C-w          kill to beginning of word
C-k          kill to end of line
C-u          kill to beginning of line
M-r          revert all modifications to current line
C-]          search forwards in line
M-C-]        search backwards in line
C-t          transpose characters
M-t          transpose words
M-u          uppercase word
M-l          lowercase word
M-c          capitalize word
COMPLETION
----------
M-/          complete filename
M-~          complete user name
M-@          complete host name
M-\$         complete variable name
M-!          complete command name
M-^          complete history
IF-THEN TEST OPERATORS
______________________
FILES
-a, -e       file exists
-d           file is a directory
-h, -L       file is a symbolic link
-r           file has read permissiong
-s           file exists, is not zero size
-f           file exists, is a regular file(?)
-w           file exists, has write permission
-x           file exists, has execute permission
-nt, -ot     file is newer/older than file2
-eq, -ne, -gt, -ge, -lt, -le
STRINGS
-n           length of string is not zero
-z           length of string is zero
== != < >"
}
