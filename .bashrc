## Notes
# /usr/share/doc/bash
# man fold
# man sysctl


# Localization
export LC_ALL=C
# export LC_ALL=en_US.UTF-8

### Colors ---------------------------------------------------------
export TERM=xterm-256color
# Change for different colors, to magenta
#  export GREP_OPTIONS='--color=auto' GREP_COLOR='0;35'
alias grep='grep --color=auto '
alias egrep='egrep --color=auto '
export GREP_COLOR='0;35'

# ls colors: Folder symbols, colors, human-readable sizes
if [[ $OSTYPE == darwin* ]]; then
    export CLICOLOR=1
    alias ls='ls -FGh'
elif [[ $OSTYPE == linux* ]]; then
    alias ls='ls -Fh --color=auto'
    # ls colors, see: http://www.linux-sxs.org/housekeeping/lscolors.html
    # Should probably make this match OSX, investigate dircolors FIXME TODO
    export LSCOLORS='di=1:fi=0:ln=31:pi=5:so=5:bd=5:cd=5:or=31:mi=0:ex=35:*.rb=90'
    # export LSCOLORS="ExGxBxDxCxEgEdxbxgxcxd"
    # export LSCOLORS='no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.ogg=01;35:*.mp3=01;35:*.wav=01;35:';
fi


# Setup some colors to use later in interactive shell or scripts
# ~/bin/colordump and termcolors give some clues about these
# 0; = regular
# 1; = bold
# 2; = faint
# 3; = italic
# 4; = underline
# 5; = blinking
# 6; = ???
# 7; = highlighted
# 8; = invisible

export Color_zOff="\033[0m"	  # Reset all attributes, stupid stupid hax for sorting
export Color_zBold="\033[1m"	  # Bold
export Color_zFaint="\033[2m"	  # Faint
export Color_zItalic="\033[3m"    # Italic
export Color_zInvisible="\033[8m" # Invisible

# Regular Colors
export Color_Black="\033[0;30m"        # Black
export Color_Red="\033[0;31m"          # Red
export Color_Green="\033[0;32m"        # Green
export Color_Yellow="\033[0;33m"       # Yellow
export Color_Blue="\033[0;34m"         # Blue
export Color_Magenta="\033[0;35m"      # Magenta
export Color_Cyan="\033[0;36m"         # Cyan
export Color_White="\033[0;37m"        # White

# Intense
export Color_Black_Intense="\033[0;90m"       # Black
export Color_Red_Intense="\033[0;91m"         # Red
export Color_Green_Intense="\033[0;92m"       # Green
export Color_Yellow_Intense="\033[0;93m"      # Yellow
export Color_Blue_Intense="\033[0;94m"        # Blue
export Color_Magenta_Intense="\033[0;95m"     # Magenta
export Color_Cyan_Intense="\033[0;96m"        # Cyan
export Color_White_Intense="\033[0;97m"       # White

# Bold
export Color_Black_Bold="\033[1;30m"       # Black
export Color_Red_Bold="\033[1;31m"         # Red
export Color_Green_Bold="\033[1;32m"       # Green
export Color_Yellow_Bold="\033[1;33m"      # Yellow
export Color_Blue_Bold="\033[1;34m"        # Blue
export Color_Magenta_Bold="\033[1;35m"     # Magenta
export Color_Cyan_Bold="\033[1;36m"        # Cyan
export Color_White_Bold="\033[1;37m"       # White

# Bold Intense
export Color_Black_Bold_Intense="\033[1;90m"      # Black
export Color_Red_Bold_Intense="\033[1;91m"        # Red
export Color_Green_Bold_Intense="\033[1;92m"      # Green
export Color_Yellow_Bold_Intense="\033[1;93m"     # Yellow
export Color_Blue_Bold_Intense="\033[1;94m"       # Blue
export Color_Magenta_Bold_Intense="\033[1;95m"    # Magenta
export Color_Cyan_Bold_Intense="\033[1;96m"       # Cyan
export Color_White_Bold_Intense="\033[1;97m"      # White

# Faint
export Color_Black_Faint="\033[2;30m"       # Black
export Color_Red_Faint="\033[2;31m"         # Red
export Color_Green_Faint="\033[2;32m"       # Green
export Color_Yellow_Faint="\033[2;33m"      # Yellow
export Color_Blue_Faint="\033[2;34m"        # Blue
export Color_Magenta_Faint="\033[2;35m"     # Magenta
export Color_Cyan_Faint="\033[2;36m"        # Cyan
export Color_White_Faint="\033[2;37m"       # White

# Faint Intense
export Color_Black_Faint_Intense="\033[2;90m"       # Black
export Color_Red_Faint_Intense="\033[2;91m"         # Red
export Color_Green_Faint_Intense="\033[2;92m"       # Green
export Color_Yellow_Faint_Intense="\033[2;93m"      # Yellow
export Color_Blue_Faint_Intense="\033[2;94m"        # Blue
export Color_Magenta_Faint_Intense="\033[2;95m"     # Magenta
export Color_Cyan_Faint_Intense="\033[2;96m"        # Cyan
export Color_White_Faint_Intense="\033[2;97m"       # White

# Italic
export Color_Black_Italic="\033[3;30m"       # Black
export Color_Red_Italic="\033[3;31m"         # Red
export Color_Green_Italic="\033[3;32m"       # Green
export Color_Yellow_Italic="\033[3;33m"      # Yellow
export Color_Blue_Italic="\033[3;34m"        # Blue
export Color_Magenta_Italic="\033[3;35m"     # Magenta
export Color_Cyan_Italic="\033[3;36m"        # Cyan
export Color_White_Italic="\033[3;37m"       # White

# Italic Intense
export Color_Black_Italic_Intense="\033[3;90m"       # Black
export Color_Red_Italic_Intense="\033[3;91m"         # Red
export Color_Green_Italic_Intense="\033[3;92m"       # Green
export Color_Yellow_Italic_Intense="\033[3;93m"      # Yellow
export Color_Blue_Italic_Intense="\033[3;94m"        # Blue
export Color_Magenta_Italic_Intense="\033[3;95m"     # Magenta
export Color_Cyan_Italic_Intense="\033[3;96m"        # Cyan
export Color_White_Italic_Intense="\033[3;97m"       # White

# # Underline
# export Color_Black_Underline="\033[4;30m"       # Black
# export Color_Red_Underline="\033[4;31m"         # Red
# export Color_Green_Underline="\033[4;32m"       # Green
# export Color_Yellow_Underline="\033[4;33m"      # Yellow
# export Color_Blue_Underline="\033[4;34m"        # Blue
# export Color_Magenta_Underline="\033[4;35m"     # Magenta
# export Color_Cyan_Underline="\033[4;36m"        # Cyan
# export Color_White_Underline="\033[4;37m"       # White

# # Underline Intense
# export Color_Black_Underline_Intense="\033[4;90m"       # Black
# export Color_Red_Underline_Intense="\033[4;91m"         # Red
# export Color_Green_Underline_Intense="\033[4;92m"       # Green
# export Color_Yellow_Underline_Intense="\033[4;93m"      # Yellow
# export Color_Blue_Underline_Intense="\033[4;94m"        # Blue
# export Color_Magenta_Underline_Intense="\033[4;95m"     # Magenta
# export Color_Cyan_Underline_Intense="\033[4;96m"        # Cyan
# export Color_White_Underline_Intense="\033[4;97m"       # White

# # Blinking
# export Color_Black_Blink="\033[5;30m"       # Black
# export Color_Red_Blink="\033[5;31m"         # Red
# export Color_Green_Blink="\033[5;32m"       # Green
# export Color_Yellow_Blink="\033[5;33m"      # Yellow
# export Color_Blue_Blink="\033[5;34m"        # Blue
# export Color_Magenta_Blink="\033[5;35m"     # Magenta
# export Color_Cyan_Blink="\033[5;36m"        # Cyan
# export Color_White_Blink="\033[5;37m"       # White

# # Blinking Intense
# export Color_Black_Blink_Intense="\033[5;90m"       # Black
# export Color_Red_Blink_Intense="\033[5;91m"         # Red
# export Color_Green_Blink_Intense="\033[5;92m"       # Green
# export Color_Yellow_Blink_Intense="\033[5;93m"      # Yellow
# export Color_Blue_Blink_Intense="\033[5;94m"        # Blue
# export Color_Magenta_Blink_Intense="\033[5;95m"     # Magenta
# export Color_Cyan_Blink_Intense="\033[5;96m"        # Cyan
# export Color_White_Blink_Intense="\033[5;97m"       # White

# # Reverse
# export Color_Black_Reverse="\033[7;30m"       # Black
# export Color_Red_Reverse="\033[7;31m"         # Red
# export Color_Green_Reverse="\033[7;32m"       # Green
# export Color_Yellow_Reverse="\033[7;33m"      # Yellow
# export Color_Blue_Reverse="\033[7;34m"        # Blue
# export Color_Magenta_Reverse="\033[7;35m"     # Magenta
# export Color_Cyan_Reverse="\033[7;36m"        # Cyan
# export Color_White_Reverse="\033[7;37m"       # White

# # Reverse Intense
# export Color_Black_Reverse_Intense="\033[7;90m"       # Black
# export Color_Red_Reverse_Intense="\033[7;91m"         # Red
# export Color_Green_Reverse_Intense="\033[7;92m"       # Green
# export Color_Yellow_Reverse_Intense="\033[7;93m"      # Yellow
# export Color_Blue_Reverse_Intense="\033[7;94m"        # Blue
# export Color_Magenta_Reverse_Intense="\033[7;95m"     # Magenta
# export Color_Cyan_Reverse_Intense="\033[7;96m"        # Cyan
# export Color_White_Reverse_Intense="\033[7;97m"       # White

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

# Background Intense
export Color_Black_zBackground_Intense="\033[0;100m"   # Black
export Color_Red_zBackground_Intense="\033[0;101m"     # Red
export Color_Green_zBackground_Intense="\033[0;102m"   # Green
export Color_Yellow_zBackground_Intense="\033[0;103m"  # Yellow
export Color_Blue_zBackground_Intense="\033[0;104m"    # Blue
export Color_Magenta_zBackground_Intense="\033[0;105m" # Magenta
export Color_Cyan_zBackground_Intense="\033[0;106m"    # Cyan
export Color_White_zBackground_Intense="\033[0;107m"   # White

# Used in prompt: Reset color, but faint and italic
zColor_Prompt="$Color_zOff$Color_zFaint$Color_zItalic"

# See also 256-colors, colordump, colors_and_formatting, & tput_colors
alias colorslist="set | egrep '^Color_\w*' | sort"
# Lists colors in their actual color, on one line; not ideal
alias colors="echo -e \`colorslist | sed 's/\(.*\)=\(.*\)/\2 \1/'\`"


### Bash readline settings -----------------------------------------
## note: bind used instead of sticking these in .inputrc
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


### Prompts --------------------------------------------------------
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
    local NCPU
    local SYSLOAD
    if [[ $OSTYPE == darwin* ]]; then
	NCPU=$(sysctl -n hw.ncpu)
	SYSLOAD=$(sysctl -n vm.loadavg | cut -f 2 -d ' ')
    else
	NCPU=$(nproc)
	# Anything without `uptime` (e.g. Toolforge k8s webservice) will always
	# appear as normal load
	SYSLOAD=$(uptime 2>/dev/null | cut -d ":" -f 4- | sed s/,//g | cut -f 2 -d " ")
    fi
    # Remove decimal, essentially treating it as a percentage (40 instead of
    # 0.40) since bash can't do math with floating points
    SYSLOAD=$(tr -d '.' <<< "$SYSLOAD")

    if [[ "${SYSLOAD}" -lt $((100*NCPU)) ]]; then
	echo -en "${zColor_Prompt}" # Normal load
    elif [[ "${SYSLOAD}" -lt $((200*NCPU)) ]]; then
	echo -en "${Color_Magenta_Intense}" # Small load
    elif [[ "${SYSLOAD}" -lt $((400*NCPU)) ]]; then
	echo -en "${Color_Red_Bold_Intense}" # Medium load
    else
	echo -en "${Color_Red_zBackground}${Color_Red_Bold_Intense}" # Large load
    fi
}

# Hostname when connected via SSH
function _cnx() {
    if [[ $SSH_TTY || $INSTANCEPROJECT || $KUBERNETES_PORT ]]; then
	echo -en "${Color_Blue_Intense}@${Color_Red}\h"
    fi
}

# Test user: id -un, logname, $LOGNAME, $USER: not necessarily the same!
function _uid() {
    if [[ $USER != "$USER_NAME" ]]; then # Only if not meeeee
	# Backup just in case there's no $USER
	local color="${Color_Red_zBackground}"

	# We actually *have* a user.  The toolforge kubernetes (webservice) is a
	# bit annoying: has no logname (but I gave it one!), has no $USER or
	# $USERNAME, but `id -un` and thus \u exist.
	if [[ -n "$USER" ]]; then
	    case $USER in
		root) color="${Color_Red}";; # User is root
		"$(logname)") color="${Color_Green}";; # User is normal (mostly)
		*) color="${Color_Red_zBackground_Intense}";; # User is not login user
	    esac
	fi

	# In theory, on the tf k8s webservice, would be nice to use the custom
	# $LOGNAME I have it rather than \u, but that's dumb.  Right?
	echo -en "${Color_zOff}$color\u"
    fi
}

# Return a color according to running/suspended jobs.
function _job_color() {
    if [ "$(jobs -s | wc -l)" -gt "0" ]; then
	echo -en "${Color_Red_Bold_Intense}"
    elif [ "$(jobs -r | wc -l)" -gt "0" ]; then
	echo -en "${Color_Yellow_Intense}"
    fi
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
    PS1="\[$Color_zOff\]"'$fill'"\n\[$Color_Cyan\]┌─"

    psuser="$(_uid)$(_cnx)"
    if [[ -n "$psuser" ]]; then
	PS1+="[$psuser\[$Color_Cyan\]]-"
    fi

    PS1+="[\[$(_load_color)\]\t $(date +'%a %d %b')\[$Color_Cyan\]]-[\[$zColor_Prompt\]\w\[$Color_Cyan\]]"

    gitprompt="$(gitprompt.sh)"
    if [[ -n "$gitprompt" ]]; then
	PS1+="-[$gitprompt\[$Color_Cyan\]]"
    fi

    # Not ideal, but check if emacs server is running
    if [[ -z "$(pgrep -f 'emacs --daemon')" ]]; then
	PS1+="-<!e!>"
    fi

    holiday_greeting="$(holiday_greeting.sh)"
    if [[ -n "$holiday_greeting" ]]; then
	PS1+="\n$holiday_greeting"
    fi

    PS1+="\n\[$Color_Cyan\]└─["

    if ((ERRORS > 0)); then
	PS1+="\[$Color_Red_Intense\]\$"
    else
	PS1+="\[$Color_Magenta\]\#"
    fi

    PS1+="\[$Color_Cyan\]]\[$(_job_color)\]->\[$Color_zOff\] "
    export PS1

    # create a $fill of all screen width minus the time string and a space:
    ((fillsize=COLUMNS))	# fullscreen
    # room for battery charge plus the color control codes, not if sshing
    if [[ ! $SSH_TTY && ! $INSTANCEPROJECT && ! $KUBERNETES_PORT && $(which battery) ]]; then
	# Takes care of the system type itself
	battery=$(battery -a 2>/dev/null | tr -d ' ')
	((fillsize=fillsize-${#battery}+12))
    fi

    if [[ $(which hr) ]]; then
	fill="$(COLUMNS=$fillsize hr -)${battery}"
    else
	fill=""
	while [ "$fillsize" -gt "0" ]
	do
	    fill="-${fill}"
	    ((fillsize=fillsize-1))
	done
	fill="${fill}${battery}"
    fi

    # Forcibly save history after each command
    history -a
}

export PS2="\[$Color_Cyan\]→\[$Color_zOff\] " # Secondary prompt, multiline commands
export PS3='#? '			      # Tertiary prompt, select menus
export PS4='+ '				      # Quaternary prompt, ???

PROMPT_COMMAND=prompt_command

# Trim directory (\w) to just three, bash >=4
export PROMPT_DIRTRIM=3

# Function to run upon exit of shell
function _exit() {
    echo -e "${Color_Red_Bold_Intense}Thanks for playing${Color_zOff}"
}
trap _exit EXIT


### Sourcin' -------------------------------------------------------
## Throw all potential sources into an array, check, then source
# Some other work done along the way
# Consider loading in .bash_profile, with GIT_PERS_DIR and GIT_EXTL_DIR??

# Some personal/private stuff, used in various places, both bash and lisp.
# USER_NAME, NAME, EMAIL, USER_BIRTHDAY
# LATITUDE, LONGITUDE, LOCATION
# WIKI_USERNAME, WIKI_EMAIL_ADDRESS, TOOLFORGE_USERNAME
# DIARY_DIR, TIL_DIR, HOME_SSID, TWITTER_USERNAME, GITHUB_USERNAME
# STOCK_TICKERS OPENAI_API_KEY
# BotPasswords for k8s
sources=("$HOME/.config/bash/priv-env.bash")
# Advanced bash completion https://github.com/scop/bash-completion
# Installed from either Macports or Homebrew, preferably the latter
# Macports, will source /opt/local/share/bash-completion/bash_completion
if [[ -n "$PORT_INSTALLED" ]]; then
    sources+=("/opt/local/etc/bash_completion")
fi
# Homebrew.  Confident glob probably fine given the -f -r checks below.
# bash-completion@2 covers folks in "${HOMEBREW_PREFIX}"/etc/bash_completion.d/
if [[ -n "$BREW_INSTALLED" ]]; then
    sources+=("${HOMEBREW_PREFIX}"/etc/profile.d/*)

    # fzf via homebrew doesn't install everything, instead there's an install
    # script that homebrew offers as a means by which to source files, including
    # auto-completion.  Let's just do them individually!
    if [[ -f $(command -v fzf) ]]; then
	# Bash auto-completion
	sources+=("${HOMEBREW_PREFIX}"/opt/fzf/shell/completion.bash)
	# Add fzf keybindings:
	# C-r (history search)
	# C-t (copy path)
	# M-c (cd to directory)
	sources+=("${HOMEBREW_PREFIX}"/opt/fzf/shell/key-bindings.bash)

	# Bash keybindings for git and fzf https://github.com/junegunn/fzf-git.sh
	# Might as well put here as long as we're sourcing fzf-related things,
	# and I suppose, yeah, don't want to source this if I don't have fzf!
	# Bindings (all begin with C-g):
	## C-g C-f: files, with diff
	## C-g C-b: branches, with log of commits
	## C-g C-t: tags, with commit message
	## C-g C-h: commits, with diff and commit message
	## C-g C-r: remotes, with url
	## C-g C-s: stashes, with commit message
	## C-g C-e: each-ref
	# Also can do C-_ to toggle preview options (technically C-/, but that's
	# not normal and is technically undo, like C-_)
	fzfgit_file="${GIT_EXTL_DIR}"/fzf-git.sh@junegunn/fzf-git.sh
	if [[ -f "$fzfgit_file" ]]; then
	    sources+=("${fzfgit_file}")
	fi
    fi

    # z, the awesome helper for moving around to popular directories
    z_file="${HOMEBREW_PREFIX}"/etc/profile.d/z.sh
    if [[ -f "$z_file" ]]; then
	sources+=("$z_file")

	# These need to be *before* z is sourced, but of course, z isn't sourced
	# now but later, when this whole array is dealt with.  Yay!
	export _Z_MAX_SCORE=26000	# Up from 9000, entries persist longer
	export _Z_OWNER="$USER"		# Maybe this should be $USER_NAME?
    fi

fi
# Perlbrew completion
if [[ -n "$PERLBREW_INSTALLED" ]]; then
    sources+=("${PERLBREW_ROOT}"/etc/perlbrew-completion.bash)
fi

# Supplement the above with some missing items, at this point just some
# mac-specific peccadilloes (defaults, eject)
sources+=(~/.completions.d/*)

# Source all the (readable) things (files)!
for file in "${sources[@]}"; do
    # shellcheck source=/dev/null
    [ -f "$file" ] && [ -r "$file" ] && source "$file";
done;
unset file;

#  alias reload="exec $SHELL -l"
alias reload='. ~/.bashrc'

# Half-assedly replicate ssh, etc. completion for whois
# Honestly, should just use fzf
if [[ -e ~/.ssh/known_hosts ]]; then
    complete -o default -W "$(sed 's/[, ].*//' < ~/.ssh/known_hosts | sort | uniq | grep -v '[0-9]')" whois
fi
# macOS networksetup completion
if [[ $OSTYPE == darwin* ]]; then
    complete -o default -W "$(networksetup -printcommands | grep -Ee "-.+?\b" -o | grep -v delete | grep -v rofile)" networksetup;
fi


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

if [[ -f $(command -v tree) ]]; then
    # Colorized, recursive ls-like tree
    function treed {
	tree -aC -I ".git" --dirsfirst "$@" | less -FRNX
    }
    alias tree='tree -Csh'
fi

# Preserve environment
alias sudo='sudo -E '

### Git stuff
# Should fix git signing when ssh'd, no issues locally?
GPG_TTY=$(tty)
export GPG_TTY
# Alias hub as git for github https://github.com/github/hub
if [[ -f $(command -v hub) ]] ; then
    alias git='hub'
fi
# Quick
function g {
    local ref
    ref=$(git rev-parse --is-inside-work-tree 2> /dev/null)
    if [[ $ref ]]; then
	if [[ $# -gt 0 ]]; then
	    git "$@"
	else
	    git lr5
	    git status --short --branch
	fi
    else
	case "$1" in
	    scan|'help'|h|browse|grab|config|cfg|version|notifications) git "$@";;
	    *) echo "Not a git repository or other fatal issue"
	esac
    fi
}
# Completion for g alias
if type __git_complete &> /dev/null; then
    __git_complete g __git_main
fi


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

# Open the manual page for the last command you executed.
function lastman {
    set -- $(fc -nl -1);
    while [ $# -gt 0 ] && [ "sudo" = "$1" ] || [ "-" = "${1:0:1}" ]; do
	shift;
    done;
    local cmd
    cmd="$(basename "$1")";
    man "$cmd" || help "$cmd";
}
alias lman='lastman'


# Use lesspipe.sh (look inside archives) just in case
# Exports $LESSOPEN
if [[ -f $(command -v lesspipe.sh) ]]; then
    eval "$(lesspipe.sh)"
fi

# Give man pages some color.  Can't use color names?
# I actually think this is better than using batman
export LESS_TERMCAP_mb=$'\033[0;37m' # section titles
export LESS_TERMCAP_md=$'\033[0;34m' # bold header
export LESS_TERMCAP_me=$'\033[0m'    # end bold
export LESS_TERMCAP_so=$'\033[0;31m' # begin standout-mode - info box
export LESS_TERMCAP_se=$'\033[0m'    # end standout-mode
export LESS_TERMCAP_ue=$'\033[0m'    # end underline
export LESS_TERMCAP_us=$'\033[4;35m' # begin underline

# more is less
alias more='less'

# bat is even more, and at this point, the muscle memory is ingrained
# https://github.com/sharkdp/bat
if [[ ! -f $(command -v bat) ]]; then
    alias bat='more'

    # http://linux.die.net/man/1/pygmentize
    # Colorized cat, but slow, so don't make it the default
    if [[ -f $(command -v pygmentize) ]]; then
	alias cot='pygmentize -O bg=light -g | more'
    fi
else
    # Just in case
    alias cot='bat '

    # The manpager above is fine (less -FXiRgMw) but for perldoc bat can at
    # least make an iota of effort.  It's far from perfect--pretty miserable
    # actually--but at least there's *some* delineation.  Using batman from
    # bat-extras does much the same thing, as it automatically colors things
    # appropriately, albeit with the same pitiable support for pod.  I bet I can
    # be smarter about all this, at least in terms of column width FIXME TODO
    export PERLDOC_PAGER="sh -c 'col -bx | bat -l man -p'"

    # Should probably be set in the config file, but this is fine enough.  This
    # is necessary because bat (intelligently(?)), as of 0.22 (See
    # <https://github.com/sharkdp/bat/pull/2197>), adjusts its default behavior
    # on macOS to be dependent on light/dark mode.  However, it's making the
    # assumption that there is a correspondence with light/dark mode in the
    # terminal, which doesn't happen automatically.  iTerm2's beta of 3.5
    # supposedly handles this, and there are some little hacks to do so in
    # Apple's terminal, but until I do one of those or decide it's not worth
    # it/desirable, setting a specific theme keeps things from adjusting.  I
    # kind of like how the Coldark themes look, although the defaults (Monokai
    # Extended (Light)) are fine and probably better for comments.  That would
    # of course imply that I'd need to specify a method for switching between
    # Coldark-Cold and Coldark-Dark depending on the status of light/dark mode
    # (See <https://github.com/sharkdp/bat/issues/1746>).  Sigh.  Maybe things
    # are different now with <https://github.com/sharkdp/bat/pull/2896>?  Should
    # investigate FIXME TODO
    # export BAT_THEME='Coldark-Cold'

    # Preserve default behavior for cat but use bat
    alias cat='bat --paging=never '

    # Let me view my configs how I want, dammit!
    export BAT_OPTS="--map-syntax .emacs:Lisp --map-syntax='.local-gitconfig*:Git Config'"
fi


## Sundry fzf options, mostly taken from the readme
## https://github.com/junegunn/fzf
if [[ -f $(command -v fzf) ]]; then
    # fd is so much faster than find https://github.com/sharkdp/fd
    if [[ -f $(command -v fd) ]]; then
	# Show hidden files, including those hidden by .gitignore, but *don't*
	# follow symlinked directories (do, however, include the links
	# themselves).  Exclude .git and node_modules folders.  Use colors!
	# They're butt-ugly though
	export FZF_DEFAULT_COMMAND='fd --type f --type l --strip-cwd-prefix --hidden --exclude .git --exclude node_modules --no-ignore --color=always'

	# Also use fd for fuzzy completion of paths and directories
	_fzf_compgen_path() {
	    fd --hidden --follow --exclude ".git" . "$1"
	}
	_fzf_compgen_dir() {
	    fd --type d --hidden --follow --exclude ".git" . "$1"
	}
    fi

    # Default to multi-select mode (TAB or Shift-TAB to mark multiple items), not
    # taking up the whole screen, and a sweet border.  Want preview, but it ruins
    # piping to fzf, and besides, with all the below, most of the time I've got
    # it.  ansi required for fd --color=always, and makes things slower.  Should
    # maybe reconsider...
    export FZF_DEFAULT_OPTS='-m --height 70% --border --ansi'

    # Use ~~ as the trigger sequence instead of the default **
    export FZF_COMPLETION_TRIGGER='~~'
    # Extra options for fuzzy completing via ~~, less sure about this one
    export FZF_COMPLETION_OPTS='--border --info=inline'

    # Expand fuzzy completion to more functions and aliases
    # usage: _fzf_setup_completion path|dir|var|alias|host COMMANDS...
    # Could consider expanding this to some of my custom completions?
    # TODO https://github.com/junegunn/fzf#custom-fuzzy-completion
    _fzf_setup_completion path g e bat icdiff prove
    _fzf_setup_completion dir tree

    # Some options for the bash bindings.  Some variables defined here to
    # keep preview window stuff aligned.  Plus, they're pretty sweet.
    # C-t to snag a file
    if [[ -f $(command -v bat) ]]; then
	FZF_PREVIEW_FILES="'bat --color=always --line-range :500 {}' --preview-window '~3'"
    else
	FZF_PREVIEW_FILES="'file {}' --preview-window up,1,border-horizontal"
    fi
    FZF_PREVIEW_FILES="--preview $FZF_PREVIEW_FILES --bind 'ctrl-/:change-preview-window(down|hidden|)'"
    export FZF_CTRL_T_OPTS="$FZF_PREVIEW_FILES"
    # M-c to cd, print tree structure in the preview window
    FZF_PREVIEW_DIR_LIMIT=200
    if [[ $(type -a tree 2>/dev/null) ]]; then
	FZF_PREVIEW_DIR="'tree -Csh {} | head -$FZF_PREVIEW_DIR_LIMIT'"
    else
	FZF_PREVIEW_DIR="'du -hca {} | head -$FZF_PREVIEW_DIR_LIMIT'"
    fi
    export FZF_ALT_C_OPTS="--preview $FZF_PREVIEW_DIR"
    # C-r to search history.  Dunno how valuable the preview is, but the C-y to
    # copy the command is nice
    export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window up:3:hidden:wrap --bind 'ctrl-/:toggle-preview' --bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort' --color header:italic --header 'Press CTRL-Y to copy command into clipboard'"

    # Advanced customization of fzf options via _fzf_comprun function
    # - The first argument to the function is the name of the command.
    # - You should make sure to pass the rest of the arguments to fzf.
    # Mostly for fancy shit with bat and tree
    _fzf_comprun() {
	local command=$1
	shift

	case "$command" in
	    cd)           fzf --preview "'$FZF_PREVIEW_DIR'" "$@" ;;
	    export|unset) fzf --preview "eval 'echo \$'{}"         "$@" ;;
	    ssh)          fzf --preview 'dig {}'                   "$@" ;;
	    *)            FZF_DEFAULT_OPTS="$FZF_DEFAULT_OPTS $FZF_PREVIEW_FILES" fzf "$@" ;;
	esac
    }


    # Kill processes, but only list the ones you can kill.  The if is in case
    # of, e.g., https://github.com/sindresorhus/fkill and its cli.  Via
    # https://github.com/junegunn/fzf/wiki/examples#processes
    if [[ ! -f $(command -v fkill) ]]; then
	fkill() {
	    local pid
	    if [ "$UID" != "0" ]; then
		pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
	    else
		pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
	    fi

	    if [ "x$pid" != "x" ]; then
		echo "$pid" | xargs kill -"${1:-9}"
	    fi
	}
    fi

    # Redefine z so that, if nothing is provided, use fzf to pick through the
    # list.  Via https://github.com/junegunn/fzf/wiki/examples#z
    if [[ $(type -t z) == "alias" ]]; then
	unalias z 2> /dev/null
	function z() {
	    if [ $# -gt 0 ]; then
		_z "$*"
		# This overwrought nonsense is just to get around the fact that
		# z -l exits as an error, and sometimes I just want to z -l
		zerr=$?
		if [ "$1" = "-l" ] || [ $zerr = 0 ]; then
		    return
		fi
	    fi
	    cd "$(_z -l 2>&1 | fzf --height 40% --nth 2.. --reverse --inline-info +s --tac --query "${*##-* }" | sed 's/^[0-9,.]* *//')" || return
	}
    fi
fi


# https://snarky.ca/why-you-should-use-python-m-pip/
if [[ -f $(command -v pip) ]]; then
    alias pip='python -m pip '

    # Only run pip if virtualenv activated
    # export PIP_REQUIRE_VIRTUALENV=true
fi


# Make perl -d automatically use NYTProf
export PERL5DB='use Devel::NYTProf'

alias provel='prove -l'
alias provelv='provel -v'
alias proveq='prove -Q'
alias provelq='prove -lQ'
# Easier Devel::Cover testing by setting the harness.
alias provecover='HARNESS_PERL_SWITCHES=-MDevel::Cover prove '
alias provecoverl='provecover -l'
alias provecoverlv='provecoverl -v'
alias provecoverq='provecover -Q'
alias provecoverlq='provecover -lQ'
# Often useful
alias proverelease='RELEASE_TESTING=1 prove '
alias provereleasel='proverelease -l'
alias provereleaselv='provereleasel -v'
alias provereleaseq='proverelease -Q'
alias provereleaselq='proverelease -lQ'
alias provecoverrelease='RELEASE_TESTING=1 provecover '
alias provecoverreleasel='provecoverrelease -l'
alias provecoverreleaselv='provecoverreleasel -v'
alias provecoverreleaseq='provecoverrelease -Q'
alias provecoverreleaselq='provecoverrelease -lQ'
# Why not?
alias covergen='cover -delete && provecoverlq && cover'
alias covergenrelease='cover -delete && provecoverreleaselq && cover'

# Access Perl::Critic documentation
if [[ -f $(command -v perlcritic) && -n "$PERLBREW_INSTALLED" ]]; then
    function explain_perlcritic() {
	perldoc Perl::Critic::Policy::"$1"
    }
    _explain_perlcritic() {
	local cur cword
	COMPREPLY=()
	# Part of bash-completion, this is instead of doing the classic
	# cur="${COMP_WORDS[COMP_CWORD]}", etc.  Shout out to
	# https://stackoverflow.com/a/12495480/2521092
	_get_comp_words_by_ref -n : cur cword

	# local list of Perl::Critic policies
	if [[ $cword -eq 1 ]]; then
	    mapfile -t COMPREPLY < <(compgen -W "$(find "${PERLBREW_MANPATH}/man3" -maxdepth 1 -path '*Perl::Critic::Policy::*'|sed 's/^.*man3\/Perl::Critic::Policy:://g' | sed 's/\.3$//g')" -- "$cur")
	    # https://stackoverflow.com/a/12495480/2521092
	    __ltrim_colon_completions "$cur"
	    return 0
	fi
    }
    complete -F _explain_perlcritic explain_perlcritic

    # Unnecessary with -n : and __ltrim, but cite anyway:
    # https://stackoverflow.com/a/2806133/2521092
    # COMP_WORDBREAKS=${COMP_WORDBREAKS//:}
fi

# perldoc shorthands
alias pd='perldoc '
# perlfunc
function pf() {
    if [ $# -eq 0 ]; then
	perldoc perlfunc
    else
	perldoc -f "$@"
    fi
}
# perlvar
function pv() {
    if [ $# -eq 0 ]; then
	perldoc perlvar
    else
	perldoc -v "$@"
    fi
}
# perlfaq
function pq() {
    if [ $# -eq 0 ]; then
	perldoc perlfaq
    else
	perldoc -q "$@"
    fi
}
alias perlcheat='man perlcheat'
if [[ -n "$PERLBREW_INSTALLED" ]]; then
    # I can never remember
    alias brewperl='perlbrew '

    # <https://metacpan.org/dist/perl/view/Porting/epigraphs.pod>
    perlepigraphs_file=${PERLBREW_ROOT}/build/${PERLBREW_PERL}/${PERLBREW_PERL}/Porting/epigraphs.pod
    if [[ -r "$perlepigraphs_file" ]]; then
	alias perlepigraphs='perldoc "$perlepigraphs_file"'
    fi

    # <https://metacpan.org/pod/perlsecret> Man in
    # $PERLBREW_MANPATH/man3/perlsecret.3
    if [[ $(perldoc -l perlsecret 2>/dev/null) ]]; then
	alias perlsecret='man perlsecret'
    fi
fi


# Tell tidy to use a config file if it's there
if [[ -f $(command -v tidy) ]]; then
    export HTML_TIDY=~/.tidyrc
fi

# Emacs stuff.  Makes which/type confused, but order is important
# emacs daemon/emacsclient
alias emd='\emacs --daemon '
# Get server status
# Something like <https://stackoverflow.com/a/10565139/2521092> is neat, i.e.
# `emacsclient -ca false -e '(delete-frame)'`, but this is much faster
function eserver-status() {
    if pgrep -qf "emacs(-[0-9]+\.[0-9]+)? --daemon"; then
	echo "Emacs server running"
    else
	echo "Emacs server not running"
    fi
}
# Improvable with https://emacs.stackexchange.com/a/5453/2051 ?
alias killemacs-server="\emacsclient -e '(kill-emacs)'"
alias kemacs-server='killemacs-server'
alias ke='kemacs-server'
alias killemacs-server-force="\emacsclient -e '(server-force-delete)'"
alias kemacs-server-force='killemacs-server-force'
alias kef='killemacs-server-force'
alias emacsclient='\emacsclient -tqu '
alias emacs='emacsclient '
# Recompile all elisp files, with proper warnings/output
function recompile_emacs() {
    \emacs -batch --eval '(byte-recompile-directory "~/.emacs.d/" 0)'
}
alias ii=recompile_emacs

alias magit='emacs --eval "(progn (magit-status) (delete-other-windows))"'
alias m='magit'
# Make customization easier
# Should these use $HOME? FIXME TODO
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
	ignore="$HOME/.global-gitignore"
    elif [[ -n $1 && $1 = "g" ]]; then
	ignore="$HOME/.global-gitignore"
    fi
    $VISUAL "$ignore"
}

# Javascript alias, can also just use node
alias jsc='/System/Library/Frameworks/JavaScriptCore.framework/Versions/Current/Helpers/jsc '
# Too much node repl
alias .exit='exit'

# Navigation -------------------------------------------------------
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~="cd ~"
alias -- --='cd -'		# go back a directory, -- means end of options
alias -- -='cd -'

# Shortcuts
alias bin="cd ~/bin"
alias dt="cd ~/dotfiles"
alias dr="cd ~/Dropbox"
alias de="cd ~/Desktop"
alias dg='cd $GIT_MAIN_DIR'
alias dge='cd $GIT_EXTL_DIR'
alias dgt='cd $GIT_EXTL_DIR/twinkle@azatoth'
alias dgm='cd $GIT_EXTL_DIR/twinkle@azatoth/modules'
alias dgj="dgm"
alias dgp='cd $GIT_PERS_DIR'
alias dp="dgp"
alias dgs='cd $GIT_PERS_DIR/mls'
alias dgw='cd $GIT_PERS_DIR/mediawiki'
alias dps='cd $GIT_PERS_DIR/sandbox'
alias dpk='cd $GIT_PERS_DIR/ksp'
alias dpw='cd $GIT_PERS_DIR/wiki'
alias dwc='cd $GIT_PERS_DIR/wiki/crathighlighter'
alias dws='cd $GIT_PERS_DIR/wiki/sysopIndex'
alias dwu='cd $GIT_PERS_DIR/wiki/userScripts'
alias drk="cd ~/Documents/R/kinship/"
alias edd="cd ~/.emacs.d/"
alias e='$VISUAL '
alias hig='history | grep -i'
alias cl='clear'
alias cls='clear'
alias count='wc -l'
alias linecount='count'


# Get equivalent of open on non-macOS systems
if [[ ! -f $(command -v open) ]]; then
    case "$OSTYPE" in
	cygwin*)
	    alias open="cmd /c start";;
	linux*)
	    alias open="xdg-open";;
	darwin*)
	    alias open='echo "How is there no open here?"'
    esac
fi

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
    cd "$1" || return
    ls
}

# Run perl without going there, in case it needs local files
# Not sure this works or is even useful
function peek() {
    (cd "$1" && perl "$2");
}

# Get current lockscreen quote
alias getquote='defaults read /Library/Preferences/com.apple.loginwindow.plist LoginwindowText'

# thefuck https://github.com/nvbn/thefuck
if [[ -f $(command -v thefuck) ]]; then
    eval "$(thefuck --alias)"
    eval "$(thefuck --alias fuck)"
fi

# Prompt before overwrite, be vocal about it
alias mv='mv -vi' # add -f to override, or \ before command
alias cp='cp -Rvi' # recursive if folder, the ending / makes a difference
# alias rm='rm -i' # Too annoying, perhaps?
alias rm='rm -v'
# Move the given file(s) to the Trash. Favor osxutils https://github.com/vasi/osxutils
# Often suggest aliasing rm to this instead, but it just sort of engenders
# unsafe behavior.
if [[ ! -f $(command -v trash) ]]; then
    # Can handle globs, directories, multiple files
    # https://www.stefanjudis.com/snippets/a-native-shell-alternative-to-the-trash-cli/
    function trash() {
	for var in "$@"; do mv "$var" "$HOME/.trash"; done
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
    if [ -z "$1" ]; then
	echo "Enter a directory name"
    elif [ -d "$1" ]; then
	echo "$1 already exists"
	cd "$1" || return
    else
	mkdir "$1" || return
	cd "$1" || return
    fi
}
alias mkd='mkcd'

# Make a directory and move a file into it
function mkmv() {
    if [[ $# -ne 2 ]]; then
	echo "Usage: mkmv <file> <directory>"
    elif [ -d "$2" ]; then
	echo "$1 already exists"
    else
	mkdir "$2"
	mv "$1" "$2"
    fi
}


# Rarely do I need the calendar for years in Roman times, and it's really
# annoying to have to enter this year when I just want last or next month or
# something, so let's make this more useful for day-to-day usage.
function cal {
    # Smartypants
    if [[ -z "$1" ]]; then
	command cal
    elif [[ "$1" == '-m' ]]; then
	command cal "$@"
    else
	month=$1
	year=$2
	if [[ $month -lt 13 ]]; then
	    if [[ -z $year ]]; then
		command cal -m "$month"
	    else
		command cal "$month" "$year"
	    fi
	else
	    command cal "$month"
	fi
    fi
}


# Human-readable values, and a total for du
alias df='df -h'
alias du='du -hc'
# Depth of 1, the minimum
alias dud='du -d 1'

# Better format for uptime
alias utime="uptime | egrep -o -e 'up [0-9]*.*[0-9]* user[s]?' | tr 'u' 'U'"

# Base 10 instead of base 2, up here to "beat" the alias below
function diskusage() {
    df -H "$(pwd)" | awk 'NR==2 { print "Used " $3 " of " $2 ", " $4 " (" $5 ") remaining" }'
}
# Displays user owned processes status.
function psu {
    ps -U "${1:-$USER}" -o 'pid,%cpu,%mem,command'
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
function last-modified {
    ls -t "$@" 2> /dev/null | head -n 5
}

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
if [[ -f $(command -v colordiff) ]]; then
    alias diff='colordiff';
fi
if [[ -f $(command -v cwdiff) ]]; then
    alias wcolordiff='cwdiff'
elif [[ -f $(command -v wdiff) ]]; then
    # What is this?  Need to explain FIXME TODO
    alias wcolordiff="wdiff -n -w $'\033[30;41m' -x $'\033[0m' -y $'\033[30;42m' -z $'\033[0m'"
fi
alias diffc='wcolordiff'
alias diffw='wcolordiff'
# But for real?  Just use dwdiff: git-like giff
# Use -3 option to only show changes
alias dwdiff='dwdiff -sc'

# Delta just better than diff.  The full config is in .gitconfig (well,
# .local-gitconfig), but the basic gist is that I like icdiff and want to
# emulate it's features, namely by the side-by-side style.  Setting this up as
# an alias as opposed to hardcoding $DELTA_FEATURES also means that it can adapt
# to light/dark-mode on macOS (we once again repeat ourselves).
if [[ -f $(command -v delta) ]]; then
    alias delta='DELTA_FEATURES="+side-styles $(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo dark-mode || echo light-mode)" delta '
fi

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
if [[ -f $(command -v rg) ]]; then
    # Includes --smart-case, --hidden, --glob=!.git
    export RIPGREP_CONFIG_PATH="$HOME/.ripgreprc"

    # -i largely meaningless with -S/--smart-case but whatever
    alias rgi='rg -i'
    alias rgc='rg -C 3'
    alias rgic='rgc -i'
    alias rgC='rg -C 10'
    alias rgiC='rgC -i'

    # If delta exists, additional function to pipe output there
    if [[ $(type -a delta 2>/dev/null) ]]; then
	function rg-delta() {
	    rg --json "$1" | delta
	}
    fi

    # Some paging for long results
    function rgp() {
	rg -p "$1" | more
    }

    # Search files for text, open that line in your editor
    # https://news.ycombinator.com/item?id=38473516
    if [[ -f $(command -v fzf) ]]; then
	function frg() {
	    if [ $# -gt 0 ]; then
		result=$(rg --ignore-case --color=always --line-number --no-heading "$@" |
			     fzf --ansi \
				 --color 'hl:-1:underline,hl+:-1:underline:reverse' \
				 --delimiter ':' \
				 --preview 'bat --color=always {1} --highlight-line {2}' \
				 --preview-window 'up,60%,border-bottom,+{2}+3/3,~3')
		file="${result%%:*}"
		linenumber=$(echo "${result}" | cut -d: -f2)
		if [ -n "$file" ]; then
		    $VISUAL +"${linenumber}" "$file"
		fi
	    else
		echo "You need to provide text with which to search"
	    fi
	}
    fi
fi

# Applications, probably only useful on macOS
if [[ $OSTYPE == darwin* ]]; then
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
    alias numbers='open -a numbers'
fi

# Macports
if [[ -n "$PORT_INSTALLED" ]]; then
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
	port search "$1" | grep @ | cut -f 1 -d ' ' | while read -r prt; do port info "$prt"; done;
    }

    # Macports changelog
    # https://trac.macports.org/browser/contrib/port-whatsnew/port-whatsnew
    alias whatsnew='port echo outdated | cut -f 1 -d" " | xargs -n 1 ~/bin/port-whatsnew.sh'
fi

# Homebrew/Cask
if [[ -n "$BREW_INSTALLED" ]]; then
    export HOMEBREW_DISPLAY_INSTALL_TIMES=1

    # Make homebrew verbose by default
    # export HOMEBREW_VERBOSE=1

    # Disable analytics
    # export HOMEBREW_NO_ANALYTICS=1
    # Or just GA
    # export HOMEBREW_NO_GOOGLE_ANALYTICS=1

    # Use bat for brew cat. As of v3.3.6, homebrew will attempt to automatically
    # install bat if this is set and it's not installed (by homebrew!)
    export HOMEBREW_BAT=1

    # New in 4.6.0, future default
    export HOMEBREW_DOWNLOAD_CONCURRENCY=auto

    # See also brew shellenv
    HOMEBREW_REPOSITORY=$(brew --repo)
    alias dgh='cd $HOMEBREW_REPOSITORY'
    alias dgf='cd $HOMEBREW_REPOSITORY/Library/Taps/homebrew/homebrew-core'
    alias dgc='cd $HOMEBREW_REPOSITORY/Library/Taps/homebrew/homebrew-cask'

    # Shorthand, useful?
    alias b='brew '
    # Toss in the completion
    complete -o bashdefault -o default -F _brew_with_services b

    alias ball='brew update ; brew outdated --greedy-auto-updates ; brew upgrade --greedy-auto-updates'
    alias bclean='brew cleanup ; brew cleanup -s'
    alias bsearch='brew search'
    alias bs='bsearch '
    alias bout='brew outdated --greedy-auto-updates'
    alias bupdate='brew update'
    alias bupgrade='brew upgrade --greedy-auto-updates'
    alias binfo='brew info'
    alias bhome='brew home'
    alias blist='brew list'
    alias bdoctor='brew doctor'

    # This could just be `brew deps/uses --installed --formula` and would be
    # *much* faster, but the blue coloring is nice?  Maybe stupid
    # Do --tree
    function homebrew-deps() {
	brew list --formula | while read -r formula; do echo -en "${Color_Blue_Bold}$formula ->${Color_zOff}"; brew deps --formula --installed "$formula" | awk '{printf(" %s ", $0)}'; echo ""; done
    }
    function homebrew-dependents() {
	brew list --formula | while read -r formula; do echo -en "${Color_Blue_Bold}$formula ->${Color_zOff}"; brew uses --formula --installed "$formula" | awk '{printf(" %s ", $0)}'; echo ""; done
    }

    # Homebrew-cask
    # Symlink in /Applications, remove quarantine
    export HOMEBREW_CASK_OPTS="--appdir=/Applications --qlplugindir=/Library/Quicklook --fontdir=/Library/Fonts --no-quarantine"

    # Quick homebrew livecheck, relies on HOMEBREW_LIVECHECK_WATCHLIST,
    # which defaults to ~/.brew_livecheck_watchlist
    function livecheck-formula() {
	if [ -f "$HOMEBREW_LIVECHECK_WATCHLIST" ] || [ -f "$HOME/.brew_livecheck_watchlist" ]; then
	    brew update && brew livecheck --formula --newer-only -q
	else
	    echo "No livecheck file found, it should probably be at $HOME/.brew_livecheck_watchlist"
	fi
    }
    function livecheck-cask() {
	if [ -f "$HOMEBREW_LIVECHECK_WATCHLIST" ] || [ -f "$HOME/.brew_livecheck_watchlist" ]; then
	    brew update && brew livecheck --cask --newer-only -q
	else
	    echo "No livecheck file found, it should probably be at $HOME/.brew_livecheck_watchlist"
	fi
    }

    # livecheck-cask, but open pull requests for any cask that needs it without
    # manual action.  brew bump is also slow and, I dunno, the whole repology
    # thing is a drag, so maybe this is better?
    # https://stackoverflow.com/a/1521498/2521092
    function livecheck-cask-bump {
	__livecheck-cask-bump "$HOME/.brew_livecheck_watchlist"
    }
    function livecheck-cask-bump-bulk {
	echo "Warning: This will probably take a while!"
	__livecheck-cask-bump "$HOME/.brew_livecheck_watchlist_casks_bulk"
    }
    # The helper function upon which the above rely
    function __livecheck-cask-bump {
	local watchlist
	watchlist="$1"
	if [ -z "$1" ] || [ ! -f "$watchlist" ]; then
	    echo "No livecheck file found, it should probably be at $watchlist"
	else
	    export HOMEBREW_NO_INSTALL_FROM_API=1 # Just in case
	    brew update
	    while read -r cask; do
		# Stop once we're done with the casks
		if [[ -z "$cask" ]]; then
		    break
		fi

		mapfile -t results < <(brew livecheck --quiet --newer-only "$cask" --json|jq .[].version.outdated,.[].version.latest,.[].version.current)
		if [[ "${results[0]}" = 'true' ]]; then
		    version="${results[1]//\"/}"
		    older="${results[2]//\"/}"
		    cmd="brew bump-cask-pr --no-browse $cask --version $version"
		    echo "$cask outdated, $older to $version: $cmd"
		    $cmd
		    echo
		fi
	    done <"$watchlist"
	fi
    }
fi

# bundle exec alias
alias bex='bundle exec'
alias bexj='be jekyll serve --watch'
alias jes='jekyll serve --watch'

# Quickly open and make a new file executable; emacs will add headers and move
# point to the appropriate location if there's a well-defined skeleton
function newscript() {
    if [ $# -eq 0 ]; then
	echo "No arguments provided";
    else
	if [ -a "$1" ]; then
	    echo "$1 already exists, opening";
	else
	    touch "$1"
	    chmod 755 "$1"
	fi
	$VISUAL "$1"
    fi
}

alias d="diary "
# Completion for diary
_diary_completion() {
    local cur prev opts
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    case "${prev}" in
	diary|d)
	    opts="new note notes last show showlast rand random search ss log list onthis memory remove rm delete stats"
	    mapfile -t COMPREPLY < <(compgen -W "${opts}" -- "${cur}")
	    return 0
	    ;;
	show)
	    opts=$(find "$DIARY_DIR" -maxdepth 1 -type f -name "*.md" ! -name "notes.md" -exec basename {} + 2>/dev/null)
	    mapfile -t COMPREPLY < <(compgen -W "${opts}" -- "${cur}")
	    return 0
	    ;;
	rand|random|list)
	    files=$(find "$DIARY_DIR" -maxdepth 1 -type f -name "*.md" ! -name "notes.md" -exec basename {} + 2>/dev/null)
	    # YYYY-MM
	    yopts=$(echo "$files" | cut -d'-' -f1-2 | sort -u)
	    # YYYY
	    mopts=$(echo "$files" | cut -d'-' -f1 | sort -u)
	    mapfile -t COMPREPLY < <(compgen -W "${yopts} ${mopts}" -- "${cur}")
	    return 0
	    ;;
	*)
	    ;;
    esac
}
complete -F _diary_completion diary d




alias keys="more ~/.ssh/id_rsa.pub | pbcopy | echo '=> Public key copied to pasteboard.'"

# Quickly open MetaCPAN.  Probably pointless, but I find I'm reaching for the
# terminal and not a launcher or something more correct
function metacpan() {
    if [ -n "$1" ]; then
	open "https://metacpan.org/pod/${1}"
    else
	open "https://metacpan.org/"
    fi

}

# Start an HTTP server from a directory, optionally specifying the port
function server() {
    local port="${1:-8000}"
    sleep 1 && open "http://localhost:${port}/" &
    python3 -m http.server "$port"
}

# Functions to start/stop mysql server (installed via dmg, not macports/homebrew)
# Should it be via macports/homebrew? FIXME TODO
# Names are awful for completion
function mysqlstart() {
    unset TMPDIR	 # Not sure why but this is apparently quite necessary
    if [[ -n $(ls /usr/local/mysql/data/*.pid 2>/dev/null) ]]; then
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
    if [[ -z $(ls /usr/local/mysql/data/*.pid 2>/dev/null) ]]; then
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
    if [[ -n $(ls /usr/local/mysql/data/*.pid 2>/dev/null) ]]; then
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
    if [ "$(tty -s)" ]; then
	basefile=$(basename "$1" | sed -e 's/[^a-zA-Z0-9._-]/-/g')
	tmp=$(curl --progress-bar --upload-file "$1" "https://transfer.sh/$basefile")
    else
	tmp=$(curl --progress-bar --upload-file "$1" "https://transfer.sh/$1")
    fi
    echo -n "$tmp" | pbcopy
    echo "$tmp"
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
# https://github.com/illusori/bash-itunes
if [[ -f $(command -v itunes) ]]; then
    alias i='itunes '
    alias next='itunes next'
    alias previous='itunes prev'
    alias play='itunes play'
    alias pause='itunes pause'
fi

# Volume control, potentially using osxutils
# https://github.com/specious/osxutils
if [[ ! -f $(command -v setvolume) ]]; then
    function setvolume() {
	if [ ! "$1" ]; then
	    echo "setvolume <0-100>"
	elif [ "$1" -lt 0 ] || [ "$1" -gt 100 ]; then
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
function profileme {
    history | awk '{print $4}' | awk 'BEGIN{FS="|"}{print $1}' | sort | uniq -c | sort -n | tail -n 20 | sort -nr
}

# Better which.  Lack of space important!
alias which='type -a 2>/dev/null'

# Find a function definition, via https://superuser.com/a/229038/240421
# Would be nice to autocomplete a la which/type
function wherefunction() (
    shopt -s extdebug; declare -F "$1";
)

# Pretty-print PATH
alias path='echo -e ${PATH//:/\\n}'
alias pretty-manpath="manpath | tr ':' '\n'"

# Matrix-esque screensaver-esque; man tr for different options
alias matrix='tr -c "[:print:]" " " < /dev/urandom | dd cbs=$COLUMNS conv=lcase,unblock | GREP_COLOR="1;32" grep --color "[^ ]"'
# Star Wars
function starwars() {
    echo "Loading Star Wars..."
    echo "Use C-] to exit"
    sleep 1
    telnet towel.blinkenlights.nl
}
# Lightcycle https://github.com/zachlatta/sshtron
function lightcycle() {
    echo "Loading Tron Lightcycle..."
    echo "Use WASD to play, ctrl-C to quit"
    sleep 1
    ssh sshtron.zachlatta.com
}
alias tron='lightcycle'
# Hackers script https://twitter.com/paultag/status/644160355409752065
# 2>&1 | less -S for chunks
alias zerocool='nc z.ero.cool 1337'
# Rebels in the Sky, space pirates playing basketball game
# <https://github.com/ricott1/rebels-in-the-sky> and
# <https://rebels.frittura.org>
function rebels-in-the-sky() {
	echo "Loading Rebels in the Sky..."
	echo "Run `mpv https://radio.frittura.org/rebels.ogg` for music"
	echo "ESC to quit"
	sleep 1
	ssh rebels.frittura.org -p 3788
}


# Lock the screen (when going AFK)
# Busted in monterey FIXME TODO
alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"
# Better as just starting screensaver
alias screensaver='open -a ScreenSaverEngine'
# Originally from @cowboy - https://github.com/cowboy/dotfiles/commit/28a3fd898f93c602080e3c3112b0f04854b66f22
# Modified for modern macOS
function lock-screen() {
    sleep 0.5

    if "$(ioreg -c AppleSmartBattery | grep -q '"ExternalConnected" = Yes')"; then
	# Plugged in: start screensaver
	/System/Library/CoreServices/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine
    else
	# Battery power: blank screen
	pmset displaysleepnow
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


# Combine all IP shit to give local and external, only if appropriate
# ######### ;;;;;;;;;; FIXME TODO
# Get my local IP
# ip
#alias ip="ifconfig | grep -E '(192|10)'"
#alias ip="ifconfig | grep -E '192'"
#alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
#alias ip="ifconfig -a | grep -o 'inet6\? \(\([0-9]\+\.[0-9]\+\.[0-9]\+\.[0-9]\+\)\|[a-fA-F0-9:]\+\)' | sed -e 's/inet6* //'"
#alias ip='curl -s icanhazip.com'
# function myip() {
#     MY_IP=$(ifconfig en1 | awk '/inet/ { print $2 } ' | sed -e s/addr://)
#     echo "${MY_IP:-"Not connected"}"
# }
# curl ifconfig.io
function ip() {
    local iplist
    iplist=$(ifconfig -a | perl -nle'/inet (?:addr:)?(\d+\.\d+\.\d+\.\d+)/ && print $1')

    if [ "$1" ]; then
	if "$(echo "$iplist" | grep -wq "$1")"; then
	    echo "$1"
	fi
    else
	echo "$iplist"
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

if [[ $OSTYPE == darwin* ]]; then
    # Should include fallback for no network, etc. FIXME TODO
    function ssid() {
	local ssid
	# <https://stackoverflow.com/a/79123269/2521092>
	# Could probably just default to en0 and skip the `networksetup` call,
	# but why not be more portable?

	# Broken in 15.6 (from 15.5) for no fucking reason? FIXME TODO  Can
	# still use `system_profiler SPAirPortDataType`, which is slow, and
	# various other things, but come on!
	ssid=$(ipconfig getsummary "$(networksetup -listallhardwareports | awk '/Hardware Port: Wi-Fi/{getline; print $2}')" | awk -F ' SSID : ' '/ SSID : / {print $2}')

	if [ "$1" ]; then
	    if echo "$ssid" | grep -qw "$1"; then
		echo :
	    fi
	else
	    echo "$ssid"
	fi
    }
    # Should probably export more/all of these... FIXME TODO
    export -f ssid

    # Toggle wifi status, via <http://apple.stackexchange.com/a/36897/53735>
    # Could try and handle determining the interface, but meh
    function toggle-wifi {
	local interface=$1
	if [[ -z $interface ]]; then
	    interface=en0
	fi
	[[ "$(networksetup -getairportpower "$interface")" == *On ]] && v=off || v=on
	networksetup -setairportpower "$interface" "$v"
    }

    # Change mac address https://jezenthomas.com/free-internet-on-trains/
    function remac {
	sudo /System/Library/PrivateFrameworks/Apple80211.framework/Resources/airport -z
	sudo ifconfig en0 ether "$(openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//')"
	sudo networksetup -detectnewhardware
	ifconfig en0 | grep ether
    }
fi

# View HTTP traffic
alias sniff="sudo ngrep -d 'en1' -t '^(GET|POST) ' 'tcp and port 80'"
alias httpdump='sudo tcpdump -i en1 -n -s 0 -w - | grep -a -o -E \"Host\: .*|GET \/.*\"'
# Which processes are listening on ports
function eaves {
    lsof -iTCP -sTCP:LISTEN -P "$@"
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
    sort "$1" | uniq -c | sort -nr
}
# quicksort in three lines from http://git.io/UzwyWQ
qsort() {
    local L=""; local G=""; [ $# -eq 1 ] && echo "$1" && return;
    P=$1; shift; for i in "$@"; do [ "$i" -lt "$P" ] && L="$L $i" || G="$G $i"; done
    [ -z "$L" ] || L=$(qsort "$L"); [ -z "$G" ] || G=$(qsort "$G"); echo "$L $P $G"
}
# Maybe maybe maybe I'll use comm if I do something with it
# <https://leancrew.com/all-this/2021/08/checking-it-twice/>
compare-lists() {
    if [[ ! "$1" || ! "$2" ]]; then
	echo "Usage: compare-lists <list1> <list2>"
    else
	comm -3 <(sort "$1") <(sort -u "$2")
    fi
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
if [[ -f $(command -v playball) ]]; then
    alias watch-mlb='playball';
fi


# macOS doesn't have some by default, so alias if not available
command -v md5sum > /dev/null || alias md5sum="md5"
command -v sha1sum > /dev/null || alias sha1sum="shasum"
command -v sha256sum > /dev/null || alias sha256sum="shasum -a 256"
command -v sha512sum > /dev/null || alias sha512sum="shasum -a 512"
if [[ -f $(command -v sha3sum) ]]; then
    for sum in 256 512; do
	command="sha3-$sum""sum"
	command -v "$command" > /dev/null || alias "$command"="sha3sum -a $sum"
    done
fi


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
function backup-file() {
    local filenames=("$@")

    for i in "${filenames[@]}"
    do
	cp "$i" "$i.bak"
    done
}
# Backup file with timestamp
function backup-file-with-timestamp() {
    local filenames=("$@")
    local filetime
    filetime=$(date +%Y%m%d_%H%M%S)

    for i in "${filenames[@]}"
    do
	cp "${i}" "${i}_${filetime}"
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
    if [ ! "$1" ]; then
	echo "Usage: mergepdf -o output.pdf input{1,2,3}.pdf";
    else
	/System/Library/Automator/Combine\ PDF\ Pages.action/Contents/Resources/join.py "$@";
    fi
}

# Can never remember
alias htmltopdf='wkhtmltopdf'

# Change working directory to the top-most Finder window location
function cdfinder() {
    cd "$(osascript -e 'tell app "Finder" to POSIX path of (insertion location as alias)')" || return
}
alias cdf='cdfinder'


## Couple of functions jacked from bash-it, lightly tweaked
## https://github.com/revans/bash-it
# Pick random line from a file
pickfrom () {
    local file=$1
    [ -z "$file" ] && reference "${FUNCNAME[0]}" && return
    length=$(wc -l < "$file")
    n=$((RANDOM * length / 32768 + 1))
    head -n "$n" "$file" | tail -1
}
# Generate random password from dictionary words
# Passed number gives length
pass () {
    local i pass length=${1:-4}
    readarray -t pass < <(for i in $(eval echo "{1..$length}"); do pickfrom /usr/share/dict/words; done)
    echo "With spaces (easier to memorize): ${pass[*]}"
    echo "Without (use this as the pass): $(echo "${pass[*]}" | tr -d ' ')"
}
# Same, but from /dev/urandom
randpass () {
    local length=${1:-8}
    alpha=$(env LC_CTYPE=C tr -cd '[:alpha:]' < /dev/urandom | head -c "$length")
    echo "$alpha"
    alnum=$(env LC_CTYPE=C tr -cd '[:alnum:]' < /dev/urandom | head -c "$length")
    echo "$alnum"
    graph=$(env LC_CTYPE=C tr -cd '[:graph:]' < /dev/urandom | head -c "$length")
    echo "$graph"
}
#############

# Fill gap between head and tail
body () {
    if [ -z "$1" ]; then
	echo "You must provide a number"
    elif [ "$1" == 1 ]; then
	echo "Must be greater than 1"
    else
	local line=$1
	local file=$2
	head -n "$line" "$file" | tail -n 1
    fi
}

# Quick view of the market
function marketupdate() {
    local tickers=(".SPX" ".DJI" ".IXIC")
    # Add any personal tickers
    tickers+=("${STOCK_TICKERS[@]}")

    ticker "${tickers[@]}"
}
alias mu='marketupdate'
alias stockmarket='ticker'
alias inflation='perl $GIT_PERS_DIR/sandbox/inflation.pl'


# Easy access to toolforge, but only if not on toolforge or k8s; if we're on the
# toolforge (but not k8s) then easy access to toolforge commands.  Checking the
# kubernetes webservice relies on the manual adding of $LOGNAME via toolforge
# envvars, since id -un matches the toolforge bot account.
if [[ "$LOGNAME" != "tools.amorybot.k8s" ]]; then
    if [[ ! "$INSTANCEPROJECT" ]]; then
	# shellcheck disable=SC2262 # Doesn't like the function tf below
	alias toolforge='ssh -i ~/.ssh/id_rsa_toolforge $TOOLFORGE_USERNAME@login.toolforge.org'
	# This won't get confusing at all!
	alias tf='toolforge '
    elif [[ "$LOGNAME" == "tools.amorybot" ]]; then
	# If we're the toolforge **bot account** (but not the kubernetes
	# webservice), alias some things for ease of use!  Okay, function, to
	# make the completion work better.  Oh, yeah, completion too!
	function tf() {
	    toolforge "$@"
	}
	complete -F _toolforge tf
	function tfj {
	    toolforge-jobs "$@"
	}
	# Only exists once we've tried to complete on `toolforge jobs`
	complete -F _toolforge_jobs tfj
	alias tfjl='tfj list '
	function tfe {
	    toolforge-envvars "$@"
	}
	alias tfjq='tfj quota '
	# No completion for toolforge-envvars.  Could rewrite toolforge-jobs
	# completion, currently in /usr/share/bash-completion/completions/.  Far
	# from perfect, doesn't like pieces and won't actually complete FIXME
	_toolforge_envvars_completion() {
	    mapfile -t COMPREPLY < <( compgen -W 'create delete list quota show' -- "$cur" )
	    return 0
	}
	complete -F _toolforge_envvars_completion toolforge-envvars
	complete -F _toolforge_envvars_completion tfe
	alias tfel='tfe list'
    else
	# Alias for toolforge non-bot-account
	alias ba='become amorybot'
    fi
fi


# Get the weather
function weather() {
    local where
    if [ "$1" ]; then
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


# Open AI/ChatGPT stuff from https://kadekillary.work/posts/1000x-eng/
if [[ -n "$OPENAI_API_KEY" && -f $(command -v jq) ]]; then
    function gpt() {
	local prompt=$*
	# Seems to help
	prompt=$(echo "$prompt"|tr -d '\n')

	# JSON in bash... sucks?
	curl https://api.openai.com/v1/chat/completions -s \
	     -H "Content-Type: application/json" \
	     -H "Authorization: Bearer $OPENAI_API_KEY" \
	     -d "{\"model\": \"gpt-3.5-turbo\", \"messages\": [{\"role\": \"user\", \"content\": \"$prompt\"}], \"temperature\": 0.7 }" \
	    | jq -r '.choices[0].message.content'
    }
    function gpt-data {
	local prompt=$1
	local prompt_data=$2
	prompt_data=$(cat "$prompt_data")

	gpt "$prompt: $(echo "$prompt_data"|tr -d '\n')"
    }
    function gpt-image {
	local prompt=$*

	url=$(curl https://api.openai.com/v1/images/generations -s \
		   -H "Content-Type: application/json" \
		   -H "Authorization: Bearer $OPENAI_API_KEY" \
		   -d "{\"prompt\": \"$prompt\", \"n\": 1, \"size\": \"1024x1024\" }" \
		  | jq -r '.data[0].url')
	curl -s "$url" -o "$prompt".png
	open "$prompt.png"
    }
fi


# From https://gist.github.com/komasaru/9635884
# Busted, replace with https://www.aviationweather.gov/metar FIXME TODO
function metar() {
    if [[ ! "$1" =~ [0-9A-Z]{4} ]]; then
	echo "Please enter an appropriate METAR code"
	echo "Suggestions: KEDU KSMF KSAC KNYC KBOS KSFO"
	return
    fi
    # Convert to all caps
    local code
    code=$(echo -n "$1" | tr '[:lower:]' '[:upper:]')

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

function dadjoke {
    curl https://icanhazdadjoke.com/
    echo
}

# Print the given text in the center of the screen.
function center {
    local width
    width=$(tput cols);
    local str="$*";
    local len=${#str};
    [ "$len" -ge "$width" ] && echo "$str" && return;
    for ((i = 0; i < $(((width - len) / 2)); i++)); do
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
function escape() {
    printf "\\\x%s" "$(printf "%s" "$@" | xxd -p -c1 -u)"
    # print a newline unless we're piping the output to another program
    if [ -t 1 ]; then
	echo # newline
    fi
}

# Get a character's Unicode code point
function codepoint() {
    perl -e "use utf8; print sprintf('U+%04X', ord(\"$*\"))"
    # print a newline unless we're piping the output to another program
    if [ -t 1 ]; then
	echo # newline
    fi
}

# Create a data URL from a file
function dataurl() {
    local mimeType
    mimeType=$(file -b --mime-type "$1");
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

    local tmp
    tmp=$(echo -e "GET / HTTP/1.0\nEOT" \
	      | openssl s_client -connect "${domain}:443" 2>&1);

    if [[ "${tmp}" = *"-----BEGIN CERTIFICATE-----"* ]]; then
	local certText
	certText=$(echo "${tmp}" \
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
    tail -f "$1" | while read -r line; do printf "$(date '+%F %T')\t%s\n" "$line"; done;
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
# Five random indieweb links, via <https://news.ycombinator.com/item?id=43148408>
function indieweb {
    for _ in {1..5}; do
	curl -sSw '%header{location}\n' https://indieblog.page/random | sed -e s/.utm_.*$//
      done
}

# Test how fast the machine is, 32GB
function writetest() {
    dd if=/dev/zero of=/dev/null bs=1048576 count=32768;
}

# Find empty directories
function emptydirs {
    dir=. && [ -n "$1" ] && dir="$1"
    find "$dir" -type d -empty -not -path '*/\.git/*'
}
# And delete them
function deleteemptydirs {
    dir=. && [ -n "$1" ] && dir="$1"
    find "$dir" -type d -empty -not -path '*/\.git/*' -delete
}

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
    if [[ -n "$*" ]]; then
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
function repeat() {
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


# Just easier and better
# https://github.com/yt-dlp/yt-dlp
if [[ -f $(command -v yt-dlp) ]]; then
    alias youtube-dl='yt-dlp '
fi
######## Scripts originally by @exogen
# type since it might be an alias
if type -a &>/dev/null youtube-dl; then
    function aac {
	# Get best audio, convert it to AAC, and save it to the current directory.
	yt-dlp --default-search=ytsearch: \
		   --restrict-filenames \
		   --format=bestaudio \
		   --extract-audio \
		   --audio-format=aac \
		   --audio-quality=0 "$*"
    }
    function mp3 {
	# Get best audio, convert it to MP3, and save it to the current directory.
	yt-dlp --default-search=ytsearch: \
		   --restrict-filenames \
		   --format=bestaudio \
		   --extract-audio \
		   --audio-format=mp3 \
		   --audio-quality=0 "$*"
    }
    # Probably better to just do mpv directly?  It can handle it, but maybe just
    # audio like above?  Also skip-dash-manifest is deprecated...
    function listen-youtube {
	# Skip DASH manifest for speed purposes. This might actually disable being
	# able to specify things like 'bestaudio' as the requested format, but try
	# anyway. Use "$*" so that quoting the requested song isn't necessary.
	yt-dlp --default-search=ytsearch: \
		   --youtube-skip-dash-manifest \
		   --output="${TMPDIR:-/tmp}/%(title)s-%(id)s.%(ext)s" \
		   --restrict-filenames \
		   --format=bestaudio \
		   --exec=mpv "$*"
    }
fi
########

# Generate family tree without polutting current directory
function family() {
    Rscript ~/Documents/R/kinship/family_tree.R
    rm Rplots.pdf
}

# Using Rscript allows more complex constructions, wrap () in quotes
# http://www.compbiome.com/2010/06/r-command-line-calculator-using-rscript.html
alias calc='Rscript -e "eval( parse( text=commandArgs( TRUE ) ) )"'
function =() {
    calc "$@"
}

# Basic summary statistics.  md/msdir was better
function summary-stats {
    Rscript -e 'summary (as.numeric (readLines ("stdin")))' "$@"
}

# Calculate factors
# https://twitter.com/climagic/status/550355281415503872
function factors {
    local num=$1;
    seq $((num/2)) | awk '"'"$num"'"%$0==0'
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
    echo Extracting "$1" ...
    if [ -f "$1" ] ; then
	case "$1" in
	    *.tar.bz2)tar xjf "$1";;
	    *.tar.gz)tar xzf "$1";;
	    *.tar.Z)tar xzf "$1";;
	    *.bz2)bunzip2 "$1";;
	    *.rar)unrar x "$1";;
	    *.gz) gunzip "$1";;
	    *.jar)unzip "$1";;
	    *.tar)tar xf "$1";;
	    *.tbz2)tar xjf "$1";;
	    *.tgz)tar xzf "$1";;
	    *.zip)unzip "$1";;
	    *.Z)uncompress "$1";;
	    *.7z)7z x "$1";;
	    *)echo "'$1' cannot be extracted via extract()" ;;
	esac
    else
	echo "'$1' is not a valid file"
    fi
}

# Create a .tar.gz archive, using `zopfli`, `pigz` or `gzip` for compression
function targz() {
    local tmpFile
    tmpFile="${*%/}.tar";
    tar -cvf "${tmpFile}" --exclude=".DS_Store" "${@}" || return 1;

    local size
    size=$(
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
    local origsize
    origsize=$(wc -c < "$1")
    local gzipsize
    gzipsize=$(gzip -c "$1" | wc -c)
    # Percent of total
    local ratio
    ratio=$(echo "$gzipsize * 100/ $origsize" | bc -l)
    # Percent reduction
    #    local ratio=$(echo "100 - ($gzipsize * 100/ $origsize)" | bc -l)
    printf "orig: %d bytes\n" "$origsize"
    # printf "gzip: %d bytes (%2.2f%%)\n" "$gzipsize" "$ratio"
    printf "gzip: %d bytes, %2.2f%% of original\n" "$gzipsize" "$ratio"
}


# Copy OCR text from an image using tesseract
# from https://www.stefanjudis.com/snippets/how-to-extract-text-from-an-image-via-the-cli/
# via https://til.simonwillison.net/tesseract/tesseract-cli
function extract-text-from-image() {
  if [ $# -eq 0 ]; then
    echo "Please specify the file you want to scan.";
    echo "  -> extract-text-from-image /some/path/image.png";
    return 1;
  fi

  TARGET_DIR=$(dirname "$1");
  FILENAME=$(basename -- "$1");
  FILENAME_WITHOUT_EXTENSION="${FILENAME%.*}";

  tesseract "$1" "$TARGET_DIR/$FILENAME_WITHOUT_EXTENSION" -l eng txt;
  pbcopy < "$TARGET_DIR/$FILENAME_WITHOUT_EXTENSION.txt";
  rm "$TARGET_DIR/$FILENAME_WITHOUT_EXTENSION.txt";
  echo "Text copied to clipboard!";
}


#### Dictionary stuff
# wordnet
dictionary () { curl dict://dict.org/d:"${1}":wn; }
alias define='dictionary'
alias dict='dictionary'
# Moby thesaurus
thesaurus () { curl dict://dict.org/d:"${1}":moby-thesaurus; }
alias synonym='thesaurus'
# vera acronyms
acronym () { curl dict://dict.org/d:"${1}":vera; }
# elements lookup
elements () { curl dict://dict.org/d:"${1}":elements; }
# jargon files
jargon () { curl dict://dict.org/d:"${1}":jargon; }
# foldoc, free-online dictionary of computing
foldoc () { curl dict://dict.org/d:"${1}":foldoc; }
# CIA World Factbook 2002
cia-world-factbook () { curl dict://dict.org/d:"${1}":world02; }
# Ambrose Bierce's classic Devil's Dictionary
devils-dictionary () { curl dict://dict.org/d:"${1}":devil; }
# urbandictionary
urban() { word="$*"; curl -s https://api.urbandictionary.com/v0/define?term="${word// /%20}" | jq -reM .list[0].definition; }
