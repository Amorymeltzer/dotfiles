## Some helpful environment vars and configs
# Worth noting that export doesn't take in aliases
export EDITOR='emacsclient -tqu '
# Used by emacsclient in case no daemon is running, such as with sudo
# Ideally, this would be empty, so emacsclient would start the server as a
# daemon, but that means those specific cases wouldn't fly
export ALTERNATE_EDITOR='emacs '

# Make CPAN always select the default option
export PERL_MM_USE_DEFAULT=1
# Use https with cpanm:
# http://blogs.perl.org/users/neilb/2021/11/addressing-cpan-vulnerabilities-related-to-checksums.html
export PERLCPANMOPT="--verify --from https://www.cpan.org"

# Used in .bashrc as well
# Generic default
export GIT_MAIN_DIR="$HOME"
export GIT_PERS_DIR="$GIT_MAIN_DIR"
export GIT_EXTL_DIR="$GIT_MAIN_DIR"

# Path ------------------------------------------------------------
# Hold onto original path, defined first from /etc/profile, which on macOS uses
# /usr/libexec/path_helper to read /etc/paths and then add items from
# /etc/paths.d/.  In this way, we ensure that the additional items below show up
# in our desired order, or closer, at least, without unsightly duplications or
# going through the extra effort of bash variable expansion to insert in-place.
# The main disadvantage here is that items in /usr/local/bin are not as high as
# might be desired, given that it is used by Homebrew for intel installations,
# so some duplication ends up happening.
# Order will be: ~/bin:macports:brew:perl:local_npm:python:etc:git_repos
orig_path=$PATH
new_path=''

# Add MacPorts (if present), ahead of Homebrew
macports_pathstring=''
if [[ -d "/opt/local/bin" ]]; then
    macports_pathstring="/opt/local/bin"
fi
if [[ -d "/opt/local/sbin" ]]; then
    macports_pathstring="${macports_pathstring:+${macports_pathstring}:}/opt/local/sbin"
fi
if [[ -n "$macports_pathstring" ]]; then
    new_path="$macports_pathstring"
fi

# Add Homebrew (if present); /usr/local already present from /etc/paths but
# homebrew should be higher than /usr/bin.  See also brew shellenv
homebrew_pathstring=''
HOMEBREW_PREFIX=$(brew --prefix)
if [[ -d "$HOMEBREW_PREFIX/bin" ]]; then
    homebrew_pathstring="$HOMEBREW_PREFIX/bin"
fi
if [[ -d "$HOMEBREW_PREFIX/sbin" ]]; then
    homebrew_pathstring="${homebrew_pathstring:+${homebrew_pathstring}:}$HOMEBREW_PREFIX/sbin"
fi
if [[ -n "$homebrew_pathstring" ]]; then
    new_path="${new_path:+${new_path}:}$homebrew_pathstring"
fi

# Intermediate export, required for getting proper perl/python versions/paths
export PATH="$macports_pathstring:$PATH"

# Perl major version, e.g. 5.30.  Used below for building up some paths, but
# also briefly used in .bashrc a bit.  Here because newer perls (from
# macports/homebrew) are now available in path.  Could also do -V::version:,
# PERL_VERSION, etc., but all need massaging.
PERL5=$(perl -e'print substr($^V, 1, -2)'); export PERL5 # trim leading v and trailing subversion

# Differentiate between home machine and ssh, mainly for perl, which needs
# different items (local::lib perl, etc.)
# toolforge tool account isn't SSH_TTY, but does have the same env variable
if [[ $SSH_TTY || $INSTANCEPROJECT ]]; then
    # This from cpan
    new_path="${new_path:+${new_path}:}$HOME/perl5/bin" # Okay this one was modified

    PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
    PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
    PERL_MB_OPT="--install_base $HOME/perl5"; export PERL_MB_OPT;
    PERL_MM_OPT="INSTALL_BASE=/$HOME/perl5"; export PERL_MM_OPT;
else
    # Perhaps the next time I upgrade perl, I'll wait before installing
    # anything, and set PERL5LIB, etc. to something like the above.  Might be
    # easier (https://formyfriendswithmacs.com/macports.html) but seems a pain
    # to actively convert the current setup.
    ## Add perl execs
    # Wrapped quotes and trailing space is annoying, but probably more
    # portable not to hardcode these
    perl5_vendorbin=$(perl -V::vendorbin:|tr -d ' '|tr -d \')
    perl5_sitebin=$(perl -V::sitebin:|tr -d ' '|tr -d \')
    new_path="${new_path:+${new_path}:}$perl5_sitebin:$perl5_vendorbin"

    # Add unloved perl modules' manpages to the manpath, not as easy as above
    # since perl -V returns man/man1, man/man3, siteman/man1, siteman/man3
    MANPATH="/opt/local/share/perl$PERL5/siteman:/opt/local/share/perl$PERL5/man:$(manpath)"; export MANPATH

    export GIT_MAIN_DIR="$HOME/Documents/git"
    export GIT_PERS_DIR="$GIT_MAIN_DIR/personal"
    export GIT_EXTL_DIR="$GIT_MAIN_DIR/external"
fi

# Add $HOME's node_modules, if present
# Globally installed modules should already be on the path
if [[ $(command -v npm) ]]; then
    npm_bin=$(npm bin)
    if [[ -d "$npm_bin" ]]; then
	# export PATH="$PATH:$(npm bin)"
	new_path="$new_path:$(npm bin)"
    fi
fi

# Add python execs
# Doesn't really work on SSH FIXME TODO
# local; preferable to global
new_path="$new_path:$(python -m site --user-base)/bin"
# global; not preferred but just in case
# With various installations (*cough macports cough*) there are a bunch of
# symlinks to follow, so this actually gets the full bin path
# https://stackoverflow.com/q/749711/2521092
new_path="$new_path:$(python -c 'import os;print(os.path.join(os.__file__.split("lib/")[0],"bin","python"))')"

# Tack on original path
new_path="$new_path:$orig_path"

# Latecomers
if [[ -d "$GIT_EXTL_DIR/git-extra-commands@unixorn" ]]; then
    # Add git-extra-commands https://github.com/unixorn/git-extra-commands
    new_path="$new_path:$GIT_EXTL_DIR/git-extra-commands@unixorn/bin"
fi
if [[ -d "$GIT_EXTL_DIR/tiny-scripts@vitorgalvao" ]]; then
    # Add tiny-scripts stuff https://github.com/vitorgalvao/tiny-scripts
    # Don't need 'em all but better than alias/function-ing just a handful
    new_path="$new_path:$GIT_EXTL_DIR/tiny-scripts@vitorgalvao"
fi

# Add ~/bin ahead of everybody
if [[ -d "$HOME/bin" ]]; then
    new_path="$HOME/bin:$new_path"
fi

# Finally done building path, so remove dupes and export it
# via https://unix.stackexchange.com/a/14896/43935
PATH="$(printf "%s" "$new_path" | awk -v RS=':' '!a[$1]++ { if (NR > 1) printf RS; printf $1 }')"
export PATH

# Modern day, I always want a visual editor
# https://unix.stackexchange.com/q/4859/43935
export VISUAL="$EDITOR "

# Ignore sequential duplicates in history
export HISTCONTROL=erasedups:ignoreboth
# Up history size
export HISTSIZE=5000000
export HISTFILESIZE=$HISTSIZE
# Display timestamp
# export HISTTIMEFORMAT='[%D %H:%M] '
export HISTTIMEFORMAT='[%F %T] '
export HISTIGNORE='ls:la:ll:lal:lla:l1:ltr:lsr:lasr:lt:l:cdl:pwd:clear:exit:logout:pg'


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
# Do not autocomplete when accidentally pressing Tab on an empty line - takes
# forever and yields "Display all 15 gazillion possibilites?"
shopt -s no_empty_cmd_completion
# If a command contains an invalid history operation, let me correct it
shopt -s histreedit
# Prevent file overwrite on stdout redirection
# set -o noclobber
# Expand aliases in non-interactive shells
shopt -s expand_aliases


# Load in .bashrc -------------------------------------------------
# shellcheck source=./.bashrc
source ~/.bashrc

# Welcome Messsage --------------------------------------------------
echo -ne "Welcome to ${Color_Green}$(hostname -s)${Color_zOff} on "
if [[ $OSTYPE == darwin* ]]; then
    echo -ne "${Color_Green}$(sw_vers -productName) $(sw_vers -productVersion)"
elif [[ $OSTYPE == linux* ]]; then
    echo -ne "${Color_Green}$(uname -o)"
else
    echo -ne "${Color_Green}$(uname -a)"
fi
echo -e "${Color_zOff} ($(uname -m))"
if [[ $(command -v fortune) && $UID != '0' && $- == *i* && $TERM != 'dumb' ]]; then
    echo -ne "${Color_White}"; fortune -s; echo -ne "${Color_zOff}" # only short ones
fi
echo -ne "${Color_Magenta}$(uname -sr) up" ; uptime | awk -F'(  |up)' '{print $2$3$4}'
# if [[ ! $SSH_TTY && ! $INSTANCEPROJECT && $(which weather) ]]; then
#     weather ?Qn0		# Uses wttr.in
# fi

# Default designed to always take up same space without leading zero for day,
# meaning dates before the 10th get two spaces, that bugs me
# The system copyright notice, the date and time of the last login, the
# message of the day as well as other information is silenced via .hushlogin
echo -ne "${Color_Red}Local time: ${Color_zOff}" ; date +'%a %b %d %H:%M:%S %Z %Y'
