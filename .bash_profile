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
export PERL_CPANM_OPT="--verify --from https://www.cpan.org"
# Generally the default but let's just make this explicit.  Used throughout.
export PERL5_DIR="$HOME/perl5"

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
# The main disadvantage here is that on some older macOS', items in
# /usr/local/bin might not be as high as might be desired, given that it is used
# by Homebrew for intel installations, so some duplication ends up happening.
# Order will be: ~/bin:perlbrew:macports:brew:perl:local_npm:python:etc:git_repos
orig_path=$PATH
new_path=''


# Add MacPorts (if present), ahead of Homebrew
macports_pathstring=''
# Feels like this should be defined somewhere else
# https://guide.macports.org/chunked/reference.variables.html
macports_prefix='/opt/local'
if [[ -d "$macports_prefix/bin" ]]; then
    macports_pathstring="$macports_prefix/bin"
fi
if [[ -d "$macports_prefix/sbin" ]]; then
    macports_pathstring="${macports_pathstring:+${macports_pathstring}:}$macports_prefix/sbin"
fi
if [[ -n "$macports_pathstring" ]]; then
    new_path="$macports_pathstring"
fi


# Add Homebrew (if present):
# If on intel, /usr/local/bin is already present from /etc/paths, but we want
# homebrew to take precedence over /usr/bin.  Not sure when but at some point,
# Apple moved /usr/local/bin up the precedence list.
# If on arm/Apple Silicon, it's /opt/homebrew.
# See also brew shellenv FIXME TODO
if [[ "$(/usr/bin/uname -m)" == "arm64" ]]; then
    HOMEBREW_PREFIX="/opt/homebrew"
else
    HOMEBREW_PREFIX="/usr/local"
fi

homebrew_pathstring=''
if [[ -d "$HOMEBREW_PREFIX/bin" ]]; then
    homebrew_pathstring="$HOMEBREW_PREFIX/bin"
fi
if [[ -d "$HOMEBREW_PREFIX/sbin" ]]; then
    homebrew_pathstring="${homebrew_pathstring:+${homebrew_pathstring}:}$HOMEBREW_PREFIX/sbin"
fi
if [[ -n "$homebrew_pathstring" ]]; then
    new_path="${new_path:+${new_path}:}$homebrew_pathstring"
fi

# Intermediate pathing to get the right perl/python versions/paths
# favor brewperl over macports over homebrew.  This also starts to get the
# manpath set up properly, since manpath uses the actual path, although I'm not
# sure *when* it gets run.
PATH="$new_path:$PATH"
# Can also now confirm whether certain things (e.g. homebrew) are definitely
# installed.  brew isn't necessarily installed somewhere already on the default
# path, and in fact it isn't on Apple Silicon, so this is necessary.
if [[ -n "$macports_pathstring" && $(command -v port) ]]; then
    export PORT_INSTALLED=1  # Largely used to confirm installation for sourcing
fi
if [[ -n "$homebrew_pathstring" && $(command -v brew) ]]; then
    export BREW_INSTALLED=1  # Largely used to confirm installation for sourcing
fi

# Yay perlbrew.  The default, but again, make explicit
export PERLBREW_ROOT="$PERL5_DIR/perlbrew"
# Used later to test if perlbrew exists AND there are installs
PERLBREW_INSTALLED=''
if [[ -d "$PERLBREW_ROOT" ]]; then
    # This will put perlbrew first in line of the path, so now we have:
    # perlbrew:macports:homebrew:system
    # shellcheck source=/dev/null
    source "$PERLBREW_ROOT/etc/bashrc"

    # `perlbrew list` plenty effective much slower
    if [[ -n "$(ls -A "$PERL5_DIR/perlbrew/perls/")" ]]; then
	PERLBREW_INSTALLED='true'
    fi
fi

# Perl major version, e.g. 5.30.  Used below for building up some paths, but
# also briefly used in .bashrc a bit.  Here because newer perls (from
# perlbrew/macports/homebrew) are now available in path.  Could also do
# -V::version:, PERL_VERSION, etc., but all need massaging.
PERL5_V=$(perl -e'print substr($^V, 1, -2)'); export PERL5_V # trim leading v
# and trailing subversion

# Perlbrew doesn't exist or we don't have any perls installed with it, so let's
# set the perl environment ourselves.  Perlbrew doesn't like these, see
# <https://github.com/gugod/App-perlbrew/issues/513> and
# <https://stackoverflow.com/q/5447595/2521092>.  I think perlbrew will take
# care of any such statements it finds by itself, but there's a world where
# perlbrew is installed (yay) but no perls are installed with it (boo) where we
# *would* want some of this.
# This all is from cpan, to enable local::lib perl stuff if desired (i.e. not
# using perlbrew and maybe not if macports)
if [[ -z "$PERLBREW_INSTALLED" ]]; then

    # Perl from macports, so use a different directory
    # if it's installed
    # Check the actual string FIXME TODO
    if [[ -n "$macports_pathstring" ]]; then
	export PERL5_DIR="$macports_prefix"
	macports_perl="$PERL5_DIR/lib/perl5/$PERL5_V"

	# See <https://formyfriendswithmacs.com/macports.html>, note the
	# different path from what is used for cpan/local::lib
	export PERL5LIB="$macports_perl${PERL5LIB:+:${PERL5LIB}}"
	export PERL_LOCAL_LIB_ROOT="$macports_perl${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
	export PERL_MB_OPT="--install_base $macports_perl"
	export PERL_MM_OPT="INSTALL_BASE=/$macports_perl"

	## Add perl execs
	# Wrapped quotes and trailing space is annoying, but probably more
	# portable not to hardcode these
	perl5_vendorbin=$(perl -V::vendorbin:|tr -d ' '|tr -d \')
	perl5_sitebin=$(perl -V::sitebin:|tr -d ' '|tr -d \')
	new_path="${new_path:+${new_path}:}$perl5_sitebin:$perl5_vendorbin"

	# Add unloved perl modules' manpages to the manpath, not as easy as above
	# since perl -V returns man/man1, man/man3, siteman/man1, siteman/man3
	MANPATH="$macports_prefix/share/perl$PERL5_V/siteman:$macports_prefix/share/perl$PERL5_V/man:$(manpath)"; export MANPATH

    else # Investigate homebrew? FIXME TODO

	# This path already added if perlbrew or macports
	new_path="${new_path:+${new_path}:}$PERL5_DIR/bin" # Okay this one was modified

	export PERL5LIB="$PERL5_DIR/lib/perl5${PERL5LIB:+:${PERL5LIB}}"
	export PERL_LOCAL_LIB_ROOT="$PERL5_DIR${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"
	export PERL_MB_OPT="--install_base $PERL5_DIR"
	export PERL_MM_OPT="INSTALL_BASE=/$PERL5_DIR"
    fi

fi

# Differentiate between home machine and ssh.  Toolforge tool account isn't
# SSH_TTY, but does have the same $INSTANCEPROJECT and $INSTANCENAME.  And k8s
# doesn't have anything!
if [[ $SSH_TTY || $INSTANCEPROJECT || $KUBERNETES_PORT ]]; then
    # Get ripgrep on toolforge https://wikitech.wikimedia.org/wiki/Tool:Rustup
    if [[ $INSTANCEPROJECT || $KUBERNETES_PORT ]]; then
	rustup="/data/project/rustup/rustup/.cargo/bin"
	if [[ -d "$rustup" ]]; then
	    new_path="${new_path:+${new_path}:}$rustup"
	fi
    fi
else
    # Some personal, local directories
    export GIT_MAIN_DIR="$HOME/Documents/git"
    export GIT_PERS_DIR="$GIT_MAIN_DIR/personal"
    export GIT_EXTL_DIR="$GIT_MAIN_DIR/external"
fi

# Add $HOME's node_modules, if present
# Globally installed modules should already be on the path
if [[ $(command -v npm) ]]; then
    # Skip the warnings.  Should probably do something with prefix or root
    # instead FIXME TODO
    npm_bin=$(npm bin 2>/dev/null)
    if [[ -d "$npm_bin" ]]; then
	new_path="$new_path:$(npm bin)"
    fi
fi

# Add python execs
# In a perfect world everything is python3, but just in case...
alias python='python3'
# Doesn't really work on SSH FIXME TODO
if [[ -n "$PORT_INSTALLED" ]]; then
    # local; preferable to global
    new_path="$new_path:$(python -m site --user-base)/bin"
    # global; not preferred but just in case
    # With various installations (*cough macports cough*) there are a bunch of
    # symlinks to follow, so this actually gets the full bin path
    # https://stackoverflow.com/q/749711/2521092
    new_path="$new_path:$(python -c 'import os;print(os.path.join(os.__file__.split("lib/")[0],"bin","python"))')"
elif [[ -n "$BREW_INSTALLED" ]]; then
    # Unversioned symlinks e.g. python for python3
    new_path="$new_path:$(brew --prefix python)/libexec/bin"

    # pip packages, alias for e.g. /opt/homebrew/lib/python3.10/site-packages
    new_path="$new_path:$(python -c 'import site; print(site.getsitepackages()[0])')"
fi
unalias python
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
# Personal lockquote script
if [[ -d "$GIT_PERS_DIR/lockquote" ]]; then
    new_path="$new_path:$GIT_PERS_DIR/lockquote"
fi
# CKAN?  CKAN
if [[ -d /Applications/CKAN.app/Contents/MacOS/ ]]; then
    new_path="$new_path:/Applications/CKAN.app/Contents/MacOS/"
fi


# Nearly done building path, so remove dupes and prepare for export
# via https://unix.stackexchange.com/a/14896/43935
PATH="$(printf "%s" "$new_path" | awk -v RS=':' '!a[$1]++ { if (NR > 1) printf RS; printf $1 }')"

# Ensure perlbrew is on top, but only if it's useful
if [[ -n "$PERLBREW_INSTALLED" ]]; then
    # shellcheck source=/dev/null
    source "$PERLBREW_ROOT/etc/bashrc"
fi
# Add ~/bin ahead of everybody
if [[ -d "$HOME/bin" ]]; then
    PATH="$HOME/bin:$PATH"
fi

export PATH

# Modern day, I always want a visual editor
# https://unix.stackexchange.com/q/4859/43935
export VISUAL="$EDITOR "

# Ignore sequential duplicates in history
export HISTCONTROL=erasedups:ignoreboth
# Eternal bash history fuck yeah https://stackoverflow.com/q/9457233/2521092
# Use -1 to be explicit, only in bash 4.3+
export HISTFILESIZE=-1
export HISTSIZE=-1
# Display timestamp
# export HISTTIMEFORMAT='[%D %H:%M] '
export HISTTIMEFORMAT='[%F %T] '
# Apparently certain bash sessions truncate .bash_history on close
# https://superuser.com/a/753158/240421
export HISTFILE=~/.bash_eternal_history
# Ignore some things
# export HISTIGNORE='ls:la:ll:lal:lla:l1:ltr:lsr:lasr:lt:l:cdl:pwd:clear:exit:logout:pg'


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
# if [[ ! $SSH_TTY && ! $INSTANCEPROJECT && !$KUBERNETES_PORT && $(which weather) ]]; then
#     weather ?Qn0		# Uses wttr.in
# fi

# Default designed to always take up same space without leading zero for day,
# meaning dates before the 10th get two spaces, that bugs me
# The system copyright notice, the date and time of the last login, the
# message of the day as well as other information is silenced via .hushlogin
echo -ne "${Color_Red}Local time: ${Color_zOff}" ; date +'%a %b %d %H:%M:%S %Z %Y'
