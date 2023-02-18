#!/usr/bin/env bash
# newprompt.sh by Amory Meltzer
# Inspired by/heavily borrows from git-prompt.sh
# https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh
# Last checked February 2023


# Exit early but get all the info desired if present
repo_info="$(git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree --short HEAD 2>/dev/null)"
rev_parse_exit_code="$?"

if [ -z "$repo_info" ]; then
    exit
fi

# parse
if [ "$rev_parse_exit_code" = "0" ]; then
    short_sha="${repo_info##*$'\n'}"
    repo_info="${repo_info%$'\n'*}"
fi
inside_worktree="${repo_info##*$'\n'}"
repo_info="${repo_info%$'\n'*}"
bare_repo="${repo_info##*$'\n'}"
repo_info="${repo_info%$'\n'*}"
inside_gitdir="${repo_info##*$'\n'}"
gitdir="${repo_info%$'\n'*}"

# Separator, possibly unnecessary FIXME TODO
z=" "


### Helper functions ###
# Read the first line of a file into a variable.
# __git_eread requires 2 arguments, the file path and the name of the
# variable, in that order.
### CAN USE FOR SINGLE LINE???
__git_eread () {
    test -r "$1" && IFS=$'\r\n' read -r "$2" <"$1"
}

# Define some colors
Color_Black="\033[0;30m"        # Black
Color_Red="\033[0;31m"          # Red
Color_Green="\033[0;32m"        # Green
Color_Yellow="\033[0;33m"       # Yellow
Color_Blue="\033[0;34m"         # Blue
Color_Magenta="\033[0;35m"      # Magenta
Color_Cyan="\033[0;36m"         # Cyan
Color_White="\033[0;37m"        # White
Color_zOff="\033[0m"            # No color

# Wrap colors around a variable
# There has got to be a smarter way to do this FIXME TODO
__wrap_color () {
    local color
    case $2 in
	Black) color=${Color_Black};;
	Red) color=${Color_Red};;
	Green) color=${Color_Green};;
	Yellow) color=${Color_Yellow};;
	Blue) color=${Color_Blue};;
	Magenta) color=${Color_Magenta};;
	Cyan) color=${Color_Cyan};;
	White) color=${Color_White};;
	Clear) color=${Color_zOff};;

	*) color=${Color_Yellow} # default
    esac
    echo -ne "$color$1${Color_Yellow}"
}

# See if a cherry-pick or revert is in progress, if the user has committed a
# conflict resolution with 'git commit' in the middle of a sequence of picks or
# reverts then CHERRY_PICK_HEAD/REVERT_HEAD will not exist so we have to read
# the todo file.
#### Should expand with /sequencer (if available?) to get steps
#### todo count for steps left???
#### abort-safety for... done with already done
#### head is fine???  No onto?
__git_sequencer_status () {
    local todo
    if test -f "$gitdir/CHERRY_PICK_HEAD"
    then
	r="Cherry-picking"
	return 0;
    elif test -f "$gitdir/REVERT_HEAD"
    then
	r="Reverting"
	return 0;
    elif __git_eread "$gitdir/sequencer/todo" todo
    then
	case "$todo" in
	    p[\ \	]|pick[\ \	]*)
		r="Cherry-picking"
		return 0
		;;
	    revert[\ \	]*)
		r="Reverting"
		return 0
		;;
	esac
    fi
    return 1
}


### REBASE NOTES ###
# If there's a conflict, presence of .git/MERGE_MSG should do it?
# stopped-sha = edit or conflict
# Investigate amend?  Presence indicates e (versus r)

#### REBASE FILES ####
##### .git #####
# REBASE_HEAD points to current, when would be different than short_sha???
# Present for edit, not reword, break
##### SHARED (.git/rebase-merge/) #####
# author-script: Name, email, date
# done: Which steps done/applied, includes current action
# end: Number of steps
# git-rebase-todo: Left
# git-rebase-todo.backup: What the initial interactive menu was like (pick, pick, pick)
# gpg_sign_opt: -S, yeah
# head-name: Branch
# interactive: Present if interactive, or stopped for any reason(?)
# drop-redundant-commit: Present if option is provided (not interactive)
# msgnum: What step we're on
# onto: Base revision for the rebase (not included in rebase)
# orig-head: Original head before rebase
##### MODE-SPECIFIC #####
##### EDIT #####
# amend: sha of the revision we're currently adjusting
# message
# patch
# stopped-sha: sha where we stopped, not the same as amend if rebasing merges???
##### REWORD #####
# (none)
##### BREAK #####
# (none)

r=""
a=""
o=""
b=""
step=""
total=""

if [ -d "$gitdir/rebase-merge" ]; then
    __git_eread "$gitdir/rebase-merge/head-name" b
    __git_eread "$gitdir/rebase-merge/msgnum" step
    __git_eread "$gitdir/rebase-merge/end" total
    r="Rebasing"
    if __git_eread "$gitdir/rebase-merge/orig-head" orighead; then
	orighead=$(git rev-parse --short "$orighead")
	r="$r $(__wrap_color "$orighead")"
    fi
    if __git_eread "$gitdir/rebase-merge/onto" o; then
	o=$(git rev-parse --short "$o")
	o="$(__wrap_color "onto" "Red") $o"
    fi
    # Get action, probably needs work FIXME TODO
    done=$(tail -n 1 "$gitdir/rebase-merge/done" 2>/dev/null)
    if [[ -n "$done" ]]; then
	a="$done"
	if [[ "$a" != "break" && $(echo "$done" | cut -f 1 -d ' ') != "exec" ]]; then
	    a=$(echo -n "$done" | grep "$short_sha" | cut -f 1 -d ' ')
	    # There's a way to get better message here?  reword sha steps
	    # instead of just reword steps, like with stopped? FIXME TODO
	    # With break, maybe grab the last?  or next?  Or both?

	    # Weirdness with merges and shit, hopefully don't get here
	    # Could be smarter and check for stopped and amend
	    # then output extra info if present and different
	    if [[ -z "$a" ]]; then
		a="stopped"
		__git_eread "$gitdir/rebase-merge/stopped-sha" stopped
		if [[ -n "$stopped" ]]; then
		    stopped=$(git rev-parse --short "$stopped") # Just in case
		    a="stopped at $stopped"
		fi
	    fi
	fi
    fi
else
    if [ -d "$gitdir/rebase-apply" ]; then
	__git_eread "$gitdir/rebase-apply/next" step
	__git_eread "$gitdir/rebase-apply/last" total
	if [ -f "$gitdir/rebase-apply/rebasing" ]; then
	    __git_eread "$gitdir/rebase-apply/head-name" b
	    r="Rebasing"
	elif [ -f "$gitdir/rebase-apply/applying" ]; then
	    r="Applying"
	else
	    r="Applying/Rebasing"
	fi
    elif [ -f "$gitdir/MERGE_HEAD" ]; then
	r="Merging"
    elif __git_sequencer_status; then
	:
    elif [ -f "$gitdir/BISECT_LOG" ]; then
	r="Bisecting"
    fi

    if [ -n "$b" ]; then
	: # bash builtin for true, already have a branch name from the above actions
    elif [ -h "$gitdir/HEAD" ]; then
	# symlink symbolic ref
	b="$(git symbolic-ref HEAD 2>/dev/null)"
    else
	head=""
	# Quit, what state is this?  No head...
	if ! __git_eread "$gitdir/HEAD" head; then
	    echo -n "$out"
	    exit
	fi
	# is it a symbolic ref?
	b="${head#ref: }"
	if [ "$head" = "$b" ]; then
	    detached=yes	# Used to colorize detched head in red
	    b="$(git describe --contains --all HEAD)"
	fi
    fi
fi

if [[ -n "$r" ]]; then
    r=$(__wrap_color "$r" "Red")
    # How far along in the rebase we are
    # Consider moving to end of gitstring??? FIXME TODO
    if [ -n "$step" ] && [ -n "$total" ]; then
	a="(${a:+$a$z}$step/$total)"
	a="$(__wrap_color "$a" "Blue")"
    fi
    r="$r${a:+$z$a}"
fi

# Possibly redundant?? FIXME
conflict="" # state indicator for unresolved conflicts
if [[ $(git ls-files --unmerged 2>/dev/null) ]]; then
    conflict="(conflict)"
fi

w=""
i=""
u=""
h=""
t=""
m=""
d=""
x=""
y=""
n=""
e=""
s=""
c=""
p=""

if [[ "$inside_gitdir" == "true" ]]; then
    # Not sure I care about this?
    if [[ "$bare_repo" == "true" ]]; then
	c="BARE:"
    else
	b="GIT_DIR!"
    fi
elif [[ "$inside_worktree" == "true" ]]; then

    ##### GIT STATUS KEY #####
    # X          Y     Meaning
    # -------------------------------------------------
    #          [AMD]   not updated
    # M        [ MD]   updated in index
    # A        [ MD]   added to index
    # D                deleted from index
    # R        [ MD]   renamed in index
    # C        [ MD]   copied in index
    # [MARC]           index and work tree matches
    # [ MARC]     M    work tree changed since index
    # [ MARC]     D    deleted in work tree
    # [ D]        R    renamed in work tree
    # [ D]        C    copied in work tree
    # -------------------------------------------------
    # D           D    unmerged, both deleted
    # A           U    unmerged, added by us
    # U           D    unmerged, deleted by them
    # U           A    unmerged, added by them
    # D           U    unmerged, deleted by us
    # A           A    unmerged, both added
    # U           U    unmerged, both modified
    # -------------------------------------------------
    # ?           ?    untracked
    # !           !    ignored
    # -------------------------------------------------

    # porcelain=v2 is inconsistent in the leading character for untracked
    # ?? is hard to match properly in a case, behaves very weirdly
    status=$(git status --porcelain|cut -c 1-2|sed 's/??/untracked/'|sort|uniq)
    if [[ -n "$status" ]]; then
	mapfile -t status <<< "$status"
	for stat in "${status[@]}"; do
	    case "$stat" in
		# Green=not staged, magenta=staged
		# Currently ignores [ D][RC] and could maybe do better with
		# renames (R), copies (C), and deletions (D)
		[MARC][MD]) w=$(__wrap_color "+" "Green")
			    i=$(__wrap_color "!" "Magenta");;
		' '[MAC]) w=$(__wrap_color "+" "Green");;
		[MAC]' ') i=$(__wrap_color "!" "Magenta");;
		untracked) u=$(__wrap_color "?" "Cyan");;
		' T') t=$(__wrap_color "T" "Green");;
		'T ') t=$(__wrap_color "T" "Magenta");;
		' R') m=$(__wrap_color "R" "Green");;
		'R ') m=$(__wrap_color "R" "Magenta");;
		' D') d=$(__wrap_color "D" "Green");;
		'D ') d=$(__wrap_color "D" "Magenta");;
		# Various merge states in red
		UU) x=$(__wrap_color "U" "Red");;
		DD|DU|UD) y=$(__wrap_color "D" "Red");;
		AA|AU|UA) n=$(__wrap_color "A" "Red");;
		*) e=$(__wrap_color "FIX" "Red");;
	    esac
	done
    fi
    # Old, imperfect:
    # git diff --no-ext-diff --quiet || w=$(__wrap_color "+" "Green")	     # Unstaged
    # git diff --no-ext-diff --cached --quiet || i=$(__wrap_color "!" "Magenta") # Staged
    # if git ls-files --others --exclude-standard --directory --no-empty-directory --error-unmatch -- ':/*' >/dev/null 2>/dev/null

    # Not entirely sure what this does yet...
    if [ -z "$short_sha" ] && [ -z "$i" ]; then
	i=$(__wrap_color "#" "Red")
    fi
    # Stash
    if git rev-parse --verify --quiet refs/stash >/dev/null
    then
	s=$(__wrap_color "$" "Green")
    fi

    # Include text for sparse checkout
    if [ "$(git config --bool core.sparseCheckout)" = "true" ]; then
	h="(sparse)"
    fi
fi

# Remove refs/heads/ from string, a good example of where git status would be simpler
b=${b##refs/heads/}
# Not needed on bash AFAICT
# __git_ps1_branch_name=$b
# b="\${__git_ps1_branch_name}"

if [[ "$b" != "master" ]] && [[ "$b" != "main" ]]; then
    if [[ "$detached" = 'yes' ]]; then
	b=$(__wrap_color "Detached: $b" "Red")
    else
	b=$(__wrap_color "$b" "White")
    fi
fi

# @ separator, but resort to stash ($s, in green) if present
if [[ -n "$s" ]]; then
    at="$s"
else
    at=$(__wrap_color "@" "Clear")
fi

######## ###########
count="$(git rev-list --count --left-right "@{upstream}"...HEAD 2>/dev/null)"
## Alternatively, if doing git status --porcelain for the above file status,
## could do --branch here, parse for ahead, behind (maybe also ## branchname..upstream)
# calculate the result
# note the tabs
case "$count" in
    "") # no upstream
	p="" ;;
    "0	0") # equal to upstream
	p="=" ;;
    "0	"*) # ahead of upstream
	p="→$z${count#0	}";;
    *"	0") # behind upstream
	p="←$z${count%	0}";;
    *)	    # diverged from upstream
	# p="$z${count#*	}⇵$z${count%	*}";;
	p="$z↓${count#*	}↑${count%	*}";;
esac

# KEY:
# u=symbol (?) for untracked
# w=symbol (+) for unstaged
# i=symbol (!) for staged
# t=symbol (T) for typechange
# m=symbol (R) for renames
# d=symbol (D) for deleted
# x=symbol (U) for merge: conflicts/modified
# y=symbol (D) for merge: deleted
# n=symbol (A) for merge: added
# e=symbol (FIX) for others
# c=BARE or empty
# b=branch name
# o=rebasing onto commit
# f=string combining above, so probably something like *+$.
# z=separator, just a space
# ${f:+$z$f}: if empty, nothing; if present, then separator then f itself
# r=rebasing/bisecting/cherry/reverting/etc.  ACTION: Should customize more, put first
# h=(sparse) if sparseCheckout
# conflict=(conflict) if there are files in conflict.  Possible just duplicates $x/$y/$a
# p=differential from upstream, expand

f="$u$w$i$t$m$d$n$x$y$e"
# ${f:-=}: above dirty state, = if not
gitstring="${h:+$h$z}${conflict:+$conflict$z}${r:+$r$z}$c$b$at$short_sha${o:+$z$o}$z${f:-=}$p"
# Ensure gitstring is string, etc.
printf -v gitstring '%s' "$gitstring"
out="$gitstring"

echo -n "$out"
