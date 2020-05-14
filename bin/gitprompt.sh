#!/usr/bin/env bash
# newprompt.sh by Amory Meltzer
# Inspired by/heavily borrows from git-prompt.sh
# https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh
### [ "true" = asd ] convert to [[ ]]

# Helper function to read the first line of a file into a variable.
# __git_eread requires 2 arguments, the file path and the name of the
# variable, in that order.
### MAYBE STILL USEFUL FOR rebase/cherrypick/revert ###
__git_eread ()
{
	test -r "$1" && IFS=$'\r\n' read "$2" <"$1"
}

# see if a cherry-pick or revert is in progress, if the user has committed a
# conflict resolution with 'git commit' in the middle of a sequence of picks or
# reverts then CHERRY_PICK_HEAD/REVERT_HEAD will not exist so we have to read
# the todo file.
### EXPAND FOR REBSAE??? FIXME TODO ###
__git_sequencer_status ()
{
	local todo
	if test -f "$g/CHERRY_PICK_HEAD"
	then
		r="|CHERRY-PICKING"
		return 0;
	elif test -f "$g/REVERT_HEAD"
	then
		r="|REVERTING"
		return 0;
	elif __git_eread "$g/sequencer/todo" todo
	then
		case "$todo" in
		p[\ \	]|pick[\ \	]*)
			r="|CHERRY-PICKING"
			return 0
		;;
		revert[\ \	]*)
			r="|REVERTING"
			return 0
		;;
		esac
	fi
	return 1
}




pcmode=yes
ps1pc_start="$1"
ps1pc_end="$2"
printf_format="${3:-$printf_format}"
# set PS1 to a plain prompt so that we can
# simply return early if the prompt should not
# be decorated
PS1="$ps1pc_start$ps1pc_end"



# Default output if not in git directory
out='-'

repo_info="$(git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree --short HEAD 2>/dev/null)"
rev_parse_exit_code="$?"

if [ -z "$repo_info" ]; then
    echo $out
fi

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


r=""
b=""
step=""
total=""
if [ -d "$gitdir/rebase-merge" ]; then
    __git_eread "$gitdir/rebase-merge/head-name" b
    __git_eread "$gitdir/rebase-merge/msgnum" step
    __git_eread "$gitdir/rebase-merge/end" total
    r="|REBASE"
else
    if [ -d "$gitdir/rebase-apply" ]; then
	__git_eread "$gitdir/rebase-apply/next" step
	__git_eread "$gitdir/rebase-apply/last" total
	if [ -f "$gitdir/rebase-apply/rebasing" ]; then
	    __git_eread "$gitdir/rebase-apply/head-name" b
	    r="|REBASE"
	elif [ -f "$gitdir/rebase-apply/applying" ]; then
	    r="|AM"
	else
	    r="|AM/REBASE"
	fi
    elif [ -f "$gitdir/MERGE_HEAD" ]; then
	r="|MERGING"
    elif __git_sequencer_status; then
	:
    elif [ -f "$gitdir/BISECT_LOG" ]; then
	r="|BISECTING"
    fi

    if [ -n "$b" ]; then
	:			# bash builtin for true, just to initialize or something
    elif [ -h "$gitdir/HEAD" ]; then
	# symlink symbolic ref
	b="$(git symbolic-ref HEAD 2>/dev/null)"
    else
	head=""
	if ! __git_eread "$gitdir/HEAD" head; then
	    echo $out
	fi
	# is it a symbolic ref?
	b="${head#ref: }"
	if [ "$head" = "$b" ]; then
	    detached=yes
	    b="$(git describe --contains --all HEAD)"
	fi
    fi
fi

if [ -n "$step" ] && [ -n "$total" ]; then
    r="$r $step/$total"
fi

w=""
i=""
s=""
u=""
c=""
p=""

if [ "true" = "$inside_gitdir" ]; then
    # Not sure I care about this?  Not sure what it does...
    if [ "true" = "$bare_repo" ]; then
	c="BARE:"
    else
	b="GIT_DIR!"
    fi
elif [ "true" = "$inside_worktree" ]; then
    # Probably ALWAYS want this
    if [ -n "${GIT_PS1_SHOWDIRTYSTATE-}" ] &&
	   [ "$(git config --bool bash.showDirtyState)" != "false" ]
    then
	git diff --no-ext-diff --quiet || w="*"
	git diff --no-ext-diff --cached --quiet || i="+"
	if [ -z "$short_sha" ] && [ -z "$i" ]; then
	    i="#"
	fi
    fi
    # Couldn't hurt, now that I'm trying to keep stash clean...
    if [ -n "${GIT_PS1_SHOWSTASHSTATE-}" ] &&
	   git rev-parse --verify --quiet refs/stash >/dev/null
    then
	s="$"
    fi

    if [ -n "${GIT_PS1_SHOWUNTRACKEDFILES-}" ] &&
	   [ "$(git config --bool bash.showUntrackedFiles)" != "false" ] &&
	   git ls-files --others --exclude-standard --directory --no-empty-directory --error-unmatch -- ':/*' >/dev/null 2>/dev/null
    then
	# HUH
	u="%${ZSH_VERSION+%}"
    fi

    if [ -n "${GIT_PS1_SHOWUPSTREAM-}" ]; then
	__git_ps1_show_upstream
    fi
fi

# just a space by default
z="${GIT_PS1_STATESEPARATOR-" "}"

# Remove refs/heads/ from string, a good example of where git status would be simpler
b=${b##refs/heads/}
# Not needed on bash AFAICT
# __git_ps1_branch_name=$b
# b="\${__git_ps1_branch_name}"

# KEY:
# w=dirty state symbol (*) for unstaged
# i=dirty state symbol (+) for staged
# s=symbol ($) to indicate something being stashed
# u=shell expansion, soe % on bash and %%(?) on zsh.  Not sure why...
# c=BARE or empty
# b=
# f=string combining w, i, s, and u, so probably *+$.
# z=separator, just a space
# ${f:+$z$f}: if empty, nothing; if present, then separator then f itself
# r=rebasing/bisecting/cherry/reverting/etc.  Should customize more
# p=differential from upstream, expand
f="$w$i$s$u"
gitstring="$c$b${f:+$z$f}$r$p"
echo $gitstring

# check whether printf supports -v
# bash does...? but can just set to var
__git_printf_supports_v=
printf -v __git_printf_supports_v -- '%s' yes >/dev/null 2>&1

if [ "${__git_printf_supports_v-}" != yes ]; then
    gitstring=$(printf -- "$printf_format" "$gitstring")
else
    printf -v gitstring -- "$printf_format" "$gitstring"
fi
# PS1="$ps1pc_start$gitstring$ps1pc_end"
out="$ps1pc_start$gitstring$ps1pc_end"

echo $out







######## ###########
count="$(git rev-list --count --left-right "@{upstream}"...HEAD 2>/dev/null)"
# calculate the result
case "$count" in
    "") # no upstream
	p="" ;;
    "0	0") # equal to upstream
	p="=" ;;
    "0	"*) # ahead of upstream
	p=">"
	p=" u+${count#0	}" ;;
    *"	0") # behind upstream
	p="<"
	p=" u-${count%	0}" ;;
    *)	    # diverged from upstream
	p="<>"
	p=" u+${count#*	}-${count%	*}" ;;
esac

echo $p
