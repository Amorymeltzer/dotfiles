#!/usr/bin/env bash
# newprompt.sh by Amory Meltzer
# Inspired by/heavily borrows from git-prompt.sh
# https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh
### [ "true" = asd ] convert to [[ ]]

# Helper function to read the first line of a file into a variable.
# __git_eread requires 2 arguments, the file path and the name of the
# variable, in that order.
### STILL USEFUL FOR rebase/cherrypick/revert ###
__git_eread ()
{
	test -r "$1" && IFS=$'\r\n' read "$2" <"$1"
}

# see if a cherry-pick or revert is in progress, if the user has committed a
# conflict resolution with 'git commit' in the middle of a sequence of picks or
# reverts then CHERRY_PICK_HEAD/REVERT_HEAD will not exist so we have to read
# the todo file.
#### Should expand with sequencer (if available?) to get steps
#### todo count for steps left???
#### abort-safety for... done with already done
#### head is fine???  No onto?
__git_sequencer_status ()
{
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

# Separator, possibly unnecessary FIXME TODO
z=" "

r=""
o=""
b=""
step=""
total=""
# NEED TO FIGURE OUT ACTION FIXME TODO
# Can get head/stopped-sha then grep it in todo.backup, find first column
# stopped-sha = edit
# Might need to use done(+next)...
# Maybe investigate amend?
# ALSO: Get ONTO for action, and current head FIXME TODO
## edit
## head: HEAD|REBASE_HEAD|rm/amend|end of rm/done onto: rm/onto return: rm/orig-head
# amend
# author-script
# done
# end
# git-rebase-todo
# git-rebase-todo.backup
# gpg_sign_opt
# head-name
# interactive
# message
# msgnum
# onto
# orig-head
# patch
# stopped-sha
## reword
## head: HEAD||end of rm/done onto: rm/onto return: rm/orig-head
# author-script
# done
# end
# git-rebase-todo
# git-rebase-todo.backup
# gpg_sign_opt
# head-name
# interactive
# msgnum
# onto
# orig-head
if [ -d "$gitdir/rebase-merge" ]; then
    __git_eread "$gitdir/rebase-merge/head-name" b
    __git_eread "$gitdir/rebase-merge/msgnum" step
    __git_eread "$gitdir/rebase-merge/end" total
    r="Rebasing"
    __git_eread "$gitdir/rebase-merge/onto" o
    if [ -n "$o" ]; then
	o=$(git rev-parse --short "$o")
	o="onto $o"
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

# How far along in the rebase we are
if [ -n "$step" ] && [ -n "$total" ]; then
    r="$r ($step/$total)"
fi

w=""
i=""
s=""
u=""
c=""
p=""

if [ "true" = "$inside_gitdir" ]; then
    # Not sure I care about this?
    if [ "true" = "$bare_repo" ]; then
	c="BARE:"
    else
	b="GIT_DIR!$z"
    fi
elif [ "true" = "$inside_worktree" ]; then
    # WHAT ABOUT UNMERGED U FIXME TODO
    # Need to deal with fact that =! will show up for U
    # git itself sugests using status or diff-files (--porcelain), probably
    # the former FIXME TODO
    git diff --no-ext-diff --quiet || w="+"	     # Unstaged
    git diff --no-ext-diff --cached --quiet || i="!" # Staged
    # huh???  No sha and no cached, so... ???  something like detached???
    if [ -z "$short_sha" ] && [ -z "$i" ]; then
	i="#"
    fi
    # Untracked
    if git ls-files --others --exclude-standard --directory --no-empty-directory --error-unmatch -- ':/*' >/dev/null 2>/dev/null
    then
	u="?"
    fi
    # Stash
    if git rev-parse --verify --quiet refs/stash >/dev/null
    then
	s="$"
    fi

fi

# Remove refs/heads/ from string, a good example of where git status would be simpler
b=${b##refs/heads/}
# Not needed on bash AFAICT
# __git_ps1_branch_name=$b
# b="\${__git_ps1_branch_name}"


######## ###########
count="$(git rev-list --count --left-right "@{upstream}"...HEAD 2>/dev/null)"
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
	p="⇵+$z${count#*	}-${count%	*}";;
esac

# KEY:
# w=dirty state symbol (*) for unstaged
# i=dirty state symbol (+) for staged (or # if weird???)
# s=symbol ($) to indicate something is stashed
# u=symbol (%) for untracked files
# c=BARE or empty
# b=branch name
# o=rebasing onto commit
# f=string combining w, i, s, and u, so probably *+$.
# z=separator, just a space
# ${f:+$z$f}: if empty, nothing; if present, then separator then f itself
# r=rebasing/bisecting/cherry/reverting/etc.  ACTION: Should customize more, put first
# p=differential from upstream, expand

# f="$w$i$s$u"
f="$w$i$u$s"
# ${f:-=}: above dirty state, = if not
# gitstring="$c$b${f:+$z$f}$r$p"
gitstring="${r:+$r:$z}$c$b@$short_sha${o:+$z$o}$z${f:-=}$p"

# Ensure gitstring is string, etc.
printf -v gitstring '%s' "$gitstring"
out="$gitstring"

echo $out
