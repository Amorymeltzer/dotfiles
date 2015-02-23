#!/usr/bin/env python
# coding=UTF-8
# Report on git status of a given folder.  Faster than some bash-based options
# Borrows HEAVILY from mnagel's excellent clustergit (Github: mnagel/clustergit)


## Potentially useful for parsing branchname, in particular if everything ends
## up FUBARd for some reason FIXME TODO
# # Get the short symbolic ref.
# # If HEAD isn't a symbolic ref, get the short SHA for the latest commit
# # Otherwise, just give up.
# branchName="$(git symbolic-ref --quiet --short HEAD 2> /dev/null || \
# git rev-parse --short HEAD 2> /dev/null || \
# echo '(unknown)')";

# Grab some libraries
import re
import sys
import os
import commands


def run(command):
    return commands.getoutput(command)

def check(dirname):
    """
    Check the subdirectories of a single directory.
    See if they are versioned in git and display the requested information.
    """

    messages = []
    if run('git rev-parse --is-inside-work-tree 2>/dev/null'):
        clean = True
        out = run('LC_ALL=C git status')
        if re.search(r'nothing to commit.?.?working directory clean.?', out):
            messages.append("=")
        if 'Changes not staged for commit' in out:
            messages.append("+")
            clean = False
        if 'Untracked files' in out:
            messages.append("?")
            clean = False
        if 'Changes to be committed' in out:
            messages.append("!")
            clean = False
        if 'Your branch is ahead of' in out:
            # Output
            # ⚡ → ↑ ↓ ↕ ○ ☿ ± ✘ ¤ « ¬ ¼ ½ ¾ × ƴ ˃ ˧ ૦ ᐅ ᗆ ᗌ ᗒ ᗘ ↀ ⇛ ⇒ ⇨ ↝ ∇ ⋕
            #  ⌁ ⌇ ⎋ ⏆ ▶ ▷ ▸ ▹ ► ▻ ◆ ◇ ◈ ◊ ☇ ☈ ✈ ➤ ➙ ⨠ 𝆓
            messages.append("→")
            clean = False
        # p = re.search("Your branch is ahead of .* by (\d+) commit", out)
        # messages.append(p.group(1))
        if 'On branch ' in out:
            branch = re.search('^On branch (.*)\n', out)
            messages.insert(0, ' ')
            messages.insert(0, branch.group(1))
    else:
        messages = ["-"]

    sys.stdout.write("".join(messages))

#-------------------
# Now, onto the main event!
#-------------------

def main():
    try:
        check(".")
    except (KeyboardInterrupt, SystemExit):
        sys.stdout.write("\n")

if __name__ == '__main__':
    main()
