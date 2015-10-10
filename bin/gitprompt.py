#!/usr/bin/env python
# coding=UTF-8
# Report on git status of a given folder.  Faster than some bash-based options
# Borrows HEAVILY from mnagel's excellent clustergit (Github: mnagel/clustergit)

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
    out = run('LC_ALL=C git status 2>/dev/null')
    if out:
        if re.search(r'nothing to commit.?.?working directory clean.?', out):
            messages.append("=")
        else:
            if 'Changes not staged for commit' in out:
                messages.append("\033[0;32m+\033[0;33m")
            if 'Untracked files' in out:
                messages.append("\033[0;36m?\033[0;33m")
            if 'Changes to be committed' in out:
                messages.append("\033[0;35m!\033[0;33m")
        if 'On branch ' in out:
            branch = re.search('^On branch (.*)\n', out)
            messages.insert(0, '\033[0;33m ')
            messages.insert(0, branch.group(1))
            if not re.search('^master$', branch.group(1)):
                messages.insert(0, "\033[0;37m")
            if 'Your branch is ahead of ' in out:
                messages.append("→ ")
                p = re.search("Your branch is ahead of .* by (\d+) commit", out)
                messages.append(p.group(1))
            elif 'Your branch is behind ' in out:
                # Of course the left unicode arrow is different than the right
                messages.append("← (")
                p = re.search("Your branch is behind .* by (\d+) commit", out)
                messages.append(p.group(1))
                messages.append("\033[0;33m)")
            elif 'Your branch is up-to-date with ' in out:
                messages.append("=")
            elif re.search(r'Your branch and .* have diverged.?', out):
                messages.append("⇵")
        elif 'HEAD detached' in out:
            messages.insert(0, ' ')
            messages.insert(0, "\033[0;31m!!ERROR - DETACHED HEAD!!\033[0;33m")
        else:
            messages.insert(0, ' ')
            messages.insert(0, "\033[0;31m!!ERROR - NOT ON A BRANCH!!\033[0;33m")
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
