#! /usr/bin/env python

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
    # Is there a .git file
    if os.path.exists( os.path.join(dirname, ".git/") ):
        #infile = os.path.join(dirname, ".git/")
        # Yay, we found one!  Let's descend into it and ask git for a status
        out = run('cd "%s"; LC_ALL=C git status' % dirname)

        messages = []
        clean = True
        if re.search(r'nothing to commit.?.?working directory clean.?', out):
            messages.append("=")
        # Check for untracked files
        if 'nothing added to commit but untracked files present' in out:
            messages.append("?")
            clean = False
        if 'Changes not staged for commit' in out:
            messages.append("+")
            clean = False
        if 'Changes to be committed' in out:
            messages.append("!")
            clean = False
        # if 'Your branch is ahead of' in out:
        #     messages.append(colorize(Colors.FAIL, "Unpushed commits"))
        #     sys.stdout.write("".join(messages))
        #     clean = False
        # else:
        #     clean = True

        # if clean:
        #     sys.stdout.write("".join(messages))
        # else:
    else:
        messages = ["-"]

    sys.stdout.write("".join(messages))

    run('cd ../')

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
