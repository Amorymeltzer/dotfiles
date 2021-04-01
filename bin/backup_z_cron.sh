#!/usr/bin/env bash
# backup_z_cron.sh by Amory Meltzer
# Dumb script to back-up .z database and warn me if shit's fucked
# Run every five minutes via cron
# Could be simple with awk, but this is clearer and will provide better reminders

### I *think* this is related to checking out a repo in git that removes
### certain paths or files, which if it's dotfiles, could mess up z complete,
### and if it's a path in z's history, will remove/reset that item

z="$(wc -l < ~/.z | tr -d ' ')"
b="$(wc -l < ~/.z.bk | tr -d ' ')"

if [ "$z" -lt "$b" ]; then
    date="$(date)"
    # Hellow darkness my old friend...
    /opt/local/bin/terminal-notifier -message "As of $date
Restore from ~/.z.bk" -title "ISSUE WITH .z DATABASE"
else
    # Party hard
    cp ~/.z ~/.z.bk
fi
