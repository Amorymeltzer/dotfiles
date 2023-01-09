#!/usr/bin/env bash
# See backup_z_cron.sh

b="$(wc -l < ~/.bash_eternal_history | tr -d ' ')"
h="$(wc -l < ~/.bash_eternal_history.bk | tr -d ' ')"

if [ "$b" -lt "$h" ]; then
    date="$(date)"
    # Hellow darkness my old friend...
    /opt/homebrew/bin/terminal-notifier -message "As of $date
Restore from ~/.bash_eternal_history.bk" -title "ISSUE WITH BASH HISTORY"
else
    # Party hard
    cp ~/.bash_eternal_history ~/.bash_eternal_history.bk
fi
