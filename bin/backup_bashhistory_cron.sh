#!/usr/bin/env bash
# See backup_z_cron.sh

b="$(wc -l < ~/.bash_history | tr -d ' ')"
h="$(wc -l < ~/.bash_history.bk | tr -d ' ')"

if [ "$b" -lt "$h" ]; then
    date="$(date)"
    # Hellow darkness my old friend...
    /opt/local/bin/terminal-notifier -message "As of $date
Restore from ~/.bash_history.bk" -title "ISSUE WITH BASH HISTORY"
else
    # Party hard
    cp ~/.bash_history ~/.bash_history.bk
fi
