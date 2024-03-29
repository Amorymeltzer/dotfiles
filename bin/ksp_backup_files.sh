#!/usr/bin/env bash
# ksp_backup_files.sh by Amory Meltzer
# Simply copy pilot files to a save directory

dateandtime=$(date +%Y.%m.%d_%H%M%S)
folderpath='/Users/Amory/Documents/pilot_backups'
ksppath='/Applications/KSP_osx'

userlist=$(ls -1 $ksppath'/saves' | grep -v scenarios | grep -v training)

mkdir "$folderpath"

for i in $userlist
do
    mkdir -p "$folderpath/$i/$dateandtime"
    cp "$ksppath/saves/$i/persistent.sfs" "$folderpath/$i/$dateandtime/persistent.sfs"
    cp "$ksppath/saves/$i/persistent.loadmeta" "$folderpath/$i/$dateandtime/persistent.loadmeta"
    ls -lFGhR "$folderpath/$i/$dateandtime"
done
