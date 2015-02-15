#!/usr/bin/env bash
# ksp_backup_files.sh by Amory Meltzer
# Simply copy pilot and alarm files to a save directory

dateandtime=$(date +%Y.%m.%d_%H%M%S)
folderpath='/Users/Amory/Dropbox/KSP stuff/KSP saves backups/'$dateandtime
ksppath='/Applications/KSP_osx'
kacpath=$ksppath'/GameData/TriggerTech/PluginData/KerbalAlarmClock'

mkdir "$folderpath"

cp "$ksppath/saves/McJohn/persistent.sfs" "$folderpath/McJohn.persistent.sfs"
cp "$ksppath/saves/Kergarin/persistent.sfs" "$folderpath/Kergarin.persistent.sfs"
#cp "$kacpath/config.xml" "$folderpath/"
#cp "$kacpath/Alarms-McJohn (CAREER).txt" "$folderpath/"

ls -lFGh "$folderpath/"
