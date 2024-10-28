#!/usr/bin/env bash
# jellyfin_backup_config.sh by Amory Meltzer
# Backup config data <https://jellyfin.org/docs/general/administration/backup-and-restore/>

dateandtime=$(date +%Y.%m.%d_%H%M%S)
folderpath='/Users/Amory/Documents/jellyfin_backups'
jellydata='/Users/Amory/Library/Application Support/Jellyfin/'
version=$(defaults read /Applications/Jellyfin.app/Contents/Info.plist CFBundleShortVersionString)

bk="$folderpath/$dateandtime-v$version"
mkdir -p "$bk"
cp -R "$jellydata/config/" "$bk/config/"
cp -R "$jellydata/data/" "$bk/data/"
cp -R "$jellydata/root/" "$bk/root/"
ls -lFGh "$bk"
