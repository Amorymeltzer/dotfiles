#!/usr/bin/env bash
# Don't change header with nyancat, also play the music

mpv -really-quiet ~/Music/nyan/cat.mp3 -loop inf </dev/null & nyancat -s
