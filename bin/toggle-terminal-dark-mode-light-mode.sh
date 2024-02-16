#!/usr/bin/env bash
# toggle-terminal-dark-mode-light-mode by Amory Meltzer
# Toggle Terminal profiles light/dark mode via AppleScript

osascript -e 'tell application "Terminal"
	set currentSettings to name of current settings of tab 1 of window 1

	if currentSettings is "Basic" then
		set newSettings to "Basic Dark"
	else if currentSettings is "Basic Dark" then
		set newSettings to "Basic"
	else
		display dialog "Unknown settings set"
		return
	end if

	set current settings of tabs of windows to settings set newSettings
end tell'
