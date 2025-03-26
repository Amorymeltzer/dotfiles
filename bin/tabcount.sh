#!/usr/bin/env osascript
# tabcount by Amory Meltzer

tell application "Safari"
    set windowCount to count of windows
    set tabCount to 0

    repeat with w in windows
	set tabCount to tabCount + (count of tabs in w)
    end repeat

    set windowText to "window"
    if windowCount is not 1 then set windowText to "windows"

    set tabText to "tab"
    if tabCount is not 1 then set tabText to "tabs"

    set output to "Safari has " & windowCount & " " & windowText & " and " & tabCount & " " & tabText & " open."
end tell

do shell script "echo " & quoted form of output
