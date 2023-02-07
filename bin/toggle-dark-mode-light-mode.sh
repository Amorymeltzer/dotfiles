#!/usr/bin/env bash
# toggle-dark-mode-light-mode by Amory Meltzer
# Toggle macOS' light/dark mode via AppleScript

osascript -e 'tell application "System Events"
  tell appearance preferences
    set dark mode to not dark mode
  end tell
end tell'
