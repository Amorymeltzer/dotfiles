#!/bin/sh

# Generic draw routines

# Show the cursor.  Call before exiting.
show_cursor()
{
    echon '[?25h'                           # show cursor
}

# Hide the cursor.  Call regularly.
hide_cursor()
{
    echon '[?25l'                           # hide cursor
}

# Resets the display and configures various settings.
vtreset()
{
    echon 'c'                               # VT100 reset
    echon '[?71'                            # Turn off auto-wrap
    echo ; echo ''                            # clear screen
    hide_cursor
}

# Sets the cursor to a given row and column.
vtsetpos()
{
    local VTS_ROW=$1
    local VTS_COL=$2

    # echo "ROW: '$VTS_ROW' COL: '$VTS_COL'"

    echon '['
    echon "$VTS_ROW"
    echon ';'
    echon "$VTS_COL"
    echon 'H'
    hide_cursor
}


# VT100 Commands

# Set
SET="["

# Options
BOOLEAN="?"
SHOWHIDE=25
HIDDEN=l
VISIBLE=h

# Colors
FOREGROUND=3
BACKGROUND=4

BLACK=0
RED=1
GREEN=2
YELLOW=3
BLUE=4
MAGENTA=5
CYAN=6
WHITE=7
ORANGE=8 # Not a real color, but some special draw routines like in shelltris use it as a trick.
DEFAULT=9

# Attributes
INVERSE=7
UNDERLINE=4
BOLD=1

ENABLE=0
DISABLE=2

ATTRIBUTE="m"

