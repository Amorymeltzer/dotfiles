#!/bin/sh

# These are carefully crafted two-row-high numbers and letters for cards.
# The recommended font is Monaco, at a minimum of a 5 point size, though
# smaller sizes may be readable with anti-aliasing.

# Ace
CARD_1_LEN=5
CARD_1_0=" /_\\ "
CARD_1_1='/   \'
CARD_1_0R="\__/"
CARD_1_1R=' \/ '

# CARD_1_0="/| "
# CARD_1_1="_|_"
# CARD_1_0R=" -,-"
# CARD_1_1R="  |_"

CARD_2_LEN=5
CARD_2_0=' ^)  '
CARD_2_1=" /_  "
CARD_2_0R=' -> '
CARD_2_1R=" (_ "

CARD_3_LEN=5
CARD_3_0=" --, "
CARD_3_1=" _\`| "
CARD_3_0R=" .-- "
CARD_3_1R=" |\`_ "

CARD_4_LEN=5
CARD_4_0=' |_| '
CARD_4_1='   | '
CARD_4_0R=' |_ '
CARD_4_1R=' | |'

CARD_5_LEN=5
CARD_5_0=" |_\" "
CARD_5_1=' ._) '
CARD_5_0R="(_\" "
CARD_5_1R='._| '

CARD_6_LEN=5
CARD_6_0="  /  "
CARD_6_1=" (_) "
CARD_6_0R=" (\")"
CARD_6_1R="  \"/"

CARD_7_LEN=5
CARD_7_0=' --, '
CARD_7_1='  /  '
CARD_7_0R="  /  "
CARD_7_1R=" /__ "

CARD_8_LEN=5
CARD_8_0=" (_) "
CARD_8_1=" (_) "
CARD_8_0R=" (_) "
CARD_8_1R=" (_) "

CARD_9_LEN=5
CARD_9_0=" (\") "
CARD_9_1="   | "
CARD_9_0R=" |_  "
CARD_9_1R=" (_) "

CARD_10_LEN=5
CARD_10_0='| /\ '
CARD_10_1='| \/ '
CARD_10_0R=' /\ |'
CARD_10_1R=' \/ |'

# Jack
CARD_11_LEN=5
CARD_11_0='  |  '
CARD_11_1="'v'  "
CARD_11_0R="  ,^,"
CARD_11_1R="  |  "

# Queen
CARD_12_LEN=5
CARD_12_0='/ \  '
CARD_12_1='\_x_ '
CARD_12_0R=' "/ \'
CARD_12_1R='  \_/'

# King
CARD_13_LEN=5
CARD_13_0='|_/  '
CARD_13_1='| \  '
CARD_13_0R='  \_|'
CARD_13_1R='  / |'

