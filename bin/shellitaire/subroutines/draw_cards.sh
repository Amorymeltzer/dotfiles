#!/bin/sh


if [ "x$INITIALIZED" = "x" ] ; then
	GAMEDIR="."
fi

. "$GAMEDIR/subroutines/draw_routines.sh"
. "$GAMEDIR/subroutines/card_back.sh"
. "$GAMEDIR/subroutines/card_numbers.sh"
. "$GAMEDIR/subroutines/card_icons.sh"

STACK_MAX=13

# COLOR SETTINGS
SCREEN_BGCOLOR="$GREEN"
SCREEN_FGCOLOR="$WHITE"

CARD_BORDER_FG_BG="$BACKGROUND"
DEFAULT_CARD_BORDER="$YELLOW"
SOURCE_CARD_BORDER="$RED"
DESTINATION_CARD_BORDER="$BLUE"
OUTLINE_CARD_BORDER="$WHITE"

SKIP_FG="$YELLOW"
SKIP_BG="$WHITE"

CARD_CONTENT_SKIP=1
CARD_CONTENT_HEIGHT=39


# CARD POSITIONS
STACK_VPOS=1
STACK_HPOS_1=12
STACK_HPOS_2=67
STACK_HPOS_3=122
STACK_HPOS_4=177
STACK_HPOS_5=232
STACK_HPOS_6=287
STACK_HPOS_7=342

# STACK_8=395  for calculations.

PILE_VPOS=80
PILE_HPOS_1=287
PILE_HPOS_2=342

ACE_PILE_VPOS=80
ACE_PILE_HPOS_1=12
ACE_PILE_HPOS_2=67
ACE_PILE_HPOS_3=122
ACE_PILE_HPOS_4=177

ACE=1
JACK=11
QUEEN=12
KING=13

HEARTS=1
DIAMONDS=2
CLUBS=3
SPADES=4

FRONT=0
BACK=1
OUTLINE=2
CLEAROUTLINE=3
OUTLINETOP=4
TOP=5
BACKTOP=6
SKIP=7
ACEOUTLINE=8

TTYSETTING='raw' # or cbreak

LL=`stty -a | grep rows | sed 's/^.*;\(.*\)rows\(.*\);.*$/\1\2/' | sed 's/;.*$//' | sed 's/[^0-9]//g'` # ROWS
LC=`stty -a | grep columns | sed 's/^.*;\(.*\)columns\(.*\);.*$/\1\2/' | sed 's/;.*$//' | sed 's/[^0-9]//g'` # COLUMNS
if [ $LC -lt  400 ] ; then
        echo "This game requires at least an 400-column by 120 row terminal display.  Sorry."
        exit 0;
fi
if [ $LL -lt  120 ] ; then
        echo "This game requires at least an 400-column by 120 row terminal display.  Sorry."
        exit 0;
fi

draw_back()
{
    local ROW=$1
    local COL=$2
    local BORDER_COLOR=$3
    local MODE=$4

    local POS=0

    local LIMIT=$CARD_CONTENT_HEIGHT

    if [ $MODE -eq $TOP -o $MODE -eq $BACKTOP ] ; then
	LIMIT=2
    fi

    vtsetpos $ROW $COL
    printf "%s" "$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE.zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz.$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE"
    while [ $POS -lt $LIMIT ] ; do
	local LINE="$(eval echo "$"BACK_ROW_$(($POS + $CARD_CONTENT_SKIP)))"
	vtsetpos $(($ROW + $POS + 1)) $COL
	printf "%s" "$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE""X$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$BLACK$ATTRIBUTE$LINE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE""X$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE"
	POS=$(($POS + 1))
    done
    if [ $MODE -ne $TOP -a $MODE -ne $BACKTOP ] ; then
	vtsetpos $(($ROW + $POS + 1)) $COL
	printf "%s" "$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'$SET$CARD_BORDER_FG_BG$SCREEN_BGCOLOR$ATTRIBUTE"
    fi
}

draw_front_outline()
{
    local ROW=$1
    local COL=$2
    local BORDER_COLOR=$3
    local BACKGROUND_COLOR=$4
    local DRAW_MODE=$5

    local POS=0
    local LIMIT=$CARD_CONTENT_HEIGHT
    if [ $DRAW_MODE -eq $TOP -o $DRAW_MODE -eq $OUTLINETOP ] ; then
	LIMIT=2
    fi

    vtsetpos $ROW $COL
    printf "%s" "$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE.zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz.$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE"
    # 45 spaces
    local LINE="                                             "
    # echo DM: $DRAW_MODE > /tmp/card_draw_mode
    if [ $DRAW_MODE -eq $OUTLINE -o $DRAW_MODE -eq $OUTLINETOP ] ; then
	LINE="$(echo "$LINE" | sed 's/ /[1C/g')"
    fi
    while [ $POS -lt $LIMIT ] ; do
	vtsetpos $(($ROW + $POS + 1)) $COL
	printf "%s" "$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE""X$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$BACKGROUND_COLOR$ATTRIBUTE$LINE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE""X$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE"
	POS=$(($POS + 1))
    done
    if [ $DRAW_MODE != $TOP ] ; then
	vtsetpos $(($ROW + $POS + 1)) $COL
	printf "%s" "$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE'zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz'$SET$CARD_BORDER_FG_BG$SCREEN_BGCOLOR$ATTRIBUTE"
    fi
}

draw_skip()
{
    local ROW=$1
    local COL=$2

    vtsetpos $ROW $COL
    printf "%s" "$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE'$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SKIP_BG$ATTRIBUTE$SET$FOREGROUND$SKIP_FG$ATTRIBUTE=============================================$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE'$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE"
    vtsetpos $(($ROW + 1)) $COL
    printf "%s" "$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE'$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SKIP_BG$ATTRIBUTE$SET$FOREGROUND$SKIP_FG$ATTRIBUTE=============================================$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE'$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE"
    # printf "%s" "$SET$BACKGROUND$SKIP_BG$ATTRIBUTE$SET$FOREGROUND$SKIP_FG$ATTRIBUTE'============================================='$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE"
    vtsetpos $(($ROW + 2)) $COL
    printf "%s" "$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE'$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SKIP_BG$ATTRIBUTE$SET$FOREGROUND$SKIP_FG$ATTRIBUTE=============================================$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$CARD_BORDER_FG_BG$BORDER_COLOR$ATTRIBUTE'$SET$CARD_BORDER_FG_BG$SCREEN_FGCOLOR$ATTRIBUTE"
    # printf "%s" "$SET$BACKGROUND$SKIP_BG$ATTRIBUTE$SET$FOREGROUND$SKIP_FG$ATTRIBUTE'============================================='$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE"

}

draw_ace_background()
{
    local ROW=$1
    local COL=$2

    local POS=1
    local BASEROW=$((($CARD_CONTENT_HEIGHT / 2) - 14 + $ROW + 1))
    local BASECOL=$(($COL + 10))
    while [ $POS -lt 30 ] ; do
	LINE="$(eval echo "\"\$ACE_BG_$POS\"")"

	vtsetpos $(($BASEROW + $POS)) $(($BASECOL))
	printf "%s" "$SET$FOREGROUND$ACE_FG$ATTRIBUTE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$LINE$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE"
	POS=$(($POS + 1))
    done
}

draw_card()
{
    local ROW=$1
    local COL=$2
    local DRAW_MODE=$3
    local BORDER_COLOR=$4
    local CARDNUM=$5
    local SUIT=$6

    # echo "1=$1 2=$2 3=$3 4=$4 5=$5 6=$6" >> /tmp/drawlog
	# echo "ACEOUTLINE $ACEOUTLINE ($DRAW_MODE)" >> /tmp/aceinfo
	# echo "draw_front_outline $ROW $COL $BORDER_COLOR $SCREEN_BGCOLOR $DRAW_MODE" >> /tmp/aceinfo

    if [ $DRAW_MODE -eq $SKIP ] ; then
	draw_skip $ROW $COL
	return
    elif [ $DRAW_MODE -eq $BACK -o $DRAW_MODE -eq $BACKTOP ] ; then
	draw_back $ROW $COL $BORDER_COLOR $DRAW_MODE
	return
    elif [ $DRAW_MODE -eq $OUTLINE -o $DRAW_MODE -eq $OUTLINETOP -o $DRAW_MODE -eq $CLEAROUTLINE ] ; then
	draw_front_outline $ROW $COL $BORDER_COLOR $SCREEN_BGCOLOR $DRAW_MODE
	return
    elif [ $DRAW_MODE -eq $ACEOUTLINE ] ; then
	draw_front_outline $ROW $COL $BORDER_COLOR $SCREEN_BGCOLOR $DRAW_MODE
	draw_ace_background $ROW $COL
	return
    fi

    draw_front_outline $ROW $COL $BORDER_COLOR $WHITE $DRAW_MODE
    # echo "CARDNUM: $CARDNUM"

    # get card number text
    local NUM_TEXT_LEN="$(eval echo "\"\$"CARD_$CARDNUM"_LEN\"")"
    local NUM_TEXT_0="$(eval echo "\"\$"CARD_$CARDNUM"_0\"")"
    local NUM_TEXT_1="$(eval echo "\"\$"CARD_$CARDNUM"_1\"")"

    local NUM_TEXT_0_ROT=""
    local NUM_TEXT_1_ROT=""

    if [ $DRAW_MODE -ne $TOP ] ; then
	# get rotated card number text
	NUM_TEXT_0_ROT="$(eval echo "\"\$"CARD_$CARDNUM"_0R\"")"
	NUM_TEXT_1_ROT="$(eval echo "\"\$"CARD_$CARDNUM"_1R\"")"
    fi

    local CARD_COLOR=""
    if [ $SUIT -eq $HEARTS -o $SUIT -eq $DIAMONDS ] ; then
	CARD_COLOR="$RED"
    else
	CARD_COLOR="$BLACK"
    fi

    local SM_ICON_LEN="$(eval echo "\"\$"LITTLE_$SUIT"_LEN\"")"
    local SM_ICON_0="$(eval echo "\"\$"LITTLE_$SUIT"_0\"")"
    local SM_ICON_1="$(eval echo "\"\$"LITTLE_$SUIT"_1\"")"
    local SM_ICON_0_ROT=""
    local SM_ICON_1_ROT=""

    if [ $DRAW_MODE != $TOP ] ; then
	SM_ICON_0_ROT="$(eval echo "\"\$"LITTLE_$SUIT"_0R\"")"
	SM_ICON_1_ROT="$(eval echo "\"\$"LITTLE_$SUIT"_1R\"")"
    fi

    local STR_WIDTH=$(( $SM_ICON_LEN + $NUM_TEXT_LEN ))

    # print card number
    vtsetpos $(($ROW + 1)) $(($COL + 3))
    printf "%s" "$SET$ENABLE$BOLD$ATTRIBUTE$SET_$SET$FOREGROUND$CARD_COLOR$ATTRIBUTE$SET$BACKGROUND$WHITE$ATTRIBUTE$NUM_TEXT_0$SM_ICON_0$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$DISABLE$BOLD$ATTRIBUTE"
    vtsetpos $(($ROW + 2)) $(($COL + 3))
    printf "%s" "$SET$ENABLE$BOLD$ATTRIBUTE$SET$FOREGROUND$CARD_COLOR$ATTRIBUTE$SET$BACKGROUND$WHITE$ATTRIBUTE$NUM_TEXT_1$SM_ICON_1$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$DISABLE$BOLD$ATTRIBUTE"

    if [ $DRAW_MODE != $TOP ] ; then
	# print rotated card number
	vtsetpos $(($ROW + $CARD_CONTENT_HEIGHT - 1)) $(($COL + 44 - $STR_WIDTH))
	printf "%s" "$SET$ENABLE$BOLD$ATTRIBUTE$SET$FOREGROUND$CARD_COLOR$ATTRIBUTE$SET$BACKGROUND$WHITE$ATTRIBUTE$SM_ICON_0_ROT$NUM_TEXT_0_ROT$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$DISABLE$BOLD$ATTRIBUTE"
	vtsetpos $(($ROW + $CARD_CONTENT_HEIGHT)) $(($COL + 44 - $STR_WIDTH))
	printf "%s" "$SET$ENABLE$BOLD$ATTRIBUTE$SET$FOREGROUND$CARD_COLOR$ATTRIBUTE$SET$BACKGROUND$WHITE$ATTRIBUTE$SM_ICON_1_ROT$NUM_TEXT_1_ROT$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$DISABLE$BOLD$ATTRIBUTE"

	# draw face
	if [ $CARDNUM -eq $ACE ] ; then
		# Ace
		local POS=0
		local BASEROW=$((($CARD_CONTENT_HEIGHT / 2) - 7 + $ROW + 1))
		local BASECOL=$(($COL + 11))
		# echo "BR: $BASEROW BC: $BASECOL"
		printf "%s" "$SET$BACKGROUND$WHITE$ATTRIBUTE$SET$FOREGROUND$CARD_COLOR$ATTRIBUTE"
		while [ $POS -le 14 ] ; do
			local LINE="$(eval echo "\"\$BIG_$SUIT""_$POS\"")"
			vtsetpos $(( $BASEROW + $POS )) $BASECOL
			printf "%s" "$LINE"
			POS=$(($POS + 1))
		done
		printf "%s" "$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE"
	elif [ $CARDNUM -eq $JACK -o $CARDNUM -eq $QUEEN -o $CARDNUM -eq $KING ] ; then
		# Face card
		local BASEROW=$((($CARD_CONTENT_HEIGHT / 2) - 14 + $ROW + 1))
		local BASECOL=$(($COL + 10))
		# echo "BR: $BASEROW BC: $BASECOL"
		printf "%s" "$SET$BACKGROUND$WHITE$ATTRIBUTE$SET$FOREGROUND$CARD_COLOR$ATTRIBUTE"
		local POS=0
		local LINE=""
		while [ $POS -le 28 ] ; do
			# echo "POSITION: $POS"
			if [ $CARDNUM -eq $JACK ] ; then
				LINE="$(eval echo "\"\$JACK_$SUIT""_$POS\"")"
			elif [ $CARDNUM -eq $QUEEN ] ; then
				LINE="$(eval echo "\"\$QUEEN_$SUIT""_$POS\"")"
			elif [ $CARDNUM -eq $KING ] ; then
				LINE="$(eval echo "\"\$KING_$SUIT""_$POS\"")"
			fi
			vtsetpos $(( $BASEROW + $POS )) $BASECOL
			printf "%s" "$LINE"
			POS=$(($POS + 1))
		done
		printf "%s" "$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE"
	else
		# Number card

		printf "%s" "$SET$BACKGROUND$WHITE$ATTRIBUTE$SET$FOREGROUND$CARD_COLOR$ATTRIBUTE"

		local MED_0="$(eval echo "\"\$MEDIUM_$SUIT""_0\"")"
		local MED_1="$(eval echo "\"\$MEDIUM_$SUIT""_1\"")"
		local MED_2="$(eval echo "\"\$MEDIUM_$SUIT""_2\"")"
		local MED_3="$(eval echo "\"\$MEDIUM_$SUIT""_3\"")"

		local MAP="$(eval echo "\"\$MAP_$CARDNUM\"")"
		while [ "x$MAP" != "x" ] ; do
			local POINT="$(echo $MAP | sed 's/ .*$//')"
			MAP="$(echo $MAP | sed 's/^[0-9][0-9]*,[0-9][0-9]*//')"
			MAP="$(echo $MAP | sed 's/^ //')"

			# echo "MAP: \"$MAP\" POINT: \"$POINT\""

			# echo "POINT: $POINT"
			local POINT_ROW="$(echo $POINT | sed 's/,.*$//')"
			local POINT_COL="$(echo $POINT | sed 's/^.*,//')"

			# echo "POINT_ROW: \"$POINT_ROW\" POINT_COL \"$POINT_COL\""

			vtsetpos $(($ROW + $POINT_ROW)) $(($COL + $POINT_COL))
			printf "%s" "$MED_0"
			vtsetpos $(($ROW + $POINT_ROW + 1)) $(($COL + $POINT_COL))
			printf "%s" "$MED_1"
			vtsetpos $(($ROW + $POINT_ROW + 2)) $(($COL + $POINT_COL))
			printf "%s" "$MED_2"
			vtsetpos $(($ROW + $POINT_ROW + 3)) $(($COL + $POINT_COL))
			printf "%s" "$MED_3"
		done

		printf "%s" "$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE"
	fi
    fi
}


draw_test()
{
local TEST=1

if [ $TEST -eq 1 ] ; then
    # draw_card 50 50 4 $SPADES $BACK
    echo "$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE"
    clear
    draw_card $STACK_VPOS $STACK_HPOS_1 $FRONT $DEFAULT_CARD_BORDER 2 $DIAMONDS
    draw_card $STACK_VPOS $STACK_HPOS_2 $BACK $SOURCE_CARD_BORDER 4 $SPADES
    draw_card $STACK_VPOS $STACK_HPOS_3 $BACK $DESTINATION_CARD_BORDER 4 $SPADES
    draw_card $STACK_VPOS $STACK_HPOS_4 $BACK $DEFAULT_CARD_BORDER 4 $SPADES
    draw_card $STACK_VPOS $STACK_HPOS_5 $BACK $DEFAULT_CARD_BORDER 4 $SPADES
    draw_card $STACK_VPOS $STACK_HPOS_6 $BACK $DEFAULT_CARD_BORDER 4 $SPADES
    draw_card $STACK_VPOS $STACK_HPOS_7 $FRONT $DEFAULT_CARD_BORDER 4 $SPADES

    draw_card $(($STACK_VPOS + 3))  $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER $KING $SPADES
    draw_card $(($STACK_VPOS + 6)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER $QUEEN $HEARTS
    draw_card $(($STACK_VPOS + 9)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER $JACK $CLUBS
    draw_card $(($STACK_VPOS + 12)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER 10 $DIAMONDS
    draw_card $(($STACK_VPOS + 15)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER 9 $SPADES
    draw_card $(($STACK_VPOS + 18)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER 8 $HEARTS
    draw_card $(($STACK_VPOS + 21)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER 7 $CLUBS
    draw_card $(($STACK_VPOS + 24)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER 6 $DIAMONDS
    draw_card $(($STACK_VPOS + 27)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER 5 $SPADES
    draw_card $(($STACK_VPOS + 30)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER 4 $HEARTS
    draw_card $(($STACK_VPOS + 33)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER 3 $CLUBS
    draw_card $(($STACK_VPOS + 36)) $STACK_HPOS_6 $TOP $DEFAULT_CARD_BORDER 2 $DIAMONDS
    draw_card $(($STACK_VPOS + 39)) $STACK_HPOS_6 $FRONT $DEFAULT_CARD_BORDER $ACE $HEARTS
    #draw_card $(($STACK_VPOS + 39)) $STACK_HPOS_6 $FRONT $DEFAULT_CARD_BORDER $ACE $DIAMONDS
    #draw_card $(($STACK_VPOS + 39)) $STACK_HPOS_6 $FRONT $DEFAULT_CARD_BORDER $ACE $CLUBS
    #draw_card $(($STACK_VPOS + 39)) $STACK_HPOS_6 $FRONT $DEFAULT_CARD_BORDER $ACE $SPADES

    draw_card $PILE_VPOS $PILE_HPOS_1 $BACK $DEFAULT_CARD_BORDER
# 4 $SPADES
    draw_card $PILE_VPOS $PILE_HPOS_2 $CLEAROUTLINE $OUTLINE_CARD_BORDER
#  4 $SPADES
    vtsetpos 100 1

elif [ $TEST -eq 2 ] ; then
    echo "$SET$ENABLE$BOLD$ATTRIBUTE"
    echo "$CARD_0_0 $CARD_1_0 $CARD_2_0 $CARD_3_0 $CARD_4_0 $CARD_5_0 $CARD_6_0 $CARD_7_0 $CARD_8_0 $CARD_9_0 $CARD_10_0 $CARD_11_0 $CARD_12_0 $CARD_13_0"
    echo "$CARD_0_1 $CARD_1_1 $CARD_2_1 $CARD_3_1 $CARD_4_1 $CARD_5_1 $CARD_6_1 $CARD_7_1 $CARD_8_1 $CARD_9_1 $CARD_10_1 $CARD_11_1 $CARD_12_1 $CARD_13_1"
    echo
    echo
    echo "$CARD_0_0R $CARD_1_0R $CARD_2_0R $CARD_3_0R $CARD_4_0R $CARD_5_0R $CARD_6_0R $CARD_7_0R $CARD_8_0R $CARD_9_0R $CARD_10_0R $CARD_11_0R $CARD_12_0R $CARD_13_0R"
    echo "$CARD_0_1R $CARD_1_1R $CARD_2_1R $CARD_3_1R $CARD_4_1R $CARD_5_1R $CARD_6_1R $CARD_7_1R $CARD_8_1R $CARD_9_1R $CARD_10_1R $CARD_11_1R $CARD_12_1R $CARD_13_1R"

    echo "$SET$DISABLE$BOLD$ATTRIBUTE"
elif [ $TEST -eq 3 ] ; then
    clear
    echo "$LITTLE_1_0 $LITTLE_2_0 $LITTLE_3_0 $LITTLE_4_0"
    echo "$LITTLE_1_1 $LITTLE_2_1 $LITTLE_3_1 $LITTLE_4_1"
    echo;echo;echo
    echo "$LITTLE_1_0R $LITTLE_2_0R $LITTLE_3_0R $LITTLE_4_0R"
    echo "$LITTLE_1_1R $LITTLE_2_1R $LITTLE_3_1R $LITTLE_4_1R"
    echo;echo;echo
    echo "$MEDIUM_1_0 $MEDIUM_2_0 $MEDIUM_3_0 $MEDIUM_4_0"
    echo "$MEDIUM_1_1 $MEDIUM_2_1 $MEDIUM_3_1 $MEDIUM_4_1"
    echo "$MEDIUM_1_2 $MEDIUM_2_2 $MEDIUM_3_2 $MEDIUM_4_2"
    echo "$MEDIUM_1_3 $MEDIUM_2_2 $MEDIUM_3_3 $MEDIUM_4_3"
    echo;echo;echo

fi

}

draw_stack()
{
    local STACK_NUM=$1
    local OUTLINE_ONLY=$2

    if [ $STACK_NUM -gt 7 -a $STACK_NUM -le 11 ] ; then
	# echo "draw_stack STACK_NUM $STACK_NUM OUTLINE_ONLY $OUTLINE_ONLY" >> /tmp/drawlog
	# echo "(Calling draw_aces "$(( $STACK_NUM - 7 ))")" >> /tmp/drawlog
	draw_aces $(( $STACK_NUM - 7 ))
	return;
    elif [ $STACK_NUM -eq 12 ] ; then
	# echo "draw_stack STACK_NUM $STACK_NUM OUTLINE_ONLY $OUTLINE_ONLY" >> /tmp/drawlog
	# echo "(Calling draw_piles 2)" >> /tmp/drawlog
	draw_piles 2
	return;
    fi

    # echo "draw_stack STACK_NUM $STACK_NUM OUTLINE_ONLY $OUTLINE_ONLY" >> /tmp/drawlog

    local HPOS="$(eval echo "\"\$STACK_HPOS_$STACK_NUM\"")"
    local VPOS="$STACK_VPOS"
    local POS=1

    local STACK="$(eval echo "\"\$STACK_$STACK_NUM\"")"
    local COVERED_COUNT="$(eval echo "\"\$COV_$STACK_NUM\"")"

# echo "STACK IS $STACK"
    local STACK_COUNT="$(echo "$STACK" | tr ' ' '\n' | grep -c .)"
    local SKIP_AT=0
    local SKIP_COUNT=0

    # STACK_COUNT=15
    if [ $STACK_COUNT -gt $STACK_MAX ] ; then
	local UNCOVERED=$(($STACK_COUNT - $COVERED_COUNT))
	SKIP_AT=$((($UNCOVERED / 2) + $COVERED_COUNT))
	SKIP_COUNT=$(($STACK_COUNT - $STACK_MAX))
    fi
# echo "SC: $STACK_COUNT" >> /tmp/cardcounts
# echo "SA: $SKIP_AT (COVERED: $COVERED_COUNT, SKIP_COUNT: $SKIP_COUNT)" >> /tmp/cardcounts
    # SKIP_AT=3
    # SKIP_COUNT=2
    # COVERED_COUNT=0

    if [ "x$STACK" = "x" ] ; then
	if [ $STACK_NUM -eq $DESTINATION_STACK ] ; then
		draw_card $VPOS $HPOS $CLEAROUTLINE $DESTINATION_CARD_BORDER
	else
		draw_card $VPOS $HPOS $CLEAROUTLINE $OUTLINE_CARD_BORDER
	fi
	POS=2
	VPOS=$(( $VPOS + 3 ))
    else
	BORDER="$DEFAULT_CARD_BORDER"
	while [ "x$STACK" != "x" ] ; do
		local CARD="$(echo $STACK | sed 's/ .*$//')"
		STACK="$(echo $STACK | sed 's/^[0-9][0-9]*,[0-9][0-9]*//')"
		STACK="$(echo $STACK | sed 's/^ //')"

		if [ $SOURCE_STACK -eq $STACK_NUM -a $(( $STACK_COUNT - $POS )) -lt $SELECT_DEPTH ] ; then
			BORDER="$SOURCE_CARD_BORDER"
		elif [ $DESTINATION_STACK -eq $STACK_NUM -a $STACK_COUNT -eq $POS ] ; then
			BORDER="$DESTINATION_CARD_BORDER"
		fi

		# echo "STACK: \"$STACK\" CARD: \"$CARD\""

		# echo "CARD: $CARD"
		local CARD_SUIT="$(echo $CARD | sed 's/,.*$//')"
		local CARD_NUM="$(echo $CARD | sed 's/^.*,//')"

		# echo "CARD_SUIT: \"$CARD_SUIT\" CARD_NUM \"$CARD_NUM\"" >> /tmp/cardlist
		# echo "COVERED_COUNT: $COVERED_COUNT" >> /tmp/cardlist
		# echo "POS: $POS" >> /tmp/cardlist

		local COVERED=$TOP
		if [ "x$STACK" = "x" ] ; then
			COVERED=$FRONT
		fi
		if [ $POS -eq $SKIP_AT ] ; then
			COVERED=$SKIP
			while [ $SKIP_COUNT -gt 0 ] ; do
				STACK="$(echo $STACK | sed 's/^[0-9][0-9]*,[0-9][0-9]*//')"
				STACK="$(echo $STACK | sed 's/^ //')"
				SKIP_COUNT=$(($SKIP_COUNT - 1))
				COVERED_COUNT=$(($COVERED_COUNT - 1))
				STACK_COUNT=$(($STACK_COUNT - 1))
			done
		elif [ $POS -le $COVERED_COUNT ] ; then
			if [ "x$STACK" = "x" ] ; then
				COVERED=$BACK
			else
				COVERED=$BACKTOP
			fi
		fi
		if [ "x$OUTLINE_ONLY" = "x1" ] ; then
			if [ $COVERED -eq $SKIP ] ;then
				draw_card $VPOS $HPOS $SKIP $BORDER
			elif [ $COVERED -eq $FRONT ] ;then
				draw_card $VPOS $HPOS $OUTLINE $BORDER
			else # TOP
				draw_card $VPOS $HPOS $OUTLINETOP $BORDER
			fi
		else
			draw_card $VPOS $HPOS $COVERED $BORDER $CARD_NUM $CARD_SUIT
		fi
		VPOS=$(($VPOS + 3))
		POS=$(($POS + 1))
	done
    fi

    VPOS=$(($VPOS + $CARD_CONTENT_HEIGHT - 1))

    # echo "CLEARING MARGIN FOR STACK: $STACK_NUM VPOS: $VPOS POS: $VPOS HPOS: $HPOS" >> /tmp/cardmarginlog
    while [ $POS -le $STACK_MAX ] ; do
	clear_margin $VPOS $HPOS
	VPOS=$(($VPOS + 3))
	POS=$(($POS + 1))
    done
}


clear_margin()
{
    local VPOS=$1
    local HPOS=$2

    # 47 spaces
    local LINE="                                               "

    vtsetpos $VPOS $HPOS
    printf "%s" "$LINE"
    vtsetpos $(( $VPOS + 1)) $HPOS
    printf "%s" "$LINE"
    vtsetpos $(( $VPOS + 2)) $HPOS
    printf "%s" "$LINE"

}

draw_piles()
{
	local TOPCARD="$(echo "$PILE_2" | sed 's/^.* //g')"
	local ONLYDRAW=$1

	if [ "x$ONLYDRAW" = "x" -o "x$ONLYDRAW" = "x1" ] ; then
		if [ "x$PILE_1" = "x" ] ; then
			# Deck is empty.  Draw an outline.
			draw_card $PILE_VPOS $PILE_HPOS_1 $CLEAROUTLINE $OUTLINE_CARD_BORDER
		else
			draw_card $PILE_VPOS $PILE_HPOS_1 $BACK $DEFAULT_CARD_BORDER
		fi
	fi

	if [ "x$ONLYDRAW" = "x" -o "x$ONLYDRAW" = "x2" ] ; then
		if [ "x$TOPCARD" = "x" ] ; then
			# We haven't flipped any cards.  Draw an outline on the right.
			if [ $SOURCE_STACK -eq 12 ] ; then
				draw_card $PILE_VPOS $PILE_HPOS_2 $CLEAROUTLINE $SOURCE_CARD_BORDER
			elif [ $DESTINATION_STACK -eq 12 ] ; then
				draw_card $PILE_VPOS $PILE_HPOS_2 $CLEAROUTLINE $DESTINATION_CARD_BORDER
			else
				draw_card $PILE_VPOS $PILE_HPOS_2 $CLEAROUTLINE $OUTLINE_CARD_BORDER
			fi
		else
			# Draw the top card.
			local CARD_SUIT="$(echo $TOPCARD | sed 's/,.*$//')"
			local CARD_NUM="$(echo $TOPCARD | sed 's/^.*,//')"
			if [ $SOURCE_STACK -eq 12 ] ; then
				draw_card $PILE_VPOS $PILE_HPOS_2 $FRONT $SOURCE_CARD_BORDER $CARD_NUM $CARD_SUIT
			elif [ $DESTINATION_STACK -eq 12 ] ; then
				draw_card $PILE_VPOS $PILE_HPOS_2 $FRONT $DESTINATION_CARD_BORDER $CARD_NUM $CARD_SUIT
			else
				draw_card $PILE_VPOS $PILE_HPOS_2 $FRONT $DEFAULT_CARD_BORDER $CARD_NUM $CARD_SUIT
			fi
		fi
	fi
}


draw_aces()
{
	local ONLYDRAW=$1

	if [ "x$ONLYDRAW" = "x" -o "x$ONLYDRAW" = "x1" ] ; then
		if [ "x$ACE_PILE_1" = "x" ] ; then
			# Deck is empty.  Draw an outline.
			if [ $SOURCE_STACK -eq 8 ] ; then
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_1 $ACEOUTLINE $SOURCE_CARD_BORDER
			elif [ $DESTINATION_STACK -eq 8 ] ; then
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_1 $ACEOUTLINE $DESTINATION_CARD_BORDER
			else
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_1 $ACEOUTLINE $OUTLINE_CARD_BORDER
			fi
		else
			local TOPCARD="$(echo "$ACE_PILE_1" | sed 's/^.* //g')"

                	# Draw the top card.
                	local CARD_SUIT="$(echo $TOPCARD | sed 's/,.*$//')"
                	local CARD_NUM="$(echo $TOPCARD | sed 's/^.*,//')"

			if [ $SOURCE_STACK -eq 8 ] ; then
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_1 $FRONT $SOURCE_CARD_BORDER $CARD_NUM $CARD_SUIT
			elif [ $DESTINATION_STACK -eq 8 ] ; then
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_1 $FRONT $DESTINATION_CARD_BORDER $CARD_NUM $CARD_SUIT
			else
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_1 $FRONT $DEFAULT_CARD_BORDER $CARD_NUM $CARD_SUIT
			fi
		fi
	fi
	if [ "x$ONLYDRAW" = "x" -o "x$ONLYDRAW" = "x2" ] ; then
		if [ "x$ACE_PILE_2" = "x" ] ; then
			# Deck is empty.  Draw an outline.
			if [ $SOURCE_STACK -eq 9 ] ; then
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_2 $ACEOUTLINE $SOURCE_CARD_BORDER
			elif [ $DESTINATION_STACK -eq 9 ] ; then
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_2 $ACEOUTLINE $DESTINATION_CARD_BORDER
			else
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_2 $ACEOUTLINE $OUTLINE_CARD_BORDER
			fi
		else
			local TOPCARD="$(echo "$ACE_PILE_2" | sed 's/^.* //g')"

                	# Draw the top card.
                	local CARD_SUIT="$(echo $TOPCARD | sed 's/,.*$//')"
                	local CARD_NUM="$(echo $TOPCARD | sed 's/^.*,//')"
			if [ $SOURCE_STACK -eq 9 ] ; then
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_2 $FRONT $SOURCE_CARD_BORDER $CARD_NUM $CARD_SUIT
			elif [ $DESTINATION_STACK -eq 9 ] ; then
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_2 $FRONT $DESTINATION_CARD_BORDER $CARD_NUM $CARD_SUIT
			else
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_2 $FRONT $DEFAULT_CARD_BORDER $CARD_NUM $CARD_SUIT
			fi
		fi
	fi
	if [ "x$ONLYDRAW" = "x" -o "x$ONLYDRAW" = "x3" ] ; then
		if [ "x$ACE_PILE_3" = "x" ] ; then
			# Deck is empty.  Draw an outline.
			if [ $SOURCE_STACK -eq 10 ] ; then
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_3 $ACEOUTLINE $SOURCE_CARD_BORDER
			elif [ $DESTINATION_STACK -eq 10 ] ; then
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_3 $ACEOUTLINE $DESTINATION_CARD_BORDER
			else
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_3 $ACEOUTLINE $OUTLINE_CARD_BORDER
			fi
		else
			local TOPCARD="$(echo "$ACE_PILE_3" | sed 's/^.* //g')"

                	# Draw the top card.
                	local CARD_SUIT="$(echo $TOPCARD | sed 's/,.*$//')"
                	local CARD_NUM="$(echo $TOPCARD | sed 's/^.*,//')"
			if [ $SOURCE_STACK -eq 10 ] ; then
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_3 $FRONT $SOURCE_CARD_BORDER $CARD_NUM $CARD_SUIT
			elif [ $DESTINATION_STACK -eq 10 ] ; then
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_3 $FRONT $DESTINATION_CARD_BORDER $CARD_NUM $CARD_SUIT
			else
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_3 $FRONT $DEFAULT_CARD_BORDER $CARD_NUM $CARD_SUIT
			fi
		fi
	fi
	if [ "x$ONLYDRAW" = "x" -o "x$ONLYDRAW" = "x4" ] ; then
		if [ "x$ACE_PILE_4" = "x" ] ; then
			# Deck is empty.  Draw an outline.
			if [ $SOURCE_STACK -eq 11 ] ; then
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_4 $ACEOUTLINE $SOURCE_CARD_BORDER
			elif [ $DESTINATION_STACK -eq 11 ] ; then
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_4 $ACEOUTLINE $DESTINATION_CARD_BORDER
			else
				draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_4 $ACEOUTLINE $OUTLINE_CARD_BORDER
			fi
		else
			local TOPCARD="$(echo "$ACE_PILE_4" | sed 's/^.* //g')"

                	# Draw the top card.
                	local CARD_SUIT="$(echo $TOPCARD | sed 's/,.*$//')"
                	local CARD_NUM="$(echo $TOPCARD | sed 's/^.*,//')"
			if [ $SOURCE_STACK -eq 11 ] ; then
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_4 $FRONT $SOURCE_CARD_BORDER $CARD_NUM $CARD_SUIT
			elif [ $DESTINATION_STACK -eq 11 ] ; then
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_4 $FRONT $DESTINATION_CARD_BORDER $CARD_NUM $CARD_SUIT
			else
                		draw_card $ACE_PILE_VPOS $ACE_PILE_HPOS_4 $FRONT $DEFAULT_CARD_BORDER $CARD_NUM $CARD_SUIT
			fi
		fi
	fi

}


draw_board()
{
    echo "$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE"
    # clear
    echon "[2J"

    print_blanks

    draw_stack 1
    draw_stack 2
    draw_stack 3
    draw_stack 4
    draw_stack 5
    draw_stack 6
    draw_stack 7

    draw_piles

    draw_aces
	# flip_deck
    vtsetpos 100 1
}


if [ "x$INITIALIZED" = "x" ] ; then
	draw_test
fi

INITIALIZED=1

