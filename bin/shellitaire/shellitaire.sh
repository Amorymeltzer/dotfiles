#!/bin/sh

THREEATONCE=1

TTYSETTING='raw -echo' # or cbreak

INITIALIZED=1
GAMEDIR="."

. "$GAMEDIR/subroutines/draw_cards.sh"
. "$GAMEDIR/subroutines/array_routines.sh"
. "$GAMEDIR/subroutines/chartables.sh"
. "$GAMEDIR/subroutines/print_blanks.sh"

SOURCE_STACK=1
DESTINATION_STACK=0
SELECT_DEPTH=1

SELECTING_SOURCE=1
SELECTING_DESTINATION=2

SELECT_MODE=$SELECTING_SOURCE


beep()
{
	DEBUGMSG="$1"

	printf ""

	# echo "$DEBUGMSG" >> /tmp/movedebug
}

update_stack_highlight()
{
	local STACK_NUM=$1

	if [ $STACK_NUM -le 7 ] ; then
		draw_stack $STACK_NUM 1
	elif [ $STACK_NUM -le 11 ] ; then
		draw_aces $(( $STACK_NUM - 7 ))
	else
		draw_piles 2
	fi
}

select_deeper()
{
	local STACK="$(eval echo "\"\$STACK_$SOURCE_STACK\"")"
	local COVER_COUNT="$(eval echo "\"\$COV_$SOURCE_STACK\"")"
	local COUNT="$(echo "$STACK" | tr ' ' '\n' | grep -c .)"

	# This only is used when selecting the source card/cards.
	# Otherwise, we ignore the keystroke
	if [ $SELECT_MODE -eq $SELECTING_SOURCE ] ; then
		# We only want to allow more than one card off the
		# main stacks, not the ace piles or the discard pile
		if [ $SOURCE_STACK -le 7 ] ; then
			if [ $SELECT_DEPTH -lt $(( $COUNT - $COVER_COUNT )) ] ; then
				SELECT_DEPTH=$(($SELECT_DEPTH + 1))
				update_stack_highlight $SOURCE_STACK
			else
				beep "Can't increase depth beyond visible cards (stack $SOURCE_STACK is $STACK)"
			fi
		else
			beep "Can't increase depth for stack $SOURCE_STACK"
		fi
	else
		beep "Can't increase depth of destination stack."
	fi
	# echo "SELECT_DEPTH: $SELECT_DEPTH COUNT: $COUNT COVER_COUNT: $COVER_COUNT" >> /tmp/card_sel_debug
}

select_shallower()
{
	# This only is used when selecting the source card/cards.
	# Otherwise, we ignore the keystroke
	if [ $SELECT_MODE -eq $SELECTING_SOURCE ] ; then
		if [ $SELECT_DEPTH -gt 1 ] ; then
			SELECT_DEPTH=$(($SELECT_DEPTH - 1))
			update_stack_highlight $SOURCE_STACK
		else
			beep "Can't reduce depth below 1."
		fi
	else
		beep "Can't reduce depth of destination stack."
	fi
}

select_left()
{
	if [ $SELECT_MODE -eq $SELECTING_SOURCE ] ; then
		local ORIG_SRC=$SOURCE_STACK
		SELECT_DEPTH=1
		SOURCE_STACK=$(($SOURCE_STACK - 1));
		if [ $SOURCE_STACK -eq 0 ] ; then
			SOURCE_STACK=12
		fi
		update_stack_highlight $ORIG_SRC
		update_stack_highlight $SOURCE_STACK
		local SRC="$(get_stack $SOURCE_STACK)"
		if [ "x$SRC" = "x" ] ; then
			select_left
		fi
	else
		local ORIG_DEST=$DESTINATION_STACK
		DESTINATION_STACK=$(($DESTINATION_STACK - 1));
		if [ $DESTINATION_STACK -eq 0 ] ; then
			if [ $SELECT_DEPTH -eq 1 ] ; then
				DESTINATION_STACK=11
			else
				DESTINATION_STACK=7
			fi
		fi
		update_stack_highlight $ORIG_DEST
		update_stack_highlight $DESTINATION_STACK
		if [ $DESTINATION_STACK -eq $SOURCE_STACK ] ; then
			select_left
		fi
	fi
}

select_right()
{
	if [ $SELECT_MODE -eq $SELECTING_SOURCE ] ; then
		local ORIG_SRC=$SOURCE_STACK
		SELECT_DEPTH=1
		SOURCE_STACK=$(($SOURCE_STACK + 1));
		if [ $SOURCE_STACK -eq 13 ] ; then
			SOURCE_STACK=1
		fi
		update_stack_highlight $ORIG_SRC
		update_stack_highlight $SOURCE_STACK
		local SRC="$(get_stack $SOURCE_STACK)"
		if [ "x$SRC" = "x" ] ; then
			select_right
		fi
	else
		local ORIG_DEST=$DESTINATION_STACK
		DESTINATION_STACK=$(($DESTINATION_STACK + 1));
		if [ $SELECT_DEPTH -eq 1 ] ; then
			if [ $DESTINATION_STACK -eq 12 ] ; then
				DESTINATION_STACK=1
			fi
		else
			# Can't put more than one card on an ace pile
			if [ $DESTINATION_STACK -eq 8 ] ; then
				DESTINATION_STACK=1
			fi
		fi
		update_stack_highlight $ORIG_DEST
		update_stack_highlight $DESTINATION_STACK
		if [ $DESTINATION_STACK -eq $SOURCE_STACK ] ; then
			select_right
		fi
	fi
}

update_instructions()
{
	vtsetpos 90 230
	printf "%s" "$SET$BACKGROUND$WHITE$ATTRIBUTE$SET$FOREGROUND$BLACK$ATTRIBUTE"
	printf "%s" "                                                   "
	vtsetpos 91 230
	printf "%s" "                                                   "
	vtsetpos 92 230
	printf "%s" "                                                   "
	vtsetpos 93 230
	printf "%s" "                                                   "
	vtsetpos 94 230
	printf "%s" "                                                   "
	vtsetpos 95 230
	printf "%s" "                                                   "
	vtsetpos 96 230
	printf "%s" "                                                   "
	vtsetpos 97 230
	printf "%s" "                                                   "
	vtsetpos 98 230
	printf "%s" "                                                   "
	vtsetpos 99 230
	printf "%s" "                                                   "
	vtsetpos 100 230
	printf "%s" "                                                   "
	vtsetpos 101 230
	printf "%s" "                                                   "
	vtsetpos 102 230
	printf "%s" "                                                   "
	vtsetpos 103 230
	printf "%s" "                                                   "

	printf "%s" "$SET$ENABLE$BOLD$ATTRIBUTE"

	printstring 91 234 "Choose"
	printstring 95 252 "a"

	if [ $SELECT_MODE = $SELECTING_SOURCE ] ; then
		printstring 99 234 "source"
	else
		printstring 99 234 "target"
	fi

	printf "%s" "$SET$DISABLE$BOLD$ATTRIBUTE"

	printf "%s" "$SET$BACKGROUND$SCREEN_BGCOLOR$ATTRIBUTE$SET$FOREGROUND$SCREEN_FGCOLOR$ATTRIBUTE"

	return
}


set_source()
{
	SELECT_MODE=$SELECTING_DESTINATION
	DESTINATION_STACK=$(($SOURCE_STACK + 1))
	# if [ $SELECT_DEPTH -gt 1 ] ; then
		if [ $DESTINATION_STACK -gt 7 ] ; then
			DESTINATION_STACK=1
		fi
	# else
		# if [ $DESTINATION_STACK -gt 11 ] ; then
			# DESTINATION_STACK=1
		# fi
	# fi
	update_stack_highlight $DESTINATION_STACK
	update_instructions
}


is_valid_move()
{
	local ON_CARD="$1"
	local NEW_CARD="$2"
	local TARGET_STACK_NUM="$3"
	local DEBUG="$4"

	if [ "x$DEBUG" = "x" ] ; then
		DEBUG=0
	fi
	if [ $DEBUG -eq 1 ] ; then
		echo "IN is_valid_move."
		echo "ON CARD: $ON_CARD"
		echo "NEW CARD: $NEW_CARD"
	fi

	local NEW_CARD_SUIT="$(echo $NEW_CARD | sed 's/,.*$//')"
	local NEW_CARD_NUM="$(echo $NEW_CARD | sed 's/^.*,//')"

	# Placing card on an empty stack or ace pile
	if [ "x$ON_CARD" = "x" ] ; then
		if [ $TARGET_STACK_NUM -le 7 ] ; then
			# Empty stack.  Only allow kings
			if [ $NEW_CARD_NUM -eq $KING ] ; then
				if [ $DEBUG -eq 1 ] ; then
					echo "Valid move: King to empty stack."
				fi
				return $(true)
			fi
			if [ $DEBUG -eq 1 ] ; then
				echo "Invalid move: Non-king to empty stack."
			fi
			return $(false)
		else
			# Empty stack.  Only allow aces
			if [ $NEW_CARD_NUM -eq $ACE ] ; then
				if [ $DEBUG -eq 1 ] ; then
					echo "Valid move: Ace to empty ace pile."
				fi
				return $(true)
			fi
			if [ $DEBUG -eq 1 ] ; then
				echo "Invalid move: Non-ace to empty ace pile."
			fi
			return $(false)
		fi
	fi

	local ON_CARD_SUIT="$(echo $ON_CARD | sed 's/,.*$//')"
	local ON_CARD_NUM="$(echo $ON_CARD | sed 's/^.*,//')"

	# Placing card on another card.
	if [ $TARGET_STACK_NUM -le 7 ] ; then
		if [ $ON_CARD_SUIT -eq $HEARTS -o $ON_CARD_SUIT -eq $DIAMONDS ] ; then
			if [ $NEW_CARD_SUIT -ne $CLUBS -a $NEW_CARD_SUIT -ne $SPADES ] ; then
				if [ $DEBUG -eq 1 ] ; then
					echo "Invalid move: Not opposite color on stack ($NEW_CARD_SUIT suit on $ON_CARD_SUIT)."
				fi
				return $(false)
			fi
		else
			# ON_CARD is either CLUBS or SPADES
			if [ $NEW_CARD_SUIT -ne $HEARTS -a $NEW_CARD_SUIT -ne $DIAMONDS ] ; then
				if [ $DEBUG -eq 1 ] ; then
					echo "Invalid move: Not opposite color on stack ($NEW_CARD_SUIT suit on $ON_CARD_SUIT)."
				fi
				return $(false)
			fi
		fi
		if [ $NEW_CARD_NUM -ne $(( $ON_CARD_NUM - 1 )) ] ; then
			if [ $DEBUG -eq 1 ] ; then
				echo "Invalid move: Card not descending on stack ($NEW_CARD_NUM on $ON_CARD_NUM)."
			fi
			return $(false)
		fi
		if [ $DEBUG -eq 1 ] ; then
			echo "Valid move: Descending, opposite-color addition to pile."
		fi
	else
		if [ $NEW_CARD_SUIT -ne $ON_CARD_SUIT ] ; then
			if [ $DEBUG -eq 1 ] ; then
				echo "Invalid move: Different suit on ace pile ($NEW_CARD_SUIT != $ON_CARD_SUIT)."
			fi
			return $(false)
		fi
		if [ $NEW_CARD_NUM -ne $(( $ON_CARD_NUM + 1 )) ] ; then
			if [ $DEBUG -eq 1 ] ; then
				echo "Invalid move: Card not ascending on ace pile ($NEW_CARD_NUM on $ON_CARD_NUM)."
			fi
			return $(false)
		fi
		if [ $DEBUG -eq 1 ] ; then
			echo "Valid move: Ascending, same-suit addition to pile."
		fi
	fi

	return $(true)
}


set_destination()
{
	# Chose a destination.
	local SRC="$(get_stack $SOURCE_STACK)"
	local DEST="$(get_stack $DESTINATION_STACK)"
	local DESTTOP="$(echo $DEST | sed 's/^.* //')"

	# echo "SRC ($SOURCE_STACK): $SRC" >> /tmp/movedebug
	# echo "DEST ($DESTINATION_STACK): $DEST" >> /tmp/movedebug

	local TEMP=""

	local POS=1
	while [ $POS -le $SELECT_DEPTH ] ; do
		TEMP="$(echo "$SRC" | sed 's/^.* //') $TEMP"
		TEMP="$(echo "$TEMP" | sed 's/ $//')"
		SRC="$(echo "$SRC" | sed 's/[0-9][0-9]*,[0-9][0-9]*$//')"
		SRC="$(echo "$SRC" | sed 's/ $//')"

		POS=$(( $POS + 1 ))
	done

	local TEMPBOTTOM="$(echo $TEMP | sed 's/^ //' | sed 's/ .*$//')"
	DEST="$DEST $TEMP"
	DEST="$(echo "$DEST" | sed 's/^ //')"

	if is_valid_move "$DESTTOP" "$TEMPBOTTOM" "$DESTINATION_STACK" ; then
		# beep "Valid destination.  Message was $(is_valid_move "$DESTTOP" "$TEMPBOTTOM" "$DESTINATION_STACK" 1)"

		local ORIG_DEST=$DESTINATION_STACK
		local SRC_COUNT="$(echo $SRC | tr ' ' '\n' | grep -c '.')"

		if [ $SOURCE_STACK -le 7 ] ; then
			local SRC_COVERED="$(eval echo "\"\$COV_$SOURCE_STACK\"")"

			# If we need to flip the top card, go ahead and flip it.
			if [ $SRC_COUNT -eq $SRC_COVERED -a $SRC_COVERED -gt 0 ] ; then
				eval "COV_$SOURCE_STACK=$(( $SRC_COVERED - 1))"
			fi
		fi

		# Commit the change
		set_stack "$SOURCE_STACK" "$SRC"
		set_stack "$DESTINATION_STACK" "$DEST"

		# Reset selection values
		DESTINATION_STACK=0
		SELECT_DEPTH=1
		SELECT_MODE=$SELECTING_SOURCE

		ORIG_SRC=$SOURCE_STACK
		SOURCE_STACK=1

		draw_stack $ORIG_SRC
		draw_stack $ORIG_DEST
		draw_stack $SOURCE_STACK

		local SRC="$(get_stack $SOURCE_STACK)"
		if [ "x$SRC" = "x" ] ; then
			select_right
		fi

		update_instructions
	else
		beep "Not valid destination.  Message was $(is_valid_move "$DESTTOP" "$TEMPBOTTOM" "$DESTINATION_STACK" 1)"
	fi
}


get_stack()
{
    local STACK="$1"

    if [ $STACK -le 7 ] ; then
	eval echo "\"\$STACK_$STACK\""
    elif [ $STACK -eq 12 ] ; then
	eval echo "\"\$PILE_2\""
    else
	eval echo "\"\$ACE_PILE_$(($STACK - 7))\""
    fi
}


set_stack()
{
    local STACK="$1"
    local DATA="$2"

	# echo "set stack $STACK to $DATA" >> /tmp/movedebug

    if [ $STACK -le 7 ] ; then
	eval "STACK_$STACK=\"$DATA\""
    elif [ $STACK -eq 12 ] ; then
	eval "PILE_2=\"$DATA\""
    else
	eval "ACE_PILE_$(($STACK-7))=\"$DATA\""
    fi
}


cancel_select()
{
		SELECT_MODE=$SELECTING_SOURCE
		local ORIG_DEST=$DESTINATION_STACK
		DESTINATION_STACK=0
		update_stack_highlight $ORIG_DEST
		update_instructions
}


main_loop()
{
    local CONTINUE=1

    update_instructions

    while [ $CONTINUE = 1 ] ; do
	vtsetpos 100 1
	CH="$(dd if=/dev/tty bs=1 count=1 2>/dev/null | sed 's/ /RETURN/')"
	# echo "CH was $CH"
	if [ "$CH" = "q" -o "$CH" = "Q" ] ; then
		CONTINUE=0
	elif [ "$CH" = "f" -o "$CH" = "F" ] ; then
		flip_deck
	elif [ "$CH" = "o" -o "$CH" = "O" ] ; then
		# Cheat.
		local OLDTAO=$THREEATONCE
		THREEATONCE=0
		flip_deck
		THREEATONCE=$OLDTAO
	elif [ "$CH" = "i" -o "$CH" = "I" ] ; then
		select_deeper
	elif [ "$CH" = "m" -o "$CH" = "M" ] ; then
		select_shallower
	elif [ "$CH" = "j" -o "$CH" = "J" ] ; then
		select_left
	elif [ "$CH" = "k" -o "$CH" = "K" ] ; then
		select_right
	elif [ "$CH" = "RETURN" -o "$CH" = " " ] ; then
		if [ $SELECT_MODE = $SELECTING_SOURCE ] ; then
			set_source
		else
			set_destination
		fi
	elif [ "$CH" = "" ] ; then
		if [ $SELECT_MODE = $SELECTING_DESTINATION ] ; then
			# Escape.
			cancel_select
		else
			beep "Hit escape while selecting source."
		fi
	fi
    done
}

reset 2>/dev/null; clear

stty $TTYSETTING

hide_cursor
printstring 55 145 "Shuffling the deck."
printstring 65 167 "Please wait."

# init_stacks_test
init_stacks

clear

draw_board

main_loop
vtsetpos 100 1
printf "%s" "$SET$BACKGROUND$BLACK$ATTRIBUTE$SET$FOREGROUND$WHITE$ATTRIBUTE"
clear
show_cursor
stty sane
reset 2>/dev/null
clear

