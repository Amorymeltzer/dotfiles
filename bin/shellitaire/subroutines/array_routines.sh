#!/bin/sh


randcards()
{
    local LIST=""
    local SUIT=1
    while [ $SUIT -le -4 ] ; do
	local CARD=$ACE
	while [ $CARD -le $KING ] ; do
		LIST="$LIST $SUIT,$CARD"
		CARD=$(($CARD + 1))
	done
	SUIT=$(($SUIT + 1))
    done
    LIST="$(echo "$LIST" | sed 's/^ //')"
    echo "$LIST"
}


init_stacks_test()
{
	# Deck
	PILE_1="4,9 4,10 4,11 4,12"

	# Discard pile
	PILE_2=""

	# Stacks of cards
	STACK_1="1,4 1,5 1,6 1,7 1,8 1,9 1,10"
	COV_1=0
	STACK_2="2,1 2,2 2,3 2,4 2,5 2,6"
	COV_2=5
	STACK_3="3,1 3,2 3,3 3,4 3,5"
	COV_3=4
	STACK_4="4,1 4,2 4,3 4,4"
	COV_4=3
	STACK_5="2,7 3,6 3,7"
	COV_5=2
	STACK_6="4,5 4,6"
	COV_6=1
	# STACK_7="4,7"
	STACK_7="1,1 1,2 1,3 1,4 1,5 1,6 1,7 1,8 1,9 1,10 1,11 1,12 1,13 2,1"
	COV_7=0
}


randcard()
{
	TESTMODE=0

	if [ $TESTMODE -eq 1 ] ; then
		echo 7
	else
		# Linux workaround: The tr command below shouldn't be
		# necessary, but for some reason, GNU sed seems to think
		# that non-printable characters are in the set [a-zA-Z]....

		# Mac OS X 10.5 workaround: tr can't handle non-printable characters on input and sed now behaves like
		# GNU sed.  Have to use awk.  This is, unfortunately, much slower, but should work anywhere....

		# CH="$(dd if=/dev/urandom bs=10 count=1 2>/dev/null | tr '\n' ' ' | tr -cd '[:print:]' | sed 's/^[^a-zA-Z]*//g' | sed 's/^\(.\).*$/\1/g')"

		CH="$(dd if=/dev/urandom bs=1 count=1 2>/dev/null | awk '$0 ~ /[a-zA-Z]/ { print $0; }')"

		while [ "x$CH" = "x" ] ; do
			# CH="$(dd if=/dev/urandom bs=10 count=1 2>/dev/null | tr '\n' ' ' | tr -cd '[:print:]' | sed 's/^[^a-zA-Z]*//g' | sed 's/^\(.\).*$/\1/g')"
			CH="$(dd if=/dev/urandom bs=1 count=1 2>/dev/null | awk '$0 ~ /[a-zA-Z]/ { print $0; }')"
		done
		NUM="$(eval echo "\$VAL_FOR_CHAR_$CH")"
		echo $NUM
		# echo "CHAR: \"$CH\" NUM: $NUM" >> /tmp/randdebug
	fi
}


shuffle_deck()
{
	# echo "POINT1" 1>&2
	local COUNT=0
	while [ $COUNT -lt 52 ] ; do
		eval "CARD_USED_$COUNT=0"
		COUNT=$(($COUNT + 1))
	done

	# echo "POINT2" 1>&2
	COUNT=0
	while [ $COUNT -lt 52 ] ; do
		# for now
		CHOSEN="$(randcard)"

		# echo "POINT3 CHOSEN='$CHOSEN'" 1>&2
		USED="$(eval echo "\$CARD_USED_$CHOSEN")"
		# echo "USED: $USED" >> /tmp/cardcheck

		# Increment card until we find one above the randomly-chosen
		# card that hasn't been used.
		while [ $USED = 1 ] ; do
			# echo "USED WAS $USED, CHOSEN WAS $CHOSEN" >> /tmp/cardcheck
			CHOSEN=$(($CHOSEN + 1))
			if [ $CHOSEN -ge 52 ] ; then CHOSEN=0 ; fi
			USED="$(eval echo "\$CARD_USED_$CHOSEN")"
		done
		eval "CARD_USED_$CHOSEN=1"

		SUIT=$((($CHOSEN / $KING) + 1))
		CARD=$((($CHOSEN % $KING) + 1))

		DECK="$DECK $SUIT,$CARD"

		COUNT=$(($COUNT + 1))
	done
		# echo "POINT4" 1>&2
	DECK="$(echo "$DECK" | sed 's/^ //')"
	# echo "DECK: $DECK" > /tmp/deck
}


init_stacks()
{
	# init_stacks_test ; return

	shuffle_deck

	COUNT=7
	while [ $COUNT -gt 0 ] ; do
		POS=1
		while [ $POS -le $COUNT ] ; do
			CARD="$(echo "$DECK" | sed 's/^.* //')"
			DECK="$(echo "$DECK" | sed 's/[0-9][0-9]*,[0-9][0-9]*$//')"
			DECK="$(echo "$DECK" | sed 's/ $//')"

			eval STACK_$POS="\"\$STACK_$POS $CARD\""
			POS=$(($POS + 1))
			# echo "added $CARD to stack $POS." >> /tmp/cardstacks
		done
		COUNT=$(($COUNT - 1))
	done

	STACK_1="$(echo "$STACK_1" | sed 's/^ //')"
	STACK_2="$(echo "$STACK_2" | sed 's/^ //')"
	STACK_3="$(echo "$STACK_3" | sed 's/^ //')"
	STACK_4="$(echo "$STACK_4" | sed 's/^ //')"
	STACK_5="$(echo "$STACK_5" | sed 's/^ //')"
	STACK_6="$(echo "$STACK_6" | sed 's/^ //')"
	STACK_7="$(echo "$STACK_7" | sed 's/^ //')"

	# The first few cards are flipped over.
	COV_1=6
	COV_2=5
	COV_3=4
	COV_4=3
	COV_5=2
	COV_6=1
	COV_7=0

	PILE_1="$DECK"

}


flip_deck()
{
	local PULLED

	if [ "x$PILE_1" != "x" ] ; then
		# echo "Flipping." >> /tmp/pilecheck
		# echo "PILE_1 $PILE_1" >> /tmp/pilecheck
		# echo "PILE_2 $PILE_2" >> /tmp/pilecheck

		PULLED="$(echo "$PILE_1" | sed 's/ .*$//')"
		PILE_1="$(echo "$PILE_1" | sed 's/^[0-9][0-9]*,[0-9][0-9]*//')"
		PILE_1="$(echo "$PILE_1" | sed 's/^ //')"
		PILE_2="$PILE_2 $PULLED"
		PILE_2="$(echo "$PILE_2" | sed 's/^ //')"

		# echo "PULLED: $PULLED" >> /tmp/pilecheck
		# echo "PILE_1 $PILE_1" >> /tmp/pilecheck
		# echo "PILE_2 $PILE_2" >> /tmp/pilecheck

		if [ $THREEATONCE = 1 ] ; then
			if [ "x$PILE_1" != "x" ] ; then
				PULLED="$(echo "$PILE_1" | sed 's/ .*$//')"
				PILE_1="$(echo "$PILE_1" | sed 's/^[0-9][0-9]*,[0-9][0-9]*//')"
				PILE_1="$(echo "$PILE_1" | sed 's/^ //')"
				PILE_2="$PILE_2 $PULLED"
				PILE_2="$(echo "$PILE_2" | sed 's/^ //')"

				if [ "x$PILE_1" != "x" ] ; then
					PULLED="$(echo "$PILE_1" | sed 's/ .*$//')"
					PILE_1="$(echo "$PILE_1" | sed 's/^[0-9][0-9]*,[0-9][0-9]*//')"
					PILE_1="$(echo "$PILE_1" | sed 's/^ //')"
					PILE_2="$PILE_2 $PULLED"
					PILE_2="$(echo "$PILE_2" | sed 's/^ //')"
				fi
			fi
		fi
	else
		# Discard pile contains all the cards in the deck.
		# Reset the deck.
		PILE_1="$PILE_2"
		PILE_2=""

		if [ $SOURCE_STACK -eq 12 ] ; then
			SOURCE_STACK=1
	                update_stack_highlight $SOURCE_STACK
		fi
	fi
	draw_piles
}


VAL_FOR_CHAR_a=0
VAL_FOR_CHAR_b=1
VAL_FOR_CHAR_c=2
VAL_FOR_CHAR_d=3
VAL_FOR_CHAR_e=4
VAL_FOR_CHAR_f=5
VAL_FOR_CHAR_g=6
VAL_FOR_CHAR_h=7
VAL_FOR_CHAR_i=8
VAL_FOR_CHAR_j=9
VAL_FOR_CHAR_k=10
VAL_FOR_CHAR_l=11
VAL_FOR_CHAR_m=12
VAL_FOR_CHAR_n=13
VAL_FOR_CHAR_o=14
VAL_FOR_CHAR_p=15
VAL_FOR_CHAR_q=16
VAL_FOR_CHAR_r=17
VAL_FOR_CHAR_s=18
VAL_FOR_CHAR_t=19
VAL_FOR_CHAR_u=20
VAL_FOR_CHAR_v=21
VAL_FOR_CHAR_w=22
VAL_FOR_CHAR_x=23
VAL_FOR_CHAR_y=24
VAL_FOR_CHAR_z=25
VAL_FOR_CHAR_A=26
VAL_FOR_CHAR_B=27
VAL_FOR_CHAR_C=28
VAL_FOR_CHAR_D=29
VAL_FOR_CHAR_E=30
VAL_FOR_CHAR_F=31
VAL_FOR_CHAR_G=32
VAL_FOR_CHAR_H=33
VAL_FOR_CHAR_I=34
VAL_FOR_CHAR_J=35
VAL_FOR_CHAR_K=36
VAL_FOR_CHAR_L=37
VAL_FOR_CHAR_M=38
VAL_FOR_CHAR_N=39
VAL_FOR_CHAR_O=40
VAL_FOR_CHAR_P=41
VAL_FOR_CHAR_Q=42
VAL_FOR_CHAR_R=43
VAL_FOR_CHAR_S=44
VAL_FOR_CHAR_T=45
VAL_FOR_CHAR_U=46
VAL_FOR_CHAR_V=47
VAL_FOR_CHAR_W=48
VAL_FOR_CHAR_X=49
VAL_FOR_CHAR_Y=50
VAL_FOR_CHAR_Z=51


if [ "x$INITIALIZED" = "x" ] ; then
	ACE=1
	KING=13
	init_stacks
fi

