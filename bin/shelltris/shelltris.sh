#!/usr/bin/env bash

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# For faster launch, run with the -calibrate.  This sscript will print
# the following values for your machine:

# CALIBRATED="yes"
# ONE_SECOND=5600
# DRAW_PENALTY=1300
# ROT_PENALTY=1200

# Once you have obtained those values, change the numbers above to
# match the actual values for your system, then uncomment the above
# lines.
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

LL=`stty -a | grep rows | sed 's/^.*;\(.*\)rows\(.*\);.*$/\1\2/' | sed 's/;.*$//' | sed 's/[^0-9]//g'` # ROWS
LC=`stty -a | grep columns | sed 's/^.*;\(.*\)columns\(.*\);.*$/\1\2/' | sed 's/;.*$//' | sed 's/[^0-9]//g'` # COLUMNS
if [ $LC -lt 80 ] ; then
	echo "This game requires at least an 80-column terminal display.  Sorry."
	exit 0;
fi


# BASH in Mac OS X 10.4 has a very serious variable handling bug
# in which subroutine arguments overwrite the main arguments, then
# the exit statement within an "if" statement ends up dropping
# out of the "if" statement instead of exiting the subshell.
# Copy the first argument off NOW to prevent bizarre infinite
# recursion later.

ARG1=$1

#Change for testing
INITIAL_SCORE=0

TTYSETTING='raw -echo' # or cbreak

MODE="box"
CURPIECE=""
NEXT="square"
# MODE="block"

TBLOCKCOLOR='[32m'				# green (42 bg, 32 fg)
SBLOCKCOLOR1='[32m'				# green (42 bg, 32 fg)
SBLOCKCOLOR2='[35m'				# purple
SQUARECOLOR='[31m'				# red
LBLOCKCOLOR1='[33m'				# yellow
LBLOCKCOLOR2='[36m'				# cyan
FOURCOLOR='[34m'				# blue
ENDCOLOR='[39m'				# default
BLACK='[38m'					# default
FILLED='[7m'
RESET='[m'
CLEAR='[39m[49m'				# default
WHITE='[37m'					# white

BASELINE=$(( $LL - 1 ))

STARTING_HPOS=$(( ( $LC - 13 ) / 2 ))
STARTING_VPOS=1

LEFTBOUND=$(( $STARTING_HPOS - 10 ))
RIGHTBOUND=$(( $STARTING_HPOS + 10 ))

LBOUND_four_0=1
RBOUND_four_0=2
LBOUND_four_1=0
RBOUND_four_1=0
LBOUND_four_2=1
RBOUND_four_2=2
LBOUND_four_3=0
RBOUND_four_3=0

LBOUND_square_0=1
RBOUND_square_0=0
LBOUND_square_1=1
RBOUND_square_1=0
LBOUND_square_2=1
RBOUND_square_2=0
LBOUND_square_3=1
RBOUND_square_3=0

LBOUND_sblock_0=0
RBOUND_sblock_0=1
LBOUND_sblock_1=1
RBOUND_sblock_1=1
LBOUND_sblock_2=0
RBOUND_sblock_2=1
LBOUND_sblock_3=1
RBOUND_sblock_3=1

LBOUND_sblock_r_0=1
RBOUND_sblock_r_0=0
LBOUND_sblock_r_1=1
RBOUND_sblock_r_1=1
LBOUND_sblock_r_2=1
RBOUND_sblock_r_2=0
LBOUND_sblock_r_3=1
RBOUND_sblock_r_3=1

LBOUND_lblock_0=0
RBOUND_lblock_0=1
LBOUND_lblock_1=1
RBOUND_lblock_1=2
LBOUND_lblock_2=1
RBOUND_lblock_2=0
LBOUND_lblock_3=1
RBOUND_lblock_3=1

LBOUND_lblock_r_0=0
RBOUND_lblock_r_0=1
LBOUND_lblock_r_1=1
RBOUND_lblock_r_1=1
LBOUND_lblock_r_2=1
RBOUND_lblock_r_2=0
LBOUND_lblock_r_3=1
RBOUND_lblock_r_3=1

LBOUND_tblock_0=1
RBOUND_tblock_0=1
LBOUND_tblock_1=0
RBOUND_tblock_1=1
LBOUND_tblock_2=1
RBOUND_tblock_2=1
LBOUND_tblock_3=1
RBOUND_tblock_3=0

# Set the level based on your score.
function set_level()
{
	LEVEL=$(( ( $SCORE / 1000 ) + 1 ))
	CUR_TICK_RATE=`get_tick_rate`
}



## Check block placement routines.  Pass in a row,column pair.  It will
## check to see if the specified block can be placed at the specified
## location.

# Compares four values and prints two values.
function quad_cmp()
{
    local TOPBOUND=0
    local BLOCK=0
    # echo "QC: \"$1\" \"$2\" \"$3\" \"$4\" \"$5\" \"$6\" \"$7\" \"$8\"" >> qc_log

    local R1="$1"
    local C1="$2"
    local R2="$3"
    local C2="$4"
    local R3="$5"
    local C3="$6"
    local R4="$7"
    local C4="$8"

    if [ $R1 -lt 1 ] ; then
	TOPBOUND=1
    else
	local CMD="echo \"\$r$R1""c$C1\""
	# echo "CMD1: $CMD" >> qc_log

	VAL=`eval "$CMD"`
	# echo "VAL1: $VAL" >> qc_log
	if [ "$VAL" = 1 ] ; then
		TOPBOUND=1
	elif [ "$VAL" != 0 ] ; then
		BLOCK=2
	fi
    fi
    if [ $R2 -lt 1 ] ; then
	TOPBOUND=1
    else
	local CMD="echo \"\$r$R2""c$C2\""
	# echo "CMD2: $CMD" >> qc_log

	VAL=`eval "$CMD"`
	# echo "VAL2: $VAL" >> qc_log
	if [ "$VAL" = 1 ] ; then
		TOPBOUND=1
	elif [ "$VAL" != 0 ] ; then
		BLOCK=2
	fi
    fi

    if [ $R3 -lt 1 ] ; then
	TOPBOUND=1
    else
	local CMD="echo \"\$r$R3""c$C3\""
	# echo "CMD3: $CMD" >> qc_log

	VAL=`eval "$CMD"`
	# echo "VAL3: $VAL" >> qc_log
	if [ "$VAL" = 1 ] ; then
		TOPBOUND=1
	elif [ "$VAL" != 0 ] ; then
		BLOCK=2
	fi
    fi

    if [ $R4 -lt 1 ] ; then
	TOPBOUND=1
    else
	local CMD="echo \"\$r$R4""c$C4\""
	# echo "CMD4: $CMD" >> qc_log

	VAL=`eval "$CMD"`
	# echo "VAL4: $VAL" >> qc_log
	if [ "$VAL" = 1 ] ; then
		TOPBOUND=1
	elif [ "$VAL" != 0 ] ; then
		BLOCK=2
	fi
    fi

    # echo "$BLOCK$TOPBOUND" >> qc_log
    echo "$BLOCK$TOPBOUND"
}

# Check if a square can be placed at the specified ROW,COL
function check_square()
{
    local ROW=$1
    local COL=$2
    local ROW2=$(( $ROW - 1 ))
    local COL2=$(( $COL - 1 ))
    local R1="$ROW"
    local C1="$COL"
    local R2="$ROW2"
    local C2="$COL"
    local R3="$ROW"
    local C3="$COL2"
    local R4="$ROW2"
    local C4="$COL2"
    quad_cmp $R1 $C1 $R2 $C2 $R3 $C3 $R4 $C4
}

# Check if a horizontal or vertical line can be placed at the specified ROW,COL
function check_four()
{
    local ROW=$1
    local COL=$2
    ORIENTATION=$(( $3 % 2 ))

	# echo "ROW: $ROW COL: $COL" >> cp_log

    local R1="$ROW"
    local C1="$COL"
    if [ $ORIENTATION -eq 0 ] ; then
	local R2="$ROW"
	local C2="$(( $COL + 1 ))"
	local R3="$ROW"
	local C3="$(( $COL - 1 ))"
	local R4="$ROW"
	local C4="$(( $COL + 2 ))"
    else
	local R2="$(( $ROW - 1 ))"
	local C2="$COL"
	local R3="$(( $ROW - 2 ))"
	local C3="$COL"
	local R4="$(( $ROW - 3 ))"
	local C4="$COL"
    fi
    quad_cmp $R1 $C1 $R2 $C2 $R3 $C3 $R4 $C4
}

# Check if an s-shaped block can be placed at the specified ROW,COL
function check_sblock()
{
    local ROW=$1
    local COL=$2
    local ORIENTATION=$(( $3 % 2 ))

    if [ $ORIENTATION -eq '1' ] ; then
	ROW=$(( $ROW - 1 )) ;
    else
	ROW=$(( $ROW - 2 )) ;
    fi

    local R1="$ROW"
    local C1="$COL"
    if [ $ORIENTATION -eq '0' ] ; then
	local R2="$(( $ROW + 1 ))"
	local C2="$COL"
	local R3="$(( $ROW + 1 ))"
	local C3="$(( $COL + 1 ))"
	local R4="$(( $ROW + 2 ))"
	local C4="$(( $COL + 1 ))"
    else
	local R2="$(( $ROW + 1 ))"
	local C2="$COL"
	local R3="$(( $ROW + 1 ))"
	local C3="$(( $COL - 1 ))"
	local R4="$ROW"
	local C4="$(( $COL + 1 ))"
    fi
    quad_cmp $R1 $C1 $R2 $C2 $R3 $C3 $R4 $C4
}

# Check if an s-shaped block (reversed) can be placed at the specified ROW,COL
function check_sblock_r()
{
    local ROW=$1
    local COL=$2
    local ORIENTATION=$(( $3 % 2 ))

    if [ $ORIENTATION -eq '0' ] ; then
	ROW=$(( $ROW - 2 )) ;
    fi

    # echo "check_sblock_r: $ROW $COL"
    local R1="$ROW"
    local C1="$COL"
    if [ $ORIENTATION -eq '0' ] ; then
	local R2="$(( $ROW + 1 ))"
	local C2="$COL"
	local R3="$(( $ROW + 1 ))"
	local C3="$(( $COL - 1 ))"
	local R4="$(( $ROW + 2 ))"
	local C4="$(( $COL - 1 ))"
    else
	local R2="$(( $ROW - 1 ))"
	local C2="$(( $COL - 1 ))"
	local R3="$(( $ROW - 1 ))"
	local C3="$COL"
	local R4="$ROW"
	local C4="$(( $COL + 1 ))"
    fi
    quad_cmp $R1 $C1 $R2 $C2 $R3 $C3 $R4 $C4
}

# Check if an l-shaped block (reversed) can be placed at the specified ROW,COL
function check_lblock_r()
{
    local ROW=$1
    local COL=$2
    local ORIENTATION=$3

    if [ $ORIENTATION -eq '0' ] ; then
	COL=$(( $COL + 1 ))
	ROW=$(( $ROW - 2 ))
    elif [ $ORIENTATION -eq '1' ] ; then
	COL=$(( $COL - 1 ))
	ROW=$(( $ROW - 1 ))
    elif [ $ORIENTATION -eq '2' ] ; then
	ROW=$(( $ROW - 2 ))
	COL=$(( $COL - 1 ))
    else
	ROW=$(( $ROW - 1 ))
    fi

    local R1="$ROW"
    local C1="$COL"
    if [ $ORIENTATION -eq '0' ] ; then
	local R2="$(( $ROW + 1 ))"
	local C2="$COL"
	local R3="$(( $ROW + 2 ))"
	local C3="$COL"
	local R4="$(( $ROW + 2 ))"
	local C4="$(( $COL - 1 ))"
    elif [ $ORIENTATION -eq '1' ] ; then
	local R2="$(( $ROW + 1 ))"
	local C2="$COL"
	local R3="$(( $ROW + 1 ))"
	local C3="$(( $COL + 1 ))"
	local R4="$(( $ROW + 1 ))"
	local C4="$(( $COL + 2 ))"
    elif [ $ORIENTATION -eq '2' ] ; then
	local R2="$ROW"
	local C2="$(( $COL + 1 ))"
	local R3="$(( $ROW + 1 ))"
	local C3="$COL"
	local R4="$(( $ROW + 2 ))"
	local C4="$COL"
    else
	local R2="$ROW"
	local C2="$(( $COL + 1 ))"
	local R3="$(( $ROW + 1 ))"
	local C3="$(( $COL + 1 ))"
	local R4="$ROW"
	local C4="$(( $COL - 1 ))"
    fi
    quad_cmp $R1 $C1 $R2 $C2 $R3 $C3 $R4 $C4
}

# Check if an l-shaped block can be placed at the specified ROW,COL
function check_lblock()
{
    local ROW=$1
    local COL=$2
    local ORIENTATION=$3

    if [ $ORIENTATION -eq '0' ] ; then
	ROW=$(( $ROW - 1 ))
    elif [ $ORIENTATION -eq '1' ] ; then
	COL=$(( $COL - 1 ))
    elif [ $ORIENTATION -eq '2' ] ; then
	ROW=$(( $ROW - 2 ))
	COL=$(( $COL - 1 ))
    fi

    local R1="$ROW"
    local C1="$COL"
    if [ $ORIENTATION -eq '0' ] ; then
	local R2="$(( $ROW - 1 ))"
	local C2="$COL"
	local R3="$(( $ROW + 1 ))"
	local C3="$COL"
	local R4="$(( $ROW + 1 ))"
	local C4="$(( $COL + 1 ))"
    elif [ $ORIENTATION -eq '1' ] ; then
	local R2="$(( $ROW - 1 ))"
	local C2="$COL"
	local R3="$(( $ROW - 1 ))"
	local C3="$(( $COL + 1 ))"
	local R4="$(( $ROW - 1 ))"
	local C4="$(( $COL + 2 ))"
    elif [ $ORIENTATION -eq '2' ] ; then
	local R2="$ROW"
	local C2="$(( $COL + 1 ))"
	local R3="$(( $ROW + 1 ))"
	local C3="$(( $COL + 1 ))"
	local R4="$(( $ROW + 2 ))"
	local C4="$(( $COL + 1 ))"
    else
	local R2="$ROW"
	local C2="$(( $COL + 1 ))"
	local R3="$ROW"
	local C3="$(( $COL - 1 ))"
	local R4="$(( $ROW - 1 ))"
	local C4="$(( $COL + 1 ))"
    fi
    quad_cmp $R1 $C1 $R2 $C2 $R3 $C3 $R4 $C4
}

# Check if a t-shaped block can be placed at the specified ROW,COL
function check_tblock()
{
    local ROW=$1
    local COL=$2
    local ORIENTATION=$3
    if [ $ORIENTATION -eq '1' ] ; then
	ROW=$(( $ROW - 2 ))
    elif [ $ORIENTATION -eq '3' ] ; then
	ROW=$(( $ROW - 2 ))
    else
	ROW=$(( $ROW - 1 ))
    fi

    local R1="$ROW"
    local C1="$COL"
    if [ $ORIENTATION -eq '0' ] ; then
	local R2="$(( $ROW + 1 ))"
	local C2="$(( $COL - 1 ))"
	local R3="$(( $ROW + 1 ))"
	local C3="$COL"
	local R4="$(( $ROW + 1 ))"
	local C4="$(( $COL + 1 ))"
    elif [ $ORIENTATION -eq '1' ] ; then
	local R2="$(( $ROW + 1 ))"
	local C2="$COL"
	local R3="$(( $ROW + 1 ))"
	local C3="$(( $COL + 1 ))"
	local R4="$(( $ROW + 2 ))"
	local C4="$COL"
    elif [ $ORIENTATION -eq '2' ] ; then
	local R2="$ROW"
	local C2="$(( $COL - 1 ))"
	local R3="$(( $ROW + 1 ))"
	local C3="$COL"
	local R4="$ROW"
	local C4="$(( $COL + 1 ))"
    else
	local R2="$(( $ROW + 1 ))"
	local C2="$COL"
	local R3="$(( $ROW + 1 ))"
	local C3="$(( $COL - 1 ))"
	local R4="$(( $ROW + 2 ))"
	local C4="$COL"
    fi
    quad_cmp $R1 $C1 $R2 $C2 $R3 $C3 $R4 $C4
}

# Check to see if a row can be placed at the specified row and column.
function check_piece()
{
    local ROW=$1
    local COL=$2
    local ROT=$3

    # echo "ROW \"$ROW\" COL \"$COL\" ROT \"$ROT\"" >> check_log

    eval "check_$CURPIECE $ROW $COL $ROT"
}

## Row check routines for reading entire rows.
function check_rows()
{
    ROW1="$1"
    ROW2="$2"
    ROW3="$3"
    ROW4="$4"

    # R1F=0
    # R2F=0
    # R3F=0
    # R4F=0
    local ROW_POINTS=0

    # echo "ROWS: $ROW1 $ROW2 $ROW3 $ROW4" >> cr_log
    # date >> cr_log

    # Go from the row with the lowest number (highest on the board)
    # to the row with the highest number (lowest on the board)
    # to avoid the rows shifting out from under us.

    LOWEST_ROW=1
    if [ "x$ROW4" != "x" ] ; then
	eval `echo $GETROW | sed "s/R/$ROW4/g"`
	# Row contents are now in $ST
	ST=`echo "$ST" | sed 's/[1-9]/2/g'`
	if [ $ST = "2222222222222222222222" ] ; then
		# echo "FULL ROW $ROW4"
		# R4F=1
		# shift_rows 1 $(( $ROW1 - 1 )) 1 1
		mark_row $ROW4
		LOWEST_ROW=$ROW4
		ROW_POINTS=1
		# ROW_POINTS=$(( $ROW_POINTS + 1 ))
		# echo "ROW4 ($ROW4) FILLED" >> cr_log
	fi
    fi
    if [ "x$ROW3" != "x" ] ; then
	eval `echo $GETROW | sed "s/R/$ROW3/g"`
	# Row contents are now in $ST
	ST=`echo "$ST" | sed 's/[1-9]/2/g'`
	if [ $ST = "2222222222222222222222" ] ; then
		# echo "FULL ROW $ROW3"
		# R3F=1
		# shift_rows 1 $(( $ROW1 - 1 )) 1 1
		mark_row $ROW3
		LOWEST_ROW=$ROW3
		ROW_POINTS=$(( $ROW_POINTS + 1 ))
		# echo "ROW3 ($ROW3) FILLED" >> cr_log
	fi
    fi
    if [ "x$ROW2" != "x" ] ; then
	eval `echo $GETROW | sed "s/R/$ROW2/g"`
	# Row contents are now in $ST
	ST=`echo "$ST" | sed 's/[1-9]/2/g'`
	if [ $ST = "2222222222222222222222" ] ; then
		# echo "FULL ROW $ROW2"
		# R2F=1
		# shift_rows 1 $(( $ROW1 - 1 )) 1 1
		mark_row $ROW2
		LOWEST_ROW=$ROW2
		ROW_POINTS=$(( $ROW_POINTS + 1 ))
		# echo "ROW2 ($ROW2) FILLED" >> cr_log
	fi
    fi
    if [ "x$ROW1" != "x" ] ; then
	eval `echo $GETROW | sed "s/R/$ROW1/g"`
	# Row contents are now in $ST
	ST=`echo "$ST" | sed 's/[1-9]/2/g'`
	if [ $ST = "2222222222222222222222" ] ; then
		# echo "FULL ROW $ROW1"
		# R1F=1
		# shift_rows 1 $(( $ROW1 - 1 )) 1 1
		mark_row $ROW1
		LOWEST_ROW=$ROW1
		ROW_POINTS=$(( $ROW_POINTS + 1 ))
		# echo "ROW1 ($ROW1) FILLED" >> cr_log
	fi
    fi
    # date >> cr_log

    case $ROW_POINTS in
    1)
	delete_rows $LOWEST_ROW
	SCORE=$(( $SCORE + 100 ))
	fill_board
	set_level
	draw_status
	;;
    2)
	delete_rows $LOWEST_ROW
	SCORE=$(( $SCORE + 250 ))
	fill_board
	set_level
	draw_status
	;;
    3)
	delete_rows $LOWEST_ROW
	SCORE=$(( $SCORE + 500 ))
	fill_board
	set_level
	draw_status
	;;
    4)
	delete_rows $LOWEST_ROW
	SCORE=$(( $SCORE + 1000 ))
	fill_board
	set_level
	draw_status
	;;
    esac
    # date >> cr_log
}

function badopt()
{
  # Attempt at optimization gone wrong.  Once I realized that I still needed
  # to double the number of cases, I gave up on this.
  if false ; then

    if [ "x$ROW2" = "x" ] ; then
	ROW2=1
	ROW3=1
	ROW4=1
    elif [ "x$ROW3" = "x" ] ; then
	ROW3=1
	ROW4=1
    elif [ "x$ROW4" = "x" ] ; then
	ROW4=1
    fi

    if [ $R1F = 1 ] ; then
	if [ $R2F = 1 -a $ROW2 -eq $(( $ROW1 - 1 )) ] ; then
	    if [ $R3F = 1 -a $ROW3 -eq $(( $ROW2 - 2 )) ] ; then
		if [ $R4F = 1 -a $ROW4 -eq $(( $ROW3 - 3 )) ] ; then
		    # Four in a row.  Delete them at once.

		    # Reset the full flags.
		    R2F=0
		    R3F=0
		    R4F=0
		    shift_rows 1 $(( $ROW1 - 1 )) 4
		else
		    # Three in a row.   Delete them at once.
		    R2F=0
		    R3F=0
		    shift_rows $(( $ROW4 + 1 )) $(( $ROW1 - 1 )) 3
		fi
	    else
		# Two in a row.   Delete them at once.
		R2F=0
		shift_rows $(( $ROW3 + 1 )) $(( $ROW1 - 1 )) 2
	    fi
	else
	    # One.  Delete it by itself.
	    shift_rows $(( $ROW2 + 1 )) $(( $ROW1 - 1 )) 1
	fi
    fi
    if [ $R2F = 1 ] ; then
	if [ $R3F = 1 -a $ROW3 -eq $(( $ROW2 - 2 )) ] ; then
	    if [ $R4F = 1 -a $ROW4 -eq $(( $ROW3 - 3 )) ] ; then
		# Three in a row.   Delete them at once.
		R3F=0
		R4F=0
		shift_rows 1 $(( $ROW2 - 1 )) 3
	    else
		# Two in a row.   Delete them at once.
		R3F=0
		shift_rows $(( $ROW4 + 1 )) $(( $ROW2 - 1 )) 2
	    fi
	else
	    # One.  Delete it by itself.
	    shift_rows $(( $ROW3 + 1 )) $(( $ROW2 - 1 )) 1
	fi
    fi
    if [ $R3F = 1 ] ; then
	if [ $R4F = 1 -a $ROW4 -eq $(( $ROW3 - 3 )) ] ; then
	    # Two in a row.   Delete them at once.
	    R4F=0
	    shift_rows 1 $(( $ROW3 - 1 )) 2
	else
	    # One.  Delete it by itself.
	    shift_rows $(( $ROW4 + 1 )) $(( $ROW3 - 1 )) 1
	fi
    fi
    if [ $R4F = 1 ] ; then
	    # One.  Delete it by itself.
	    shift_rows 1 $(( $ROW4 - 1 )) 1
    fi
  fi
}

## Place routines.  These are called with no parameters.
## They read the global HPOS and VPOS values and mark the
## individual piece in the data tables.

# Store the row data for a square.
function place_square()
{
    VP2=$(( $VPOS - 1 ))
    HP2=$(( $HPOS - 1 ))
    eval "r$VPOS""c$HPOS=3"
    eval "r$VP2""c$HPOS=3"
    eval "r$VPOS""c$HP2=3"
    eval "r$VP2""c$HP2=3"
    check_rows $VPOS $VP2
}

# Store the row data for a horizontal or vertical row block.
function place_four()
{
    ORIENTATION=$(( $ROT % 2 ))
    local LINE=$VPOS;
    local COL=$HPOS;

    eval "r$LINE""c$COL=4"
    if [ $ORIENTATION -eq 0 ] ; then
	eval "r$LINE""c$(( $COL + 1 ))=4"
	eval "r$LINE""c$(( $COL - 1 ))=4"
	eval "r$LINE""c$(( $COL + 2 ))=4"
	check_rows $LINE
    else
	eval "r$(( $LINE - 1 ))c$COL=4"
	eval "r$(( $LINE - 2 ))c$COL=4"
	eval "r$(( $LINE - 3 ))c$COL=4"
	check_rows $LINE $(( $LINE - 1 )) $(( $LINE - 2 )) $(( $LINE - 3 ))
    fi
}

# Store the row data for an s-shaped block.
function place_sblock()
{
    local ORIENTATION=$(( $ROT % 2 ))
    local LINE;
    local COL=$HPOS;

    if [ $ORIENTATION -eq '1' ] ; then
	LINE=$(( $VPOS - 1 )) ;
    else
	LINE=$(( $VPOS - 2 )) ;
    fi

    eval "r$LINE""c$COL=5"
    if [ $ORIENTATION -eq '0' ] ; then
	eval "r$(( $LINE + 1 ))c$COL=5"
	eval "r$(( $LINE + 1 ))c$(( $COL + 1 ))=5"
	eval "r$(( $LINE + 2 ))c$(( $COL + 1 ))=5"
	check_rows $(( $LINE + 2 )) $(( $LINE + 1 )) $LINE
    else
	eval "r$(( $LINE + 1 ))c$COL=5"
	eval "r$(( $LINE + 1 ))c$(( $COL - 1 ))=5"
	eval "r$LINE""c$(( $COL + 1 ))=5"
	check_rows $(( $LINE + 1 )) $LINE
    fi
}

# Store the row data for an s-shaped block (reversed).
function place_sblock_r()
{
    local ORIENTATION=$(( $ROT % 2 ))
    local LINE;
    local COL=$HPOS;

    if [ $ORIENTATION -eq '0' ] ; then
	LINE=$(( $VPOS - 2 )) ;
    else
	LINE=$VPOS
    fi

    # echo "place_sblock_r: $LINE $COL"
    eval "r$LINE""c$COL=6"
    if [ $ORIENTATION -eq '0' ] ; then
	eval "r$(( $LINE + 1 ))c$COL=6"
	eval "r$(( $LINE + 1 ))c$(( $COL - 1 ))=6"
	eval "r$(( $LINE + 2 ))c$(( $COL - 1 ))=6"
	check_rows $(( $LINE + 2 )) $(( $LINE + 1 )) $LINE
    else
	eval "r$(( $LINE - 1 ))c$(( $COL - 1 ))=6"
	eval "r$(( $LINE - 1 ))c$COL=6"
	eval "r$LINE""c$(( $COL + 1 ))=6"
	check_rows $LINE $(( $LINE - 1 ))
    fi
}

# Store the row data for an l-shaped block (reversed).
function place_lblock_r()
{
    local ORIENTATION=$ROT
    local LINE=$VPOS
    local COL=$HPOS

    if [ $ORIENTATION -eq '0' ] ; then
	COL=$(( $COL + 1 ))
	LINE=$(( $LINE - 2 ))
    elif [ $ORIENTATION -eq '1' ] ; then
	COL=$(( $COL - 1 ))
	LINE=$(( $LINE - 1 ))
    elif [ $ORIENTATION -eq '2' ] ; then
	LINE=$(( $LINE - 2 ))
	COL=$(( $COL - 1 ))
    else
	LINE=$(( $LINE - 1 ))
    fi

    eval "r$LINE""c$COL=7"
    if [ $ORIENTATION -eq '0' ] ; then
	eval "r$(( $LINE + 1 ))c$COL=7"
	eval "r$(( $LINE + 2 ))c$COL=7"
	eval "r$(( $LINE + 2 ))c$(( $COL - 1 ))=7"
	check_rows $(( $LINE + 2 )) $(( $LINE + 1 )) $LINE
    elif [ $ORIENTATION -eq '1' ] ; then
	eval "r$(( $LINE + 1 ))c$COL=7"
	eval "r$(( $LINE + 1 ))c$(( $COL + 1 ))=7"
	eval "r$(( $LINE + 1 ))c$(( $COL + 2 ))=7"
	check_rows $(( $LINE + 1 )) $LINE
    elif [ $ORIENTATION -eq '2' ] ; then
	eval "r$LINE""c$(( $COL + 1 ))=7"
	eval "r$(( $LINE + 1 ))c$COL=7"
	eval "r$(( $LINE + 2 ))c$COL=7"
	check_rows $(( $LINE + 2 )) $(( $LINE + 1 )) $LINE
    else
	eval "r$LINE""c$(( $COL + 1 ))=7"
	eval "r$(( $LINE + 1 ))c$(( $COL + 1 ))=7"
	eval "r$LINE""c$(( $COL - 1 ))=7"
	check_rows $(( $LINE + 1 )) $LINE
    fi
}

# Store the row data for an l-shaped block.
function place_lblock()
{
    local ORIENTATION=$ROT

    local LINE=$VPOS
    local COL=$HPOS
    if [ $ORIENTATION -eq '0' ] ; then
	LINE=$(( $VPOS - 1 ))
    elif [ $ORIENTATION -eq '1' ] ; then
	COL=$(( $HPOS - 1 ))
    elif [ $ORIENTATION -eq '2' ] ; then
	LINE=$(( $VPOS - 2 ))
	COL=$(( $HPOS - 1 ))
    fi

    eval "r$LINE""c$COL=8"
    if [ $ORIENTATION -eq '0' ] ; then
	eval "r$(( $LINE - 1 ))c$COL=8"
	eval "r$(( $LINE + 1 ))c$COL=8"
	eval "r$(( $LINE + 1 ))c$(( $COL + 1 ))=8"
	check_rows $(( $LINE + 1 )) $LINE $(( $LINE - 1 ))
    elif [ $ORIENTATION -eq '1' ] ; then
	eval "r$(( $LINE - 1 ))c$COL=8"
	eval "r$(( $LINE - 1 ))c$(( $COL + 1 ))=8"
	eval "r$(( $LINE - 1 ))c$(( $COL + 2 ))=8"
	check_rows $LINE $(( $LINE - 1 ))
    elif [ $ORIENTATION -eq '2' ] ; then
	eval "r$LINE""c$(( $COL + 1 ))=8"
	eval "r$(( $LINE + 1 ))c$(( $COL + 1 ))=8"
	eval "r$(( $LINE + 2 ))c$(( $COL + 1 ))=8"
	check_rows $(( $LINE + 2 )) $(( $LINE + 1 )) $LINE
    else
	eval "r$LINE""c$(( $COL + 1 ))=8"
	eval "r$LINE""c$(( $COL - 1 ))=8"
	eval "r$(( $LINE - 1 ))c$(( $COL + 1 ))=8"
	check_rows $LINE $(( $LINE - 1 ))
    fi
}

# Store the row data for a t-shaped block.
function place_tblock()
{
    local LINE=$VPOS
    local COL=$HPOS
    local ORIENTATION=$ROT
    if [ $ORIENTATION -eq '1' ] ; then
	LINE=$(( $LINE - 2 ))
    elif [ $ORIENTATION -eq '3' ] ; then
	LINE=$(( $LINE - 2 ))
    else
	LINE=$(( $LINE - 1 ))
    fi

    eval "r$LINE""c$COL=9"
    if [ $ORIENTATION -eq '0' ] ; then
	eval "r$(( $LINE + 1 ))c$(( $COL - 1 ))=9"
	eval "r$(( $LINE + 1 ))c$COL=9"
	eval "r$(( $LINE + 1 ))c$(( $COL + 1 ))=9"
	check_rows $(( $LINE + 1 )) $LINE
    elif [ $ORIENTATION -eq '1' ] ; then
	eval "r$(( $LINE + 1 ))c$COL=9"
	eval "r$(( $LINE + 1 ))c$(( $COL + 1 ))=9"
	eval "r$(( $LINE + 2 ))c$COL=9"
	check_rows $(( $LINE + 2 )) $(( $LINE + 1 )) $LINE
    elif [ $ORIENTATION -eq '2' ] ; then
	eval "r$LINE""c$(( $COL - 1 ))=9"
	eval "r$(( $LINE + 1 ))c$COL=9"
	eval "r$LINE""c$(( $COL + 1 ))=9"
	check_rows $(( $LINE + 1 )) $LINE
    else
	eval "r$(( $LINE + 1 ))c$COL=9"
	eval "r$(( $LINE + 1 ))c$(( $COL - 1 ))=9"
	eval "r$(( $LINE + 2 ))c$COL=9"
	check_rows $(( $LINE + 2 )) $(( $LINE + 1 )) $LINE
    fi
}

# Store the row data for any arbitrary piece.
function place_piece()
{
    eval "place_$CURPIECE"
}


## Row maintenance routines.  These routines copy row data around.

# Fills in the COPYROW and CLEARROW variables.  These are used for
# fast copying and clearing of rows.
function set_row_copy()
{
    COPYROW="";
    local POS=1; local SEMI=""
    while [ $POS -le 20 ] ; do
	COPYROW="$COPYROW$SEMI""rDc$(( $POS + $LEFTBOUND ))=\$rSc$(( $POS + $LEFTBOUND ))"
	CLEARROW="$CLEARROW$SEMI""rRc$(( $POS + $LEFTBOUND ))=0";
	SEMI=";"
	POS=$(( $POS + 1 ))
    done

    # Set left border to 2
    COPYROW="$COPYROW$SEMI""rDc$LEFTBOUND=2";
    CLEARROW="$CLEARROW$SEMI""rRc$LEFTBOUND=2";

    # Set right border to 2
    COPYROW="$COPYROW$SEMI""rDc$(( $LEFTBOUND + 21 ))=2";
    CLEARROW="$CLEARROW$SEMI""rRc$(( $LEFTBOUND + 21 ))=2";

    # echo "LEFTBOUND=$LEFTBOUND"
    # echo "CLEARROW=$CLEARROW"; exit
    GETROW="ST=\""
    POS=0; # From one space left of the board to one space past the right.
    SEMI=""
    while [ $POS -le 21 ] ; do
	GETROW="$GETROW\$rRc$(( $POS + $LEFTBOUND ))"
	POS=$(( $POS + 1 ))
    done
    GETROW="$GETROW\""

    GETBOARD="echo \""
    local POS=1
    while [ $POS -le 23 ] ; do
	GETBOARD="$GETBOARD`echo $GETROW | sed -e 's/ST=\"//g' -e 's/\"//g' -e "s/R/$POS/g"`
" # newline, 22 backspaces.
	POS=$(( $POS + 1 ))
    done
    GETBOARD="$GETBOARD\"";
    # echo "GETBOARD=\"$GETBOARD\"" ; exit
}

# This routine does mass copying of row data.  It takes a first and last
# row to copy and a distance, then copies FIRST to FIRST+DISSTANCE,
# and so on, up through LAST+DISTANCE
# row 3: r3c0 r3c1 r3c2 r3c3 r3c4 ... r3c20
# row 2: r2c0 r2c1 r2c2 r2c3 r2c4 ... r2c20
# row 1: r1c0 r1c1 r1c2 r1c3 r1c4 ... r1c20
# row 0: r0c0 r0c1 r0c2 r0c3 r0c4 ... r1c20
# Example: shift_rows 2 3 1 0 would read rows 2 and 3 and overwrite
# rows 1 and 2.  Row 3 would not be cleared because CLEAR is 0.
###function shift_rows()
###{
    ###local FIRST=$1
    ###local LAST=$2
    ###local DISTANCE=$3
    ###local CLEAR=$4
###
###
    ###echo "SR: FIRST=$FIRST LAST=$LAST DISTANCE=$DISTANCE CLEAR=$CLEAR" >> sr_log
#### @@@ FIXME DAG Check direction @@@
###
    ###if [ "x$WROWCOPY" = "x" ] ; then
	###set_row_copy
    ###fi
    ###local FROM=$FIRST;
    ###local TO=$(( $FIRST - $DISTANCE ))
    ###local CMD=""
    ###local SEMI=""
    ###if [ "x$CLEAR" = "x1" ] ; then
	###LASTCLEAR=$(( $FIRST + $DISTANCE - 1 ))
    ###else
	###LASTCLEAR=0
    ###fi
    ###while [ $FROM -le $LAST ] ; do
	###CMD="$CMD$SEMI`echo $COPYROW | sed "s/S/$FROM/g" | sed "s/D/$TO/g"`"
	###if [ $FROM -le $LASTCLEAR ] ; then
		###CMD="$CMD$SEMI`echo $CLEARROW | sed "s/S/$FROM/g"`"
	###fi
	###SEMI=";"
	###FROM=$(( $FROM + 1 ))
	###TO=$(( $TO + 1 ))
    ###done
    ###eval cmd
###}

### # Rewrite of shift_rows to be sane.
### function delete_row()
### {
    ### local ROW=$1
### 
    ### # echo "DR: $ROW"
### 
    ### if [ "x$WROWCOPY" = "x" ] ; then
	### set_row_copy
    ### fi
    ### local SEMI=""
    ### local CMD=""
    ### while [ $ROW -gt 2 ] ; do
	### local ROW_MINUS_ONE=$(( $ROW - 1 ))
	### CMD="$CMD$SEMI`echo $COPYROW | sed "s/S/$ROW_MINUS_ONE/g" | sed "s/D/$ROW/g"`"
	### SEMI=";"
	### ROW=$ROW_MINUS_ONE
	### # echo "CMD_LOOP: $CMD" >> dr_log
    ### done
    ### # echo "CMD_OUT_OF_LOOP: $CMD" >> dr_log
    ### CMD="$CMD$SEMI`echo $CLEARROW | sed "s/R/2/g"`"
    ### # echo "CMD_END_OF_FUNC: $CMD" >> dr_log
    ### # echo "CMD $CMD" >> dr_log
    ### eval $CMD
### }

function unmark_row()
{
    local ROW=$1
    eval "DELETE_ROW_$ROW=0"
}
function mark_row()
{
    local ROW=$1
    eval "DELETE_ROW_$ROW=1"
}

# Re-rewrite of shift_rows to be sane.
function delete_rows()
{
    local FIRST_ROW=$1

    # echo "DR: $ROW"

    if [ "x$WROWCOPY" = "x" ] ; then
	set_row_copy
    fi
    local SEMI=""
    local CMD=""
    local ROW_MINUS_ONE=$(( $ROW - 1 ))
    local SRC_ROW=$FIRST_ROW
    local DEST_ROW=$FIRST_ROW
    while [ $SRC_ROW -gt 2 ] ; do
	# echo "Delete loop: $SRC_ROW,$DEST_ROW" >> delete_log
	# echo "Delete loop mark check: $(eval "echo \$DELETE_ROW_$DEST_ROW")" >> delete_log
	while [ "$(eval "echo \$DELETE_ROW_$SRC_ROW")" = 1 ] ; do
		unmark_row $SRC_ROW
		# echo "Row $SRC_ROW was marked.  Deleting." >> delete_log
		SRC_ROW=$(( SRC_ROW - 1 ))
	done
		
	CMD="$CMD$SEMI`echo $COPYROW | sed "s/S/$SRC_ROW/g" | sed "s/D/$DEST_ROW/g"`"
	SEMI=";"
	SRC_ROW=$(( $SRC_ROW - 1 ))
	DEST_ROW=$(( $DEST_ROW - 1 ))
	# echo "CMD_LOOP: $CMD" >> dr_log
    done
    # echo "CMD_OUT_OF_LOOP: $CMD" >> dr_log

    while [ $DEST_ROW -ge 2 ] ; do
	CMD="$CMD$SEMI`echo $CLEARROW | sed "s/R/$DEST_ROW/g"`"
	DEST_ROW=$(( $DEST_ROW - 1 ))
    done
    # echo "CMD_END_OF_FUNC: $CMD" >> dr_log
    # echo "CMD $CMD" >> dr_log
    eval $CMD
}

# Clears the board (both visible and row data)
function clear_board()
{
    local LINE=2;
    # local BASELINE=$(( $LL - 1 ))
    while [ $LINE -lt $BASELINE ] ; do
	# echo "LINE: $LINE"
	vtsetpos $LINE $(( $LEFTBOUND + 1 ))
	DEBUG=0
	if [ $DEBUG -eq 1 ] ; then
		echo -n "$SBLOCKCOLOR1$FILLED                    $RESET$CLEAR"; # 20 spaces
	else
		echo -n "$WHITE                    $RESET"; # 20 spaces
	fi
	LINE=$(( $LINE + 1 ))
    done

    if [ "x$CLEARROW" = "x" ] ; then
	# echo "SETTING CLEARROW"
	set_row_copy
    fi
    # echo "CLEARING DATA"
    # echo "CLEARROW IS $CLEARROW"
    # echo "LL is $LL"
    local POS=2
    local SEMI=""
    local CMD=""
    while [ $POS -lt $LL ] ; do
	CMD="$CMD$SEMI`echo $CLEARROW | sed "s/R/$POS/g"`"
	SEMI=";"
	POS=$(( $POS + 1 ))
	# echo "POS $POS LL $LL"
    done

    # Set the top border to 1.  It's special because pieces fall through it.
    CMD="$CMD$SEMI`echo $CLEARROW | sed "s/R/1/g" | sed "s/=0/=1/g"`"
    # Set the bottom border to 2.  It is equivalent to any other block.
    CMD="$CMD$SEMI`echo $CLEARROW | sed "s/R/$BASELINE/g" | sed "s/=0/=2/g"`"

    # echo "CMD is $CMD"
    eval $CMD
}

# Chooses the next piece.
function choose_next()
{
    local RAWVAL=$(dd if=/dev/random bs=1 count=1 2> /dev/null)
    local NEXTNUM=$(echo $RAWVAL | perl -e '$/ = undef; my $val = <STDIN>; print ord($val) % 7;')

# NEXT='sblock'
# NEXT='sblock_r'
# NEXT='lblock'
# NEXT='lblock_r'
# NEXT='tblock'
# NEXT='four'
# NEXT='square'
    local NEWNEXT="";

    if [ $NEXTNUM -le 2 ] ; then
    	if [ $NEXTNUM -eq 0 ] ; then
		# 0
		NEWNEXT='sblock'
	else
		if [ $NEXTNUM -eq 1 ] ; then
			#1
			NEWNEXT='sblock_r'
		else
			# 2
			NEWNEXT='lblock'
		fi
	fi
    else 
    	if [ $NEXTNUM -le 4 ] ; then
		if [ $NEXTNUM -eq 3 ] ; then
			# 3
			NEWNEXT='lblock_r'
		else
			# 4
			NEWNEXT='tblock'
		fi
    	else
		if [ $NEXTNUM -eq 5 ] ; then
			# 5
			NEWNEXT='four'
		else
			# 6
			NEWNEXT='square'
		fi
	fi
    fi

    # @@@ DAG DELETE THE NEXT LINE @@@
    # NEWNEXT='four'

    GLOBAL_TICK_COUNT=0;
    MODE="clear"
    draw_next 0
    CURPIECE=$NEXT
    NEXT=$NEWNEXT
    MODE="box"
    draw_next 0
    ROT=0
}

# Show the cursor.  Call before exiting.
function show_cursor()
{
    echo -n '[?25h'				# show cursor
}

# Hide the cursor.  Call regularly.
function hide_cursor()
{
    echo -n '[?25l'				# hide cursor
}

# Resets the display and configures various settings.
function vtreset()
{
    echo -n 'c'				# VT100 reset
    echo -n '[?71'				# Turn off auto-wrap
    echo ; echo ''				# clear screen
    hide_cursor
}

# Sets the cursor to a given row and column.
function vtsetpos()
{
    local VTS_ROW=$1
    local VTS_COL=$2
    echo -n '['
    echo -n "$VTS_ROW"
    echo -n ';'
    echo -n "$VTS_COL"
    echo -n 'H'
    hide_cursor
}

## Draw functions.  These draw blocks in various locations, among other
## drawing tasks.

# Draws a single square.
function draw_block()
{
    local L=$1
    local C=$2
    local MODE=$3
    local COLOR=$4

    if [ $L -lt 1 ] ; then
	# This block is off the screen.
	return
    elif [ $L -gt $BASELINE ] ; then
	# This block is off the screen.
	return
    fi

    vtsetpos $L $C
    if [ $L -eq 1 ] ; then
	echo -n "$FILLED $RESET"
    else
	echo -n $COLOR
	if [ $MODE = "box" ] ; then
		echo -n "$FILLED"
		echo -n '◙' # rectangular box
		echo -n "$RESET"
	elif [ $MODE = "clear" ] ; then
		echo -n $CLEAR
		echo -n ' '
	else
		echo -n '[7m'
		echo -n ' '
		echo -n '[m'
	fi
    fi

    # vtsetpos $LL $LC
    # hide_cursor
}

function draw_square
{
    local LINE=$1
    local COL=$2

    local LINEA=$(($LINE - 1))
    local COLA=$(($COL - 1))
    # echo
    # echo "LINE IS \"$LINE\""
    # echo "COL IS \"$COL\""
    # echo "LINEA IS \"$LINEA\""
    # echo "COLA IS \"$COLA\""

    draw_block $LINE $COL $MODE $SQUARECOLOR
    draw_block $LINEA $COL $MODE $SQUARECOLOR
    draw_block $LINE $COLA $MODE $SQUARECOLOR
    draw_block $LINEA $COLA $MODE $SQUARECOLOR
}

function draw_sblock()
{
    local LINE=$1
    local COL=$2
    local ORIENTATION=$(( $3 % 2 ))

    if [ $ORIENTATION -eq '1' ] ; then
	LINE=$(( $LINE - 1 )) ;
    else
	LINE=$(( $LINE - 2 )) ;
    fi

    draw_block $LINE $COL $MODE $SBLOCKCOLOR1
    if [ $ORIENTATION -eq '0' ] ; then
	draw_block $(( $LINE + 1 )) $COL $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $(( $COL + 1 )) $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 2 )) $(( $COL + 1 )) $MODE $SBLOCKCOLOR1
    else
	draw_block $(( $LINE + 1 )) $COL $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $(( $COL - 1 )) $MODE $SBLOCKCOLOR1
	draw_block $LINE $(( $COL + 1 )) $MODE $SBLOCKCOLOR1
    fi
}

function draw_sblock_r()
{
    local LINE=$1
    local COL=$2
    local ORIENTATION=$(( $3 % 2 ))

    if [ $ORIENTATION -eq '0' ] ; then
	LINE=$(( $LINE - 2 ))
    fi

    draw_block $LINE $COL $MODE $SBLOCKCOLOR2
    if [ $ORIENTATION -eq '0' ] ; then
	draw_block $(( $LINE + 1 )) $COL $MODE $SBLOCKCOLOR2
	draw_block $(( $LINE + 1 )) $(( $COL - 1 )) $MODE $SBLOCKCOLOR2
	draw_block $(( $LINE + 2 )) $(( $COL - 1 )) $MODE $SBLOCKCOLOR2
    else
	echo -n ''
	draw_block $(( $LINE - 1 )) $(( $COL - 1 )) $MODE $SBLOCKCOLOR2
	draw_block $(( $LINE - 1 )) $COL $MODE $SBLOCKCOLOR2
	draw_block $LINE $(( $COL + 1 )) $MODE $SBLOCKCOLOR2
    fi
}

function draw_lblock()
{
    local LINE=$1
    local COL=$2
    local ORIENTATION=$3

    if [ $ORIENTATION -eq '0' ] ; then
	LINE=$(( $LINE - 1 ))
    elif [ $ORIENTATION -eq '1' ] ; then
	COL=$(( $COL - 1 ))
    elif [ $ORIENTATION -eq '2' ] ; then
	LINE=$(( $LINE - 2 ))
	COL=$(( $COL - 1 ))
    fi

    draw_block $LINE $COL $MODE $LBLOCKCOLOR1
    if [ $ORIENTATION -eq '0' ] ; then
	draw_block $(( $LINE - 1 )) $COL $MODE $LBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $COL $MODE $LBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $(( $COL + 1 )) $MODE $LBLOCKCOLOR1
    elif [ $ORIENTATION -eq '1' ] ; then
	draw_block $(( $LINE - 1 )) $COL $MODE $LBLOCKCOLOR1
	draw_block $(( $LINE - 1 )) $(( $COL + 1 )) $MODE $LBLOCKCOLOR1
	draw_block $(( $LINE - 1 )) $(( $COL + 2 )) $MODE $LBLOCKCOLOR1
    elif [ $ORIENTATION -eq '2' ] ; then
	draw_block $LINE $(( $COL + 1 )) $MODE $LBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $(( $COL + 1 )) $MODE $LBLOCKCOLOR1
	draw_block $(( $LINE + 2 )) $(( $COL + 1 )) $MODE $LBLOCKCOLOR1
    else
	draw_block $LINE $(( $COL + 1 )) $MODE $LBLOCKCOLOR1
	draw_block $LINE $(( $COL - 1 )) $MODE $LBLOCKCOLOR1
	draw_block $(( $LINE - 1 )) $(( $COL + 1 )) $MODE $LBLOCKCOLOR1
    fi
}

function draw_lblock_r()
{
    local LINE=$1
    local COL=$2
    local ORIENTATION=$3

    if [ $ORIENTATION -eq 0 ] ; then
	COL=$(( $COL + 1 ))
	LINE=$(( $LINE - 2 ))
    elif [ $ORIENTATION -eq '1' ] ; then
	COL=$(( $COL - 1 ))
	LINE=$(( $LINE - 1 ))
    elif [ $ORIENTATION -eq 2 ] ; then
	LINE=$(( $LINE - 2 ))
	COL=$(( $COL - 1 ))
    else
	LINE=$(( $LINE - 1 ))
    fi

    draw_block $LINE $COL $MODE $LBLOCKCOLOR2
    if [ $ORIENTATION -eq '0' ] ; then
	draw_block $(( $LINE + 1 )) $COL $MODE $LBLOCKCOLOR2
	draw_block $(( $LINE + 2 )) $COL $MODE $LBLOCKCOLOR2
	draw_block $(( $LINE + 2 )) $(( $COL - 1 )) $MODE $LBLOCKCOLOR2
    elif [ $ORIENTATION -eq '1' ] ; then
	draw_block $(( $LINE + 1 )) $COL $MODE $LBLOCKCOLOR2
	draw_block $(( $LINE + 1 )) $(( $COL + 1 )) $MODE $LBLOCKCOLOR2
	draw_block $(( $LINE + 1 )) $(( $COL + 2 )) $MODE $LBLOCKCOLOR2
    elif [ $ORIENTATION -eq '2' ] ; then
	draw_block $LINE $(( $COL + 1 )) $MODE $LBLOCKCOLOR2
	draw_block $(( $LINE + 1 )) $COL $MODE $LBLOCKCOLOR2
	draw_block $(( $LINE + 2 )) $COL $MODE $LBLOCKCOLOR2
    else
	draw_block $LINE $(( $COL + 1 )) $MODE $LBLOCKCOLOR2
	draw_block $(( $LINE + 1 )) $(( $COL + 1 )) $MODE $LBLOCKCOLOR2
	draw_block $LINE $(( $COL - 1 )) $MODE $LBLOCKCOLOR2
    fi
}

function draw_tblock()
{
    local LINE=$1
    local COL=$2
    local ORIENTATION=$3

    if [ $ORIENTATION -eq '1' ] ; then
	LINE=$(( $LINE - 2 ))
    elif [ $ORIENTATION -eq '3' ] ; then
	LINE=$(( $LINE - 2 ))
    else
	LINE=$(( $LINE - 1 ))
    fi

    draw_block $LINE $COL $MODE $SBLOCKCOLOR1
    if [ $ORIENTATION -eq '0' ] ; then
	draw_block $(( $LINE + 1 )) $(( $COL - 1 )) $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $COL $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $(( $COL + 1 )) $MODE $SBLOCKCOLOR1
    elif [ $ORIENTATION -eq '1' ] ; then
	draw_block $(( $LINE + 1 )) $COL $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $(( $COL + 1 )) $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 2 )) $COL $MODE $SBLOCKCOLOR1
    elif [ $ORIENTATION -eq '2' ] ; then
	draw_block $LINE $(( $COL - 1 )) $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $COL $MODE $SBLOCKCOLOR1
	draw_block $LINE $(( $COL + 1 )) $MODE $SBLOCKCOLOR1
    else
	draw_block $(( $LINE + 1 )) $COL $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 1 )) $(( $COL - 1 )) $MODE $SBLOCKCOLOR1
	draw_block $(( $LINE + 2 )) $COL $MODE $SBLOCKCOLOR1
    fi
}

function draw_four()
{
    local LINE=$1
    local COL=$2
    local ORIENTATION=$(( $3 % 2 ))

    # echo -n "O=$ORIENTATION" $MODE

    draw_block $LINE $COL $MODE $FOURCOLOR
    if [ $ORIENTATION -eq '0' ] ; then
	draw_block $LINE $(( $COL + 1 )) $MODE $FOURCOLOR
	draw_block $LINE $(( $COL - 1 )) $MODE $FOURCOLOR
	draw_block $LINE $(( $COL + 2 )) $MODE $FOURCOLOR
    else
	draw_block $(( $LINE - 1 )) $COL $MODE $FOURCOLOR
	draw_block $(( $LINE - 2 )) $COL $MODE $FOURCOLOR
	draw_block $(( $LINE - 3 )) $COL $MODE $FOURCOLOR
    fi
}

function draw_lines()
{
    # local BASELINE=$(( $LL - 1 ))
    vtsetpos $BASELINE 1
    local POS=1

if false; then
    echo -n '[4m'				# set underline
    while [ $POS -lt $LC ] ; do
	echo -n ' '
	POS=$(( $POS + 1 ))
    done
    echo -n '[m'				# clear underline

    local HPOS=$(( $LC - 13 ))
    POS=1;
    while [ $POS -lt $BASELINE ] ; do
	vtsetpos $POS $HPOS
	echo -n '|'
	POS=$(( $POS + 1 ))
    done
fi

    vtsetpos 1 1
    BOTTOMSTR='[7m'					# set inverse
    STR='[7m'					# set inverse
    local COUNT=0
    # Add spaces for left fill
    while [ $COUNT -lt $LEFTBOUND ] ; do
	STR="$STR "
	BOTTOMSTR="$BOTTOMSTR "
	COUNT=$(( $COUNT + 1 ))
    done
    STR="$STR"'[m'				# clear formatting
	# echo "INITCOUNT: $COUNT"
	# echo "INITSTR: \"$STR\""
    COUNT=0
    # Add spaces for center gap
    while [ $COUNT -lt $(( $RIGHTBOUND - $LEFTBOUND )) ] ; do
	STR="$STR "
	BOTTOMSTR="$BOTTOMSTR "
	COUNT=$(( $COUNT + 1 ))
    done
    STR="$STR"'[7m'				# set inverse
    COUNT=0
    # Add spaces for right fill
    while [ $COUNT -lt $(( $LC - $RIGHTBOUND - 13 )) ] ; do
	STR="$STR "
	BOTTOMSTR="$BOTTOMSTR "
	COUNT=$(( $COUNT + 1 ))
    done
    STR="$STR"'[m'				# clear formatting
    STR="$STR            "'[m'		# clear formatting (12 spaces)
    STR="$STR"'[7m'				# set inverse
    STR="$STR "					# one space
    STR="$STR"'[m'				# clear formatting
    BOTTOMSTR="$BOTTOMSTR             "'[m'	# clear formatting (13 spaces)
    echo "$BOTTOMSTR"
    COUNT=1
    # Print fills
    while [ $COUNT -lt $(( $BASELINE - 1 )) ] ; do
	COUNT=$(( $COUNT + 1 ))
	echo "$STR"
    done
    echo "$BOTTOMSTR"

    local HPOSL=$(( $LC - 10 ))
    local HPOSR=$(( $LC - 3 ))
    vtsetpos 5 7
    echo -n '[1;7m'				# set inverse
    echo -n "ShellTris"
    echo -n '[m'				# clear formatting
    vtsetpos 6 8
    echo -n '[7m'				# set inverse
    echo -n "A Bourne"
    echo -n '[m'				# clear formatting
    vtsetpos 7 7
    echo -n '[7m'				# set inverse
    echo -n "Shell Game"
    echo -n '[m'				# clear formatting
    vtsetpos 8 7
    echo -n '[7m'				# set inverse
    echo -n "Written By"
    echo -n '[m'				# clear formatting
    vtsetpos 9 4
    echo -n '[7m'				# set inverse
    echo -n "David A. Gatwood"
    echo -n '[m'				# clear formatting
    vtsetpos 10 4
    echo -n '[7m'				# set inverse
    echo -n "Copyright © 2007"
    echo -n '[m'				# clear formatting
    vtsetpos 12 6
    echo -n '[7m'				# set inverse
    echo -n "May be freely"
    echo -n '[m'				# clear formatting
    vtsetpos 13 4
    echo -n '[7m'				# set inverse
    echo -n "redistributed as"
    echo -n '[m'				# clear formatting
    vtsetpos 14 3
    echo -n '[7m'				# set inverse
    echo -n "long as author and"
    echo -n '[m'				# clear formatting
    vtsetpos 15 4
    echo -n '[7m'				# set inverse
    echo -n "copyright notice"
    echo -n '[m'				# clear formatting
    vtsetpos 16 4
    echo -n '[7m'				# set inverse
    echo -n "are not removed"
    echo -n '[m'				# clear formatting
    vtsetpos 17 7
    echo -n '[7m'				# set inverse
    echo -n "or altered."
    echo -n '[m'				# clear formatting
    vtsetpos 3 $HPOSL
    echo -n ' '
    echo -n '[4m'				# set underline
    echo -n "Next: "
    echo -n '[m'				# clear formatting

    POS=4
    while [ $POS -lt 10 ] ; do
	vtsetpos $POS $HPOSL
	echo -n '|'
	vtsetpos $POS $HPOSR
	echo -n '|'
	POS=$(( $POS + 1 ))
	    done
	    vtsetpos $POS $HPOSL
    echo -n '|'
    echo -n '[4m'				# set inverse
    echo -n "      "
    echo -n '[m'				# clear formatting
    echo -n '|'


}

# Test function - redraw the board based on where known pieces are.
# This code is used to test whether the store routines work correctly.
function fill_board()
{
    if [ "x$GETBOARD" = "x" ] ; then
	set_row_copy
    fi
    # echo "GETBOARD IS $GETBOARD"

    # local POS=1;
    local CMD="";

# echo "r5c1 is $r5c1"
# echo "r5c2 is $r5c2"

    local OUTPUT=`eval "$GETBOARD" | tr '123456789' 'ABCDEFGHI' | sed "s/0/ /g" | sed "s/A/$FILLED $RESET/g" | sed "s/B/$FILLED $RESET/g" | sed "s/C/$FILLED$SQUARECOLOR$RESET/g" | sed "s/D/$FILLED$FOURCOLOR$RESET/g" | sed "s/E/$FILLED$SBLOCKCOLOR1$RESET/g" | sed "s/F/$FILLED$SBLOCKCOLOR2$RESET/g" | sed "s/G/$FILLED$LBLOCKCOLOR1$RESET/g" | sed "s/H/$FILLED$LBLOCKCOLOR2$RESET/g" | sed "s/I/$FILLED$TBLOCKCOLOR$RESET/g"`
    ### local OUTPUT=""
    # always copy 23 lines (for debugging)
    ### while [ $POS -le 23 ] ; do
	### CMD=`echo $GETROW | sed "s/R/$POS/g"`
	### # echo "CMD IS $CMD"
	### eval $CMD
### 
	### # Test mode
	### # ST=`echo $ST | sed 's/0/ /g' | sed 's/1/|/g' | sed 's/2/X/g'`
### 
	### # Play mode
	### ST=`echo $ST | tr '123456789' 'ABCDEFGHI' | sed "s/0/ /g" | sed "s/A/$FILLED $RESET/g" | sed "s/B/$FILLED $RESET/g" | sed "s/C/$FILLED$SQUARECOLOR$RESET/g" | sed "s/D/$FILLED$FOURCOLOR$RESET/g" | sed "s/E/$FILLED$SBLOCKCOLOR1$RESET/g" | sed "s/F/$FILLED$SBLOCKCOLOR2$RESET/g" | sed "s/G/$FILLED$LBLOCKCOLOR1$RESET/g" | sed "s/H/$FILLED$LBLOCKCOLOR2$RESET/g" | sed "s/I/$FILLED$TBLOCKCOLOR$RESET/g"`

	### # echo "STRING IS \"$ST\""
	### # POS_ROW=$(( $LL - 1 - $POS ))
	### POS_ROW=$POS
	### # vtsetpos $POS_ROW $LEFTBOUND
	### # echo -n "$ST"
	### OUTPUT="$OUTPUT$ST
### " # newline, 22 backspaces.
	### POS=$(( $POS + 1 ))
    ### done
    vtsetpos 1 $LEFTBOUND
    echo -n "$OUTPUT"
    # exit
}

function clear_status()
{
    vtsetpos $LL 1;
    local COUNT=0
    while [ $COUNT -lt $(( $LC - 1 )) ] ; do
	echo -n " "
	COUNT=$(( $COUNT + 1 ))
    done
}

function draw_status()
{
    local WIDTH=$(( $(echo $SCORE | sed 's/[^0-9]//g' | tr '0123456789' '\n\n\n\n\n\n\n\n\n' | grep -c '^$') - 1 ))
    local LEFT=$(( 8 - $WIDTH ))
    vtsetpos $LL 1;
    local COUNT=0
    while [ $COUNT -lt $LEFT ] ; do
	echo -n ' '
	COUNT=$(( $COUNT + 1 ))
    done
    echo -n $SCORE
    vtsetpos 12 $(( $LC - 10 ))
    printf "Level %2d" $LEVEL; #  echo -n "Level $LEVEL"
}

function draw_next()
{
    local ROT=$1

    local HPOS=$(( $LC - 7 ))
    local FUNC="draw_$NEXT 9 $HPOS $ROT"
    eval $FUNC
}

function draw_cur()
{
    local ROT=$1
    local FUNC="draw_$CURPIECE $VPOS $HPOS $ROT"
    eval $FUNC
}

function move_left()
{
    # Add impact logic and border logic
    MODE="clear"
    draw_cur "$ROT"
    HPOS=$(( $HPOS - 1 ))
    MODE="box"
    draw_cur "$ROT"
}

function move_right()
{
    # Add impact logic and border logic
    MODE="clear"
    draw_cur "$ROT"
    HPOS=$(( $HPOS + 1 ))
    MODE="box"
    draw_cur "$ROT"
}

function clearboard_test()
{
    local COUNT=0;
    while [ $COUNT -lt 10 ] ; do
	clear_board
	COUNT=$(( $COUNT + 1 ))
    done
}

function draw_test()
{
    HPOS=$STARTING_HPOS
    VPOS=$STARTING_VPOS
    ROT=0

    clear_board

	# echo "HPOS: $HPOS VPOS: $VHPOS ROT: $ROT"

    CURPIECE='sblock'

    local COUNT=0;
    while [ $COUNT -lt 16 ] ; do
	local CP=`check_piece  $(($VPOS + 1)) $HPOS $ROT`
	if [ $CP = "00" -o $CP = "01" ] ; then
		MODE="clear"
		draw_cur $ROT
		VPOS=$(( $VPOS + 1 ))
		COUNT=$(( $COUNT + 1 ))
		MODE="box"
		draw_cur $ROT
	# else
		# echo "Error: draw_test failed ($CP)."
		# exit 0
	fi
    done
    MODE="clear"
    draw_cur 0
}

function read_test()
{
    local GLOBAL_TICK_COUNT=0
    local PAUSED=0
    local ONE_SECOND=100000			# ensure this never trips!
    local CUR_TICK_RATE=$ONE_SECOND
    while [ $GLOBAL_TICK_COUNT -lt 20000 ] ; do
       	CHAR=`./getch`
	if [ $1 = "rot" ] ; then
		CHAR=","
	fi
	if [ "x$CHAR" = "x" ] ; then
		# echo "NO DATA";
		echo -n '' # We will delay here eventually.
	elif [ $PAUSED -eq 1 ] ; then
		PAUSED=0;
		SCORE=$PAUSEDSCORE
		clear_status
		draw_status
	else
		# echo "DATA: $CHAR";

		case $CHAR in
			( "p" | "P" )
				PAUSED=1;
				PAUSEDSCORE=$SCORE
				SCORE="$SCORE      --- Paused ---"
				draw_status
			;;
			( "q" | "Q" )
				GAMEOVER=1
				# CONT=0 # Don't quit.
			;;
			( "z" | "Z" )
				CP=`check_piece  $VPOS $(expr $HPOS - 1) $ROT`
				if [ $CP = "00" -o $CP = "01" ] ; then
					move_left
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
				fi
			;;
			( "x" | "X" )
				CP=`check_piece  $VPOS $(( $HPOS + 1 )) $ROT`
				if [ $CP = "00" -o $CP = "01" ] ; then
					move_right
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
				fi
			;;
			( ',' )
				# echo $ROT >> /tmp/lastrot
				if [ "$ROT" -eq 0 ] ; then
					NEWROT=3
				else
					NEWROT=$(( $ROT - 1 ))
				fi
				CP=`check_piece $VPOS $HPOS $NEWROT`
				if [ $CP = "00" -o $CP = "01" ] ; then
					MODE="clear"
					draw_cur "$ROT"
					ROT=$NEWROT
					MODE="box"
					draw_cur $ROT
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $ROT_PENALTY ))
				fi
			;;
			( '.' )
				if [ $ROT -eq 3 ] ; then
					NEWROT=0
				else
					NEWROT=$(( $ROT + 1 ))
				fi
				CP=`check_piece $VPOS $HPOS $NEWROT`
				if [ $CP = "00" -o $CP = "01" ] ; then
					MODE="clear"
					draw_cur $ROT
					ROT=$NEWROT
					MODE="box"
					draw_cur $ROT
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $ROT_PENALTY ))
				fi
			;;
			## ( "g" )
				## # @@@ DAG DELETE ME
				## # This is to show what should happen when the game
				## # is over.
				## GAMEOVER=1
			## ;;
			## ( "f" )
				## # @@@ DAG DELETE ME
				## # This is for debugging only.
				## fill_board
				## sleep 5
				## exit
			## ;;
			## ( "n" )
				## # @@@ DAG DELETE ME
				## # This is to show what should happen when a block
				## # hits something.
				## place_piece
				## choose_next;
				## VPOS=$STARTING_VPOS;
				## HPOS=$STARTING_HPOS # $(( ( $LC - 13 ) / 2 ))
				## draw_next 0
				## GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
			## ;;
			( ' ' )
				# Drop down sooner
				CP=`check_piece  $(expr $VPOS + 1) $HPOS $ROT`
				# echo "CP: $CP" >> cp_log
				if [ $CP = "20" ] ; then
					# Hit a block, but past the top edge.
					place_piece
					choose_next;
					VPOS=$STARTING_VPOS;
					HPOS=$STARTING_HPOS # $(( ( $LC - 13 ) / 2 ))
					draw_next 0
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
				elif [ $CP = "21" ] ; then
					# Hit a block.  Piece never made it
					# through the top.
					GAMEOVER=1;
				else
					MODE="clear";
					draw_cur $ROT;
					VPOS=$(( $VPOS + 1 ))
					MODE="box";
					draw_cur $ROT
					# Delay the next drop until a full period has elapsed.
					GLOBAL_TICK_COUNT=0
				fi
			;;
		esac
	fi

	# Change the next entry every few cycles for now.
	GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + 100 ))
	while [ $GLOBAL_TICK_COUNT -ge $CUR_TICK_RATE ] ; do
		GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT - $CUR_TICK_RATE ))
		CP=`check_piece  $(( $VPOS + 1 )) $HPOS $ROT`
		# echo "CP: $CP" >> cp_log
		if [ $CP = "20" ] ; then
			# Hit a block, but past the top edge.
			place_piece
			choose_next;
			VPOS=$STARTING_VPOS;
			HPOS=$STARTING_HPOS # $(( ( $LC - 13 ) / 2 ))
			draw_next 0
			GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
		elif [ $CP = "21" ] ; then
			# Hit a block.  Piece never made it
			# through the top.
			GAMEOVER=1;
		else
			MODE="clear";
			draw_cur $ROT;
			VPOS=$(( $VPOS + 1 ))
			MODE="box";
			draw_cur $ROT
		fi
	done
    done
}

function calibrate_timers()
{
    local FOO=3
    local DEBUG="$1"
    # ONE_SECOND=25

    if [ "$CALIBRATED" = "yes" -a $DEBUG = 0 ] ; then
	CUR_TICK_RATE=$ONE_SECOND
	return
    fi

    2>/tmp/drawtesttime time -p ./shelltris.sh -testdraw
    local DRAW_DUR=`grep real /tmp/drawtesttime | sed 's/real//' | tr -d ' '`
    2>/tmp/readtesttime time -p ./shelltris.sh -testread
    local READ_DUR=`grep real /tmp/readtesttime | sed 's/real//' | tr -d ' '`
    2>/tmp/rottesttime time -p ./shelltris.sh -testrot
    local ROT_DUR=`grep real /tmp/readtesttime | sed 's/real//' | tr -d ' '`
    2>/tmp/cleartesttime time -p ./shelltris.sh -testclear
    local CLEAR_DUR=`grep real /tmp/cleartesttime | sed 's/real//' | tr -d ' '`

    # echo "DRAW DUR: $DRAW_DUR"
    # echo "READ DUR: $READ_DUR"
    # echo "CLEAR_DUR: $CLEAR_DUR"

    # echo "(1 - ($DRAW_DUR / 16.30000))"
    local CLEAR_SINGLE=`echo "scale=20; ( $CLEAR_DUR / 10 )" | bc`
    local DRAW_SINGLE=`echo "scale=20; (($DRAW_DUR - $CLEAR_SINGLE) / 16.30000)" | bc`
    local ROT_SINGLE=`echo "scale=20; ($ROT_DUR / 16.30000)" | bc`
    local READ_SINGLE=`echo "scale=20; ($READ_DUR / 200)" | bc`
    local REMAINDER_AFTER_DRAW=`echo "scale=20; (1 - ($DRAW_DUR / 16.30000))" | bc`
    # echo "REM: $REMAINDER_AFTER_DRAW"
    ONE_SECOND=`echo "scale=0; 20000 * $REMAINDER_AFTER_DRAW / $READ_DUR" | bc`
    CUR_TICK_RATE=$ONE_SECOND

    DRAW_PENALTY=`echo "scale=0; 100 * $DRAW_SINGLE / $READ_SINGLE" | bc`
    ROT_PENALTY=`echo "scale=0; ((100 * $ROT_SINGLE) - (100 * $READ_SINGLE)) / $READ_SINGLE" | bc`

    # DEBUG=1
    if [ $DEBUG -eq 1 ] ; then
	stty sane
	echo '[m'
	echo '[?25h'
	clear
	echo "Here is your calibration data.  Please change the values in"
	echo "the script and uncomment the relevant lines."
	echo
	echo "DRAW SINGLE IS $DRAW_SINGLE"
	echo "ROTATE SINGLE IS $ROT_SINGLE"
	echo "READ SINGLE IS $READ_SINGLE"
	echo "REMAINDER AFTER DRAW IS $REMAINDER_AFTER_DRAW"
	echo "ONE SECOND IS $ONE_SECOND"
	echo "DRAW PENALTY IS $DRAW_PENALTY"
	echo "ROT PENALTY IS $ROT_PENALTY"
	echo "CALIBRATED=\"yes\""
	echo "ONE_SECOND=$ONE_SECOND"
	echo "DRAW_PENALTY=$DRAW_PENALTY"
	echo "ROT_PENALTY=$ROT_PENALTY"

	echo
	show_cursor
	exit 0;
    fi
}

function draw_white_block()
{
	R=$1
	C=$2
	draw_block $R $C "block" "$WHITE"
}

function get_tick_rate()
{
	# local MULTIPLIER=$(( 80 * ( 90 ** $LEVEL ) ))
	# local DIVISOR=$(( 100 * (100 ** $LEVEL ) ))
	# This wraps over negative in level 7.  Oops.
	# local TICK_RATE=$(( $ONE_SECOND * 80 * ( 90 ** $LEVEL ) / ( 100 * ( 100 ** $LEVEL ) ) ))

	local TICK_RATE=`echo "scale=0; $ONE_SECOND * 80 * ( 90 ^ $LEVEL ) / ( 100 * ( 100 ^ $LEVEL ) )" | bc`
	# echo "$ONE_SECOND : $LEVEL: $TICK_RATE" >> tr_log
	echo $TICK_RATE
}


# echo "$SHLVL ARG1: $1" >> arg_log

if [ "x$ARG1" = "x-testclear" ] ; then
	clearboard_test;
	exit 0;
fi

if [ "x$ARG1" = "x-testdraw" ] ; then
	draw_test;
	exit 0;
fi

if [ "x$ARG1" = "x-testread" ] ; then
	read_test;
	exit 0;
fi

if [ "x$ARG1" = "x-testrot" ] ; then
	# echo "$SHLVL TESTROT" >> arg_log
	read_test rot
	exit 0;
# else
	# echo "$SHLVL NOT TESTROT" >> arg_log
fi

### if [ "x$ARG1" = "x-testrot" ] ; then
	### # This instruction should NEVER be reached, but due to a bug in
	### # BASH in Mac OS X 10.4, it is reached every time.
	### exit 0;
	### And oddly enough, the problem disappeared just as suddenly
	### as it appeared.
### fi
# echo "$SHLVL ARG1A: $ARG1" >> arg_log

# Sanitize the terminal.
stty sane
echo '[m'
echo '[?25h'
clear



# echo LL is $LL
# echo LC is $LC
# sleep 5

vtreset
# draw_block 5 5 "box"

SCORE=0
LEVEL=0
draw_lines

if [ "$CALIBRATED" = "yes" ] ; then
	SCORE="Initializing.  Please wait."
else
	SCORE="Calibrating.  Please wait."
fi
draw_status
set_row_copy

TEST=0

if [ $TEST -eq 2 ] ; then
	# clear
	# echo "Clearing board";
	clear_board
	# echo "Board cleared";
	r1c35=1
	r2c36=1
	r3c35=1
	r4c36=1
	r5c35=1
	r10c33=1
	r11c33=1
	r11c32=1
	r12c32=1
	fill_board
	# echo "Board filled";
fi

if [ $TEST -eq 1 ] ; then
	vtsetpos 1 1
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"
	echo "$BLACK$FILLED                                                               $RESET$CLEAR"

	draw_four 5 3 0
	draw_four 5 7 1
	draw_four 5 11 2
	draw_four 5 15 3
	draw_white_block 5 3
	draw_white_block 5 7
	draw_white_block 5 11
	draw_white_block 5 15

	draw_sblock 10 3 0
	draw_sblock 10 7 1
	draw_sblock 10 11 2
	draw_sblock 10 15 3
	draw_white_block 10 3
	draw_white_block 10 7
	draw_white_block 10 11
	draw_white_block 10 15

	draw_square 15 3
	draw_white_block 15 3

	draw_sblock_r 20 3 0
	draw_sblock_r 20 7 1
	draw_sblock_r 20 11 2
	draw_sblock_r 20 15 3
	draw_white_block 20 3
	draw_white_block 20 7
	draw_white_block 20 11
	draw_white_block 20 15

	draw_lblock 5 24 0
	draw_lblock 5 28 1
	draw_lblock 5 32 2
	draw_lblock 5 36 3
	draw_white_block 5 24
	draw_white_block 5 28
	draw_white_block 5 32
	draw_white_block 5 36

	draw_lblock_r 10 24 0
	draw_lblock_r 10 28 1
	draw_lblock_r 10 32 2
	draw_lblock_r 10 36 3
	draw_white_block 10 24
	draw_white_block 10 28
	draw_white_block 10 32
	draw_white_block 10 36

	draw_tblock 15 24 0
	draw_tblock 15 28 1
	draw_tblock 15 32 2
	draw_tblock 15 36 3
	draw_white_block 15 24
	draw_white_block 15 28
	draw_white_block 15 32
	draw_white_block 15 36

	sleep 60; exit
fi

# NEXT='sblock'
NEXT='sblock_r'
# NEXT='lblock'
# NEXT='lblock_r'
# NEXT='tblock'
# NEXT='four'
# NEXT='square'

if [ $TEST -eq 1 ] ; then

MODE="box"
draw_next 0
sleep 1
MODE="clear"
draw_next 0
MODE="box"
draw_next 1
sleep 1
MODE="clear"
draw_next 1
MODE="box"
draw_next 2
sleep 1
MODE="clear"
draw_next 2
MODE="box"
draw_next 3

sleep 10
exit 0
fi

CONT=1
PAUSED=0
stty $TTYSETTING

if [ "x$ARG1" = "x-calibrate" ] ; then
	calibrate_timers 1
else
	calibrate_timers 0
fi

SCORE="Press 'p' to play, 'q' to quit."
clear_status
draw_status

# main program loop
while [ $CONT -eq 1 ] ; do

    WAITING=1
    while [ $WAITING -eq 1 ] ; do
	CHAR=`./getch`
	if [ "x$CHAR" = "xq" ] ; then
		WAITING=2
		CONT=0
	fi
	if [ "x$CHAR" = "xQ" ] ; then
		WAITING=2
		CONT=0
	fi
	if [ "x$CHAR" = "xp" ] ; then
		WAITING=0
	fi
	if [ "x$CHAR" = "xP" ] ; then
		WAITING=0
	fi
    done

    if [ $WAITING -eq 0 ] ; then
      # Configure the game

      SCORE=$INITIAL_SCORE
      PAUSED=0
      set_level
      clear_status
      draw_status
      clear_board

      choose_next;
      choose_next;

      HPOS=$STARTING_HPOS # $(( ( $LC - 13 ) / 2 ))
      VPOS=$STARTING_VPOS

      MODE="box"
      draw_next 0

      GAMEOVER=0
      GLOBAL_TICK_COUNT=0
      ROT=0
      CUR_TICK_RATE=`get_tick_rate`
      # Start the game loop.
      while [ $GAMEOVER -eq 0 ] ; do
	# echo -n "Enter a character: "
	CHAR=`./getch`
	if [ "x$CHAR" = "x" ] ; then
		# echo "NO DATA";
		echo -n '' # We will delay here eventually.
	elif [ $PAUSED -eq 1 ] ; then
		PAUSED=0;
		SCORE=$PAUSEDSCORE
		# echo END: $GLOBAL_TICK_COUNT >> pause_log
		clear_status
		draw_status
	else
		# echo "DATA: $CHAR ($GLOBAL_TICK_COUNT)" >> pause_log
		# echo "DATA: $CHAR";

		case $CHAR in
			( "p" | "P" )
				PAUSED=1;
				PAUSEDSCORE=$SCORE
				SCORE="$SCORE      --- Paused --- $GLOBAL_TICK_COUNT"
				# echo START: $GLOBAL_TICK_COUNT >> pause_log
				draw_status
			;;
			( "q" | "Q" )
				GAMEOVER=1
				# CONT=0 # Don't quit.
			;;
			( "z" | "Z" )
				CP=`check_piece  $VPOS $(expr $HPOS - 1) $ROT`
				if [ $CP = "00" -o $CP = "01" ] ; then
					move_left
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
				fi
			;;
			( "x" | "X" )
				CP=`check_piece  $VPOS $(( $HPOS + 1 )) $ROT`
				if [ $CP = "00" -o $CP = "01" ] ; then
					move_right
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
				fi
			;;
			( ',' )
				# echo $ROT >> /tmp/lastrot
				if [ "$ROT" -eq 0 ] ; then
					NEWROT=3
				else
					NEWROT=$(( $ROT - 1 ))
				fi
				CP=`check_piece $VPOS $HPOS $NEWROT`
				if [ $CP = "00" -o $CP = "01" ] ; then
					MODE="clear"
					draw_cur "$ROT"
					ROT=$NEWROT
					MODE="box"
					draw_cur $ROT
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $ROT_PENALTY ))
				fi
			;;
			( '.' )
				if [ $ROT -eq 3 ] ; then
					NEWROT=0
				else
					NEWROT=$(( $ROT + 1 ))
				fi
				CP=`check_piece $VPOS $HPOS $NEWROT`
				if [ $CP = "00" -o $CP = "01" ] ; then
					MODE="clear"
					draw_cur $ROT
					ROT=$NEWROT
					MODE="box"
					draw_cur $ROT
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $ROT_PENALTY ))
				fi
			;;
			## ( "g" )
				## # @@@ DAG DELETE ME
				## # This is to show what should happen when the game
				## # is over.
				## GAMEOVER=1
			## ;;
			## ( "f" )
				## # @@@ DAG DELETE ME
				## # This is for debugging only.
				## fill_board
				## sleep 5
				## exit
			## ;;
			## ( "n" )
				## # @@@ DAG DELETE ME
				## # This is to show what should happen when a block
				## # hits something.
				## place_piece
				## choose_next;
				## VPOS=$STARTING_VPOS;
				## HPOS=$STARTING_HPOS # $(( ( $LC - 13 ) / 2 ))
				## draw_next 0
				## GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
			## ;;
			( ' ' )
				# Drop down sooner
				CP=`check_piece  $(expr $VPOS + 1) $HPOS $ROT`
				# echo "CP: $CP" >> cp_log
				if [ $CP = "20" ] ; then
					# Hit a block, but past the top edge.
					place_piece
					choose_next;

					ROT=0
					VPOS=$STARTING_VPOS;
					HPOS=$STARTING_HPOS # $(( ( $LC - 13 ) / 2 ))
					CP=`check_piece $VPOS $HPOS $ROT`
					if [ $CP = 21 ] ; then
						# Can't place next piece.
						GAMEOVER=1;
					else
						draw_next $ROT
						GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
					fi
				elif [ $CP = "21" ] ; then
					# Hit a block.  Piece never made it
					# through the top.
					GAMEOVER=1;
				else
					MODE="clear";
					draw_cur $ROT;
					VPOS=$(( $VPOS + 1 ))
					MODE="box";
					draw_cur $ROT
					# Delay the next drop until a full period has elapsed.
					GLOBAL_TICK_COUNT=0
				fi
			;;
		esac
	fi

	if [ $PAUSED -ne 1 ] ; then
		# Change the next entry every few cycles for now.
		GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + 100 ))
		while [ $GLOBAL_TICK_COUNT -ge $CUR_TICK_RATE ] ; do
			# echo "WHILECOUNT: $GLOBAL_TICK_COUNT" >> pause_log
			GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT - $CUR_TICK_RATE ))
			CP=`check_piece  $(( $VPOS + 1 )) $HPOS $ROT`
			# echo "CP: $CP" >> cp_log
			if [ $CP = "20" ] ; then
				# Hit a block, but past the top edge.
				place_piece
				choose_next;

				ROT=0
				VPOS=$STARTING_VPOS;
				HPOS=$STARTING_HPOS # $(( ( $LC - 13 ) / 2 ))
				CP=`check_piece  $(($VPOS + 1)) $HPOS $ROT`
				if [ $CP = 21 ] ; then
					# Can't place next piece.
					GAMEOVER=1;
				else
					draw_next $ROT
					GLOBAL_TICK_COUNT=$(( $GLOBAL_TICK_COUNT + $DRAW_PENALTY ))
				fi
			elif [ $CP = "21" ] ; then
				# Hit a block.  Piece never made it
				# through the top.
				GAMEOVER=1;
			else
				MODE="clear";
				draw_cur $ROT;
				VPOS=$(( $VPOS + 1 ))
				MODE="box";
				draw_cur $ROT
			fi
		done
	fi # (not PAUSED)
      done # game loop
      SCORE="$SCORE      GAME OVER.  Press 'p' to play, 'q' to quit."
      draw_status
    fi
done # main program loop

stty sane # Reenable line mode

vtsetpos $LL $LC
echo
show_cursor
