#!/bin/sh

. $GAMEDIR/subroutines/support.sh

A_1='  /\   '
A_2=' /--\  '
A_3='/    \ '
A_width=7

B_1='+---, '
B_2='|--<  '
B_3='|___| '
B_width=6

C_1=',---, '
C_2='|     '
C_3='\___, '
C_width=6

D_1='+--,  '
D_2='|   | '
D_3='|__/  '
D_width=6

E_1='+--- '
E_2='|--  '
E_3='|___ '
E_width=5

F_1='+--- '
F_2='|--  '
F_3='|    '
F_width=5

G_1=',---, '
G_2='|  __ '
G_3='|__/| '
G_width=6

H_1='|   | '
H_2='|---| '
H_3='|   | '
H_width=6

I_1='----- '
I_2='  |   '
I_3='__|__ '
I_width=6

J_1='    | '
J_2='    | '
J_3=" L__J "
J_width=6

K_1='| _/ '
K_2="|(   "
K_3="| '\ "
K_width=5

L_1='|    '
L_2='|    '
L_3='|____'
L_width=6

M_1='|\ /| '
M_2='| V | '
M_3='|   | '
M_width=6

N_1='|,  | '
N_2='| \ | '
N_3='|  \| '
N_width=6

O_1=' ,--,  '
O_2='|    | '
O_3=" \__/  "
O_width=6

P_1=',---, '
P_2="|---' "
P_3='|     '
P_width=6

Q_1=' ,--,  '
Q_2='|    | '
Q_3=" \__X_ "
Q_width=7

R_1=',---, '
R_2='|--<  '
R_3='|   \ '
R_width=6

S_1='.---  '
S_2='|___  '
S_3=' ___| '
S_width=6

T_1='----- '
T_2='  |   '
T_3='  |   '
T_width=6

U_1='|   | '
U_2='|   | '
U_3=' \_/  '
U_width=6

V_1='\   / '
V_2=' \ /  '
V_3='  V   '
V_width=6

W_1='| | | '
W_2='| | | '
W_3=' V V  '
W_width=6

X_1='\ / '
X_2=' X  '
X_3='/ \ '
X_width=4

Y_1='\ / '
Y_2=' |  '
Y_3=' |  '
Y_width=4

Z_1='---- '
Z_2='  /  '
Z_3='/___ '
Z_width=5

widespace_1='        '
widespace_2='        '
widespace_3='        '
widespace_width=8

space_1='   '
space_2='   '
space_3='   '
space_width=3

period_1='   '
period_2='   '
period_3='o  '
period_width=3

comma_1='   '
comma_2='   '
comma_3='g  '
comma_width=3

exclamation_1='n  '
exclamation_2='U  '
exclamation_3='o  '
exclamation_width=3

slash_1='  / '
slash_2=' /  '
slash_3='/   '
slash_width=4

question_1=' --, '
question_2=" ,-' "
question_3=' o   '
question_width=5

colon_1='   '
colon_2=' o '
colon_3=' o '
colon_width=3

hyphen_1='   '
hyphen_2='==='
hyphen_3='   '
hyphen_width=3

equals_1='___'
equals_2='___'
equals_3='   '
equals_width=3

N0_1=' /\ '
N0_2='| /|'
N0_3=' \/ '
N0_width=4

N1_1='/| '
N1_2=' | '
N1_3='_|_'
N1_width=3

N2_1=',-,'
N2_2='  /'
N2_3='/__'
N2_width=3

N3_1='__,'
N3_2=' _|'
N3_3='__|'
N3_width=3

N4_1='|  |'
N4_2="'--|"
N4_3='   |'
N4_width=4

N5_1='+---'
N5_2="'--,"
N5_3=',__|'
N5_width=4

N6_1=' / '
N6_2='/-,'
N6_3='\_/'
N6_width=3

N7_1='--,'
N7_2=' / '
N7_3='/  '
N7_width=3

N8_1=' __ '
N8_2='<__>'
N8_3='<__>'
N8_width=4

N9_1=',-,'
N9_2="'-|"
N9_3=' / '
N9_width=3

max_width=8

# echo "$exclamation_1"
# echo "$exclamation_2"
# echo "$exclamation_3"


# Get the name associated with a character.
getcharname()
{
	local char=$1

	# echo "CHAR: \"$char\"";

	# if [ "$char" = "widespace" ] ; then
		# echo "WIDE SPACE"
		# exit 0;
	# fi

	case "$char" in
		(.)
			char="period" ;;
		(",")
			char="comma" ;;
		("/")
			char="slash" ;;
		("?")
			char="question" ;;
		(":")
			char="colon" ;;
		("-")
			char="hyphen" ;;
		("=")
			char="equals" ;;
		("!")
			char="exclamation" ;;
		(" ")
			char="space" ;;
		("widespace")
			char="widespace" ;;
		( "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" )
			char="N$char" ;;
		(*)
			char=`echo "$char" | tr 'a-z' 'A-Z'` ;;
	esac
	
	# local LEN=`eval echo "\$""$char""_width"`
	# echo "CHAR IS '$char', length '$LEN'"; return $LEN
	echo $char
}

# /*! Print a status line character. */
printchar()
{
	local R=$1
	local C=$2
	local char=$3

	char="$(getcharname "$char")"

	setpos $R $C
	eval echon "\"\$$char""_1\""
	setpos $(($R + 1)) $C
	eval echon "\"\$$char""_2\""
	setpos $(($R + 2)) $C
	eval echon "\"\$$char""_3\""

	# Return the width.
	local LEN=`eval echo "\$""$char""_width"`
	return $LEN
}

# /*! Print a status line string.  Returns the new column number. */
printstring()
{
R=$1
C=$2
STRING=$3
local LINE_0=""
local LINE_1=""
local LINE_2=""
local CHAR_0
local CHAR_1
local CHAR_2
local CHAR_LEN

local NEWCOL="$C"

while [ "x$STRING" != "x" ] ; do
	CHAR="$(echo "$STRING" | sed 's/\(.\).*$/\1/')"
	STRING="$(echo "$STRING" | sed 's/^.//')"

	# New fast way
	CHARNAME="$(getcharname "$CHAR")"
	CHAR_0="$(eval echon "\"\$$CHARNAME""_1\"")"
	CHAR_1="$(eval echon "\"\$$CHARNAME""_2\"")"
	CHAR_2="$(eval echon "\"\$$CHARNAME""_3\"")"
	CHAR_LEN=`eval echo "\$""$CHARNAME""_width"`

	# echo "LEN: $CHAR_LEN"

	LINE_0="$LINE_0$CHAR_0 "
	LINE_1="$LINE_1$CHAR_1 "
	LINE_2="$LINE_2$CHAR_2 "

	# Old slow way
	# printchar "$R" "$C" "$CHAR"
	# CHAR_LEN=$?

	# echo "LEN WAS $CHAR_LEN"
	NEWCOL=$(($NEWCOL + $CHAR_LEN + 1))
	# echo "COL NOW $NEWCOL"
done
	setpos $R $C
	echo "$LINE_0"
	setpos $(($R + 1)) $C
	echo "$LINE_1"
	setpos $(($R + 2)) $C
	echo "$LINE_2"

return $NEWCOL
}

getkey()
{
	dd if=/dev/tty bs=1 count=1 2>/dev/null
}

getinitials()
{
	local INITIALSDONE=0
	local ROW=$1
	local COL=$2
	printstring $ROW $COL "Enter initials: "
	COL=$?
	local POS=0
	while [ $INITIALSDONE != 1 ] ; do
		key="$(getkey)"
		isnl=`echon "$key" | tr -d '1' | tr '\r' '1' | tr '\n' '1'`
		isalnum=`echon "$key" | tr '[:alnum:]' '1' | grep -v '[^1]'`
		# echo "KEY: \"$key\", isnl: $isnl" >> /tmp/staterecord
		if [ "x$isnl" = "x1" ] ; then
			# echo "newline"
			if [ $POS -ne 0 ] ; then
				INITIALSDONE=1
			fi
		elif [ "$key" = "" -o "$key" = "" ] ; then
			# echo "backspace"
			case $POS in
				(0)
					echon '' ;;
				(1)	POS0='' 
					POS=0
					printchar "$ROW" "$(($COL + ( $POS * $max_width ) ))" widespace
					setpos "$(( $ROW + 2 ))" "$(($COL + ( $POS * $max_width ) ))"
					;;
				(2)	POS1='' 
					POS=1
					printchar "$ROW" "$(($COL + ( $POS * $max_width ) ))" widespace
					setpos "$(( $ROW + 2 ))" "$(($COL + ( $POS * $max_width ) ))"
					;;
				(3)	POS2='' 
					POS=2
					printchar "$ROW" "$(($COL + ( $POS * $max_width ) ))" widespace
					setpos "$(( $ROW + 2 ))" "$(($COL + ( $POS * $max_width ) ))"
					;;
			esac
		elif [ "x$isalnum" = "x1" ] ; then
			# echo "otherkey"
			case $POS in
				(0)
					POS0=$key 
					printchar "$ROW" "$(($COL + ( $POS * $max_width ) ))" "$key"
					POS=$(( $POS + 1 ))
					;; 
				(1)
					POS1=$key
					printchar "$ROW" "$(($COL + ( $POS * $max_width ) ))" "$key"
					POS=$(( $POS + 1 ))
					;; 
				(2)
					POS2=$key
					printchar "$ROW" "$(($COL + ( $POS * $max_width ) ))" "$key"
					POS=$(( $POS + 1 ))
					;; 
			esac
		elif [ "x$key" = "x" ] ; then
			# Get rid of escape sequences like arrow keys.
			x="$(getkey)"
			if [ "x$x" = "x[" ] ; then
				# Arrow keys are esc-[-A through esc-[-D
				x="$(getkey)";
			fi
		fi
	done
	INITIALS="$POS0$POS1$POS2"
}

configure_echon

# printstring 3 5 "This is a test."
# printstring 6 5 "A test, this is."
# printstring 9 5 "Is this a test?"
# printstring 12 5 "This IS a test!"

TEST=0

if [ $TEST = 1 ] ; then
	printstring 3 5 ABCDEFGHIJKLMNOP
	printstring 7 5 QRSTUVWXYZ:-=
	printstring 10 5 0123456789
	echo; echo

	stty raw
	getinitials 15 1
	echo
	echo
	printstring 22 5 "Initials are $INITIALS"
	echo ; echo
	reset
	exit
fi

