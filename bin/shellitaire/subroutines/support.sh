#!/bin/sh

# Safety wrapper.
if [ "x$SUPPORT_INCLUDED" = "x" ] ; then

SUPPORT_INCLUDED=1

# /*! Configure echo -n support. */
configure_echon()
{
	X=`echo -n "x"`;
	if [ "$X" = "x" ] ; then
		ECHO_DASHN=1
	else
		ECHO_DASHN=0;
	fi
}

# /*! Set cursor position */
setpos()
{
	local R=$1
	local C=$2
	echon "[$R;$C""H"
}

# /*! echo -n is faster than printf. */
echon()
{
	ECHO_DASHN_BROKEN=1

	if [ $ECHO_DASHN = 1 ] ; then
		if [ $ECHO_DASHN_BROKEN = 1 ] ; then
			printf "%s" "$1"
		else
			echo -n "$1"
		fi
	else
		echo "$1\c"
	fi

}

configure_echon

fi # Safety wrapper.

