#!/usr/bin/env bash
# http://misc.flogisoft.com/bash/tip_colors_and_formatting

# This program is free software. It comes without any warranty, to
# the extent permitted by applicable law. You can redistribute it
# and/or modify it under the terms of the Do What The Fuck You Want
# To Public License, Version 2, as published by Sam Hocevar. See
# http://sam.zoy.org/wtfpl/COPYING for more details.

for fgbg in 38 48 ; do #Foreground/Background

	for color in {0..15} ; do #Basic colors
	#Display the color
	echo -en "\e[${fgbg};5;${color}m ${color}\t\e[0m"
	#Display 8 colors per line
	if [ $((($color + 1) % 8)) == 0 ] ; then
	    echo #New line
	fi
  done

  for color in {16..255} ; do #Colors
	echo -en "\e[${fgbg};5;${color}m ${color}\t\e[0m"
	#Display 6 colors per line, nicer pattern here
	if [ $((($color + 3) % 6)) == 0 ] ; then
	    echo #New line
	fi
  done
  echo #New line
done

exit 0
