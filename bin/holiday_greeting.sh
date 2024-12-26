#!/usr/bin/env bash
# holiday_greeting by Amory Meltzer
# Be festive!
# Derived from Jonathan's .bashrc file (by ~71KR117)
# http://dotshare.it/dots/516/ (deadlink)

case $(date +"%B %d") in
    "January 01")
	# get current year (for new years greeting)
	year=$(date +"%Y")
	holgreet="${Color_Magenta_Intense}Happy ${Color_Red_Intense}New ${Color_Blue_Intense}Year!${Color_zOff} Have a great $year.";;
    "February 02") holgreet="Happy Groundhog Day!";;
    "February 14") holgreet="Happy ${Color_Magenta}Valentine's Day!${Color_zOff}";;
    "April 01") holgreet="${Color_zOff}It's April Fools' Day, ${Color_Red_Intense}beware${Color_zOff}!";;
    "July 04") holgreet="Happy ${Color_Red_Intense}Fourth ${Color_White_Intense}of ${Color_Blue_Intense}July!${Color_zOff}";;
    "August 12") holgreet="Happy Birthday!";;
    "October 31") holgreet="${Color_Red_Bold}Happy Halloween!${Color_zOff}";;
    "December 24") holgreet="Happy ${Color_Green_Intense}Christmas ${Color_Red}Eve!${Color_zOff}";;
    "December 25") holgreet="${Color_Green_Intense}Merry ${Color_Red}Christmas!${Color_zOff}";;
    "December 31") holgreet="Happy New Year's Eve!";;
    *) exit 0;;
esac
if [[ -n "$holgreet" ]]; then
    echo -e "$holgreet"
fi
