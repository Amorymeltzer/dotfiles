#!/usr/bin/env bash
# PerlSwitch.sh by Amory Meltzer
# script to switch the Perl5 major version
# https://lists.macosforge.org/pipermail/macports-dev/2012-May/019454.html
# FIX PREFIX
#
# ${1}  :   the installed version
# ${2}  :   the new version
#

#
# set prefix to where you've installed MacPorts
#
declare prefix='/macports'

#
# take a snapshot of the CPAN modules installed
# http://mailman.theapt.org/pipermail/openbsd-newbies/2003-October/002021.html
#
declare snapshot=$(perl -MCPAN -e autobundle | grep 'cpan/Bundle/Snapshot')

#
# install the new version of Perl5
#
port -s install perl5.${2} +shared +threads

#
# replace all installed old p5-<name>
# with new ones dependent on the new version of perl5
#
port -q installed name:^p5.${1} \
    | while read portname version active
do
    port -f deactivate ${portname}
    port -f -s install ${portname/${1}/${2}}
done

#
# make the new version of perl5 the default and major version
#
port -n -f upgrade --force --enforce-variants perl5 _erl5_${1} +perl5_${2}

#
# check if cpan has been initialised for the new version
#
cpan

#
# force uninstall of the old version of perl5
#
port -f uninstall perl5.${1}
port clean --all perl5.${1}


#
# find and re-install all ports dependent upon old perl5 extensions
#
( ( port installed name:^p5.${1} \
    | while read portname version active
	do
	    port -q dependents ${portname}
	done ) \
	    | sort -u \
	    | grep -v p5.${1} ) \
	    | xargs -n1 port -s -n upgrade --force

#
# update all cpan modules to the new version of perl5
#
#perl -MCPAN -e "install ${snapshot}"
cpan -u

#
# remove stray files
#
find ${prefix} -iname '*.mpsaved' -delete
find ${prefix} -iname '*.mp_*' -delete

#
# remove old perl5 extensions
#
port -q installed \
    | grep -v 'active' \
    | while read portname version
do
    port -f uninstall ${portname} ${version}
    port clean --all ${portname} ${version}
    done
