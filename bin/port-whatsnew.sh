#!/usr/bin/tclsh
# -*- coding: utf-8; mode: tcl; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:fenc=utf-8:ft=tcl:et:sw=4:ts=4:sts=4
#
# Show the svn log of what changed since this port was installed.


set MY_VERSION 0.1


proc printUsage {} {
    puts "Usage: $::argv0 \[-hvV\] \[-t macports-tcl-path\] port-name"
    puts "  -h    This help"
    puts "  -t    Give a different location for the base MacPorts Tcl"
    puts "        file (defaults to /Library/Tcl)"
    puts "  -v    verbose output"
    puts "  -V    show version and MacPorts version being used"
    puts ""
    puts "port-name is the name of the port whose log you want to see"
}


# Begin

set macportsTclPath /Library/Tcl
set verbose 0
set showVersion 0

while {[string index [lindex $::argv 0] 0] == "-" } {
    switch [string range [lindex $::argv 0] 1 end] {
        h {
            printUsage
            exit 0
        }
        t {
            if {[llength $::argv] < 2} {
                puts "-t needs a path"
                printUsage
                exit 1
            }
            set macportsTclPath [lindex $::argv 1]
            set ::argv [lrange $::argv 1 end]
        }
        v {
             set verbose 1
        }
        V {
            set showVersion 1
        }
        default {
            puts "Unknown option [lindex $::argv 0]"
            printUsage
            exit 1
        }
    }
    set ::argv [lrange $::argv 1 end]
}

source ${macportsTclPath}/macports1.0/macports_fastload.tcl
package require macports
mportinit

if {$showVersion} {
    puts "Version $MY_VERSION"
    puts "MacPorts version [macports::version]"
    exit 0
}

if {![string equal ${macports::registry.format} "receipt_sqlite"]} {
    puts "registry format must be sqlite"
    exit 1
}

if {[llength $::argv] == 0} {
    puts "missing port-name"
    printUsage
    exit 1
}
set portname [lindex $::argv 0]

set portSearchResult [mportlookup $portname]
if {[llength $portSearchResult] < 1} {
    puts "port \"$portname\" not found"
    return [list]
}

if {[catch {set ilist [registry::installed $portname {}]}]} {
    puts "port \"$portname\" is not installed"
    return [list]
}

# find the active version, or if none is active, the latest version installed
set version_selected {}
foreach i $ilist {
    set variant [lindex $i 3]
    set version [lindex $i 1]
    set revision [lindex $i 2]
    set epoch [lindex $i 5]
    if { $version_selected == {} || $epoch > $epoch_selected ||
            ($epoch == $epoch_selected && [vercmp $version $version_selected] > 0)
            || ($epoch == $epoch_selected
                && [vercmp $version $version_selected] == 0
                && $revision > $revision_selected)} {
        set version_selected $version
        set revision_selected $revision
        set variant_selected $variant
        set epoch_selected $epoch
    }

    set isactive [lindex $i 4]
    if {$isactive == 1} {
        set version_selected $version
        set revision_selected $revision
        set variant_selected $variant
        set epoch_selected $epoch
        break
    }
}

# find when it was installed
set regref [registry::open_entry $portname $version_selected $revision_selected $variant_selected $epoch_selected]
set installedDate [registry::property_retrieve $regref date]

# get the svn log since then
array set portInfo [lindex $portSearchResult 1]
set portSvnUrl http://svn.macports.org/repository/macports/trunk/dports/$portInfo(portdir)/Portfile
if {[catch {set log [exec svn log -r "{[clock format $installedDate -format {%Y-%m-%dT%H:%M:%SZ} -gmt 1]}:HEAD" $portSvnUrl]}]} {
    puts "error getting svn log"
    exit 1
}

if {[llength [split $log "\n"]] == 1} {
    puts "No changes have been committed for $portname since you installed $portname @${version_selected}_$revision_selected$variant_selected on [clock format $installedDate -format {%Y-%m-%d at %H:%M:%S}]."
    exit 0
}

puts "The following changes have been committed for $portname since you installed $portname @${version_selected}_$revision_selected$variant_selected on [clock format $installedDate -format {%Y-%m-%d at %H:%M:%S}]:\n"
puts $log
