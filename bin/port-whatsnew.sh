#!/usr/bin/env port-tclsh
# -*- coding: utf-8; mode: tcl; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- vim:fenc=utf-8:ft=tcl:et:sw=4:ts=4:sts=4
#
# Show the git log of what changed since this port was installed.
set MY_VERSION 0.2

proc printUsage {} {
    puts "Usage: $::argv0 \[-hvV\] port-name"
    puts "  -h    This help"
    puts "  -v    verbose output"
    puts "  -V    show version and MacPorts version being used"
    puts ""
    puts "port-name is the name of the port whose log you want to see"
}


# Begin

set verbose 0
set showVersion 0

while {[string index [lindex $::argv 0] 0] == "-" } {
    switch [string range [lindex $::argv 0] 1 end] {
        h {
            printUsage
            exit 0
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

set sinceDate "[clock format $installedDate -format {%Y-%m-%dT%H:%M:%SZ} -gmt 1]"

# get the git log since then
array set portInfo [lindex $portSearchResult 1]

set portGitUrl https://api.github.com/repos/macports/macports-ports/commits?path=$portInfo(portdir)/Portfile&since=$sinceDate

set tempfile [mktemp "/tmp/mports.whatsnew.XXXXXXXX"]
set curl_options {}
if {[catch {curl fetch {*}$curl_options ${portGitUrl} $tempfile} error]} {
    puts "error getting git log $error"
    exit 1
}

set log {}
set chan [open $tempfile "r"]

while {1} {
    set line [gets $chan]
    if {[eof $chan]} {
        close $chan
        break
    }
    # "message" : "git commit message"
    if {[string match "*message*" $line]} {
        lappend log [set gitmessage [lindex [split $line \"] 3]]
    }
}

file delete -force $tempfile

if {[llength [split $log "\n"]] == 0} {
    puts "No changes have been committed for $portname since you installed version ${version_selected}_$revision_selected$variant_selected on [clock format $installedDate -format {%Y-%m-%d at %H:%M:%S}]."
    exit 0
}

puts "The following changes have been committed for $portname since you installed version ${version_selected}_$revision_selected$variant_selected on [clock format $installedDate -format {%Y-%m-%d at %H:%M:%S}]:"
# This is still a hack - need better logic/display
foreach messageline [lrange $log 0 end] {
    # each git message should have a blank line after first line/summary
    set list_x [split [string map [list {\n\n} "\x00"] $messageline] "\x00"]
    foreach x $list_x first 1 {
        if {$first == 1} {
            puts $x
        } else {
            puts -nonewline "  "
            puts [string map {\\n "  "} $x]
        }
        if !{$verbose} break;
    }
}
