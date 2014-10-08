#!/usr/bin/env perl
# bus.pl by Amory Meltzer
# Display appropriate Unitrans L line times
# Unused months: Apr Oct

## TODO:
# Consolidate, cleanup (e.g. isHol at end, etc.)
# Display next bus more accurately
# Colorize output, esp. next bus
# Turn cascading if/then into while/given?
# Checkdate should use while to get all months accounted?

use strict;
use warnings;
use diagnostics;

use Term::ANSIColor;
use Getopt::Std;

my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst)=localtime(time);

# Globals
my ($holiday,$back,$fut,$home,$color);

# Parse commandline options
my %opts = ();
getopts('abfhd:tcH', \%opts);
if( $opts{a} ) { $back = $opts{a}; $fut = $opts{a}; } # Display all times
if( $opts{b} ) { $back = $opts{b}; } # Display past times
if( $opts{f} ) { $fut = $opts{f}; } # Display upcoming times
if( $opts{h} ) { $home = $opts{h}; } # Home times
# This is BEFORE the -t option so that if an idiot specifies -td
# The expected result (i.e., the day AFTER -d day) is reported
if( $opts{d} ) { ($mon,$mday) = split ('\/',$opts{d}); $mon--; } # Pick a date
# I want tomorrow's times, but not necessarily at today's hour.
# This at least gives me all of 'em
if( $opts{t} ) { $mday++; $back = $opts{t}; $fut = $opts{t}; } # Tomorrow's date
if( $opts{c} ) { $color = $opts{c}; } # Tomorrow's date
if( $opts{H} ) { &usage; exit; } # Usage

# Ensure we've got the correct date
#print "$mon\t$mday\t$wday\n";
checkDate() if $mday > 28;
#print "$mon\t$mday\t$wday\n";
# Figure out which day of the week it is
$wday = calcWday();
#print "$mon\t$mday\t$wday\n";
if ($wday == 6 || $wday == 0) { noWeekend(); }
elsif ($wday > 6)
{
    print "How'd you get past Sunday?\n";
    exit;
}
else # And... go!
{
    $holiday = isHol($mon, $mday);
    genTimes();
}


#######################
# Unused months: Apr
sub isHol
{
    my ($month,$day) = shift;

    # September
    if ($month == 8)
    {
	if ($mday == 1) { noWeekend(); }
	# Break
	else { return 2; }
    }

    # October
    if ($month == 9)
    {
	# Break
	if ($mday == 1) { $holiday = 2; }
	else { return 0; }
    }

    # November
    elsif ($month == 10 && $mday > 10)
    {
	if ($mday == 11 || $mday == 28) { noWeekend(); }
	elsif ($mday == 27) { noServ(); }
	else { return 0; }
    }

    # December
    elsif ($month == 11 && $mday > 14)
    {
	# Finals
	if ($mday < 20) { return 1; }
	# Break
	elsif ($mday > 21 && $mday < 24) { $holiday = 2; }
	elsif ($mday == 24 || $mday == 25 || $mday == 31) { noServ(); }
	elsif ($mday > 25 || $mday < 31) { noWeekend(); }
	else { return 0; }
    }

    # January
    elsif ($month == 0 && $mday < 20)
    {
	# Break
	if ($mday == 2) { return 2; }
	elsif ($mday == 1) { noServ(); }
	elsif ($mday == 19) { noWeekend(); }
	else { return 0; }
    }

    # February
    elsif ($month == 1 && $mday == 16) { noWeekend(); }

    # March
    elsif ($month == 2 && $mday > 16)
    {
	# Finals
	if ($mday < 22) { return 1; }
	# Break
	elsif ($mday > 22 && $mday < 28) { return 2; }
	else { return 0; }
    }

    # May
    elsif ($month == 4 && $mday == 25) { noWeekend(); }

    # June
    elsif ($month == 5 && $mday > 4)
    {
	# Finals
	if ($mday < 12) { return 1; }
	# Break
	elsif ($mday > 14) { return 2; }
    }

    # July and August
    elsif ($month == 6 || $month == 7) { return 2; }
    else { return 0; }
}

sub genTimes
{
    # Account for the early bus later
    my @hours = 6..20;
    # Monday-Thursday they go a bit later
    if ($wday > 0 && $wday < 5 && $holiday < 2)
    {
	push @hours, qw (21 22);
	# Even later during finals
	push @hours, 23 if $holiday == 1;
    }

    foreach my $hr (@hours)
    {
	# Limited to current hour and first few unless asked for
	next if ((!$fut && $hr > $hour+3) || (!$back && $hr < $hour));#+1?

	# Bus times switch after noon
	# First two are work, last two are home
	if ( $hr > 11 ) { genSched(qw(10 40 25 55), $hr); }
	elsif ($hr < 12) {genSched(qw(00 30 15 45), $hr);}
    }
    return;
}

sub noServ
{
    print "No service on $mday $mon, sorry.\n";
    exit;
}

sub noWeekend
{
    print "No useful weekend service, sorry.\n";
    exit;
}

# Won't take into account more than one month ahead (i.e., Feb 92)
# Maybe use a while loop?  A sub return 1 if error, etc.
# Consolidate?
sub checkDate
{
    if (($mon == 0 || $mon == 2 || $mon == 4 || $mon == 6 || $mon == 7 || $mon == 9 || $mon == 12) && $mday > 31)
    {
	minusPlus(31);
    }
    elsif (($mon == 3 || $mon == 5 || $mon == 8 || $mon == 10 || $mon == 11) && $mday > 30)
    {
	minusPlus(30);
    }
    elsif ($mon == 1 && $mday > 28) { minusPlus(28); }
}

sub minusPlus
{
    $mday -= shift;
    $mon++;
}

sub calcMonth
{
    my $deca = shift;
    my $out;

    # Feb, Mar, Nov
    if ($deca == 1 || $deca == 2 || $deca == 10) { $out = 2;}
    # Jan, Oct
    elsif ($deca == 0 || $deca == 9) { $out = 6; }
    # Apr, Jul
    elsif ($deca == 3 || $deca == 6) { $out = 5; }
    # Sep, Dec
    elsif ($deca == 8 || $deca == 11) { $out = 4; }
    # May
    elsif ($deca == 4) { $out = 0; }
    # Aug
    elsif ($deca == 7) { $out = 1; }
    # Jun
    elsif ($deca == 5) { $out = 3; }
    else
    {
	print "That month ($deca) semms not to exist.\n";
	exit;
    }
    return $out;
}
# Calculate the day of the week given MM/DD/YYYY
# gmmentalgym.blogspot.com/2011/03/day-of-week-for-any-date-revised.html
# UBER simplified thanks to limited (leap) years
sub calcWday
{
    # Year output is since 1900 (i.e. 114 in 2014)
    # Wrap around the week
    my $jabberwock = ($year+1900-2012+1+calcMonth($mon)+$mday)%7;
    return $jabberwock;
}


sub genSched
{
    my @mins = @_; # Potential minutes, current hour
    my $hora = pop @mins; # Take the hour

    # Ternary ?: ($a = $test ? $b : $c;)
    # a is b if test, c if not
    # Last two inputs are home times, first two are work times
    $home ? splice @mins, 0, -2 : splice @mins ,-2;

    # Only keep the second time if needed
    # NOT on holiday schedules or after 6
    # Weekend should be taken care of above
    pop @mins unless ($holiday == 0 && $hora < 18);

    # There's a lonely 6:30 to start the day off, useful for swimming
    if ($hora == 6) {
      shift @mins; # Get rid of the nonexistant bus at the top of the hour
      print "\t";  # Correct spacing
    }

    # Times switch over after the noon bus
    if ($hora == 12)
    {
	# Ternary ?: ($a = $test ? $b : $c;)
	# a is b if test, c if not
	$mins[0] = ($mins[0] == 10) ? "00": $mins[0]-10; # Display 12:00
    }

    # Color next hours' buses
    print color 'bright_red' if ($hora == $hour+1 && !$color);
    print "$hora:$_\t" foreach (@mins);
    print color 'reset' if ($hora == $hour+1 && !$color);
    print "\n";
}

#### Usage statement ####
# Use POD or whatever?
# Escapes not necessary but ensure pretty colors
# Final line must be unindented?
sub usage
{
    print <<USAGE;
Usage: $0 [-abfht -d <month[1-12]/day[1-31]>]
      -a Display all times (equivalent to -bf)
      -b Display all past times
      -f Display all upcoming times
      -h Display home times
      -t Display tomorrow\'s times
      -d Specify a date of the form MM/DD
      -c Turn off color printing
      -H print this message
USAGE
}
