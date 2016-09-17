#!/usr/bin/env perl
# trainerLevel.pl by Amory Meltzer
# Estimate XP gain in PoGo
# Inspired by https://github.com/thomasdondorf/poke-level-calc (https://trainerlevelspeed.de/)
# Doesn't handle bias toward accelerated gains

use strict;
use warnings;
use diagnostics;

use Getopt::Std;
use Date::Calc qw(Delta_Days);

# Globals
my ($xp,$date,$rate);

my %opts = ();
getopts('x:d:r:hH', \%opts);
if($opts{x}) { $xp = $opts{x}; } # Current XP
if($opts{d}) { $date = $opts{d}; } # Pick a date, why? FIXME TODO
if($opts{r}) { $rate = $opts{r}; } # Rate of XP gain per day
if($opts{H} || $opts{h}) { &usage; exit; } # Usage


# Current XP is required
if (!$xp || $xp !~ /^\d+$/) {
  print "An integer amount of XP is required.\n";
  exit 1;
} else {
  my @date = (2016, 7, 7);
  my @today = (localtime)[5,4,3]; # mon, day, year
  $today[0] += 1900;		  # Stupid epoch
  $today[1]++;			  # localtime is 0-indexed, Delta_Days isn't

  my $days = Delta_Days(@date, @today);
  # Original value if defined, 0 if not
  $rate ||= int($xp/$days);

  print "xp: $xp\tdays: $days\trate: $rate\n";
  exit 0;
}



#### Usage statement ####
# Use POD or whatever?
# Escapes not necessary but ensure pretty colors
# Final line must be unindented?
sub usage
{
    print <<USAGE;
Usage: $0 -x <current XP> [-d <MM/DD/YY>] [-r <XP gain per day>]
      -x Current XP amount.  Required.
      -d Specify a date of the form MM/DD/YY. For what?
      -r Estimate XP gain per day
      -hH print this message
USAGE
}
