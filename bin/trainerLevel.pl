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


## The lines below do not represent Perl code, and are not examined by the
## compiler.  Rather, they are the total XP needed for each level.
## From: https://www.reddit.com/r/TheSilphRoad/comments/4s1cpq/xp_requirements_for_levels_so_far_125/
__END__
1 0
  2 1000
  3 3000
  4 6000
  5 10000
  6 15000
  7 21000
  8 28000
  9 36000
  10 45000
  11 55000
  12 65000
  13 75000
  14 85000
  15 100000
  16 120000
  17 140000
  18 160000
  19 185000
  20 210000
  21 260000
  22 335000
  23 435000
  24 560000
  25 710000
  26 900000
  27 1100000
  28 1350000
  29 1650000
  30 2000000
  31 2500000
  32 3000000
  33 3750000
  34 4750000
  35 6000000
  36 7500000
  37 9500000
  38 12000000
  39 15000000
  40 20000000
