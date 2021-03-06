#!/usr/bin/env perl
# trainerLevel.pl by Amory Meltzer
# Estimate XP gain in PoGo
# Inspired by https://github.com/thomasdondorf/poke-level-calc (https://trainerlevelspeed.de/)
# Doesn't handle bias toward accelerated gains

use strict;
use warnings;
use diagnostics;

use Getopt::Std;
use Date::Calc qw(Delta_Days Add_Delta_Days); # or qw(:all)

# Globals
my ($xp,$date,$rate,$dist,$catch,$spin,$berry);
my %lvls;
my $bDays;

my %opts = ();
getopts('x:d:r:l:c:s:b:hH', \%opts);
if($opts{x}) { $xp = $opts{x}; }	   # Current XP
if($opts{r}) { $rate = $opts{r}; }	   # Rate of XP gain per day
if($opts{l}) { $dist = $opts{l}; }	   # Current distance walked
if($opts{c}) { $catch = $opts{c}; }	   # Current catches
if($opts{s}) { $spin = $opts{s}; }	   # Current stops spun
if($opts{b}) { $berry = $opts{b}; }	   # Current berries fed
if($opts{H} || $opts{h}) { &usage; exit; } # Usage


# Current XP is required
if (!$xp || $xp !~ /^\d+$/) {
  print "An integer amount of XP is required.\n";
  exit 1;
} else {
  my @date = (2016, 7, 7);
  if ($opts{d}) {
    if ($opts{d} !~ /^\d\d\/\d\d\/20\d\d$/) {
      print "Date via -d must be in MM/DD/YYYY format\n";
      exit 1;
    }

    @date = split '\/', $opts{d};
    @date[0,1,2] = @date[2,0,1];
    if ($date[1] > 31 || $date[2] > 12) {
      print "Date via -d must be in MM/DD/YYYY format\n";
      exit 1;
    }
  }

  my @today = (localtime)[5,4,3]; # year, month, day
  $today[0] += 1900;		  # Stupid epoch
  $today[1]++;			  # localtime is 0-indexed, Delta_Days isn't

  my $days = Delta_Days(@date, @today);
  # Original value if defined, 0 if not
  $rate ||= int($xp/$days);
  if ($rate !~ /^\d+$/) {
    print "Rate must be an integer\n";
    exit 1;
  }

  # Need to subroutine this FIXME TODO
  if ($opts{l}) {
    if ($dist !~ /^\d+$/) {
      print "Distance must be an integer\n";
      exit 1;
    } else {
      $dist = sprintf("%.2f", $dist/$days);
    }
  }

  if ($opts{c}) {
    if ($catch !~ /^\d+$/) {
      print "Number of catches must be an integer\n";
      exit 1;
    } else {
      $catch = sprintf("%.2f", $catch/$days);
    }
  }

  if ($opts{s}) {
    if ($spin !~ /^\d+$/) {
      print "Number of spins must be an integer\n";
      exit 1;
    } else {
      $spin = sprintf("%.2f", $spin/$days);
    }
  }

  if ($opts{b}) {
    if ($berry !~ /^\d+$/) {
      print "Number of berries must be an integer\n";
      exit 1;
    } else {
      my @bDate = (2017, 6, 22);
      $bDays = Delta_Days(@bDate, @today);
      $berry = sprintf("%.2f", $berry/$bDays);
    }
  }

  # Parse _END_ data
  while (<DATA>) {
    chomp;
    my @tmp = split;
    $lvls{$tmp[0]} = $tmp[1];
  }

  # Figure out current level
  my $lvl = 0;
  foreach my $key (sort {$a<=>$b} keys %lvls) {
    if ($xp >= $lvls{$key}) {
      $lvl = $key;
    } else {
      last;
    }
  }

  print "Played:\t$days days\n";
  print "Player level:\t$lvl\n";
  print "Experience/day:\t$rate\n";
  print "Distance/day:\t$dist\n" if $opts{l};
  print "Catch/day:\t$catch\n" if $opts{c};
  print "Spins/day:\t$spin\n" if $opts{s};
  print "Berries/day:\t$berry\n" if $opts{b};
  print "\nNext levels:\n";

  foreach my $key (sort {$a<=>$b} keys %lvls) {
    next if $lvl >= $key;

    my $left = $lvls{$key} - $xp;
    my $timeline = sprintf("%.1f", $left/$rate);
    my @fut = Add_Delta_Days(@today,$timeline);
    $fut[2] = sprintf("%.f",$fut[2]); # Round the day
    $fut[0] = substr $fut[0], -2;

    $left = sprintf("%.2f",$left/1000);
    if ($left >= 1000) {
      $left = sprintf("%.2f",$left/1000);
      $left .= 'M';
    } else {
      $left .= 'k';
    }

    print "$key\t$left\t$timeline days\t$fut[1]/$fut[2]/$fut[0]\n";
  }
}



#### Usage statement ####
# Final line must be unindented?
sub usage
{
    print <<USAGE;
Usage: $0 -x <current XP> [-d <MM/DD/YYYY>] [-rlcsb]
      -x Current XP amount.  Required.
      -d Specify a start date of the form MM/DD/YYYY. Defaults to 7/7/2016
      -r Specify rate of XP gain per day
      -l Current distance walked
      -c Current number of catches
      -s Current number of stops spun
      -b Current number of berries fed
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
