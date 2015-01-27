#!/usr/bin/env perl
# license_helper.pl by Amory Meltzer
# Script to help automate license updating of hombrew-cask casks
## Use dedicated list to save time?
## Ask first?

use strict;
use warnings;
use diagnostics;

my $infile  = '/Users/Amory/oss_gh.txt';
my $outfile = '/Users/Amory/oss_gh.txt';
my %oss;

# Build hash for later parsing, etc.
open my $ossGH, '<', "$infile" or die $!;
while (<$ossGH>) {
  chomp;

  my @tmp = split /\t/;
  $tmp[1] ||= q{};		# Original value if defined, empty if not

  $oss{$tmp[0]} = $tmp[1];
}
close $ossGH;

foreach my $key (sort keys %oss) {
  print "$key\n";
  system "brew cask cat $key";
  system "brew cask home $key";

  print "What is the license for this cask?\n";
  print "oss, gpl, mit, mpl, [S]kip or [Q]uit\n";

  my $lic = <>;
  chomp $lic;

  if ($lic =~ /^q$/i || $lic =~ /[Qq]uit$/i) {
    open my $ossGHout, '>', "$outfile" or die $!;

    foreach my $hurry (sort keys %oss) {
      print $ossGHout "$hurry\t$oss{$hurry}\n";
    }
    close $ossGHout;
    exit;
  }

  elsif ($lic =~ /^s$/i || $lic =~ /[Ss]kip$/i) {
    next;
  }

  else {
    $oss{$key} = $lic;
  }
}
