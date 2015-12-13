#!/usr/bin/env perl
# appcastCheck.pl by Amory Meltzer
# Methodically check appcasts

use strict;
use warnings;
use diagnostics;

if (@ARGV != 1) {
  print "Usage: $0 list_of_casks\n";
  exit;
}

# List of caks
my @array;

# Build array
open my $casks, '<', "$ARGV[0]" or die $!;
while (<$casks>) {
  chomp;
  s/\.rb$//g;
  @array = (@array,$_);
}
close $casks or die $!;

open my $out, '>', 'updateme.list' or die $!;
while (@array) {
  my $cask = shift @array;
  system "brew cask cat $cask && brew cask home $cask";

  print "\n[K]eep, [S]kip, or [Q]uit?";
  my $action = <STDIN>;

  if ($action =~ m/k/i) {
    print $out "$cask\n";
    next;
  } elsif ($action =~ m/q/i) {
    open my $check, '>', 'newcheck.list' or die $!;
    print $check "$cask\n";
    foreach my $tmp (@array) {
      print $check "$tmp\n";
    }
    close $check or die $!;
    exit;
  } elsif ($action =~ m/s/i) {
    next;
  }
}
close $out or die $!;
