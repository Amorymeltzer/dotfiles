#!/usr/bin/env perl
# appcastCheck.pl by Amory Meltzer
# Methodically check appcasts

use strict;
use warnings;
use diagnostics;

if (@ARGV != 2) {
  print "Usage: $0 checkme.list skipme.list\n";
  print "Run in ~ directory (with all relevant files)\n";
  exit;
}

# Skip casks that I've checked.  Should be cleaned out now n then
my %skipList;
open my $skips, '<', "$ARGV[1]" or die $!;
while (<$skips>) {
  chomp;
  my @tmp = split /,/;
  $tmp[0] =~ s/\.rb$//g;
  $skipList{$tmp[0]} = $tmp[1];
}
close $skips or die $!;

# Build list of casks
my %caskList;
open my $casks, '<', "$ARGV[0]" or die $!;
while (<$casks>) {
  chomp;
  my @tmp = split /,/;
  $tmp[0] =~ s/\.rb$//g;
  # Skip if in skip list and hash hasn't changed
  if ($skipList{$tmp[0]}) {
    next if $tmp[1] eq $skipList{$tmp[0]};
  }
  $caskList{$tmp[0]} = $tmp[1];
}
close $casks or die $!;

open my $out, '>>', 'updateme.list' or die $!;
open my $skip, '>>', 'skipme.list' or die $!;
foreach my $cask (sort keys %caskList) {
  delete $caskList{$cask};
  system "clear";
  system "brew cask chome $cask";

  print "\n[K]eep, [S]kip, or [Q]uit?";
  my $action = <STDIN>;

  if ($action =~ m/k/i) {
    print $out "$cask\n";
    next;
  } elsif ($action =~ m/q/i) {
    open my $check, '>', 'newcheck.list' or die $!;
    print $check "$cask\n";
    foreach my $key (sort keys %caskList) {
      print $check "$key\n";
    }
    close $check or die $!;
    last;
  } elsif ($action =~ m/s/i) {
    print $skip "$cask\n";
    next;
    next;
  }
}
close $out or die $!;
close $skip or die $!;

# Replace old list with new one
system "mv -f newcheck.list checkme.list"
