#!/usr/bin/env perl
# appcastCheck.pl by Amory Meltzer
# Methodically check appcasts

use strict;
use warnings;
use diagnostics;

if (@ARGV != 2) {
  print "Usage: $0 list_of_casks list_to_skip\n";
  exit;
}

# Skip casks that I've checked.  Should be cleaned out now n then
my %hash;
open my $skips, '<', "$ARGV[1]" or die $!;
while (<$skips>) {
  chomp;
  s/\.rb$//g;
  $hash{$_} = $_;
}
close $skips or die $!;

# Build list of casks
my @array;
open my $casks, '<', "$ARGV[0]" or die $!;
while (<$casks>) {
  chomp;
  s/\.rb$//g;
  next if $hash{$_};		# Skip if skip
  @array = (@array,$_);
}
close $casks or die $!;

open my $out, '>>', 'updateme.list' or die $!;
open my $skip, '>>', 'skipme.list' or die $!;
while (@array) {
  my $cask = shift @array;
  system "brew cask chome $cask";

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
    print $skip "$cask\n";
    next;
    next;
  }
}
close $out or die $!;
close $skip or die $!;
