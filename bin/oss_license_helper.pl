#!/usr/bin/env perl
# license_helper.pl by Amory Meltzer
# Script to help automate license updating of hombrew-cask casks
## Use dedicated list to save time?
## Ask first?

use strict;
use warnings;
use diagnostics;

my $infile = '/Users/Amory/oss_gh.txt';

open my $ossGH, '<', "$infile" or die $!;
while (<$ossGH>) {
  chomp;
  print "$_\n";
}
close $ossGH;
