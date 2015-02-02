#!/usr/bin/env perl
# auto_license.pl by Amory Meltzer
#

use strict;
use warnings;
use diagnostics;

my $infile = $ARGV[0];

open my $gh, '<', "$infile" or die $!;
while (<$gh>) {
  chomp;

  my @tmp = split /\t/;

  system "sed -i '' 's/license :oss/license :$tmp[1]/' $tmp[0].rb";

  system "git add $tmp[0].rb";
  system 'git diff --word-diff';

  <STDIN>;

  system "git commit -m 'Update $tmp[0] license from oss to $tmp[1]

$tmp[2]'";
}
close $gh;
