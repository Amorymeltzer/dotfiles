#!/usr/bin/env perl
# Matt Might: http://matt.might.net/articles/shell-scripts-for-passive-voice-weasel-words-duplicates/
# Finds duplicate adjacent words.
# Modified by Amory Meltzer

use strict;
use warnings;
use diagnostics;


my $DupCount = 0;

if (!@ARGV) {
  print "usage: dups <file> ...\n";
  exit;
}

while (1) {
  my $FileName = shift @ARGV;

  # Exit code = number of duplicates found.
  exit $DupCount if (!$FileName);

  open my $file, '<', "$FileName" or die $!;

  my $LastWord = q{};
  my $LineNum = 0;

  while (<$file>) {
    chomp;

    $LineNum ++;

    my @words = split /(\W+)/;

    foreach my $word (@words) {

      # Skip spaces:
      next if $word =~ /^\s*$/;

      # Skip punctuation:
      if ($word =~ /^\W+$/) {
	$LastWord = q{};
	next;
      }

      # Found a dup?
      if (lc $word eq lc $LastWord) {
	print "$FileName:$LineNum $word\n";
	$DupCount ++;
      }				# Thanks to Sean Cronin for tip on case.

      # Mark this as the last word:
      $LastWord = $word;
    }
  }

  close $file or die $!;
}
