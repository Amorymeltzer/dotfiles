#!/usr/bin/env perl
# license_helper.pl by Amory Meltzer
# Script to help automate license updating of oss hombrew-cask casks

use strict;
use warnings;
use diagnostics;

use Term::ANSIColor;


my %licenses;			# Hash to old license from DATA
while (<DATA>) {
  chomp;
  my @tmp = split;		# Stupid hack to deal with spaces in DATA
  $licenses{$tmp[0]} = 1;
}

my $infile  = '/Users/Amory/oss_gh.txt';
my $outfile = '/Users/Amory/oss_gh.txt';
my %oss;

# Build hash for later parsing, etc.
open my $ossGH, '<', "$infile" or die $!;
while (<$ossGH>) {
  chomp;

  my @tmp = split /\t/;
  $tmp[1] ||= q{};		# Original value if defined, empty if not
  $tmp[2] ||= q{};		# Original value if defined, empty if not

  $oss{$tmp[0]} = [$tmp[1],$tmp[2]];
}
close $ossGH;

my $quit = 0;			# Are we trying to quit and save nicely?

open my $ossGHout, '>', "$outfile" or die $!;
foreach my $key (sort keys %oss) {
  # Get me outta here!
  if ($quit == 1) {
    print $ossGHout "$key\t$oss{$key}[0]\t$oss{$key}[1]\n";
    next;
  }

  # Skip if there's already a license value here
  if ($oss{$key}[0]) {
    print $ossGHout "$key\t$oss{$key}[0]\t$oss{$key}[1]\n";
    next;
  }

  system 'clear';
  print "$key...\n";
  system "brew cask home $key";
  system "brew cask cat $key";

  print color 'bright_cyan';
  print "What is the license for this cask?\n";
  print "oss, gpl, mit, mpl, [S]kip or [Q]uit\n";
  print color 'reset';

  my $lic = <>;
  chomp $lic;

  if ($lic =~ /^q$/i || $lic =~ /[Qq]uit$/i) {
    $quit = 1;
    print "One moment please\n";
    print $ossGHout "$key\t$oss{$key}[0]\t$oss{$key}[1]\n";
    next;
  } elsif ($lic =~ /^s$/i || $lic =~ /[Ss]kip$/i) {
    next;
  } elsif (!$licenses{$lic}) {
    print color 'bright_red';
    print "$lic is not a valid license, skipping\n";
    print color 'reset';

    print $ossGHout "$key\t$oss{$key}[0]\t$oss{$key}[1]\n";

    sleep 1;			# Allow for the message to be seen
    next;
  } else {
    $oss{$key}[0] = $lic;

    print color 'bright_cyan';
    print "Notes?  Link to license, etc.\n";
    print color 'reset';

    my $note = <>;
    chomp $note;
    $oss{$key}[1] = $note;

    print $ossGHout "$key\t$oss{$key}[0]\t$oss{$key}[1]\n";
    next;
  }
}
close $ossGHout;






## The lines below do not represent Perl code, and are not examined by the
## compiler.  Rather, they are the accepted :license stanza values;
__END__
oss
  closed
  gratis
  commercial
  freemium
  affero
  apache
  arphic
  artistic
  bsd
  cc
  eclipse
  gpl
  isc
  lppl
  ncsa
  mit
  mpl
  ofl
  public_domain
  ubuntu_font
  x11
