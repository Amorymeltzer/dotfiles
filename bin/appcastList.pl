#!/usr/bin/env perl
# appcastList.pl by Amory Meltzer
# Bulk process homebrew-cask appcasts to check for outdated casks

use strict;
use warnings;
use diagnostics;

if (@ARGV != 1) {
  print "Usage: $0 list_of_casks\n\n";
  print "cd /usr/local/Library/Taps/caskroom/homebrew-cask/Casks/\n";
  print "grep -i \"appcast\" *.rb|cut -f 1 -d ':' > ~/list.list\n";
  print "appcastlist.pl ~/list.list 2>/dev/null > ~/checkme.list\n";
  exit;
}

# cask = [appcast url, original sha]
my %hash;

# Build hash
open my $casks, '<', "$ARGV[0]" or die $!;
while (<$casks>) {
  chomp;
  getCasks($_);
}
close $casks or die $!;

foreach my $key (sort keys %hash) {
  my $new = `curl $hash{$key}[0] | shasum -a 256`;
  $new =~ s/\s+-\s$//;

  if ($new ne $hash{$key}[1]) {
    print "$key,$new\n";
  }
}


sub getCasks
  {
    my $cask = shift;
    my $appTick = 0;
    open my $file, '<', "$cask" or die $!;
    while (<$file>) {
      chomp;

      last if m/version :latest/;

      if (m/appcast/ && $appTick == 0) {
	s/.*\'(.*)\',/$1/;
	$hash{$cask}[0] = $_;
	$appTick = 1;
	next;
      } elsif ($appTick == 1 && m/sha256/) {
	s/.*\'(\S+)\'/$1/;
	$hash{$cask}[1] = $_;
	$appTick = 0;
	last;
      } elsif ($appTick == 1 && !m/sha256/) {
	delete $hash{$cask};
	$appTick = 0;
	last;
      }
    }
    close $file or die $!;
  }
