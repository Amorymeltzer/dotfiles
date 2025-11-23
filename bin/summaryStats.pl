#!/usr/bin/env perl
# <http://stackoverflow.com/a/9790056>
# <https://news.ycombinator.com/item?id=45688709>
use List::Util qw(max min sum);
@a = ();
while (<>) {
  $sqsum += $_ * $_;
  push(@a, $_);
}
$n    = @a;
$s    = sum(@a);
$a    = $s / @a;
$m    = max(@a);
$mm   = min(@a);
$std  = sqrt($sqsum / $n - ($s / $n) * ($s / $n));
$mid  = int @a / 2;
@srtd = sort @a;

if (@a % 2) {
  $med = $srtd[$mid];
} else {
  $med = ($srtd[$mid - 1] + $srtd[$mid]) / 2;
}
print "records:$n\nsum:$s\navg:$a\nstd:$std\nmed:$med\max:$m\nmin:$mm";
