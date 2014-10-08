#include <stdio.h>
#include <math.h>

int main ()
{
  long double n = 0, f = 1;
  int i;
  for (i = 28; i >= 1; i--) {
    f *= i;  // f = 28*27*...*i = 28! / (i-1)!
    n += f;  // n = 28 + 28*27 + ... + 28! / (i-1)!
  }  // n = 28! * (1/0! + 1/1! + ... + 1/28!), f = 28!
  n /= f;
  printf("%.64llf\n", n);
  printf("%.64llf\n", expl(1));
  printf("%llg\n", n - expl(1));
  printf("%d\n", n == expl(1));
}
