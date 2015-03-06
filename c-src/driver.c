#include <stdio.h>
#include <math.h>

#include "constants.h"
#include "primitives.h"

unsigned entryFunc();
void showImmediate(unsigned long val);

void showCons(unsigned long val)
{
  unsigned long *ptr = val - 1;
  unsigned long car = *ptr;
  unsigned long cdr = *(ptr + 1);

  showImmediate(car);

  if (isPair(cdr) == TRUE_VALUE)
  {
    printf(" ");
    showCons(cdr);
  }
  else if (cdr == NIL_VALUE)
  {
    return;
  }
  else
  {
    printf(" . ");
    showImmediate(cdr);
  }
}

void showImmediate(unsigned long val)
{
  //printf("got value: %d\n", val);
  //printf("got value: %#08x\n", val);

  if (val == TRUE_VALUE)
    printf("#t");
  else if (val == FALSE_VALUE)
    printf("#f");
  else if (val == NIL_VALUE)
    printf("()");
  else if (isChar(val) == TRUE_VALUE)
    printf("#\\%c", val >> CHAR_TAG_LEN);
  else if (isNumber(val) == TRUE_VALUE)
    printf("%lu", val >> FIXNUM_TAG_LEN);
  else if (isPair(val) == TRUE_VALUE)
  {
    printf("(");
    showCons(val);
    printf(")");
  }
  else
    printf("Unrecognized value");

}

int main(int argc, char *argv[])
{
  unsigned long retVal = entryFunc();
  showImmediate(retVal);
  printf("\n");
}
