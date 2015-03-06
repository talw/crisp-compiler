#include <stdio.h>
#include <math.h>

#include "constants.h"
#include "primitives.h"

unsigned entryFunc();

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
    unsigned long *ptr = val - 1;
    printf("(");
    showImmediate(*ptr);
    printf(" ");
    showImmediate(*(ptr+1));
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
