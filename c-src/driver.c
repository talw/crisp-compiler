#include <stdio.h>
#include <math.h>

#include "constants.h"
#include "primitives.h"

unsigned entryFunc();
void showImmediate(unsigned long val);

void showCons(unsigned long val)
{
  /*unsigned long *ptr = val - 1;*/
  /*unsigned long head = *ptr;*/
  /*unsigned long tail = *(ptr + 1);*/

  showImmediate(car(val));

  if (isPair(cdr(val)) == TRUE_VALUE)
  {
    printf(" ");
    showCons(cdr(val));
  }
  else if (cdr(val) == NIL_VALUE)
  {
    return;
  }
  else
  {
    printf(" . ");
    showImmediate(cdr(val));
  }
}

void showVector(unsigned long val)
{
  unsigned long length = vectorLength(val);

  printf("#(");

  for (unsigned i=0; i < length; i++)
  {
    showImmediate(vectorRef(val, i));
    if (i < length - 1)
      printf(" ");
  }

  printf(")");
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
  else if (isVector(val) == TRUE_VALUE)
    showVector(val); //TODO is it safe?
  else
    printf("Unrecognized value");

}

int main(int argc, char *argv[])
{
  unsigned long retVal = entryFunc();
  showImmediate(retVal);
  printf("\n");
}
