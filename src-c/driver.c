#include <stdio.h>
#include <math.h>
#include <wchar.h>

#include "constants.h"
#include "primitives.h"

unsigned entryFunc();
void showImmediate(unsigned long val);

void showCons(unsigned long val)
{
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

void showString(unsigned long val)
{
  unsigned long length = stringLength(val, STRING_TAG);

  printf("\"");

  for (unsigned i=0; i < length; i++)
  {
    unsigned char bla = stringRef(val, i << FIXNUM_TAG_LEN);

    printf("%c", bla);
  }

  printf("\"");
}

void showVector(unsigned long val)
{
  unsigned long length = vectorLength(val);

  printf("#(");

  for (unsigned i=0; i < length; i++)
  {
    showImmediate(vectorRef(val, i << FIXNUM_TAG_LEN));
    if (i < length - 1)
      printf(" ");
  }

  printf(")");
}

void showImmediate(unsigned long val)
{
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
    showVector(val);
  else if (isString(val) == TRUE_VALUE)
    showString(val);
  else
    printf("Unrecognized value");

}

int main(int argc, char *argv[])
{
  unsigned long retVal = entryFunc();
  showImmediate(retVal);
  printf("\n");
}
