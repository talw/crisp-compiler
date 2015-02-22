#include <stdio.h>
#include <math.h>

#include "constants.h"

unsigned isBoolean (unsigned val)
{
  if (val == FALSE_VALUE || val == TRUE_VALUE)
    return TRUE_VALUE;
  else
    return FALSE_VALUE;
}

unsigned isTag (unsigned val, unsigned tag, unsigned tagLen)
{
  unsigned mask = pow(2.0, tagLen) - 1;

  if ((val & mask) == tag)
    return TRUE_VALUE;
  else
    return FALSE_VALUE;
}

unsigned isChar (unsigned val)
{
  return isTag (val, CHAR_TAG, 8);
}

unsigned isNumber (unsigned val)
{
  return isTag (val, FIXNUM_TAG, 2);
}

/*int main(int argc, char *argv[])*/
/*{*/
  /*printf("%d\n", isChar(24847));*/
/*}*/
