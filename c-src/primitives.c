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
  //printf("mask is: %d", mask);
  //printf("\n");
  //printf("tag is: %d", tag);
  //printf("\n");
  //printf("val is: %d", val);
  //printf("\n");

  if ((val & mask) == tag)
    return TRUE_VALUE;
  else
    return FALSE_VALUE;
}

unsigned isChar (unsigned val)
{
  return isTag (val, CHAR_TAG, CHAR_TAG_LEN);
}

unsigned isNumber (unsigned val)
{
  return isTag (val, FIXNUM_TAG, FIXNUM_TAG_LEN);
}
