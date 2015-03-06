#include <stdio.h>
#include <math.h>

#include "constants.h"

unsigned long isBoolean (unsigned long val)
{
  if (val == FALSE_VALUE || val == TRUE_VALUE)
    return TRUE_VALUE;
  else
    return FALSE_VALUE;
}

static unsigned long isTag (unsigned long val, unsigned tag, unsigned tagLen)
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

unsigned long isChar (unsigned long val)
{
  return isTag (val, CHAR_TAG, CHAR_TAG_LEN);
}

unsigned long isNumber (unsigned long val)
{
  return isTag (val, FIXNUM_TAG, FIXNUM_TAG_LEN);
}

unsigned long isPair (unsigned long val)
{
  return isTag (val, PAIR_TAG, PAIR_TAG_LEN);
}
