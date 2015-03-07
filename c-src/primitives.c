#include <stdio.h>
#include <malloc.h>
#include <math.h>

#include "constants.h"

unsigned long isBoolean (unsigned long val)
{
  if (val == FALSE_VALUE || val == TRUE_VALUE)
    return TRUE_VALUE;
  else
    return FALSE_VALUE;
}

unsigned long isNull (unsigned long val)
{
  if (val == NIL_VALUE)
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

// ------------------------------------------------------------------
// LISTS
// ------------------------------------------------------------------

unsigned long isPair (unsigned long val)
{
  return isTag (val, PAIR_TAG, PAIR_TAG_LEN);
}

unsigned long cons (unsigned long elem1, unsigned long elem2)
{
  unsigned long *ptr =
    memalign(1, 2 * sizeof(unsigned long));

  *(ptr) = elem1;
  *(ptr+1) = elem2;

  return ((unsigned long) ptr) + PAIR_TAG;
}

unsigned long car (unsigned long val)
{
  unsigned long *ptr = val - PAIR_TAG;
  return *ptr;
}

unsigned long cdr (unsigned long val)
{
  unsigned long *ptr = val - PAIR_TAG;
  return *(ptr+1);
}

// ------------------------------------------------------------------
// VECTORS
// ------------------------------------------------------------------

unsigned long isVector (unsigned long val)
{
  return isTag (val, VECTOR_TAG, VECTOR_TAG_LEN);
}

unsigned long vectorLength (unsigned long val)
{
  unsigned long *ptr = val - VECTOR_TAG;
  return *ptr;
}

unsigned long vectorRef (unsigned long val, unsigned long index)
{
  //TODO think, the users of this function are both from scheme and from C
  //to divide by 4 or not?
  index = index >> FIXNUM_TAG_LEN;
  unsigned long *ptr = val - VECTOR_TAG;
  return *(ptr + index + 1);
}

unsigned long vectorSet (unsigned long vec, unsigned long index, unsigned long val)
{
  index = index >> FIXNUM_TAG_LEN;
  unsigned long *ptr = vec - VECTOR_TAG;
  *(ptr + index + 1) = val;

  return NIL_VALUE;
}

unsigned long makeVector (unsigned long size, unsigned long elem)
{
  size = size >> FIXNUM_TAG_LEN;
  unsigned long *ptr =
    memalign(1, (size + 1) * sizeof(unsigned long));

  *(ptr) = size;

  for (int i=1; i<=size; i++)
    *(ptr + i) = elem;

  return ((unsigned long) ptr) + VECTOR_TAG;
}
