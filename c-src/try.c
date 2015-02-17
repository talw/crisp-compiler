#include<stdio.h>
#include<math.h>

const int TRUE_VALUE = 0x6F;
const int FALSE_VALUE = 0x2F;

unsigned isBoolean (unsigned val)
{
  if (val == 0x2F || val == 0x6F)
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
  return isTag (val, 0x0F, 8);
}

unsigned isNumber (unsigned val)
{
  return isTag (val, 0, 2);
}

unsigned not (unsigned val)
{
  if (val == FALSE_VALUE)
    return TRUE_VALUE;
  else
    return FALSE_VALUE;
}

/*int main(int argc, char *argv[])*/
/*{*/
  /*printf("%d\n", isChar(24847));*/
/*}*/
