#include<stdio.h>

unsigned isBoolean (unsigned val)
{
  if (val == 0x2F || val == 0x6F)
    return 0x6F;
  else
    return 0x2F;
}

/*int main(int argc, char *argv[])*/
/*{*/
  /*printf("%d %d\n", isBoolean(5), isBoolean(0x6F));*/
/*}*/
