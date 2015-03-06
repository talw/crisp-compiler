#ifndef LC_PRIMITIVES
#define LC_PRIMITIVES

unsigned isBoolean (unsigned val);
unsigned isTag (unsigned val, unsigned tag, unsigned tagLen);
unsigned isChar (unsigned val);
unsigned isNumber (unsigned val);
unsigned isPair (unsigned val);

#endif // LC_PRIMITIVES
