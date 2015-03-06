#ifndef LC_PRIMITIVES
#define LC_PRIMITIVES

unsigned long isBoolean (unsigned long val);
unsigned long isTag (unsigned long val, unsigned tag, unsigned tagLen);
unsigned long isChar (unsigned long val);
unsigned long isNumber (unsigned long val);
unsigned long isNull (unsigned long val);
unsigned long isVector (unsigned long val);

unsigned long isPair (unsigned long val);
unsigned long car (unsigned long val);
unsigned long cdr (unsigned long val);

unsigned long isVector (unsigned long val);
unsigned long vectorLength (unsigned long val);
unsigned long vectorRef (unsigned long val, unsigned index);

#endif // LC_PRIMITIVES
