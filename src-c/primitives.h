#ifndef LC_PRIMITIVES
#define LC_PRIMITIVES

unsigned long isBoolean (unsigned long val);
unsigned long isTag (unsigned long val, unsigned tag, unsigned tagLen);
unsigned long isChar (unsigned long val);
unsigned long isNumber (unsigned long val);
unsigned long isNull (unsigned long val);

unsigned long cons (unsigned long elem1, unsigned long elem2);
unsigned long isPair (unsigned long val);
unsigned long car (unsigned long val);
unsigned long cdr (unsigned long val);
unsigned long carSet (unsigned long pair, unsigned long val);
unsigned long cdrSet (unsigned long pair, unsigned long val);

unsigned long arrayLength (unsigned long val, int tag);
unsigned char* getArrayPtr (unsigned long val, unsigned index, int tag);

unsigned long isVector (unsigned long val);
unsigned long vectorLength (unsigned long val);
unsigned long vectorRef (unsigned long val, unsigned index);
unsigned long vectorSet (unsigned long vec, unsigned index, unsigned long val);

unsigned long isString (unsigned long val);

#endif // LC_PRIMITIVES
