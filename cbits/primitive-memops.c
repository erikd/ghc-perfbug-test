#include <string.h>
#include "primitive-memops.h"

#define MEMSET(TYPE, ATYPE)                                                  \
void hsprimitive_memset_ ## TYPE (Hs ## TYPE *p, ptrdiff_t off, size_t n, ATYPE x) \
{                                                                            \
  p += off;                                                                  \
  if (x == 0)                                                                \
    memset(p, 0, n * sizeof(Hs ## TYPE));                                    \
  else if (sizeof(Hs ## TYPE) == sizeof(int)*2) {                            \
    int *q = (int *)p;                                                       \
    const int *r = (const int *)(void *)&x;                                  \
    while (n>0) {                                                            \
      q[0] = r[0];                                                           \
      q[1] = r[1];                                                           \
      q += 2;                                                                \
      --n;                                                                   \
    }                                                                        \
  }                                                                          \
  else {                                                                     \
    while (n>0) {                                                            \
      *p++ = x;                                                              \
      --n;                                                                   \
    }                                                                        \
  }                                                                          \
}

void hsprimitive_memset_Word8 (HsWord8 *p, ptrdiff_t off, size_t n, HsWord x)
{
  memset( (char *)(p+off), x, n );
}

/* MEMSET(HsWord8, HsWord) */
MEMSET(Word, HsWord)
