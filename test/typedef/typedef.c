#include <bits/wordsize.h>
typedef signed char int8_t;
typedef short	int16_t;
typedef int	int32_t;
#if __WORDSIZE == 64
typedef long int64_t;
#else
typedef long long int64_t;
#endif
typedef unsigned char	uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
#if __WORDSIZE == 64
  typedef unsigned long uint64_t;
#else
  typedef unsigned long long uint64_t;
#endif
