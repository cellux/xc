#include <bits/wordsize.h>
typedef unsigned volatile char u8;
typedef unsigned const short u16;
typedef unsigned restrict int u32;
#if (__WORDSIZE==64)
typedef unsigned long u64;
#else
typedef unsigned long long u64;
#endif
extern inline int abs(int j);
static unsigned long long cache;
void abort(void);
typedef struct {
  int i;
  long l;
} missing_t;
struct missing {
  int i;
  long l;
};
struct {
  int i;
  long l;
} sinst;
typedef union {
  int i;
  long l;
} ux_t;
union ux {
  int i;
  long l;
};
union {
  int i;
  long l;
} uinst;
enum color { BLACK = 0, WHITE, RED, GREEN, BLUE, NONE = -1 };
enum { YES = 1, NO = 0 } choice;
enum color farbe1;
typedef enum color color;
color farbe2 = GREEN;
int main(int argc, char **argv) {
  auto float f;
  double d;
  signed char sc;
  unsigned char uc;
  register int y;
  missing_t m1;
  struct missing m2;
  ux_t u1;
  union ux u2;
}
