typedef int MILES, KLICKSP();
typedef struct {
  double hi, lo;
} range;
typedef void fv(int), (*pfv)(int);
void (*signal(int, void (*)(int)))(int);
fv *signal(int, fv *);
pfv signal(int, pfv);
