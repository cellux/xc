extern const volatile int real_time_clock;
void f() {
  const struct s {
    int mem;
  } cs = { 1 };
  struct s ncs;
  typedef int A[2][3];
  const A a = { { 4, 5, 6 }, { 7, 8, 9 } };
  int *pi;
  const int *pci;
  (ncs = cs);
  (cs = ncs);
  (pi = (&(ncs.mem)));
  (pi = (&(cs.mem)));
  (pci = (&(cs.mem)));
  (pi = (a[0]));
}
void g(int n, int * restrict p, int * restrict q) {
  while (((n--)>0)) {
    ((*(p++)) = (*(q++)));
  }
}
