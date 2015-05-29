int main(int argc, char **argv) {
  int n[10];
  int n_elem_size = (sizeof(n)/sizeof(n[0]));
  int *p = (&n[0]);
  int i = (!(~(-(+(*p)))));
  int j = (++i);
  int k = (--j);
  int int_size = sizeof(int);
  int n0_size = sizeof(n[0]);
  extern void *alloc(size_t);
  double *dp = alloc(sizeof((*dp)));
}
size_t fsize3(int n) {
  char b[(n+3)];
  return sizeof(b);
}
