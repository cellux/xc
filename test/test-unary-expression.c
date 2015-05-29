int main(int argc, char **argv) {
  int n[10];
  int *p = (&n[0]);
  int i = (!(~(-(+(*p)))));
  int j = (++i);
  int k = (--j);
  int int_size = sizeof(int);
  int n0_size = sizeof(n[0]);
}
