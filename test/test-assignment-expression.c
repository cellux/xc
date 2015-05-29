int main(int argc, char **argv) {
  int a = 5;
  (a = 2);
  (a *= 20);
  (a /= 10);
  (a %= 2);
  (a += 6);
  (a -= -5);
  (a <<= 4);
  (a >>= 4);
  (a &= 15);
  (a |= 2);
  (a ^= 7);
}
int f(void) {
  char c;
  if (((c = f())==-1)) return 0;
  int i;
  long l;
  (l = (c = i));
  const char **cpp;
  char *p;
  const char c = 'a';
  (cpp = (&p));
  ((*cpp) = (&c));
  ((*p) = 0);
}
