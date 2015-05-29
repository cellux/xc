int main(int argc, char **argv) {
  int n[10];
  struct {
    int a;
    char *s;
    float f;
    double d;
  } rec;
  struct {
    int a;
    char *s;
    float f;
    double d;
  } *prec;
  n[0] = 8;
  rec.s = "hello\n";
  prec->s = "world\n";
  (rec.a++);
  (rec.a--);
  printf("hello\n");
  printf("n[0]=%d\n", n[0]);
  abort();
  f().x;
  (*pf[f1()])(f2(), (f3()+f4()));
  assert(((&rec)->s==rec.s));
}
