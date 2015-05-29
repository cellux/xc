int *p = ((int []) { 2, 4 });
void f(void) {
  int *p;
  (p = ((int [2]) { (*p) }));
  drawline(((struct point) { .x=1, .y=1 }), ((struct point) { .x=3, .y=4 }));
  drawline_p((&((struct point) { .x=1, .y=1 })), (&((struct point) { .x=3, .y=4 })));
  g(((const float []) { 1.0, 10.0, 100.0, 1000.0, 10000.0, 100000.0, 1000000.0 }));
  char *tmp = mktemp("/tmp/fileXXXXXX");
  (tmp = mktemp(((char []) { "/tmp/fileXXXXXX" })));
  (tmp = mktemp(((const char []) { "/tmp/fileXXXXXX" })));
  struct int_list {
    int car;
    struct int_list *cdr;
  };
  struct int_list endless_zeros = { 0, (&endless_zeros) };
}
struct s {
  int i;
};
int f(void) {
  struct s *p = 0, *q;
  int j = 0;
  again:
  ((q = p), (p = (&((struct s) { (j++) }))));
  if ((j<2)) goto again;
  return ((p==q) && (q->i==1));
}
