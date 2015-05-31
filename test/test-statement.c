void f() {
  char *s;
  while (((*(s++))!='\0')) ;
  while (loop1) {
    while (loop2) {
      if (want_out) goto end_loop1;
    }
    end_loop1:
    ;
  }
  switch (expr) {
    int i = 4;
    f(i);
    case 0:
      (i = 17);
    default:
      printf("%d\n", i);
  }
  for (; ; ) ;
}
