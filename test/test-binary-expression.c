int main(int argc, char **argv) {
  int a = 5;
  int b = ((a*8)/(a%2));
  int c = (a+(b*(3-a)));
  int d = ((c<<2)+(c>>3));
  if (((a>=(a&15)) && (b<=(b|16)))) {
    int e = (a^(b-c));
    if ((((a==e) || (b!=c)) || ((c<a) && (d>c)))) printf("hey!\n");
  }
  int n = 4, m = 3;
  int a[n][m];
  int (*p)[m] = a;
  (p += 1);
  ((*p)[2] = 99);
  (n = (p-a));
}
