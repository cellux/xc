int f(void), *fip(), (*pfi)();
int (*apfi[3])(int *x, int *y);
void addscalar(int n, int m, double a[n][((n*m)+300)], double x);
int main(int argc, char **argv) {
  int x = 5;
  int y = 6;
  (*(apfi[0]))((&x), (&y));
  double b[4][308];
  addscalar(4, 2, b, 2.17);
  return 0;
}
void addscalar(int n, int m, double a[n][((n*m)+300)], double x) {
  for (int i = 0; (i<n); (i++)) {
    for (int j = 0, k = ((n*m)+300); (j<k); (j++)) {
      (((a[i])[j]) += x);
    }
  }
}
int (*fpfi(int (*)(long), int))(int, ...);
double maximum(int n, int m, double a[n][m]);
double maximum(int n, int m, double a[*][*]);
double maximum(int n, int m, double a[][*]);
double maximum(int n, int m, double a[][m]);
void f(double (* restrict a)[5]);
void f(double a[restrict][5]);
void f(double a[restrict 3][5]);
void f(double a[restrict static 3][5]);
