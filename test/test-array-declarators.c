float fa[11], (*afp)[17];
extern int *x;
extern int y[];
extern int n, m;
void fcompat(void) {
  int a[n][6][m];
  int (*p)[4][(n+1)];
  int c[n][n][6][m];
  int (*r)[n][n][(n+1)];
  (p = a);
  (r = c);
}
extern int n;
int A[n];
extern int (*p2)[n];
int B[100];
void fvla(int m, int C[m][m]);
void fvla(int m, int C[m][m]) {
  typedef int VLA[m][m];
  struct tag {
    int (*y)[n];
    int z[n];
  };
  int D[m];
  static int E[m];
  extern int F[m];
  int (*s)[m];
  extern int (*r)[m];
  static int (*q)[m] = (&B);
}
