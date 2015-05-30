struct s {
  const unsigned char c;
  int i:4, j:4, k;
};
struct q {
  int i;
  const int ci;
};
struct q q;
const struct q cq;
volatile struct q vq;
union {
  struct {
    int alltypes;
  } n;
  struct {
    int type;
    int intnode;
  } ni;
  struct {
    int type;
    double doublenode;
  } nf;
} u;
int main(int argc, char **argv) {
  (((u.nf).type) = 1);
  (((u.nf).doublenode) = 3.14);
}
void f(void) {
  struct s {
    int n;
    double d[];
  };
  int m = 0;
  struct s *p = malloc((sizeof(struct s)+sizeof(double [m])));
  struct {
    int n;
    double d[m];
  } *p;
  struct s *s1, *s2;
  (s1 = malloc((sizeof(struct s)+10)));
  (s2 = malloc((sizeof(struct s)+6)));
  double *dp;
  (dp = (&((s1->d)[0])));
  ((*dp) = 42);
}
void g() {
  typedef struct tnode TNODE;
  struct tnode {
    int count;
    TNODE *left, *right;
  };
  TNODE s, *sp;
}
