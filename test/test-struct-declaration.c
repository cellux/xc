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
  u.nf.type = 1;
  u.nf.doublenode = 3.14;
}
