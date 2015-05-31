int i = 3.5;
double complex c = (5+(3*I));
int x[] = { 1, 3, 5 };
int y[4][3] = { { 1, 3, 5 }, { 2, 4, 6 }, { 3, 5, 7 } };
int y[4][3] = { 1, 3, 5, 2, 4, 6, 3, 5, 7 };
int z[4][3] = { { 1 }, { 2 }, { 3 }, { 4 } };
struct {
  int a[3], b;
} w[] = { { 1 }, 2 };
typedef int A[];
A a = { 1, 2 }, b = { 3, 4, 5 };
int a[] = { 1, 2 }, b[] = { 3, 4, 5 };
char s[] = "abc", t[3] = "abc";
char s[] = { 'a', 'b', 'c', '\0' }, t[] = { 'a', 'b', 'c' };
enum { member_one, member_two };
const char *nm[] = { [member_two] = "member_two", [member_one] = "member_one" };
div_t answer = { .quot = 2, .rem = -1 };
struct {
  int a[3], b;
} w[] = { [0].a = { 1 }, [1].a[0] = 2 };
int a[MAX] = { 1, 3, 5, 7, 9, [(MAX-5)] = 8, 6, 4, 2, 0 };
