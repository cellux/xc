inline double fahr(double t) {
  return (((9.0*t)/5.0)+32.0);
}
inline double cels(double t) {
  return ((5.0*(t-32.0))/9.0);
}
extern double fahr(double);
double convert(int is_fahr, double temp) {
  return (is_fahr ? cels(temp) : fahr(temp));
}
