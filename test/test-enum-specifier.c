void f() {
  enum hue { chartreuse, burgundy, claret = 20, winedark };
  enum hue col, *cp;
  (col = claret);
  (cp = (&col));
  if ((cp!=burgundy)) return;
}
