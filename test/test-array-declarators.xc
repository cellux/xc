(float ($array fa 11) ($array ((* afp)) 17))
((extern int) (* x))
((extern int) ($array y))
((extern int) n m)
(void ($function fcompat(void))
      ($block
       (int ($array ($array ($array a n) 6) m))
       (int ($array ($array ((* p)) 4) (+ n 1)))
       (int ($array ($array ($array ($array c n) n) 6) m))
       (int ($array ($array ($array ((* r)) n) n) (+ n 1)))
       (= p a)
       (= r c)))

((extern int) n)
(int ($array A n))
((extern int) ($array ((* p2)) n))
(int ($array B 100))

(void ($function fvla ((int m) (int ($array ($array C m) m)))))

(void ($function fvla ((int m) (int ($array ($array C m) m))))
      ($block
       ((typedef int) ($array ($array VLA m) m))
       ((struct tag ((int ($array ((* y)) n))
                     (int ($array z n)))))
       (int ($array D m))
       ((static int) ($array E m))
       ((extern int) ($array F m))
       (int ($array ((* s)) m))
       ((extern int) ($array ((* r)) m))
       ((static int) (($array ((* q)) m) (& B)))))
