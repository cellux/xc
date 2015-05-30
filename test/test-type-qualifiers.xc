((extern const volatile int) real_time_clock)

(void ($function f ())
      ($block
       ((const (struct s ((int mem)))) (cs (1)))
       ((struct s) ncs)
       ((typedef int) ($array ($array A 2) 3))
       ((const A) (a ((4 5 6) (7 8 9))))
       (int (* pi))
       ((const int) (* pci))

       (= ncs cs)
       (= cs ncs)
       (= pi (& ($at ncs mem)))
       (= pi (& ($at cs mem)))
       (= pci (& ($at cs mem)))
       (= pi (@ a 0))))

(void ($function g ((int n)
                    (int ((* restrict) p))
                    (int ((* restrict) q))))
      ($block
       (while (> (n --) 0)
         ($block
          (= (* (p ++)) (* (q ++)))))))
