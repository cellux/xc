((struct s
   (((const unsigned char) c)
    (int (i 4) (j 4) k))))

((struct q ((int i)
            ((const int) ci))))
((struct q) q)
((const (struct q)) cq)
((volatile (struct q)) vq)

((union (((struct ((int alltypes))) n)
         ((struct ((int type) (int intnode))) ni)
         ((struct ((int type) (double doublenode))) nf)))
 u)

(int ($function main ((int argc) (char ((* *) argv))))
     ($block
      (= ($at ($at u nf) type) 1)
      (= ($at ($at u nf) doublenode) 3.14)))

(void ($function f (void))
      ($block
       ((struct s ((int n) (double ($array d)))))
       (int (m 0))
       ((struct s) ((* p)
                    ($call malloc (+ (sizeof (struct s))
                                     (sizeof (double ($array m)))))))
       ((struct ((int n) (double ($array d m)))) (* p))
       ((struct s) (* s1) (* s2))
       (= s1 ($call malloc (+ (sizeof (struct s)) 10)))
       (= s2 ($call malloc (+ (sizeof (struct s)) 6)))
       (double (* dp))
       (= dp (& (@ (-> s1 d) 0)))
       (= (* dp) 42)))

(void ($function g ())
      ($block
       ((typedef (struct tnode)) TNODE)
       ((struct tnode ((int count)
                       (TNODE (* left) (* right)))))
       (TNODE s (* sp))))
