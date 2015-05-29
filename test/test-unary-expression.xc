(int ($function main ((int argc)
                      (char ((* *) argv))))
     ($block
      (int ($array n 10))
      (int (n_elem_size (/ (sizeof n) (sizeof (@ n 0)))))
      (int ((* p) (& (@ n 0))))
      (int (i (! (~ (- (+ (* p)))))))
      (int (j (++ i)))
      (int (k (-- j)))
      (int (int_size (sizeof int)))
      (int (n0_size (sizeof (@ n 0))))
      ((extern void) (* ($function alloc (size_t))))
      (double ((* dp) ($call alloc (sizeof (* dp)))))))

(size_t ($function fsize3 ((int n)))
        ($block
         (char ($array b (+ n 3)))
         (return (sizeof b))))
