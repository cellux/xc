(int ($function main ((int argc)
                      (char ((* *) argv))))
     ($block
      (int ($array n 10))
      (int ((* p) (& (@ n 0))))
      (int (i (! (~ (- (+ (* p)))))))
      (int (j (++ i)))
      (int (k (-- j)))
      (int (int_size ($sizeof:type int)))
      (int (n0_size ($sizeof:expr (@ n 0))))))
