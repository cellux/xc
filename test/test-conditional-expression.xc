(int ($function main ((int argc)
                      (char ((* *) argv))))
     ($block
      (int (a 5)
           (b 8)
           (c (? (> a b)
                 (- a 3)
                 (+ b 6))))))
