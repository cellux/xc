(int ((* p) ($compound (int ($array)) (2 4))))

(void ($function f (void))
      ($block
       (int (* p))
       (= p ($compound (int ($array 2)) ((* p))))
       ($call drawline
              ($compound (struct point)
                         ((($at x) 1)
                          (($at y) 1)))
              ($compound (struct point)
                         ((($at x) 3)
                          (($at y) 4))))
       ($call drawline_p
              (& ($compound (struct point)
                            ((($at x) 1)
                             (($at y) 1))))
              (& ($compound (struct point)
                            ((($at x) 3)
                             (($at y) 4)))))
       ($call g ($compound ((const float) ($array))
                           (1e0 1e1 1e2 1e3 1e4 1e5 1e6)))
       (char ((* tmp) ($call mktemp "/tmp/fileXXXXXX")))
       (= tmp ($call mktemp ($compound (char ($array))
                                               ("/tmp/fileXXXXXX"))))
       (= tmp ($call mktemp ($compound ((const char) ($array))
                                               ("/tmp/fileXXXXXX"))))
       ((struct int_list ((int car)
                          ((struct int_list) (* cdr)))))
       ((struct int_list) (endless_zeros (0 (& endless_zeros))))))

((struct s ((int i))))
(int ($function f (void))
     ($block
      ((struct s) ((* p) 0) (* q))
      (int (j 0))
      (: again ($seq (= q p) (= p (& ($compound (struct s) ((j ++)))))))
      (if (< j 2) (goto again))
      (return ($and (== p q) (== (-> q i) 1)))))
