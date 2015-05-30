((inline double)
 ($function fahr ((double t)))
 ($block
  (return (+ (/ (* 9.0 t) 5.0) 32.0))))

((inline double)
 ($function cels ((double t)))
 ($block
  (return (/ (* 5.0 (- t 32.0)) 9.0))))

((extern double) ($function fahr (double)))

(double ($function convert ((int is_fahr)
                            (double temp)))
        ($block
         (return (? is_fahr
                    ($call cels temp)
                    ($call fahr temp)))))
