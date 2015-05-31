(void ($function f ())
      ($block
       (char (* s))
       (while (!= (* (s ++)) ($char "\0")) ())
       (while loop1
         ($block
          (while loop2
            ($block
             (if want_out
                 (goto end_loop1))))
          (: end_loop1 ())))
       (switch
        expr
        ($block
         (int (i 4))
         ($call f i)
         (case 0 (= i 17))
         (default ($call printf "%d\n" i))))
       (for () () () ())))
