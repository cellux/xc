(void ($function f1 (int)))
(void ($function f2 ((int *))))
(void ($function f3 ((int (* ($array 3))))))
(void ($function f4 ((int ($array (*) 3)))))
(void ($function f5 ((int ($array (*) *)))))
(void ($function f6 ((int (* ($function))))))
(void ($function f7 ((int ($function (*) (void))))))
(void ($function f8 ((int ($function (((* const) ($array)))
                                     ((unsigned int) $...))))))