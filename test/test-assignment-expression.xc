(int ($function main ((int argc)
                      (char ((* *) argv))))
     ($block
      (int (a 5))
      (= a 2)
      (*= a 20)
      (/= a 10)
      (%= a 2)
      (+= a 6)
      (-= a -5)
      (<<= a 4)
      (>>= a 4)
      ($bit-and= a 15)
      ($bit-or= a 2)
      ($bit-xor= a 7)))

(int ($function f (void))
     ($block
      (char c)
      (if (== (= c ($call f)) -1) (return 0))
      (int i)
      (long l)
      (= l (= c i))
      ((const char) ((* *) cpp))
      (char (* p))
      ((const char) (c ($char "a")))
      (= cpp (& p))
      (= (* cpp) (& c))
      (= (* p) 0)))
