($pp-include ("stdlib.h"))
(int ($function main ((int argc)
                      (char ((* *) argv))))
     ($block
      ((unsigned char) (* a))
      (= a ($seq ($call malloc 4096)
                 ($call malloc 2048)))
      ($call f a ($seq (= t 3) (+ t 2)) c)))
