(int ($function main ((int argc)
                      (char ((* *) argv))))
     ($block
      (int ($array n 10))
      (= (@ n 0) 8)
      ($call printf "hello\n")
      ($call printf "n[0]=%d\n" (@ n 0))
      ($call abort)))
