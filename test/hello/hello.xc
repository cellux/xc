(pp-include ("stdio.h"))

(int (function main
               ((int argc)
                (char ((* *) argv))))
     (block
      (call printf "Hello, world!\n")))
