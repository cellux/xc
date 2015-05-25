(use-modules (srfi srfi-64)
             (ice-9 rdelim)
             (ice-9 format))

(use-modules (xc))

(define-syntax sf
  (syntax-rules ()
    ((_ fmt arg ...)
     (format #f fmt arg ...))))

(define-syntax assert-equal
  (syntax-rules ()
    ((_ x y)
     (when (not (equal? x y))
           (format #t "TEST FAILED\n\n")
           (format #t "Expected:\n---begin---\n~A---end---\n" y)
           (format #t "Actual:\n---begin---\n~A---end---\n" x)))))

(define (read-xc path)
  (with-input-from-file path
    (lambda ()
      (let loop ((obj (read))
                 (xc (list)))
        (if (eof-object? obj)
            (reverse xc)
            (loop (read) (cons obj xc)))))
    #:binary #t))

(define (read-file path)
  (with-input-from-file path
    (lambda ()
      (read-string))
    #:binary #t))

(when (< (length (program-arguments)) 2)
      (format #t "Usage: ~A [test-folder]\n" (car (program-arguments)))
      (exit))

(let* ((test-folder (cadr (program-arguments)))
       (basename (basename test-folder))
       (xc-path (sf "~a/~a.xc" test-folder basename))
       (xc (xc-translate (read-xc xc-path)))
       (c-path (sf "~a/~a.c" test-folder basename))
       (c (read-file c-path)))
  (assert-equal xc c))
