(use-modules (ice-9 rdelim)
             (ice-9 format))

(use-modules (xc))

(define-syntax sf
  (syntax-rules ()
    ((_ fmt arg ...)
     (format #f fmt arg ...))))

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
      (format #t "Usage: ~a [test-path]\n" (car (program-arguments)))
      (exit))

(define (is-file path)
  (eq? (stat:type (stat path)) 'regular))

(define (is-dir path)
  (eq? (stat:type (stat path)) 'directory))

(define (string-starts-with str prefix)
  (let ((str-len (string-length str))
        (prefix-len (string-length prefix)))
    (and (>= str-len prefix-len)
         (string=? (substring str 0 prefix-len)
                   prefix))))

(define (string-ends-with str suffix)
  (let ((str-len (string-length str))
        (suffix-len (string-length suffix)))
    (and (>= str-len suffix-len)
         (string=? (substring str (- str-len suffix-len))
                   suffix))))

(define (strip-ext path ext)
  (substring path 0 (- (string-length path)
                       (string-length ext))))

(define PASS-FAIL-COLUMN 60)

(define (test-xc xc-path)
  (format #t "~a: " xc-path)
  (let* ((xc (xc-translate (read-xc xc-path)))
         (c-path (sf "~a.c" (strip-ext xc-path ".xc")))
         (c (read-file c-path)))
    (if (equal? xc c)
        (format #t "~vtpassed\n" PASS-FAIL-COLUMN)
        (begin
          (format #t "~vtFAILED\n" PASS-FAIL-COLUMN)
          (format #t "Expected:\n---begin---\n~a---end---\n" c)
          (format #t "Actual:\n---begin---\n~a---end---\n" xc)))))

(define (test-file file-path)
  (let ((file-name (basename file-path)))
    (when (and (string-starts-with file-name "test-")
               (string-ends-with file-name ".xc"))
          (test-xc file-path))))

(define (skip x) #t)

(define (test-dir dir-path)
  (let ((dir (opendir dir-path)))
    (let loop ((entry (readdir dir)))
      (when (not (eof-object? entry))
            (test-path (string-append dir-path "/" entry) #t)
            (loop (readdir dir)))
      (closedir dir))))

(define (test-path path skip-dot)
  (cond
   ((is-file path)
    (test-file path))
   ((and (is-dir path)
         (if skip-dot
             (and (not (string=? (basename path) "."))
                  (not (string=? (basename path) "..")))
             #t))
    (test-dir path))))

(test-path (cadr (program-arguments)) #f)
