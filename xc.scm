(define-module (xc))

(use-modules (ice-9 match)
             (ice-9 format)
             (ice-9 rdelim)
             (ice-9 regex))

(define-syntax sf
  (syntax-rules ()
    ((_ fmt arg ...)
     (format #f fmt arg ...))))

; dmf = define matcher and formatter
(define-macro (dmf name . clauses)
  `(begin
     (define (,(symbol-append name '?) form)
       (match form
         ,@(map (lambda (clause) (list (car clause) #t)) clauses)
         (_ #f)))
     (define (,(symbol-append 'format- name) form)
       (match form
         ,@clauses))))

(define* (indent text #:optional (indent-level 2))
  (with-input-from-string text
    (lambda ()
      (let loop ((indented-lines (list))
                 (line (read-line)))
        (if (eof-object? line)
            (apply string-append (reverse indented-lines))
            (loop (cons (sf "~v_~a\n" indent-level line) indented-lines)
                  (read-line)))))))

;;; A.1.3 Identifiers

(define c-identifier-regexp (make-regexp "^[a-zA-Z_][a-zA-Z_0-9]*$"))

(define (c-identifier? x)
  (and (symbol? x)
       (let ((s (symbol->string x)))
         (regexp-exec c-identifier-regexp s))))

(dmf identifier
     ((? c-identifier? identifier)
      (symbol->string identifier)))

;;; A.1.5 Constants

(dmf integer-constant
     ((and (? integer? i)
           (? exact? i))
      (number->string i))
     (('$int
       (and (? integer? i)
            (? exact? i))
       . (and suffixes ((or 'U 'L) ...)))
      (sf "~a~a"
          (number->string i)
          (apply string-append (map symbol->string suffixes)))))

(dmf floating-constant
     ((? real? f)
      (number->string f))
     (('$float
       (? real? f))
      (number->string f))
     (('$float
       (? real? f)
       (and suffix (or 'F 'L)))
      (sf "~a~a"
          (number->string f)
          (symbol->string suffix))))

(dmf enumeration-constant
     ((? identifier? x)
      (format-identifier x)))

(dmf character-constant
     (('$char (and (? string-literal? string-literal)
                   (not (? string-null? string-literal))))
      (let ((c (char->integer (string-ref string-literal 0))))
        (sf "'~a'"
            (cond
             ((< c #x20)
              (case c
                ((#x07) "\\a")
                ((#x08) "\\b")
                ((#x09) "\\t")
                ((#x0a) "\\n")
                ((#x0b) "\\v")
                ((#x0c) "\\f")
                ((#x0d) "\\r")
                (else (sf "\\x~2,'0x" c))))
             ((> c #x7e) (sf "\\x~2,'0x" c))
             ((= c #x27) "\\'")
             ((= c #x5c) "\\\\")
             (else (integer->char c)))))))

(dmf constant
     ((? integer-constant? integer-constant)
      (format-integer-constant integer-constant))
     ((? floating-constant? floating-constant)
      (format-floating-constant floating-constant))
     ((? enumeration-constant? enumeration-constant)
      (format-enumeration-constant enumeration-constant))
     ((? character-constant? character-constant)
      (format-character-constant character-constant)))

;;; A.1.6 String literals

(dmf string-literal
     ((? string? s)
      (sf "~s" s)))

;;; A.2.1 Expressions

(dmf primary-expression
     ((? identifier? identifier)
      (format-identifier identifier))
     ((? constant? constant)
      (format-constant constant))
     ((? string-literal? string-literal)
      (format-string-literal string-literal)))

(dmf postfix-expression
     ((? primary-expression? e)
      (format-primary-expression e))
     (('@ (? expression? array)
          (? expression? index))
      (sf "~a[~a]"
          (format-expression array)
          (format-expression index)))
     (('$call (? expression? callable)
              (? assignment-expression? assignment-expressions) ...)
      (sf "~a(~a)"
          (format-expression callable)
          (string-join (map format-assignment-expression
                            assignment-expressions)
                       ", ")))
     (('$at (? expression? struct)
            (? identifier? element))
      (sf "~a.~a"
          (format-expression struct)
          (format-identifier element)))
     (('-> (? expression? pointer)
           (? identifier? element))
      (sf "~a->~a"
          (format-expression pointer)
          (format-identifier element)))
     (((? postfix-expression? postfix-expression)
       (and op (or '++ '--)))
      (sf "(~a~a)"
          (format-postfix-expression postfix-expression)
          (symbol->string op)))
     (('$compound
       (? type-name? type-name)
       (? initializer-list? initializer-list))
      (sf "((~a) { ~a })"
          (format-type-name type-name)
          (format-initializer-list initializer-list))))

(define unary-operators '(& * + - ~ !))

(define (unary-operator? x)
  (memq x unary-operators))

(define (format-unary-operator op)
  (symbol->string op))

(dmf unary-expression
     ((? postfix-expression? e)
      (format-postfix-expression e))
     (((? unary-operator? op)
       (? expression? expression))
      (sf "(~a~a)"
          (format-unary-operator op)
          (format-expression expression)))
     (((and op (or '++ '--))
       (? unary-expression? unary-expression))
      (sf "(~a~a)"
          (symbol->string op)
          (format-unary-expression unary-expression)))
     (('sizeof (? expression? expression))
      (sf "sizeof(~a)" (format-expression expression))))

(dmf cast-expression
     ((? unary-expression? e)
      (format-unary-expression e))
     (('$cast (? type-name? type-name)
              (? expression? expression))
      (sf "((~a) ~a)"
          (format-type-name type-name)
          (format-expression expression))))

;;; The whole cast of binary expressions (multiplicative, additive,
;;; relational, etc.) have been simplified to a single case.
;;;
;;; On the XC side, S-expressions allow us to be explicit about
;;; precendence. Generated binary expressions are always placed within
;;; parentheses so the order of operations will be preserved.

(dmf binary-expression
     ((? cast-expression? e)
      (format-cast-expression e))
     (('$and (? expression? left) (? expression? right))
      (sf "(~a && ~a)" (format-expression left) (format-expression right)))
     (('$or (? expression? left) (? expression? right))
      (sf "(~a || ~a)" (format-expression left) (format-expression right)))
     (('$bit-and (? expression? left) (? expression? right))
      (sf "(~a&~a)" (format-expression left) (format-expression right)))
     (('$bit-or (? expression? left) (? expression? right))
      (sf "(~a|~a)" (format-expression left) (format-expression right)))
     (('$bit-xor (? expression? left) (? expression? right))
      (sf "(~a^~a)" (format-expression left) (format-expression right)))
     (((and (? symbol? op)
            (or '* '/ '%
                '+ '-
                '<< '>>
                '< '> '<= '>=
                '== '!=))
       (? expression? left)
       (? expression? right))
      (sf "(~a~a~a)"
          (format-expression left)
          (symbol->string op)
          (format-expression right))))

(dmf conditional-expression
     ((? binary-expression? e)
      (format-binary-expression e))
     (('? (? expression? condition)
          (? expression? e1)
          (? expression? e2))
      (sf "(~a ? ~a : ~a)"
          (format-expression condition)
          (format-expression e1)
          (format-expression e2))))

(dmf assignment-operator
     ('$bit-and= "&=")
     ('$bit-or= "|=")
     ('$bit-xor= "^=")
     ((and (? symbol? op)
           (or '= '*= '/= '%= '+= '-= '<<= '>>=))
      (symbol->string op)))

(dmf assignment-expression
     ((? conditional-expression? e)
      (format-conditional-expression e))
     (((? assignment-operator? op)
       (? unary-expression? lvalue)
       (? expression? rvalue))
      (sf "(~a ~a ~a)"
          (format-unary-expression lvalue)
          (format-assignment-operator op)
          (format-expression rvalue))))

(dmf expression
     ((? assignment-expression? e)
      (format-assignment-expression e))
     (('$seq (? assignment-expression? exprs) ..1)
      (sf "(~a)"
          (string-join (map format-assignment-expression exprs) ", "))))

(dmf constant-expression
     ((? conditional-expression? e)
      (format-conditional-expression e)))

;;; A.2.2 Declarations

(dmf declaration
     (((? declaration-specifiers? decl-specs))
      (sf "~a;\n"
          (format-declaration-specifiers decl-specs)))
     (((? declaration-specifiers? decl-specs)
       (? init-declarator? init-declarators) ..1)
      (sf "~a ~a;\n"
          (format-declaration-specifiers decl-specs)
          (string-join (map format-init-declarator
                            init-declarators)
                       ", "))))

(dmf declaration-specifier
     ((? storage-class-specifier? spec)
      (format-storage-class-specifier spec))
     ((? type-specifier? spec)
      (format-type-specifier spec))
     ((? type-qualifier? spec)
      (format-type-qualifier spec))
     ((? function-specifier? spec)
      (format-function-specifier spec)))

(dmf declaration-specifiers
     ((? declaration-specifier? spec)
      (format-declaration-specifier spec))
     (((? declaration-specifier? decl-specs) ..1)
      (string-join (map format-declaration-specifier decl-specs) " ")))

(dmf init-declarator
     ((? declarator? declarator)
      (format-declarator declarator))
     (((? declarator? declarator)
       (? initializer? initializer))
      (sf "~a = ~a"
          (format-declarator declarator)
          (format-initializer initializer))))

(dmf storage-class-specifier
     ((and spec (or 'typedef
                    'extern
                    'static
                    'auto
                    'register))
      (symbol->string spec)))

(dmf type-specifier
     ((and spec (or 'void
                    'char
                    'short
                    'int
                    'long
                    'float
                    'double
                    'signed
                    'unsigned
                    '_Bool
                    '_Complex))
      (symbol->string spec))
     ((? struct-or-union-specifier? spec)
      (format-struct-or-union-specifier spec))
     ((? enum-specifier? spec)
      (format-enum-specifier spec))
     ((? typedef-name? spec)
      (format-typedef-name spec)))

(dmf struct-or-union-specifier
     (((? struct-or-union? struct-or-union)
       (? identifier? identifier)
       ((? struct-declaration? struct-declaration-list) ...))
      (sf "~a ~a {\n~a}"
          (format-struct-or-union struct-or-union)
          (format-identifier identifier)
          (indent (apply string-append
                         (map format-struct-declaration
                              struct-declaration-list)))))
     (((? struct-or-union? struct-or-union)
       ((? struct-declaration? struct-declaration-list) ...))
      (sf "~a {\n~a}"
          (format-struct-or-union struct-or-union)
          (indent (apply string-append
                         (map format-struct-declaration
                              struct-declaration-list)))))
     (((? struct-or-union? struct-or-union)
       (? identifier? identifier))
      (sf "~a ~a"
          (format-struct-or-union struct-or-union)
          (format-identifier identifier))))

(dmf struct-or-union
     ((and struct-or-union (or 'struct 'union))
      (symbol->string struct-or-union)))

(dmf struct-declaration
     (((? specifier-qualifier-list? specifier-qualifier-list)
       (? struct-declarator? struct-declarator-list) ..1)
      (sf "~a ~a;\n"
          (format-specifier-qualifier-list specifier-qualifier-list)
          (string-join (map format-struct-declarator
                            struct-declarator-list)
                       ", "))))

(dmf specifier-qualifier
     ((? type-specifier? type-specifier)
      (format-type-specifier type-specifier))
     ((? type-qualifier? type-qualifier)
      (format-type-qualifier type-qualifier)))

(dmf specifier-qualifier-list
     ((? specifier-qualifier? specifier-qualifier)
      (format-specifier-qualifier specifier-qualifier))
     (((? specifier-qualifier? specifier-qualifiers) ..1)
      (string-join (map format-specifier-qualifier
                        specifier-qualifiers)
                   " ")))

(dmf struct-declarator
     ((? declarator? declarator)
      (format-declarator declarator))
     (((? declarator? declarator)
       (? constant-expression? constant-expression))
      (sf "~a:~a"
          (format-declarator declarator)
          (format-constant-expression constant-expression))))

(dmf enum-specifier
     (('enum
       (? identifier? identifier)
       ((? enumerator? enumerator-list) ..1))
      (sf "enum ~a { ~a }"
          (format-identifier identifier)
          (string-join (map format-enumerator enumerator-list) ", ")))
     (('enum
       ((? enumerator? enumerator-list) ..1))
      (sf "enum { ~a }"
          (string-join (map format-enumerator enumerator-list) ", ")))
     (('enum
       (? identifier? identifier))
      (sf "enum ~a"
          (format-identifier identifier))))

(dmf enumerator
     ((? enumeration-constant? enumeration-constant)
      (format-enumeration-constant enumeration-constant))
     (((? enumeration-constant? enumeration-constant)
       (? constant-expression? constant-expression))
      (sf "~a = ~a"
          (format-enumeration-constant enumeration-constant)
          (format-constant-expression constant-expression))))

(dmf type-qualifier
     ((and type-qualifier (or 'const
                              'restrict
                              'volatile))
      (symbol->string type-qualifier)))

(dmf function-specifier
     ((and function-specifier (or 'inline))
      (symbol->string function-specifier)))

(dmf declarator
     ;; order matters here
     (((? pointer? pointer)
       (? direct-declarator? direct-declarator))
      (sf "~a~a"
          (format-pointer pointer)
          (format-direct-declarator direct-declarator)))
     ((? direct-declarator? direct-declarator)
      (format-direct-declarator direct-declarator)))

(dmf direct-declarator
     ((? identifier? identifier)
      (format-identifier identifier))
     ;; ( declarator )
     ;;
     ;; the pointer case from declarator is included here to avoid
     ;; infinite ping-pong between declarator and direct-declarator
     (((? pointer? pointer)
       (? direct-declarator? direct-declarator))
      (sf "(~a~a)"
          (format-pointer pointer)
          (format-direct-declarator direct-declarator)))
     (('$array
       (? direct-declarator? direct-declarator)
       (? assignment-expression? assignment-expression))
      (sf "~a[~a]"
          (format-direct-declarator direct-declarator)
          (format-assignment-expression assignment-expression)))
     (('$array
       (? direct-declarator? direct-declarator))
      (sf "~a[]"
          (format-direct-declarator direct-declarator)))
     (('$function
       (? direct-declarator? direct-declarator)
       ((? parameter-declaration? parameter-declarations) ..1))
      (sf "~a(~a)"
          (format-direct-declarator direct-declarator)
          (string-join (map format-parameter-declaration
                            parameter-declarations)
                       ", ")))
     (('$function
       (? direct-declarator? direct-declarator)
       ((? identifier? identifier-list) ...))
      (sf "~a(~a)"
          (format-direct-declarator direct-declarator)
          (string-join (map format-identifier
                            identifier-list)
                       ", "))))

(dmf pointer-item
     ('*
      "*")
     (('* (? type-qualifier? type-qualifiers) ...)
      (sf "* ~a "
          (string-join (map format-type-qualifier
                            type-qualifiers)
                       " "))))

(dmf pointer
     ((? pointer-item? pointer-item)
      (format-pointer-item pointer-item))
     (((? pointer-item? pointer-items) ...)
      (apply string-append (map format-pointer-item pointer-items))))

(dmf parameter-declaration
     (((? declaration-specifiers? decl-specs)
       (? declarator? declarator))
      (sf "~a ~a"
          (format-declaration-specifiers decl-specs)
          (format-declarator declarator)))
     (((? declaration-specifiers? decl-specs)
       (? abstract-declarator? abstract-declarators) ...)
      (sf "~a ~a"
          (format-declaration-specifiers decl-specs)
          (string-join (map format-abstract-declarator
                            abstract-declarators)
                       " "))))

(dmf type-name
     ((? specifier-qualifier-list? specifier-qualifier-list)
      (format-specifier-qualifier-list specifier-qualifier-list))
     (((? specifier-qualifier-list? specifier-qualifier-list)
       (? abstract-declarator? abstract-declarator))
      (sf "~a ~a"
          (format-specifier-qualifier-list specifier-qualifier-list)
          (format-abstract-declarator abstract-declarator))))

(dmf abstract-declarator
     ((? pointer? pointer)
      (format-pointer pointer))
     ((? direct-abstract-declarator? direct-abstract-declarator)
      (format-direct-abstract-declarator direct-abstract-declarator))
     (((? pointer? pointer)
       (? direct-abstract-declarator? direct-abstract-declarator))
      (sf "~a~a"
          (format-pointer pointer)
          (format-direct-abstract-declarator direct-abstract-declarator))))

(dmf direct-abstract-declarator
     (('$array
       (? direct-abstract-declarator? direct-abstract-declarator)
       (? assignment-expression? assignment-expression))
      (sf "~a[~a]"
          (format-direct-abstract-declarator direct-abstract-declarator)
          (format-assignment-expression assignment-expression)))
     (('$array
       (? direct-abstract-declarator? direct-abstract-declarator))
      (sf "~a[]"
          (format-direct-abstract-declarator direct-abstract-declarator)))
     (('$array
       (? assignment-expression? assignment-expression))
      (sf "[~a]"
          (format-assignment-expression assignment-expression)))
     (('$array)
      "[]")
     (('$function
       (? direct-abstract-declarator? direct-abstract-declarator)
       ((? parameter-declaration? parameter-declarations) ...))
      (sf "~a(~a)"
          (format-direct-abstract-declarator direct-abstract-declarator)
          (string-join (map format-parameter-declaration
                            parameter-declarations)
                       ", ")))
     (('$function
       (? direct-abstract-declarator? direct-abstract-declarator))
      (sf "~a()"
          (format-direct-abstract-declarator direct-abstract-declarator)))
     (('$function
       ((? parameter-declaration? parameter-declarations) ..1))
      (sf "(~a)"
          (string-join (map format-parameter-declaration
                            parameter-declarations)
                       ", ")))
     (('$function)
      "()"))

(dmf typedef-name
     ((? identifier? identifier)
      (format-identifier identifier)))

(dmf initializer
     ((? assignment-expression? assignment-expression)
      (format-assignment-expression assignment-expression))
     ((? initializer-list? initializer-list)
      (sf "{ ~a }"
          (format-initializer-list initializer-list))))

(dmf designator
     (('@
       (? constant-expression? constant-expression))
      (sf "[~a]"
          (format-constant-expression constant-expression)))
     (('$at
       (? identifier? identifier))
      (sf ".~a"
          (format-identifier identifier))))

(dmf designation
     ((? designator? designator)
      (format-designator designator))
     (((? designator? designators) ..1)
      (apply string-append (map format-designator designators))))

(dmf initializer*
     ((? initializer? initializer)
      (format-initializer initializer))
     (((? designation? designation)
       (? initializer? initializer))
      (sf "~a=~a"
          (format-designation designation)
          (format-initializer initializer))))

(dmf initializer-list
     (((? initializer*? initializers) ..1)
      (string-join (map format-initializer* initializers) ", ")))

;;; A.2.3 Statements

(dmf statement
     ((? labeled-statement? labeled-statement)
      (format-labeled-statement labeled-statement))
     ((? compound-statement? compound-statement)
      (format-compound-statement compound-statement))
     ((? expression-statement? expression-statement)
      (format-expression-statement expression-statement))
     ((? selection-statement? selection-statement)
      (format-selection-statement selection-statement))
     ((? iteration-statement? iteration-statement)
      (format-iteration-statement iteration-statement))
     ((? jump-statement? jump-statement)
      (format-jump-statement jump-statement)))

(dmf labeled-statement
     ((':
       (? identifier? identifier)
       (? statement? statement))
      (sf "~a:\n~a"
          (format-identifier identifier)
          (format-statement statement)))
     (('case
       (? constant-expression? constant-expression)
       (? statement? statement))
      (sf "case ~a: ~a"
          (format-constant-expression constant-expression)
          (format-statement statement)))
     (('default
       (? statement? statement))
      (sf "default: ~a"
          (format-statement statement))))

(dmf compound-statement
     (('$block
       (? block-item? block-items) ...)
      (sf "{\n~a}\n"
          (indent (apply string-append
                         (map format-block-item block-items))))))

(dmf block-item
     ((? statement? statement)
      (format-statement statement))
     ((? declaration? declaration)
      (format-declaration declaration)))

(dmf expression-statement
     ((? expression? expression)
      (sf "~a;\n" (format-expression expression))))

(dmf selection-statement
     (('if
       (? expression? expression)
       (? statement? statement))
      (sf "if (~a) ~a"
          (format-expression expression)
          (format-statement statement)))
     (('if
       (? expression? expression)
       (? statement? statement)
       (? statement? statement))
      (sf "if (~a) ~a else ~a"
          (format-expression expression)
          (format-statement statement)
          (format-statement statement)))
     (('switch
       (? expression? expression)
       (? statement? statement))
      (sf "switch (~a) ~a"
          (format-expression expression)
          (format-statement statement))))

(dmf iteration-statement
     (('while
       (? expression? expression)
       (? statement? statement))
      (sf "while (~a) ~a"
          (format-expression expression)
          (format-statement statement)))
     (('do
       (? statement? statement)
       (? expression? expression))
      (sf "do ~a while (~a)"
          (format-statement statement)
          (format-expression expression)))
     (('for
       (? expression? e1)
       (? expression? e2)
       (? expression? e3)
       (? statement? statement))
      (sf "for(~a; ~a; ~a) ~a"
          (format-expression e1)
          (format-expression e2)
          (format-expression e3)
          (format-statement statement)))
     (('for
       (? declaration? decl)
       (? expression? e2)
       (? expression? e3)
       (? statement? statement))
      (sf "for(~a; ~a; ~a) ~a"
          (format-declaration decl)
          (format-expression e2)
          (format-expression e3)
          (format-statement statement))))

(dmf jump-statement
     (('goto (? identifier? identifier))
      (sf "goto ~a;\n" (format-identifier identifier)))
     (('continue)
      (sf "continue;\n"))
     (('break)
      (sf "break;\n"))
     (('return (? expression? expression))
      (sf "return ~a;\n" (format-expression expression)))
     (('return)
      (sf "return;\n")))

(dmf translation-item
     ((? pp-directive? pp-directive)
      (format-pp-directive pp-directive))
     ((? external-declaration? external-declaration)
      (format-external-declaration external-declaration)))

(dmf translation-unit
     (((? translation-item? translation-items) ...)
      (apply string-append (map format-translation-item
                                translation-items))))

(dmf header-name
     ((? string? header-name)
      (sf "\"~a\"" header-name))
     (((? string? header-name))
      (sf "<~a>" header-name)))

(dmf pp-directive
     (('$pp-include
       (? header-name? header-name))
      (sf "#include ~a\n"
          (format-header-name header-name)))
     (((and op (or '$pp-if '$pp-elif))
       (? constant-expression? constant-expression))
      (sf "#~a ~a\n"
          (substring (symbol->string op) 4)
          (format-constant-expression constant-expression)))
     (((and op (or '$pp-ifdef '$pp-ifndef))
       (? identifier? identifier))
      (sf "#~a ~a\n"
          (substring (symbol->string op) 4)
          (format-identifier identifier)))
     (('$pp-else)
      "#else\n")
     (('$pp-endif)
      "#endif\n"))

(dmf external-declaration
     ((? function-definition? function-definition)
      (format-function-definition function-definition))
     ((? declaration? declaration)
      (format-declaration declaration)))

(dmf function-definition
     (((? declaration-specifiers? decl-specs)
       (? declarator? declarator)
       (? compound-statement? compound-statement))
      (sf "~a ~a ~a"
          (format-declaration-specifiers decl-specs)
          (format-declarator declarator)
          (format-compound-statement compound-statement))))

(define xc-translate format-translation-unit)

(export xc-translate)
