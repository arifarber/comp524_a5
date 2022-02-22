#lang racket
;; if multiple statements just return the last one
;; TODO: +,-,*,/,string-append,string<?,string=?,not,=,<
;;program     : exprList
;;exprList    : expr optExprList
;;optExprList : ɛ | exprList
;;expr        : atom | invocation
;;atom        : NAME | STRING | number
;;number      : INT | FLOAT
;;invocation  : OPAREN exprList CPAREN
(require (only-in (file "a3.rkt")
                  parse))

(define (eval code)
  (first(eval-program (parse code))))

;;program     : exprList
(define (eval-program program-expr )
  (eval-exprList (second program-expr) ))

;;exprList    : expr optExprList
(define (eval-exprList expr-exprList )
  (let ([expr-expr (second expr-exprList)]
        [expr-optExprList (third expr-exprList)])
    (cons(eval-expr expr-expr)
     (eval-optExprList expr-optExprList))))

;;expr        : atom | invocation
(define (eval-expr expr-expr )
  (let* ([subtree  (second expr-expr)]
         [sub-type (first subtree)]
         [eval-fn  (case sub-type
                     [(atom)       eval-atom]
                     [(invocation) eval-invocation])])
    (eval-fn subtree )))

;;optExprList : ɛ | exprList
(define (eval-optExprList expr-optExprList )
  (if (= (length expr-optExprList) 1)
      '()
      (eval-exprList(second expr-optExprList))))

;;atom   : NAME | STRING | number
;; TODO: +,-,*,/,string-append,string<?,string=?,not,=,<
(define (eval-atom atom-expr )
  (let* ([token      (second atom-expr)]
         [token-type (first token)]
         [token-data (second token)]
         [eval-fn (case token-type
                    [(NAME) (hash-ref name-map token-data)] ;;no need for (>)?
                    [(STRING) (token-data)]
                    [(number) (eval-number token )])])    ;;not sure this works
    eval-fn))


(define name-map (hash
                   '+ +
                   '- -
                   '* *
                   '/ /
                   'string-append string-append
                   'string<? string<?
                   'string=? string=?
                   'not not
                   '= =
                   '< <))
  

;;invocation  : OPAREN exprList CPAREN
(define (eval-invocation invocation-expr )
  (let* ([final-list (eval-exprList (third invocation-expr) )]
         [operation (first final-list)]
         [arguments (rest final-list)])
    (apply operation arguments)))

;;number      : INT | FLOAT
(define (eval-number number-expr )
  (let* ([token      (second number-expr)]
         [token-type (first token)]
         [token-data (second token)])
     token-data))


