;;; -*- Mode: Scheme; -*-

;;;; Extra pattern-matching facilities for FTRE
;;; Translated from funify.lisp, last edited 1/29/93, KDF

;;; Copyright (c) 1988-1992, Kenneth D. Forbus and Johan de Kleer,
;;; All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(define *bound-vars* '())

;;; Helper: last element of a list
(define (last-element lst)
  (if (null? (cdr lst))
      (car lst)
      (last-element (cdr lst))))

;;; Helper: substitute alist bindings in an expression (like CL sublis)
(define (sublis alist expr)
  (cond ((null? expr) '())
        ((pair? expr)
         (cons (sublis alist (car expr))
               (sublis alist (cdr expr))))
        (else
         (let ((binding (assoc expr alist)))
           (if binding
               (cdr binding)
               expr)))))

(define (quotize pattern)
  (cond ((null? pattern) '())
        ((variable? pattern) pattern)
        ((not (pair? pattern)) (list 'quote pattern))
        ((eq? (car pattern) 'EVAL) (cadr pattern))
        (else (list 'cons
                    (quotize (car pattern))
                    (quotize (cdr pattern))))))

;;; rlet in CL is a macro that expands let-clauses with quotize.
;;; In Scheme we represent it as a procedure that returns the
;;; expanded s-expression (for use in code generation).
(define (expand-rlet var-specs body)
  ;; Provides means for lisp code in body to
  ;; add information to the rule's environment.
  (let ((*bound-vars*
         (append (map car var-specs) *bound-vars*)))
    `(let ,(map (lambda (let-clause)
                  (list (car let-clause)
                        (if (and (pair? (cadr let-clause))
                                 (eq? (car (cadr let-clause))
                                      'EVAL))
                            (cadr (cadr let-clause))
                            (quotize (cadr let-clause)))))
                var-specs)
       ,@(fully-expand-body body))))

;;;; Finding free variables in a pattern

(define (pattern-free-variables pattern)
  (pattern-free-vars1 pattern '()))

(define (pattern-free-vars1 pattern vars)
  (cond ((null? pattern) vars)
        ((variable? pattern)
         (if (or (member pattern vars)
                 (member pattern *bound-vars*))
             vars
             (cons pattern vars)))
        ((not (pair? pattern)) vars)
        (else (pattern-free-vars1
               (cdr pattern)
               (pattern-free-vars1 (car pattern) vars)))))

;;;; Open-coding unification

(define (generate-match-body pattern vars extra-test)
  (let ((structure-tests '())
        (var-alist '())
        (equal-tests '())
        (binding-specs '()))
    (for-each
     (lambda (test)
       (cond ((variable? (car test))
              ;; test looks like (?x (nth p) (nth p) ...)
              (set! equal-tests
                    (append (generate-pairwise-tests (cdr test))
                            equal-tests))
              (if extra-test
                  (set! var-alist
                        (cons (cons (car test) (last-element test))
                              var-alist)))
              (set! binding-specs
                    (cons (last-element test) binding-specs)))
             (else
              (set! structure-tests
                    (cons test structure-tests)))))
     (generate-unify-tests pattern vars '() 'P))
    (set! extra-test (sublis var-alist extra-test))
    (if (not (null? (pattern-free-variables extra-test)))
        (error (string-append
                "Rule test includes free variable: "
                (format "~A" extra-test))))
    (values (append structure-tests equal-tests
                    (if extra-test (list extra-test) '()))
            binding-specs)))

(define (generate-pairwise-tests tests)
  (cond ((or (null? tests) (null? (cdr tests))) '())
        (else (cons (list 'equal? (car tests) (cadr tests))
                    (generate-pairwise-tests (cdr tests))))))

;;; Generate a list of explicit tests for matching
;;; the given pattern. Assumes that the pattern
;;;    to be tested will be in variable "P".
;;; Tests are returned in backward order.
;;; (generate-unify-tests '(foo ?x) '() '() 'P)
;;;     returns:    '((null? (cdr (cdr P)))
;;;                   (equal? ?x (car (cdr P)))
;;;                   (pair? (cdr P))
;;;                   (equal? (quote foo) (car P))
;;;                   (pair? P))

(define (generate-unify-tests pattern vars tests path)
  (cond ((null? pattern)
         ;; this is the end
         (cons `(null? ,path) tests))
        ((member pattern vars)
         ;; must see if the pattern has been bound elsewhere,
         ;; and if it has, test to see if the element here is
         ;; consistent with that earlier binding.
         (let ((previous (assoc pattern tests)))
           (cond (previous
                  ;; add this position to test it
                  (set-cdr! previous (cons path (cdr previous)))
                  tests)
                 (else (cons (list pattern path) tests)))))
        ;; if variable, it must be bound so test
        ;; against the current value.
        ((variable? pattern)
         (cons `(equal? ,pattern ,path) tests))
        ;; if not a list, then see if equal
        ((number? pattern)
         (cons `(and (number? ,path) (= ,pattern ,path))
               tests))
        ((not (pair? pattern))
         (cons `(equal? ',pattern ,path) tests))
        ;; recurse on a list
        (else
         (generate-unify-tests
          (cdr pattern) vars
          (generate-unify-tests
           (car pattern) vars
           ;; avoid errors
           (cons `(pair? ,path) tests)
           ;; extend the path
           (list 'car path))
          ;; extend path in other direction
          (list 'cdr path)))))
