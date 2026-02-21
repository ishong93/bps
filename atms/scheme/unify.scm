;;; -*- Mode: Scheme; -*-

;;;; Variables and unification
;;; Translated from unify.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1988-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; A symbol whose first character is "?"
(define (variable? x)
  (and (symbol? x)
       (char=? #\? (string-ref (symbol->string x) 0))))

(define (unify a b . bindings-opt)
  (let ((bindings (if (null? bindings-opt) '() (car bindings-opt))))
    (cond ((equal? a b) bindings)
          ((variable? a) (unify-variable a b bindings))
          ((variable? b) (unify-variable b a bindings))
          ((or (not (pair? a)) (not (pair? b))) 'FAIL)
          (else
           (let ((new-bindings (unify (car a) (car b) bindings)))
             (if (not (eq? 'FAIL new-bindings))
                 (unify (cdr a) (cdr b) new-bindings)
                 'FAIL))))))

(define (unify-variable var exp bindings)
  ;; Must distinguish no value from value of '()
  (let ((val (assoc var bindings)))
    (cond (val (unify (cdr val) exp bindings))
          ;; If safe, bind <var> to <exp>
          ((free-in? var exp bindings)
           (cons (cons var exp) bindings))
          (else 'FAIL))))

(define (free-in? var exp bindings)
  ;; Returns #f if <var> occurs in <exp>, assuming <bindings>.
  (cond ((null? exp) #t)
        ((equal? var exp) #f)
        ((variable? exp)
         (let ((val (assoc exp bindings)))
           (if val
               (free-in? var (cdr val) bindings)
               #t)))
        ((not (pair? exp)) #t)
        ((free-in? var (car exp) bindings)
         (free-in? var (cdr exp) bindings))
        (else #f)))
