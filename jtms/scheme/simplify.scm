;;; -*- Mode: Scheme; -*-

;;;; Algebraic simplifier
;;; Translated from simplify.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; This version is inspired by one of G.J. Sussman's scheme matchers.
;;; Requires: match.scm

;;;; Simplification cache
;;;; 단순화 캐시

(define *simplify-cache* '())

;;; simplify: 식을 대수적으로 단순화
;;; 캐시를 사용하여 이미 단순화한 식의 재계산을 방지
(define (simplify exp)
  (let ((cached (assoc exp *simplify-cache*)))
    (if cached
        (cdr cached)
        (let ((result (simplify-it exp *algebra-rules*)))
          (set! *simplify-cache*
                (cons (cons exp result) *simplify-cache*))
          result))))

;;; clear-simplify-cache: 단순화 캐시를 초기화
(define (clear-simplify-cache)
  (set! *simplify-cache* '()))

;;; simplify-it: 규칙을 적용하여 식을 반복적으로 단순화
(define (simplify-it exp rules)
  (let ((result (try-matcher-rules
                 (if (pair? exp) (map simplify exp) exp)
                 rules)))
    (if (equal? result exp)
        result
        (simplify-it result rules))))

;;; try-matcher-rules: 규칙 리스트에서 매칭되는 규칙을 찾아 적용
(define (try-matcher-rules exp rules)
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (rule)
        (let ((bindings (match (simplify-rule-pattern rule) exp)))
          (unless (eq? bindings 'fail)
            (when (check-predicate (simplify-rule-predicate rule) bindings)
              (return (substitute-in (simplify-rule-skeleton rule) bindings))))))
      rules)
     ;; 매칭되는 규칙이 없으면 원래 식을 반환
     exp)))

;;; check-predicate: 규칙의 술어를 확인
(define (check-predicate proc bindings)
  (if (not proc)
      #t
      (eval (substitute-in proc bindings))))

;;; 규칙 접근자
;;; 규칙은 (pattern predicate skeleton) 형태의 리스트
(define (simplify-rule-pattern rule) (car rule))
(define (simplify-rule-predicate rule) (cadr rule))
(define (simplify-rule-skeleton rule) (caddr rule))

;;;; Algebra utilities
;;;; 대수 유틸리티

;;; alg<: 대수식의 정렬 비교 함수
(define (alg< e1 e2)
  (cond ((match-equal? e1 e2) #f)
        ((pair? e1)
         (if (pair? e2)
             (if (match-equal? (car e1) (car e2))
                 (alg< (cdr e1) (cdr e2))
                 (alg< (car e1) (car e2)))
             #f))
        ((pair? e2) #t)
        ((symbol? e1)
         (if (symbol? e2)
             (string<? (symbol->string e1) (symbol->string e2))
             #f))
        ((symbol? e2) #t)
        ((and (number? e1) (number? e2)) (< e1 e2))
        (else (error "alg< cannot compare these" e1 e2))))

;;; alg=: 대수식의 동등성 비교
(define (alg= e1 e2)
  (not (or (alg< e1 e2) (alg< e2 e1))))

;;; sorted?: 리스트가 주어진 비교 함수로 정렬되어 있는지 확인
(define (sorted? lst pred)
  (cond ((or (null? lst) (null? (cdr lst))) #t)
        ((pred (cadr lst) (car lst)) #f)
        (else (sorted? (cdr lst) pred))))

;;; +/*?: + 또는 * 연산자인지 확인
(define (+/*? exp)
  (or (eq? exp '+) (eq? exp '*)))

;;; same-constant?: 식이 주어진 상수와 같은 수치 값인지 확인
(define (same-constant? exp constant)
  (and (number? exp)
       (if (inexact? exp)
           (match-equal? exp (exact->inexact constant))
           (= exp constant))))

;;; zero?: 0인지 확인
(define (zero-val? exp) (same-constant? exp 0))

;;; one?: 1인지 확인
(define (one-val? exp) (same-constant? exp 1))

;;;; Extra utilities
;;;; 추가 유틸리티

;;; occurs-in?: exp1이 exp2 내에 존재하는지 확인
(define (occurs-in? exp1 exp2)
  (cond ((equal? exp1 exp2) #t)
        ((null? exp2) #f)
        ((pair? exp2)
         (or (occurs-in? exp1 (car exp2))
             (occurs-in? exp1 (cdr exp2))))
        (else #f)))

;;;; Rules for algebraic simplification
;;;; 대수 단순화 규칙
;;; 각 규칙은 (pattern predicate skeleton) 형식의 리스트

(define *algebra-rules*
  (list
   ;; Flush degenerate cases — 퇴화 경우 제거
   (list '((? op +/*?) (? e)) #f '(? e))
   (list '(+ (? zero zero-val?) (?? e)) #f '(+ (?? e)))
   (list '(- (? zero zero-val?) (? e)) #f '(- (? e)))
   (list '(- (? e) (? zero zero-val?)) #f '(? e))
   (list '(- (? e) (? e)) #f 0)
   (list '(* (? one one-val?) (?? e)) #f '(* (?? e)))
   (list '(* (? zero zero-val?) (?? e)) #f 0)
   (list '(expt (? e) (? zero zero-val?)) #f 1)
   (list '(expt (? e) (? one one-val?)) #f '(? e))
   (list '(log (? one one-val?) (? base)) #f 0)
   (list '(log (? base) (? base)) #f 1)
   (list '(log (expt (? base) (? val)) (? base)) #f '(? val))
   (list '(expt (? base) (log (? val) (? base))) #f '(? val))

   ;; Equivalences involving powers — 거듭제곱 관련 동치
   (list '(* (? e) (? e)) #f '(sqr (? e)))
   (list `(expt (? e) (? two ,(lambda (exp) (same-constant? exp 2))))
         #f '(sqr (? e)))
   (list '(sqrt (sqr (? e))) #f '(abs (? e)))
   (list '(sqr (sqrt (? e))) #f '(? e))

   ;; Combine numerical constants — 수치 상수 결합
   (list '((? op +/*?) (? e1 number?) (? e2 number?) (?? e3))
         #f
         '((? op) (:EVAL ((? op) (? e1) (? e2))) (?? e3)))
   (list '(- (- (? e1) (? e2))) #f '(- (? e2) (? e1)))
   (list '(- (? e1 number?) (? e2 number?)) #f '(:EVAL (- (? e1) (? e2))))
   (list '(- (? e1 number?)) #f '(:EVAL (- (? e1))))
   (list '(- (? e1) (? e2 number?)) #f '(+ (- (? e2)) (? e1)))
   (list '(- (? e1 number?) (+ (? e2 number?) (?? e3)))
         #f '(- (:EVAL (- (? e1) (? e2))) (+ (?? e3))))
   (list '(- (? e1 number?) (- (? e2 number?) (?? e3)))
         #f '(+ (:EVAL (- (? e1) (? e2))) (?? e3)))
   (list '(+ (? e1 number?) (- (? e2 number?) (?? e3)))
         #f '(- (:EVAL (+ (? e1) (? e2))) (?? e3)))
   (list '(sqr (? e1 number?)) #f '(:EVAL (* (? e1) (? e1))))
   (list '(sqrt (? e1 number?)) #f '(:EVAL (sqrt (? e1))))
   (list '(expt (? e1 number?) (? e2 number?)) #f '(:EVAL (expt (? e1) (? e2))))
   (list '(/ (? e1 number?) (? e2 number?)) #f '(:EVAL (/ (? e1) (? e2))))
   (list '(* (? e1 number?) (/ (? e2) (? e3 number?)))
         #f '(* (:EVAL (/ (? e1) (? e3))) (? e2)))
   (list '(/ (* (? e1 number?) (? e2)) (? e3 number?))
         #f '(* (:EVAL (/ (? e1) (? e3))) (? e2)))
   (list '(* (?? pre) (- (? term)) (?? post))
         #f '(* (?? pre) (* -1 (? term)) (?? post)))
   (list '(abs (? e number?)) #f '(:EVAL (abs (? e))))
   (list '(log (? x number?) (? base number?))
         #f '(:EVAL (/ (log (? x)) (log (? base)))))

   ;; Flatten +,* — +,* 평탄화
   (list '((? op +/*?) (?? e1) ((? op) (?? e2) (?? e3)))
         #f
         '((? op) (?? e1) (?? e2) (?? e3)))

   ;; Combine like terms — 동류항 결합
   (list '(+ (?? pre) (* (? f1) (? thing)) (* (? f2) (? thing)) (?? post))
         #f
         '(+ (?? pre) (* (* (? f1) (? f2)) (? thing)) (?? post)))
   (list '(+ (?? pre) (* (? f1) (? thing)) (?? mid) (? thing) (?? post))
         #f
         '(+ (?? pre) (* (+ 1 (? f1)) (? thing)) (?? mid) (?? post)))

   ;; Canonicalize +,* — +,* 정규화
   (list '((? op +/*?) (?? terms))
         '(not (sorted? (quote (? terms)) alg<))
         '((? op) (:SPLICE (sort (quote (? terms)) alg<))))
   ))
