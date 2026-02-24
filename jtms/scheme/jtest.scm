;;; -*- Mode: Scheme; -*-

;;;; Simple shakedown procedure for JTRE
;;; Translated from jtest.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: jinter.scm, jdata.scm, jrules.scm, jtms.scm, funify.scm

;;;; JTRE 셰이크다운 테스트
;;; INTERN 규칙과 IN 규칙의 기본 동작을 테스트합니다.

(define (shakedown-jtre)
  (in-jtre (create-jtre "Test One"))

  ;; :INTERN 규칙 정의 — (foo ?x)와 (bar ?y)가 모두 숫자이면 (mumble ?x ?y) 단언
  ;; Scheme에서는 규칙을 insert-rule로 직접 등록
  (insert-rule
   (get-dbclass 'foo)
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'foo)
              (pair? (cdr p)) (number? (cadr p))
              (null? (cddr p)))
         (values #t (list (cadr p)) #f)
         (values #f '() #f)))
   (lambda (x)
     ;; bar에도 매칭되는 datum이 있는지 확인
     (for-each
      (lambda (bar-datum)
        (let ((form (datum-lisp-form bar-datum)))
          (when (and (pair? form) (eq? (car form) 'bar)
                     (pair? (cdr form)) (number? (cadr form)))
            (assert! (list 'mumble x (cadr form))
                     (list 'Test-intern)))))
      (get-candidates '(bar)))))
  (display "\n :INTERN rule defined okay.")

  ;; :IN 규칙 정의 — (foo ?x)와 (bar ?y)가 모두 비숫자이고 IN이면
  ;; (grumble ?x ?y) 단언
  (insert-rule
   (get-dbclass 'foo)
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'foo)
              (pair? (cdr p)) (not (number? (cadr p)))
              (null? (cddr p)))
         (values #t (list (cadr p)) #t)
         (values #f '() #f)))
   (lambda (trigger-node x)
     (when (in-node? trigger-node)
       (for-each
        (lambda (bar-datum)
          (let ((form (datum-lisp-form bar-datum)))
            (when (and (pair? form) (eq? (car form) 'bar)
                       (pair? (cdr form)) (not (number? (cadr form)))
                       (in-node? (datum-tms-node bar-datum)))
              (assert! (list 'grumble x (cadr form))
                       (list 'Test-in)))))
        (get-candidates '(bar))))))
  (display "\n :IN rule defined okay.")

  ;; referent 테스트
  (referent '(foo 1) #t)
  (cond ((not (null? (fetch '(foo 1))))
         (display "\n Referent worked okay."))
        (else (error "Referent failed.")))

  ;; 규칙 실행 테스트
  (referent '(bar 1) #t)
  (run-rules)
  (display "\n No errors during attempted rule execution.")

  ;; :INTERN 규칙 발화 테스트
  (cond ((not (null? (fetch '(mumble 1 1))))
         (display "\n:INTERN rule fired okay."))
        (else (error ":INTERN rule failed to fire.")))

  ;; :IN 규칙 조기 발화 방지 테스트
  (referent '(foo a) #t)
  (referent '(bar a) #t)
  (run-rules)
  (when (any (lambda (fact) (in? fact))
             (fetch '(grumble ?p ?q)))
    (display "\nPremature triggering of :IN rule."))

  ;; :IN 규칙 발화 테스트
  (uassume! '(foo a) 'USER)
  (uassume! '(bar a) 'USER)
  (cond ((in? '(grumble a a))
         (display "\n :IN rule worked okay."))
        (else (display "\n:IN rule failed to fire.")))

  ;; JTMS 참조 테스트
  (uassume! '(foo 1) 'USER)
  (uassume! '(bar 1) 'USER)
  (unless (in? '(mumble 1 1))
    (display "\n Reference or JTMS failure."))

  'OKAY)

;;; any: 리스트에서 하나라도 predicate를 만족하면 #t
(define (any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (any pred (cdr lst)))))
