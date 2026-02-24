;;; -*- Mode: Scheme; -*-

;;;; Operators for JSAINT
;;; Translated from jsops.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: jsaint.scm, jinter.scm, jdata.scm, jrules.scm, simplify.scm, match.scm

;;;; Integration operators
;;;; 적분 연산자

;;; 각 적분 연산자는 다음으로 정의됩니다:
;;; - name: 연산자 이름
;;; - trigger: 매칭할 패턴 (Integral <expr> <var>)
;;; - test: 선택적 조건 (조건이 맞아야 적용)
;;; - subproblems: 하위 문제 리스트 (없을 수 있음)
;;; - result: 결과 패턴

;;; Integration operator record
(define-record-type <integration-op>
  (make-integration-op name trigger test subproblems result)
  integration-op?
  (name integration-op-name)
  (trigger integration-op-trigger)
  (test integration-op-test)
  (subproblems integration-op-subproblems)
  (result integration-op-result))

;;; register-integration-operators: 모든 적분 연산자를 등록
(define (register-integration-operators)
  (for-each register-one-operator (standard-integration-ops)))

;;; register-one-operator: 하나의 적분 연산자를 JTRE 규칙으로 등록
(define (register-one-operator op)
  (let ((name (integration-op-name op))
        (trigger (integration-op-trigger op))
        (test (integration-op-test op))
        (subproblems (integration-op-subproblems op))
        (result (integration-op-result op)))

    ;; expanded (Integrate <trigger>) 패턴에 대한 규칙 등록
    (insert-rule
     (get-dbclass 'expanded)
     ;; matcher: (expanded (Integrate <trigger>)) 패턴과 매칭
     (lambda (p)
       (if (and (pair? p) (eq? (car p) 'expanded)
                (pair? (cdr p)) (pair? (cadr p))
                (eq? (caadr p) 'Integrate))
           (let ((bindings (match trigger (cadadr p))))
             (if (and (not (eq? bindings 'fail))
                      (or (not test)
                          (eval (substitute-in test bindings))))
                 (values #t (list (cadr p) bindings) #t)
                 (values #f '() #f)))
           (values #f '() #f)))
     ;; body
     (lambda (trigger-node integral bindings)
       (when (in-node? trigger-node)
         (let* ((problem (list 'Integrate (substitute-in trigger bindings)))
                (op-instance (list name (substitute-in trigger bindings))))
           ;; Operator-Instance 단언
           (assert! (list 'Operator-Instance op-instance)
                    'OP-INSTANCE-DEFINITION)

           (cond
            ;; 하위 문제가 없으면 바로 해 생성
            ((null? subproblems)
             (let ((solution (simplify (substitute-in result bindings))))
               (assert! (list 'SOLUTION-OF problem solution)
                        (list (string->symbol
                               (string-append (symbol->string name) "-result"))
                              (list 'Operator-Instance op-instance)))))

            ;; 하위 문제가 있는 경우
            (else
             ;; 제안 단언
             (assert! (list 'SUGGEST-FOR problem op-instance)
                      (list 'INTOPEXPANDER trigger-node))

             ;; try 규칙 등록
             (insert-rule
              (get-dbclass 'expanded)
              (lambda (p2)
                (if (and (pair? p2) (eq? (car p2) 'expanded)
                         (pair? (cdr p2)) (pair? (cadr p2))
                         (eq? (caadr p2) 'try)
                         (equal? (cadadr p2) op-instance))
                    (values #t '() #t)
                    (values #f '() #f)))
              (lambda (try-node)
                (when (in-node? try-node)
                  ;; 하위 문제들을 큐에 추가
                  (let ((sub-goals
                         (map (lambda (sub)
                                (let ((goal (simplify
                                             (substitute-in (cadr sub)
                                                            bindings))))
                                  (queue-problem goal problem)
                                  goal))
                              subproblems)))
                    ;; AND-SUBGOALS 단언
                    (assert! (list 'AND-SUBGOALS
                                   (list 'try op-instance)
                                   sub-goals)
                             (list (string->symbol
                                    (string-append
                                     (symbol->string name) "-DEF"))
                                   try-node))))))))))))))

;;;; Standard integration operators
;;;; 표준 적분 연산자

(define (standard-integration-ops)
  (list
   ;; Integral-of-Constant: ∫c dx = c*x (c에 var가 없을 때)
   (make-integration-op
    'Integral-of-Constant
    '(Integral (? t) (? var))
    '(not (occurs-in? (? var) (? t)))
    '()
    '(* (? t) (? var)))

   ;; Integral-of-Self: ∫x dx = x²/2
   (make-integration-op
    'Integral-of-Self
    '(Integral (? exp) (? exp))
    #f
    '()
    '(/ (expt (? exp) 2) 2))

   ;; Move-Constant-outside: ∫c*f(x) dx = c * ∫f(x) dx
   (make-integration-op
    'Move-Constant-outside
    '(Integral (* (? const) (? nonconst)) (? var))
    '(and (not (occurs-in? (? var) (? const)))
          (occurs-in? (? var) (? nonconst)))
    '(((? int) (Integrate (Integral (? nonconst) (? var)))))
    '(* (? const) (? int)))

   ;; Integral-of-Sum: ∫(t1+t2) dx = ∫t1 dx + ∫t2 dx
   (make-integration-op
    'Integral-of-Sum
    '(Integral (+ (? t1) (? t2)) (? var))
    #f
    '(((? int1) (Integrate (Integral (? t1) (? var))))
      ((? int2) (Integrate (Integral (? t2) (? var)))))
    '(+ (? int1) (? int2)))

   ;; Integral-of-uminus: ∫(-t) dx = -∫t dx
   (make-integration-op
    'Integral-of-uminus
    '(Integral (- (? term)) (? var))
    #f
    '(((? int) (Integrate (Integral (? term) (? var)))))
    '(- (? int)))

   ;; Integral-of-minus: ∫(t1-t2) dx = ∫t1 dx - ∫t2 dx
   (make-integration-op
    'Integral-of-minus
    '(Integral (- (? t1) (? t2)) (? var))
    #f
    '(((? int1) (Integrate (Integral (? t1) (? var))))
      ((? int2) (Integrate (Integral (? t2) (? var)))))
    '(- (? int1) (? int2)))

   ;; Integral-of-SQR: ∫x² dx = x³/3
   (make-integration-op
    'Integral-of-SQR
    '(Integral (sqr (? var)) (? var))
    #f
    '()
    '(/ (expt (? var) 3) 3))

   ;; Integral-of-polyterm: ∫x^n dx = x^(n+1)/(n+1) (n≠-1)
   (make-integration-op
    'Integral-of-polyterm
    '(Integral (expt (? var) (? n)) (? var))
    '(not (same-constant? (? n) -1))
    '()
    '(/ (expt (? var) (+ 1 (? n))) (+ 1 (? n))))

   ;; Simple-e-integral: ∫e^x dx = e^x
   (make-integration-op
    'Simple-e-integral
    '(Integral (expt %e (? var)) (? var))
    #f
    '()
    '(expt %e (? var)))

   ;; e-integral: ∫e^(ax) dx = e^(ax)/a
   (make-integration-op
    'e-integral
    '(Integral (expt %e (* (? a) (? var))) (? var))
    '(not (occurs-in? (? var) (? a)))
    '()
    '(/ (expt %e (* (? a) (? var))) (? a)))

   ;; non-e-power-integral: ∫b^(ax) dx = b^(ax)/(a*ln(b))
   (make-integration-op
    'non-e-power-integral
    '(Integral (expt (? b) (* (? a) (? var))) (? var))
    '(and (not (occurs-in? (? var) (? a)))
          (not (occurs-in? (? var) (? b))))
    '()
    '(/ (expt (? b) (* (? a) (? var))) (* (? a) (log (? b) %e))))

   ;; Log-Integral: ∫ln(x) dx = x*ln(x) - x
   (make-integration-op
    'Log-Integral
    '(Integral (log (? var) %e) (? var))
    #f
    '()
    '(- (* (? var) (log (? var) %e)) (? var)))

   ;; sin-integral: ∫sin(ax) dx = -cos(ax)/a
   (make-integration-op
    'sin-integral
    '(Integral (sin (* (? a) (? var))) (? var))
    '(not (occurs-in? (? var) (? a)))
    '()
    '(- (/ (cos (* (? a) (? var))) (? a))))

   ;; cos-integral: ∫cos(ax) dx = sin(ax)/a
   (make-integration-op
    'cos-integral
    '(Integral (cos (* (? a) (? var))) (? var))
    '(not (occurs-in? (? var) (? a)))
    '()
    '(/ (sin (* (? a) (? var))) (? a)))

   ;; sin-sqr-integral: ∫sin²(x) dx = x/2 - sin(2x)/4
   (make-integration-op
    'sin-sqr-integral
    '(Integral (sqr (sin (? var))) (? var))
    #f
    '()
    '(- (/ (? var) 2) (/ (sin (* 2 (? var))) 4)))

   ;; cos-sqr-integral: ∫cos²(x) dx = x/2 + sin(2x)/4
   (make-integration-op
    'cos-sqr-integral
    '(Integral (sqr (cos (? var))) (? var))
    #f
    '()
    '(+ (/ (? var) 2) (/ (sin (* 2 (? var))) 4)))
   ))
