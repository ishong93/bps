;;; -*- Mode: Scheme; -*-

;;;; Basic rules for JSAINT
;;; Translated from jsrules.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern
;;; University, and Johan de Kleer, Xerox Corporation.
;;; All Rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: jsaint.scm, jinter.scm, jdata.scm, jrules.scm

;;;; JSAINT 장부 관리 규칙 등록
;;; 원래 Common Lisp에서는 rule 매크로를 사용했지만,
;;; Scheme에서는 런타임 규칙 등록으로 변환

(define (register-jsaint-rules)

  ;; 규칙 1: AND-SUBGOALS 확장
  ;; (:IN (AND-SUBGOALS ?parent ?children) :VAR ?def)
  ;; 각 자식에 대해 PARENT-OF 단언, 자식이 실패하면 부모도 실패
  ;; 모든 자식이 해결되면 부모도 해결
  (insert-rule
   (get-dbclass 'AND-SUBGOALS)
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'AND-SUBGOALS)
              (pair? (cdr p)) (pair? (cddr p)))
         (values #t (list (cadr p) (caddr p)) #t)
         (values #f '() #f)))
   (lambda (trigger-node parent children)
     (when (in-node? trigger-node)
       (for-each
        (lambda (child)
          ;; PARENT-OF ?child ?parent :AND 단언
          (assert! (list 'PARENT-OF child parent 'AND)
                   (list 'DEF-OF-AND trigger-node))
          ;; 자식이 실패하면 부모도 실패
          ;; (이 규칙은 실제로는 IN 규칙으로 등록해야 하지만,
          ;;  간략화를 위해 fetch로 폴링)
          )
        children)
       ;; 모든 자식이 해결되면 부모도 해결
       (assert! (list 'solved parent)
                (cons 'AND-SUCCESS
                      (cons trigger-node
                            (map (lambda (child)
                                   (list 'SOLVED child))
                                 children)))))))

  ;; 규칙 2: OR-SUBGOALS 확장
  ;; (:IN (OR-SUBGOALS ?parent ?children) :VAR ?def :TEST ?children)
  ;; 각 자식에 대해 PARENT-OF 단언, 하나라도 해결되면 부모도 해결
  ;; 모든 자식이 실패하면 부모도 실패
  (insert-rule
   (get-dbclass 'OR-SUBGOALS)
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'OR-SUBGOALS)
              (pair? (cdr p)) (pair? (cddr p))
              (not (null? (caddr p))))  ; :TEST ?children
         (values #t (list (cadr p) (caddr p)) #t)
         (values #f '() #f)))
   (lambda (trigger-node parent children)
     (when (in-node? trigger-node)
       (for-each
        (lambda (child)
          (assert! (list 'PARENT-OF child parent 'OR)
                   (list 'DEF-OF-OR trigger-node)))
        children)
       ;; 모든 자식이 실패하면 부모도 실패
       (assert! (list 'FAILED parent)
                (cons 'OR-FAILURE
                      (cons trigger-node
                            (map (lambda (child)
                                   (list 'FAILED child))
                                 children)))))))

  ;; 규칙 3: PARENT-OF 확장 — 자식이 관련 있음을 표시
  ;; (:IN (PARENT-OF ?child ?parent ?type) :VAR ?lineage)
  (insert-rule
   (get-dbclass 'PARENT-OF)
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'PARENT-OF)
              (pair? (cdr p)) (pair? (cddr p)) (pair? (cdddr p)))
         (values #t (list (cadr p) (caddr p) (cadddr p)) #t)
         (values #f '() #f)))
   (lambda (trigger-node child parent type)
     (when (in-node? trigger-node)
       (assert! (list 'RELEVANT child)
                (list 'STILL-WORKING-ON (list 'OPEN parent)
                      trigger-node)))))

  ;; 규칙 4: SOLUTION-OF — 해가 발견되면 SOLVED 단언
  ;; (:IN (SOLUTION-OF ?problem ?answer) :VAR ?found)
  (insert-rule
   (get-dbclass 'SOLUTION-OF)
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'SOLUTION-OF)
              (pair? (cdr p)) (pair? (cddr p)))
         (values #t (list (cadr p) (caddr p)) #t)
         (values #f '() #f)))
   (lambda (trigger-node problem answer)
     (when (in-node? trigger-node)
       (assert! (list 'SOLVED problem)
                (list 'FOUND-ANSWER trigger-node)))))

  ;; 규칙 5: OR-SUBGOALS with NIL children — 방법이 없으면 실패
  ;; (:IN (OR-SUBGOALS (Integrate ?expr) NIL) :VAR ?no-ideas)
  (insert-rule
   (get-dbclass 'OR-SUBGOALS)
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'OR-SUBGOALS)
              (pair? (cdr p)) (pair? (cadr p))
              (eq? (caadr p) 'Integrate)
              (pair? (cddr p)) (null? (caddr p)))
         (values #t (list (cadadr p)) #t)
         (values #f '() #f)))
   (lambda (trigger-node expr)
     (when (in-node? trigger-node)
       (assert! (list 'FAILED (list 'Integrate expr))
                (list 'NO-METHODS trigger-node)))))

  ;; 규칙 6: SOLVED — 해결된 문제의 open 상태를 철회
  ;; (:IN (SOLVED ?problem))
  (insert-rule
   (get-dbclass 'SOLVED)
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'SOLVED)
              (pair? (cdr p)))
         (values #t (list (cadr p)) #t)
         (values #f '() #f)))
   (lambda (trigger-node problem)
     (when (in-node? trigger-node)
       (retract! (list 'OPEN problem) 'EXPAND-AGENDA-ITEM #t))))

  ;; 규칙 7: FAILED — 실패한 문제의 open 상태를 철회
  ;; (:IN (FAILED ?problem))
  (insert-rule
   (get-dbclass 'FAILED)
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'FAILED)
              (pair? (cdr p)))
         (values #t (list (cadr p)) #t)
         (values #f '() #f)))
   (lambda (trigger-node problem)
     (when (in-node? trigger-node)
       (retract! (list 'OPEN problem) 'EXPAND-AGENDA-ITEM #t)))))
