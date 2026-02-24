;;; -*- Mode: Scheme; -*-

;;;; N-Queens rules, JTRE version
;;; Translated from jqrule.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1986 --- 1992 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: jinter.scm, jdata.scm, jrules.scm, jqueens.scm

;;;; 퀸 규칙 등록
;;; 원래 Common Lisp에서는 rule 매크로를 사용했지만,
;;; Scheme에서는 런타임 규칙 등록으로 변환

;;; register-queens-rules: 퀸 캡처 모순 규칙을 등록
;;; 두 퀸이 같은 행이거나 대각선에 있으면 모순(Queens-capture) 발생
(define (register-queens-rules)
  ;; Queens-capture를 모순 노드로 설정
  (contradiction 'Queens-capture *jtre*)

  ;; 규칙: 두 퀸이 서로 공격할 수 있으면 Queens-capture 단언
  ;; (:IN (Queen ?column1 ?row1) :VAR ?Q1)
  ;; (:IN (Queen ?column2 ?row2) :VAR ?Q2
  ;;   :TEST (not (or (= ?column1 ?column2)
  ;;                  (queens-okay? ?column1 ?row1 ?column2 ?row2))))
  ;; => (rassert! Queens-capture (Death ?Q1 ?Q2))
  (insert-rule
   (get-dbclass 'Queen)
   ;; matcher: (Queen ?column1 ?row1) 패턴과 매칭
   (lambda (p)
     (if (and (pair? p) (eq? (car p) 'Queen)
              (pair? (cdr p)) (number? (cadr p))
              (pair? (cddr p)) (number? (caddr p))
              (null? (cdddr p)))
         (values #t (list (cadr p) (caddr p)) #t)  ; ok?, bindings, node?
         (values #f '() #f)))
   ;; body: trigger-node가 IN일 때, 다른 Queen과의 충돌 검사
   (lambda (trigger-node column1 row1)
     (when (in-node? trigger-node)
       (for-each
        (lambda (q2-datum)
          (let ((form (datum-lisp-form q2-datum)))
            (when (and (pair? form) (eq? (car form) 'Queen)
                       (pair? (cdr form)) (number? (cadr form))
                       (pair? (cddr form)) (number? (caddr form))
                       (in-node? (datum-tms-node q2-datum)))
              (let ((column2 (cadr form))
                    (row2 (caddr form)))
                ;; 같은 퀸이 아니고 서로 공격 가능하면 모순 단언
                (when (and (not (= column1 column2))
                           (not (queens-okay? column1 row1 column2 row2)))
                  (assert! 'Queens-capture
                           (list 'Death trigger-node
                                 (datum-tms-node q2-datum))))))))
        (get-candidates '(Queen)))))))
