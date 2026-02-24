;;; -*- Mode: Scheme; -*-

;;;; Examples for Justification-based TMS
;;; Translated from jtms-ex.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: jtms.scm

;;;; Helper functions
;;;; 도우미 함수

;;; get-node: datum으로 노드를 검색
(define (get-node datum jtms)
  (let loop ((nodes (jtms-nodes jtms)))
    (cond ((null? nodes) #f)
          ((equal? datum (tms-node-datum (car nodes)))
           (car nodes))
          (else (loop (cdr nodes))))))

;;; get-justification: 번호로 정당화를 검색
(define (get-justification num jtms)
  (let loop ((justs (jtms-justs jtms)))
    (cond ((null? justs) #f)
          ((= num (just-index (car justs)))
           (car justs))
          (else (loop (cdr justs))))))

;;;; Global variables for examples
;;;; 예제용 전역 변수

(define na #f)
(define nb #f)
(define nc #f)
(define nd #f)
(define ne #f)
(define nf #f)
(define ng #f)
(define contra #f)
(define *jtms* #f)

;;;; Example 1: Simple JTMS example
;;;; 예제 1: 간단한 JTMS 예제
;;; 7개의 가정 노드(a~g)와 4개의 정당화를 생성
;;; j1: a,b → f   j2: b,c → e   j3: a,e → g   j4: d,e → g
;;; 가정 a,b,c,d를 활성화하면 e,f,g가 자동으로 IN이 됨

(define (ex1)
  (set! *jtms* (create-jtms "Simple Example" 'debugging #t))
  (set! na (tms-create-node *jtms* 'a 'assumptionp #t))
  (set! nb (tms-create-node *jtms* 'b 'assumptionp #t))
  (set! nc (tms-create-node *jtms* 'c 'assumptionp #t))
  (set! nd (tms-create-node *jtms* 'd 'assumptionp #t))
  (set! ne (tms-create-node *jtms* 'e 'assumptionp #t))
  (set! nf (tms-create-node *jtms* 'f 'assumptionp #t))
  (set! ng (tms-create-node *jtms* 'g 'assumptionp #t))
  (justify-node 'j1 nf (list na nb))
  (justify-node 'j2 ne (list nb nc))
  (justify-node 'j3 ng (list na ne))
  (justify-node 'j4 ng (list nd ne))
  (enable-assumption na)
  (enable-assumption nb)
  (enable-assumption nc)
  (enable-assumption nd))

;;;; Example 2: Contradiction test
;;;; 예제 2: 모순 테스트
;;; ex1 이후에 실행. 모순 노드를 추가하여 모순 처리 테스트
;;; j5: e,f → contra (모순)

(define (ex2)
  (set! contra (tms-create-node *jtms* 'Loser 'contradictoryp #t))
  (justify-node 'j5 contra (list ne nf)))

;;;; Example 3: Multiple support example
;;;; 예제 3: 다중 지지 예제
;;; 가정 A,C,E를 생성하고 활성화
;;; R1: C,E → h   R2: A,C → g   R3: g → contradiction
;;; A와 C가 모두 IN이면 g가 IN이 되고, 모순이 발생

(define assumption-a #f)
(define assumption-c #f)
(define assumption-e #f)
(define node-h #f)
(define node-g #f)
(define contradiction-node #f)

(define (ex3)
  (set! *jtms* (create-jtms "Multiple support example"))
  (set! assumption-a (tms-create-node *jtms* 'A 'assumptionp #t))
  (set! assumption-c (tms-create-node *jtms* 'C 'assumptionp #t))
  (set! assumption-e (tms-create-node *jtms* 'E 'assumptionp #t))
  (set! node-h (tms-create-node *jtms* 'h))
  (enable-assumption assumption-a)
  (enable-assumption assumption-c)
  (enable-assumption assumption-e)
  (justify-node 'R1 node-h (list assumption-c assumption-e))
  (set! node-g (tms-create-node *jtms* 'g))
  (justify-node 'R2 node-g (list assumption-a assumption-c))
  (set! contradiction-node
        (tms-create-node *jtms* 'CONTRADICTION 'contradictoryp #t))
  (justify-node 'R3 contradiction-node (list node-g)))
