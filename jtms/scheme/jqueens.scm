;;; -*- Mode: Scheme; -*-

;;;; Example of dependency-directed search using JTRE
;;; Translated from jqueens.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1986--1992 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: jinter.scm, jdata.scm, jrules.scm, jtms.scm

;;;; Statistics
;;;; 통계

(define *n-assumptions* 0)
(define *placements* '())

;;;; Queens puzzle setup and search
;;;; 퀸 퍼즐 설정 및 탐색

;;; test-queens: n-queens 퍼즐을 from부터 to까지 테스트
(define (test-queens from to)
  (let loop ((n from))
    (when (<= n to)
      (n-queens n)
      (display (string-append "\n For n=" (number->string n)
                              ", " (number->string (length *placements*))
                              " solutions, "
                              (number->string *n-assumptions*)
                              " assumptions."))
      (loop (+ n 1)))))

;;; n-queens: n-queens 퍼즐을 풀고 해의 수를 반환
(define (n-queens n . args)
  (let ((debugging? (if (null? args) #f (car args))))
    (setup-queens-puzzle n debugging?)
    (solve-queens-puzzle (make-queens-choice-sets n))
    (length *placements*)))

;;; setup-queens-puzzle: 퍼즐 환경을 초기화
(define (setup-queens-puzzle n . args)
  (let ((debugging? (if (null? args) #f (car args))))
    (in-jtre (create-jtre (string-append (number->string n) "-Queens JTRE")
                          'debugging debugging?))
    (set! *placements* '())
    (set! *n-assumptions* 0)
    ;; 퀸 규칙 등록
    (register-queens-rules)))

;;; make-queens-choice-sets: 각 열에 대한 선택지 집합 생성
;;; 각 열의 각 행에 퀸을 놓을 수 있는 선택지를 생성
(define (make-queens-choice-sets n)
  (let loop ((column 1) (choice-sets '()))
    (if (> column n)
        (reverse choice-sets)
        (let inner ((row n) (column-queens '()))
          (if (< row 1)
              (loop (+ column 1) (cons column-queens choice-sets))
              (inner (- row 1)
                     (cons (list 'Queen column row) column-queens)))))))

;;; solve-queens-puzzle: 선택지 집합을 사용하여 퍼즐을 풂
;;; 의존성 지향 탐색(dependency-directed search) 사용
(define (solve-queens-puzzle choice-sets)
  (cond ((null? choice-sets)
         (gather-queens-solution))
        (else
         (for-each
          (lambda (choice)
            (unless (in? (list 'not choice) *jtre*)
              ;; nogood 정보를 존중
              (call-with-values
                (lambda ()
                  (try-in-context choice
                                  (lambda ()
                                    (solve-queens-puzzle (cdr choice-sets)))
                                  *jtre*))
                (lambda (nogood? asns)
                  (set! *n-assumptions* (+ *n-assumptions* 1))
                  (when nogood?
                    ;; 이 가정이 실패했으므로, 다른 관련 가정들에 기반하여
                    ;; 부정을 정당화
                    (assert! (list 'not choice)
                             (cons 'Nogood
                                   (remove choice asns))))))))
          (car choice-sets)))))

;;;; JTMS approximation to try-in-context
;;;; try-in-context의 JTMS 근사

;;; try-in-context: 가정을 컨텍스트 내에서 시도
;;; 모순이 발생하면 관련 가정들을 반환
(define (try-in-context asn thunk jtre)
  (let ((try-marker (cons 'TRY asn))
        (result #f))
    (call-with-current-continuation
     (lambda (return)
       (with-contradiction-handler
        (jtre-jtms jtre)
        (lambda (jtms contras)
          (try-contradiction-handler contras jtms asn try-marker jtre return))
        (lambda ()
          (dynamic-wind
            (lambda () #f)
            (lambda ()
              (unless (in? asn jtre)
                ;; assume! 시도
                (set! result (assume! asn try-marker jtre))
                (run-rules jtre)
                ;; thunk 실행 (검색 계속)
                (thunk)
                ;; 성공적으로 완료 — 가정을 철회하고 반환
                (retract! asn try-marker #t)
                (return (values #f #f))))
            (lambda () #f))))))))

;;; try-contradiction-handler: 모순 발생 시 처리
(define (try-contradiction-handler contras jtms asn marker jtre return)
  (when (and contras asn)
    (let ((node (get-tms-node asn)))
      (for-each
       (lambda (cnode)
         (let ((asns (assumptions-of-node cnode)))
           (when (memq node asns)
             (retract! asn marker #t)
             (return (values #t (map view-node asns))))))
       contras))))

;;;; Helper functions
;;;; 도우미 함수

;;; queens-okay?: 두 퀸이 서로 공격하지 않는지 확인
(define (queens-okay? x1 y1 x2 y2)
  (not (or (= y1 y2)
           (= (abs (- x1 x2)) (abs (- y1 y2))))))

;;; gather-queens-solution: 현재 IN인 퀸 배치를 해로 수집
(define (gather-queens-solution)
  (let ((solution (filter (lambda (q) (not (out? q *jtre*)))
                          (fetch '(Queen ?c ?r) *jtre*))))
    (set! *placements* (cons solution *placements*))))

;;; show-queens-solution: 퀸 배치를 시각적으로 표시
(define (show-queens-solution solution)
  (let ((n (length solution)))
    (let loop-i ((i 0))
      (when (< i n)
        (newline)
        (let loop-j ((j 0))
          (when (< j n)
            (display (if (member (list 'Queen i j) solution)
                         "Q" "-"))
            (loop-j (+ j 1))))
        (loop-i (+ i 1))))))

;;; remove: 리스트에서 요소를 제거
(define (remove item lst)
  (filter (lambda (x) (not (equal? x item))) lst))

;;; filter: 리스트에서 조건을 만족하는 요소만 선택
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))
