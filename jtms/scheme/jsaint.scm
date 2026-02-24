;;; -*- Mode: Scheme; -*-

;;;; JSAINT: A rational reconstruction of Slagel's SAINT program
;;; Translated from jsaint.lisp, last edited 1/29/92, by KDF

;;; Copyright (c) 1991 -- 1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, Xerox Corporation
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: jinter.scm, jdata.scm, jrules.scm, jtms.scm, match.scm, simplify.scm

;;;; JSAINT structure
;;;; JSAINT 구조체

;;; jsaint: 기호 적분 솔버
;;; title: 이름
;;; jtre: 관련 JTRE
;;; agenda: 큐에 넣은 하위 문제 리스트
;;; problem: 해결할 원래 문제
;;; solution: 캐시된 답
;;; n-subproblems: 통계 — 하위 문제 수
;;; max-tasks: 자원 한계
;;; debugging: 디버깅 플래그

(define-record-type <jsaint>
  (make-jsaint title jtre agenda problem solution
               n-subproblems max-tasks debugging)
  jsaint?
  (title jsaint-title set-jsaint-title!)
  (jtre jsaint-jtre set-jsaint-jtre!)
  (agenda jsaint-agenda set-jsaint-agenda!)
  (problem jsaint-problem set-jsaint-problem!)
  (solution jsaint-solution set-jsaint-solution!)
  (n-subproblems jsaint-n-subproblems set-jsaint-n-subproblems!)
  (max-tasks jsaint-max-tasks set-jsaint-max-tasks!)
  (debugging jsaint-debugging set-jsaint-debugging!))

(define (print-jsaint a)
  (string-append "<Agenda " (->string (jsaint-title a)) ">"))

;;;; Global variable
;;;; 전역 변수

(define *jsaint* #f)

;;; create-jsaint: 새 JSAINT 인스턴스 생성
(define (create-jsaint title problem . options)
  (let* ((debugging (get-option 'debugging options #f))
         (max-tasks (get-option 'max-tasks options 20))
         (ag (make-jsaint
              title                                        ; title
              (create-jtre (string-append "JTRE of " title)) ; jtre
              '()                                          ; agenda
              problem                                      ; problem
              #f                                           ; solution
              0                                            ; n-subproblems
              (if (integer? max-tasks) max-tasks 20)       ; max-tasks
              debugging)))                                 ; debugging
    (in-jtre (jsaint-jtre ag))
    (change-jtms (jtre-jtms (jsaint-jtre ag))
                 'contradiction-handler jsaint-contradiction-handler)
    (use-jsaint ag)))

;;; get-option: 옵션 리스트에서 키에 해당하는 값을 추출
(define (get-option key options default)
  (cond ((null? options) default)
        ((and (pair? options) (pair? (cdr options))
              (eq? (car options) key))
         (cadr options))
        ((pair? (cdr options))
         (get-option key (cddr options) default))
        (else default)))

;;; debugging-jsaint: 디버깅 모드일 때만 메시지 출력
(define (debugging-jsaint js msg . args)
  (when (jsaint-debugging js)
    (display msg)
    (for-each display args)
    (newline)))

;;; change-jsaint: JSAINT 설정 변경
(define (change-jsaint js . options)
  (let ((debugging (get-option 'debugging options 'nada))
        (problem (get-option 'problem options 'nada))
        (max-tasks (get-option 'max-tasks options 'nada)))
    (unless (eq? debugging 'nada) (set-jsaint-debugging! js debugging))
    (unless (eq? problem 'nada) (set-jsaint-problem! js problem))
    (unless (eq? max-tasks 'nada) (set-jsaint-max-tasks! js max-tasks))))

;;; use-jsaint: 현재 JSAINT를 설정
(define (use-jsaint js) (set! *jsaint* js) js)

;;;; User entry point
;;;; 사용자 진입점

;;; solve-integral: 적분 문제를 해결
(define (solve-integral integral . options)
  (let* ((title (get-option 'title options (symbol->string (gensym "G"))))
         (debugging (get-option 'debugging options #f))
         (max-tasks (get-option 'max-tasks options 20)))
    ;; 입력을 단순화하고 정규화
    (set! integral (simplify integral))
    (use-jsaint (create-jsaint title integral
                               'debugging debugging
                               'max-tasks max-tasks))
    (queue-problem (jsaint-problem *jsaint*) #f)
    ;; 규칙과 연산자 로드
    (with-jtre (jsaint-jtre *jsaint*)
               (lambda ()
                 (register-jsaint-rules)
                 (register-integration-operators)))
    (run-jsaint *jsaint*)))

;;;; Basic algorithm
;;;; 기본 알고리즘

;;; run-jsaint: JSAINT 메인 루프
(define (run-jsaint js)
  (set! *jsaint* js)
  (cond
   ;; 이미 해결됨 — 재풀기 방지
   ((jsaint-solution *jsaint*)
    (values (jsaint-solution *jsaint*) *jsaint*))
   ;; 자원 한계 초과
   ((> (jsaint-n-subproblems *jsaint*)
       (jsaint-max-tasks *jsaint*))
    (values 'time-out *jsaint*))
   (else
    (let ((failure-signal (list 'Failed
                                (list 'Integrate (jsaint-problem *jsaint*)))))
      (let loop ()
        (let ((solution (fetch-solution (jsaint-problem *jsaint*) *jsaint*)))
          (cond
           (solution
            (set-jsaint-solution! *jsaint* solution)
            (debugging-jsaint *jsaint*
                              "\n " (->string (jsaint-title *jsaint*))
                              ": Solved original problem.")
            (values solution *jsaint*))
           ((in? failure-signal (jsaint-jtre *jsaint*))
            (debugging-jsaint *jsaint*
                              "\n " (->string (jsaint-title *jsaint*))
                              ": Failed on original problem.")
            (set-jsaint-solution! *jsaint* 'failed-problem)
            (values 'failed-problem *jsaint*))
           ((null? (jsaint-agenda *jsaint*))
            (debugging-jsaint *jsaint*
                              "\n " (->string (jsaint-title *jsaint*))
                              ": Agenda empty.")
            (set-jsaint-solution! *jsaint* 'failed-empty)
            (values 'failed-empty *jsaint*))
           (else
            (let ((entry (car (jsaint-agenda *jsaint*))))
              (set-jsaint-agenda! *jsaint* (cdr (jsaint-agenda *jsaint*)))
              (process-subproblem (cdr entry)))
            (loop)))))))))

;;; process-subproblem: 하위 문제를 처리
(define (process-subproblem item)
  (let ((jtre (jsaint-jtre *jsaint*)))
    (debugging-jsaint *jsaint* "\n  Trying to solve " (->string item) ".")
    (open-subproblem item)
    (cond
     ;; 이미 해결됨
     ((fetch-solution item *jsaint*)
      (debugging-jsaint *jsaint* "\n    ..already solved.")
      #t)
     ;; 이미 확장됨
     ((any-in? (lambda (f) (in? f jtre))
               (fetch (list 'AND-SUBGOALS item '?subproblems) jtre))
      (debugging-jsaint *jsaint* "\n   ..already expanded.")
      #t)
     (else
      (let ((suggestions '()))
        (for-each
         (lambda (suggestion)
           (when (in? suggestion jtre)
             (queue-problem (list 'try (caddr suggestion)) item)
             (set! suggestions
                   (cons (list 'try (caddr suggestion)) suggestions))))
         (fetch (list 'SUGGEST-FOR item '?operator) jtre))
        ;; OR-SUBGOALS 단언
        (assert! (list 'OR-SUBGOALS item suggestions) 'OR-SUBGOALS jtre)
        (run-rules jtre))))))

;;; open-subproblem: 하위 문제를 개방
(define (open-subproblem item)
  (let ((jtre (jsaint-jtre *jsaint*)))
    (assert! (list 'expanded item) 'EXPAND-AGENDA-ITEM jtre)
    (assume! (list 'open item) 'EXPAND-AGENDA-ITEM jtre)
    (run-rules jtre)))

;;;; Queuing problems
;;;; 문제 큐잉

;;; queue-problem: 문제를 난이도 순으로 큐에 추가
(define (queue-problem problem parent)
  (let ((entry (cons (estimate-difficulty problem) problem)))
    (debugging-jsaint *jsaint*
                      "\n   Queueing " (->string problem)
                      ", difficulty = " (number->string (car entry)))
    (set-jsaint-agenda! *jsaint*
                        (merge-by-car (list entry) (jsaint-agenda *jsaint*)))))

;;; merge-by-car: car 값 기준으로 두 정렬된 리스트를 병합
(define (merge-by-car l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((< (caar l1) (caar l2))
         (cons (car l1) (merge-by-car (cdr l1) l2)))
        (else
         (cons (car l2) (merge-by-car l1 (cdr l2))))))

;;; estimate-difficulty: 문제의 난이도를 추정
(define (estimate-difficulty problem)
  (+ (max-depth problem) (count-symbols problem)))

;;; count-symbols: 식의 심볼 수를 셈
(define (count-symbols pr)
  (cond ((null? pr) 0)
        ((pair? pr)
         (apply + (map count-symbols pr)))
        (else 1)))

;;; max-depth: 식의 최대 깊이를 계산
(define (max-depth pr)
  (cond ((not (pair? pr)) 1)
        (else (+ 1 (apply max (map max-depth pr))))))

;;;; Auxiliary routines
;;;; 보조 루틴

;;; fetch-solution: 문제에 대한 해를 검색
(define (fetch-solution problem . args)
  (let* ((*jsaint* (if (null? args) *jsaint* (car args)))
         (jtre (jsaint-jtre *jsaint*)))
    (call-with-current-continuation
     (lambda (return)
       (for-each
        (lambda (solution)
          (when (in? solution jtre)
            (return (caddr solution))))
        (fetch (list 'SOLUTION-OF problem '?answer) jtre))
       #f))))

;;; jsaint-contradiction-handler: 모순 처리 핸들러
(define (jsaint-contradiction-handler jtms contradictions)
  (ask-user-handler jtms contradictions))

;;;; Explain result
;;;; 결과 설명

(define (explain-result . args)
  (let ((*jsaint* (if (null? args) *jsaint* (car args))))
    (cond ((not (jsaint-solution *jsaint*))
           (display "\n Problem not solved yet."))
          ((eq? (jsaint-solution *jsaint*) 'failed-problem)
           (display "\n Failed to find a solution."))
          ((eq? (jsaint-solution *jsaint*) 'failed-empty)
           (display "\n Ran out of things to do."))
          (else
           (display "\n Solved the problem: ")
           (display (jsaint-solution *jsaint*))))))

;;;; Helpers
;;;; 도우미

;;; any-in?: 리스트에서 하나라도 predicate를 만족하면 #t
(define (any-in? pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (any-in? pred (cdr lst)))))

;;; gensym: 고유 심볼 생성
(define *gensym-counter* 0)
(define (gensym . args)
  (let ((prefix (if (null? args) "G" (car args))))
    (set! *gensym-counter* (+ *gensym-counter* 1))
    (string->symbol
     (string-append prefix (number->string *gensym-counter*)))))

;;;; Debugging
;;;; 디버깅

(define (try-jsaint problem . args)
  (let ((title (if (null? args) "JSAINT Test" (car args))))
    (solve-integral problem 'debugging #t 'title title)))

;;; 예제 문제
(define problem1 '(Integrate (Integral 1 x)))
(define problem2 '(Integrate (Integral (+ x 5) x)))
(define problem3 '(Integrate (Integral (* 46 (log x %e)) x)))
