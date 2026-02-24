;;; -*- Mode: Scheme; -*-

;;;; JTRE definitions and interface
;;; Translated from jinter.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1989 -- 1992 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires SRFI-9 (define-record-type) and SRFI-69 (hash-tables)

;;;; JTRE structure
;;; title: JTRE의 이름
;;; jtms: JTMS에 대한 포인터
;;; dbclass-table: dbclass들의 해시 테이블
;;; datum-counter: datum 고유 번호 생성기
;;; rule-counter: 규칙 고유 번호 생성기
;;; debugging: 디버깅 출력 활성화 플래그
;;; queue: 규칙 실행 큐
;;; rules-run: 실행된 규칙 수 통계

(define-record-type <jtre>
  (make-jtre title jtms dbclass-table datum-counter
             rule-counter debugging queue rules-run)
  jtre?
  (title jtre-title set-jtre-title!)                         ; Pretty name
  (jtms jtre-jtms set-jtre-jtms!)                           ; Pointer to its JTMS
  (dbclass-table jtre-dbclass-table set-jtre-dbclass-table!) ; Table of dbclasses
  (datum-counter jtre-datum-counter set-jtre-datum-counter!) ; Unique ID for asserts
  (rule-counter jtre-rule-counter set-jtre-rule-counter!)   ; Unique ID for rules
  (debugging jtre-debugging set-jtre-debugging!)             ; If non-#f, show basic operations
  (queue jtre-queue set-jtre-queue!)                         ; Rule queue
  (rules-run jtre-rules-run set-jtre-rules-run!))           ; Statistic

(define (print-jtre j)
  (string-append "<JTRE: " (->string (jtre-title j)) ">"))

;;;; Helper: convert anything to string

(define (->string x)
  (cond ((string? x) x)
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((boolean? x) (if x "#t" "#f"))
        ((null? x) "()")
        ((pair? x)
         (string-append "(" (->string-list x) ")"))
        ((jtre? x) (print-jtre x))
        (else (let ((p (open-output-string)))
                (write x p)
                (get-output-string p)))))

(define (->string-list lst)
  (cond ((null? lst) "")
        ((not (pair? lst)) (string-append ". " (->string lst)))
        ((null? (cdr lst)) (->string (car lst)))
        (else (string-append (->string (car lst)) " "
                             (->string-list (cdr lst))))))

;;;; Default JTRE
;;; *jtre*: 현재 활성화된 JTRE 인스턴스를 가리키는 전역 변수

(define *jtre* #f)

;;; with-jtre: 주어진 jtre 환경에서 thunk를 실행
;;; dynamic-wind를 사용하여 이전 *jtre*를 보존하고 복원
(define (with-jtre jtre thunk)
  (let ((old-jtre *jtre*))
    (dynamic-wind
      (lambda () (set! *jtre* jtre))
      thunk
      (lambda () (set! *jtre* old-jtre)))))

;;; in-jtre: 현재 활성 JTRE를 설정
(define (in-jtre jtre) (set! *jtre* jtre))

;;; debugging-jtre: 디버깅 모드일 때만 메시지 출력
(define (debugging-jtre msg . args)
  (when (jtre-debugging *jtre*)
    (display msg)
    (for-each display args)
    (newline)))

;;;; JTRE 생성 및 설정

;;; create-jtre: 새로운 JTRE 인스턴스를 생성
;;; title: JTRE의 이름
;;; debugging: 디버깅 모드 활성화 여부 (선택적)
(define (create-jtre title . options)
  (let* ((debugging (if (and (not (null? options))
                             (memq 'debugging options))
                        (cadr (memq 'debugging options))
                        #f))
         (j (make-jtre title                          ; title
                       (create-jtms (list 'JTMS-OF title) ; jtms
                                    'view-node)
                       (make-hash-table)              ; dbclass-table
                       0                              ; datum-counter
                       0                              ; rule-counter
                       debugging                      ; debugging
                       '()                            ; queue
                       0)))                           ; rules-run
    (change-jtms (jtre-jtms j)
                 'enqueue-procedure
                 (lambda (rule) (enqueue rule j)))
    j))

;;; change-jtre: 기존 JTRE의 설정 변경
(define (change-jtre jtre . options)
  (when (and (not (null? options))
             (memq 'debugging options))
    (set-jtre-debugging! jtre (cadr (memq 'debugging options)))))

;;;; Running JTRE
;;;; JTRE 실행

;;; uassert!: 사실을 단언하고 규칙을 실행
(define (uassert! fact . args)
  (let ((just (if (null? args) 'user (car args))))
    (assert! fact just)
    (run-rules *jtre*)))

;;; uassume!: 사실을 가정하고 규칙을 실행
(define (uassume! fact reason)
  (assume! fact reason *jtre*)
  (run-rules *jtre*))

;;; run-forms: 폼 리스트를 평가하고 각각 후에 규칙 실행
(define (run-forms forms . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (for-each (lambda (form)
                (eval form)
                (run-rules *jtre*))
              forms)))

;;; run: 대화형 최상위 드라이버 함수
(define (run . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (display "\n>>")
    (let loop ((form (read)))
      (cond ((memq form '(quit stop exit abort)) #f)
            (else
             (display "\n")
             (display (eval form))
             (run-rules)
             (display "\n>>")
             (loop (read)))))))

;;; show: JTRE의 데이터와 규칙을 표시
(define (show . args)
  (let ((*jtre* (if (null? args) *jtre* (car args)))
        (stream (if (and (not (null? args)) (not (null? (cdr args))))
                    (cadr args)
                    (current-output-port))))
    (show-data *jtre* stream)
    (show-rules *jtre* stream)))
