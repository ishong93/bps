;;; -*- Mode: Scheme; -*-

;;;; Rule system for JTRE using JTMS
;;; Translated from jrules.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1989 --- 1992 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: jinter.scm, jdata.scm, funify.scm, jtms.scm

;;;; Rule structure
;;;; 규칙 구조체

;;; rule: JTRE에서 규칙을 표현하는 구조체
;;; id: 고유 번호
;;; jtre: 이 규칙이 속한 JTRE
;;; dbclass: 관련 패턴의 dbclass
;;; matcher: 매칭을 수행하는 프로시저
;;; body: 규칙 실행 본체 프로시저

(define-record-type <rule>
  (make-rule id jtre dbclass matcher body)
  rule?
  (id rule-id set-rule-id!)             ; Unique ID for easy lookup
  (jtre rule-jtre set-rule-jtre!)       ; The JTRE it is part of
  (dbclass rule-dbclass set-rule-dbclass!) ; Dbclass of associated pattern
  (matcher rule-matcher set-rule-matcher!) ; Procedure that performs the match
  (body rule-body set-rule-body!))       ; Procedure that does the work

(define (print-rule r)
  (string-append "<Rule " (number->string (rule-id r)) ">"))

;;; 파일 카운터 및 접두사 (규칙 프로시저 이름 생성용)
(define *file-counter* 0)
(define *file-prefix* "")

;;; rule-file: 파일 접두사를 설정하고 카운터를 초기화
(define (rule-file prefix)
  (set! *file-counter* 0)
  (set! *file-prefix* prefix))

;;;; Building and installing rules
;;;; 규칙 생성 및 설치

;;; In Scheme, the Common Lisp macro-based rule system is converted to
;;; a runtime procedure-based system. Rules are registered as procedures
;;; that match patterns and execute bodies.

;;; insert-rule: dbclass에 새 규칙을 설치하고 기존 사실들에 대해 시도
;;; dbclass: 규칙이 설치될 dbclass
;;; matcher: 매칭 프로시저 (datum의 lisp-form을 받아 (values ok? bindings node?)를 반환)
;;; body: 규칙 본체 프로시저
(define (insert-rule dbclass matcher body)
  (let ((*jtre* (dbclass-jtre dbclass)))
    (let ((new-counter (+ 1 (jtre-rule-counter *jtre*))))
      (set-jtre-rule-counter! *jtre* new-counter)
      (let ((rule (make-rule new-counter  ; id
                             *jtre*       ; jtre
                             dbclass      ; dbclass
                             matcher      ; matcher
                             body)))      ; body
        (set-dbclass-rules! dbclass
                            (cons rule (dbclass-rules dbclass)))
        (for-each (lambda (candidate)
                    (try-rule-on rule candidate))
                  (dbclass-facts dbclass))
        rule))))

;;; try-rules: datum에 대해 해당 dbclass의 모든 규칙을 시도
(define (try-rules datum)
  (for-each (lambda (rule)
              (try-rule-on rule datum))
            (dbclass-rules (datum-dbclass datum))))

;;; try-rule-on: 특정 규칙을 특정 datum에 대해 시도
;;; matcher가 성공하면 body와 바인딩을 큐에 넣음
(define (try-rule-on rule datum)
  (let ((*jtre* (dbclass-jtre (datum-dbclass datum))))
    (call-with-values
      (lambda () ((rule-matcher rule) (datum-lisp-form datum)))
      (lambda (ok? bindings node?)
        (when ok?
          (let ((bindings (if node?
                              (cons (datum-tms-node datum) bindings)
                              bindings)))
            (enqueue (cons (rule-body rule) bindings) *jtre*)))))))

;;; run-rules: 큐에 있는 모든 규칙을 실행
(define (run-rules . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (let loop ((form (dequeue *jtre*))
               (counter 0))
      (cond ((not form)
             (debugging-jtre "\n    " (number->string counter) " rules run.")
             (set-jtre-rules-run! *jtre*
                                  (+ (jtre-rules-run *jtre*) counter)))
            (else
             (apply (car form) (cdr form))
             (loop (dequeue *jtre*) (+ counter 1)))))))

;;; rules-waiting?: 큐에 대기 중인 규칙이 있는지 확인
(define (rules-waiting? jtre)
  (not (null? (jtre-queue jtre))))

;;; enqueue: 규칙을 큐에 추가
(define (enqueue new j)
  (set-jtre-queue! j (cons new (jtre-queue j))))

;;; dequeue: 큐에서 규칙을 꺼냄
(define (dequeue jtre)
  (if (null? (jtre-queue jtre))
      #f
      (let ((front (car (jtre-queue jtre))))
        (set-jtre-queue! jtre (cdr (jtre-queue jtre)))
        front)))

;;;; Display routines
;;;; 표시 루틴

;;; show-rules: JTRE의 모든 규칙을 표시
(define (show-rules . args)
  (let* ((*jtre* (if (null? args) *jtre* (car args)))
         (stream (if (and (not (null? args)) (not (null? (cdr args))))
                     (cadr args)
                     (current-output-port))))
    (display (string-append "\nThere are "
                            (number->string (jtre-rule-counter *jtre*))
                            " rules in "
                            (->string (jtre-title *jtre*)) ":")
             stream)
    (display (string-append "\n "
                            (if (null? (jtre-queue *jtre*))
                                "None"
                                (number->string (length (jtre-queue *jtre*))))
                            " queued.")
             stream)
    (map-dbclass
     (lambda (dbclass)
       (for-each (lambda (rule)
                   (display-rule rule stream))
                 (dbclass-rules dbclass))))))

;;; display-rule: 규칙을 표시
(define (display-rule rule . args)
  (let ((stream (if (null? args)
                    (current-output-port)
                    (car args))))
    (display (string-append "\n " (print-rule rule) ": "
                            (->string (rule-matcher rule)) ", "
                            (->string (rule-body rule)))
             stream)))

;;; get-rule: 번호로 규칙을 검색
(define (get-rule num . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (call-with-current-continuation
     (lambda (return)
       (map-dbclass
        (lambda (dbclass)
          (for-each
           (lambda (rule)
             (when (= (rule-id rule) num)
               (return rule)))
           (dbclass-rules dbclass))))
       #f))))
