;;; -*- Mode: Scheme; -*-

;;;; Database for Tiny Rule Engine using JTMS
;;; Translated from jdata.lisp, last edited 7/1/92, by KDF

;;; Copyright (c) 1989, 1990, 1991 Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer and the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: jinter.scm, jtms (Scheme version), unify.scm, funify.scm
;;; Requires: SRFI-69 (hash-tables)

;;;; Database structure and contents
;;;; 데이터베이스 구조 및 내용

;;; dbclass: 동일한 패턴 이름을 가진 사실(fact)과 규칙(rule)을 그룹화하는 클래스
;;; name: 해당 심볼
;;; jtre: 이 dbclass가 속한 JTRE
;;; facts: 관련된 사실들
;;; rules: 관련된 규칙들

(define-record-type <dbclass>
  (make-dbclass name jtre facts rules)
  dbclass?
  (name dbclass-name set-dbclass-name!)       ; Corresponding symbol
  (jtre dbclass-jtre set-dbclass-jtre!)       ; JTRE it is part of
  (facts dbclass-facts set-dbclass-facts!)     ; Associated facts
  (rules dbclass-rules set-dbclass-rules!))    ; Associated rules

(define (print-dbclass r)
  (string-append "<Dbclass " (->string (dbclass-name r)) ">"))

;;; datum: JTRE에서 사실을 표현하는 구조체
;;; id: 고유 번호
;;; lisp-form: 패턴 매칭을 위한 표현식
;;; tms-node: TMS 노드에 대한 포인터
;;; dbclass: 해당 패턴의 dbclass
;;; assumption?: #f가 아니면 informant를 나타냄
;;; plist: 로컬 속성 리스트

(define-record-type <datum>
  (make-datum id lisp-form tms-node dbclass assumption? plist)
  datum?
  (id datum-id set-datum-id!)                       ; Unique ID for easy lookup
  (lisp-form datum-lisp-form set-datum-lisp-form!) ; Expression for pattern-matching
  (tms-node datum-tms-node set-datum-tms-node!)     ; Pointer into TMS
  (dbclass datum-dbclass set-datum-dbclass!)         ; Dbclass of the corresponding pattern
  (assumption? datum-assumption? set-datum-assumption?!) ; if non-#f, indicates informant
  (plist datum-plist set-datum-plist!))               ; local property list

(define (print-datum d)
  (string-append "<Datum " (number->string (datum-id d)) ">"))

;;;; Making statements
;;;; 명제 생성

;;; assert!: 사실을 단언(assert)하고 정당화(justification)를 추가
;;; fact: 단언할 사실
;;; just: 정당화 (단일 심볼 또는 리스트)
(define (assert! fact just . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (let* ((datum (referent fact #t))
           (node (datum-tms-node datum))
           (just (if (list? just) just (list just))))
      (debugging-jtre "\n    Asserting " (->string fact) " via " (->string just) ".")
      (justify-node (car just) node
                    (map (lambda (f)
                           (datum-tms-node (referent f #t)))
                         (cdr just)))
      datum)))

;;; quiet-assert!: 모순 검사 없이 사실을 단언
(define (quiet-assert! fact just . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (without-contradiction-check (jtre-jtms *jtre*)
      (lambda () (assert! fact just)))))

;;; assume!: 사실을 가정(assumption)으로 설정
;;; 가정은 다른 근거 없이 참으로 가정할 수 있는 노드
(define (assume! fact reason . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (let* ((datum (referent fact #t))
           (node (datum-tms-node datum)))
      (cond ((not (datum-assumption? datum))
             (set-datum-assumption?! datum reason)
             (debugging-jtre "\n    Assuming " (->string fact) " via " (->string reason) ".")
             (assume-node node)
             (enable-assumption node))
            ((eq? reason (datum-assumption? datum))
             ;; Same reason, do nothing
             )
            (else
             (error (string-append
                     "Fact " (show-datum datum)
                     " assumed because of " (->string (datum-assumption? datum))
                     " assumed again because of " (->string reason)))))
      datum)))

;;; already-assumed?: 사실이 이미 가정으로 설정되었는지 확인
(define (already-assumed? fact)
  (datum-assumption? (referent fact #t)))

;;;; Retraction
;;;; 철회

;;; retract!: 가정을 철회
;;; fact: 철회할 사실
;;; just: 정당화 출처 (기본값: 'user)
;;; quiet?: #t이면 경고 메시지를 표시하지 않음
(define (retract! fact . args)
  (let* ((just (if (null? args) 'user (car args)))
         (quiet? (if (or (null? args) (null? (cdr args))) #f (cadr args)))
         (*jtre* (if (or (null? args) (null? (cdr args)) (null? (cddr args)))
                     *jtre* (caddr args))))
    (let* ((datum (referent fact #t))
           (node (datum-tms-node datum)))
      (cond ((not (tms-node-assumption? node))
             (unless quiet?
               (display (string-append "\n" (show-datum datum)
                                       " isn't an assumption."))))
            ((not (in-node? node))
             (unless quiet?
               (display (string-append "\nThe assumption "
                                       (->string fact)
                                       " is not currently in."))))
            ((eq? just (datum-assumption? datum))
             (debugging-jtre "\n    Retracting " (->string fact)
                            " via " (->string just) ".")
             (set-datum-assumption?! datum #f)
             (retract-assumption node))
            ((not quiet?)
             (display (string-append "\n" (->string just)
                                     " not source of assumption for "
                                     (->string fact)))))
      node)))

;;; contradiction: 사실을 모순 노드로 설정
(define (contradiction fact . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (make-contradiction (datum-tms-node (referent fact #t)))))

;;;; Interface and display of data
;;;; 데이터의 인터페이스 및 표시

;;; in?: 사실이 IN(믿어지는) 상태인지 확인
(define (in? fact . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (let ((r (referent fact)))
      (and r (in-node? (datum-tms-node r))))))

;;; out?: 사실이 OUT(믿어지지 않는) 상태인지 확인
(define (out? fact . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (let ((r (referent fact)))
      (and r (out-node? (datum-tms-node r))))))

;;; why?: 사실이 왜 IN 또는 OUT 상태인지 설명
(define (why? fact . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (let ((r (referent fact)))
      (when r (why-node (datum-tms-node r))))))

;;; assumptions-of: 사실을 지지하는 모든 가정들을 반환
(define (assumptions-of fact . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (map view-node
         (assumptions-of-node
          (datum-tms-node (referent fact #t))))))

;;; fetch: 패턴과 일치하는 사실들을 통합(unify)하여 반환
(define (fetch pattern . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (let ((unifiers '()))
      (for-each
       (lambda (candidate)
         (let ((bindings (unify pattern (datum-lisp-form candidate))))
           (unless (eq? bindings 'FAIL)
             (set! unifiers (cons (sublis bindings pattern) unifiers)))))
       (get-candidates pattern))
      unifiers)))

;;;; More display-intensive procedures
;;;; 상세 표시 프로시저

;;; wfs: 사실에 대한 잘 근거된 지지(well-founded support)를 표시
(define (wfs fact . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (cond ((out? fact)
           (display (string-append "\n " (->string fact) " is OUT.")))
          (else
           (let ((initial-node (get-tms-node fact)))
             (let loop ((queue (list initial-node))
                        (so-far (list initial-node)))
               (cond ((null? queue)
                      (display "\n--------")
                      fact)
                     (else
                      (why-node (car queue))
                      (let ((new-antes '()))
                        (unless (or (out-node? (car queue))
                                    (tms-node-assumption? (car queue)))
                          (for-each
                           (lambda (ante)
                             (unless (memq ante so-far)
                               (set! so-far (cons ante so-far))
                               (set! new-antes (cons ante new-antes))))
                           (just-antecedents
                            (tms-node-support (car queue)))))
                        (loop (append (cdr queue) new-antes)
                              so-far))))))))))

;;; say-datum-belief: datum의 믿음 상태를 표시
(define (say-datum-belief pr . args)
  (let ((*jtre* (if (null? args) *jtre* (car args)))
        (indent (if (or (null? args) (null? (cdr args))) "" (cadr args))))
    (display (string-append "\n" indent (->string pr) ": "
                            (if (in-node? (get-tms-node pr *jtre*))
                                "IN" "OUT")))))

;;; show-justifications: 사실에 대한 모든 정당화를 표시
(define (show-justifications fact . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (display (string-append "\n " (->string fact) "::"))
    (let* ((node (get-tms-node fact *jtre*))
           (justs (tms-node-justs node)))
      (cond ((null? justs)
             (display " No justifications.")
             node)
            (else
             (for-each
              (lambda (j)
                (display (string-append "\n " (->string (just-informant j))))
                (cond ((not (null? (just-antecedents j)))
                       (display ", on:")
                       (for-each
                        (lambda (ante)
                          (say-datum-belief (view-node ante) *jtre* "  "))
                        (just-antecedents j))
                       (display "."))
                      (else (display "."))))
              justs)
             node)))))

;;; show-data: JTRE의 모든 데이터를 표시
(define (show-data . args)
  (let* ((*jtre* (if (null? args) *jtre* (car args)))
         (stream (if (and (not (null? args)) (not (null? (cdr args))))
                     (cadr args)
                     (current-output-port))))
    (display (string-append "\n" (number->string (jtre-datum-counter *jtre*))
                            " facts total.")
             stream)
    (map-dbclass
     (lambda (dbclass)
       (for-each
        (lambda (datum)
          (display (string-append "\n" (show-datum datum) ": "
                                  (if (in-node? (datum-tms-node datum))
                                      "IN" "OUT"))
                   stream))
        (dbclass-facts dbclass))))))

;;;; Database system
;;;; 데이터베이스 시스템

;;; get-dbclass: 사실에 해당하는 dbclass를 가져옴 (없으면 생성)
(define (get-dbclass fact . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (cond ((null? fact)
           (error "NIL can't be a dbclass."))
          ((pair? fact)
           (get-dbclass (car fact) *jtre*))
          ((variable? fact)
           (error (string-append "Dbclass unbound: " (->string fact))))
          ((symbol? fact)
           (let ((dbclass (hash-table-ref/default
                           (jtre-dbclass-table *jtre*) fact #f)))
             (cond (dbclass dbclass)
                   (else
                    (let ((new-dbclass (make-dbclass fact *jtre* '() '())))
                      (hash-table-set! (jtre-dbclass-table *jtre*)
                                       fact new-dbclass)
                      new-dbclass)))))
          (else
           (error (string-append "Bad dbclass type: " (->string fact)))))))

;;; referent: 사실에 해당하는 datum을 찾음
;;; virtual?가 #t이면 없을 경우 새로 삽입
(define (referent fact . args)
  (let ((virtual? (if (null? args) #f (car args))))
    (if virtual? (insert fact) (referent1 fact))))

;;; referent1: 사실에 해당하는 기존 datum을 검색
(define (referent1 fact)
  (let loop ((candidates (dbclass-facts (get-dbclass fact))))
    (cond ((null? candidates) #f)
          ((equal? (datum-lisp-form (car candidates)) fact)
           (car candidates))
          (else (loop (cdr candidates))))))

;;; insert: 사실을 데이터베이스에 삽입 (이미 있으면 기존 datum 반환)
(define (insert fact)
  (let ((datum (referent1 fact)))
    (cond (datum (values datum #t))
          (else
           (let ((new-counter (+ 1 (jtre-datum-counter *jtre*))))
             (set-jtre-datum-counter! *jtre* new-counter)
             (let ((new-datum (make-datum new-counter     ; id
                                          fact            ; lisp-form
                                          #f              ; tms-node (set below)
                                          (get-dbclass fact) ; dbclass
                                          #f              ; assumption?
                                          '())))          ; plist
               (set-datum-tms-node! new-datum
                                    (tms-create-node (jtre-jtms *jtre*) new-datum))
               (set-dbclass-facts! (datum-dbclass new-datum)
                                   (cons new-datum
                                         (dbclass-facts (datum-dbclass new-datum))))
               (try-rules new-datum)
               (values new-datum #f)))))))

;;; get-candidates: 패턴에 해당하는 후보 datum들을 반환
(define (get-candidates pattern)
  (dbclass-facts (get-dbclass pattern)))

;;; map-dbclass: 모든 dbclass에 대해 프로시저를 적용
(define (map-dbclass proc . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (hash-table-walk
     (jtre-dbclass-table *jtre*)
     (lambda (name dbclass) (proc dbclass)))))

;;; get-tms-node: 사실에 해당하는 TMS 노드를 반환
(define (get-tms-node fact . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (datum-tms-node (referent fact #t))))

;;; view-node: TMS 노드에서 사실(lisp-form)을 추출
(define (view-node node)
  (datum-lisp-form (tms-node-datum node)))

;;;; More query routines
;;;; 추가 질의 루틴

;;; show-datum: datum을 문자열로 변환
(define (show-datum datum)
  (->string (datum-lisp-form datum)))

;;; get-datum: 번호로 datum을 검색
(define (get-datum num . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (call-with-current-continuation
     (lambda (return)
       (hash-table-walk
        (jtre-dbclass-table *jtre*)
        (lambda (key dbclass)
          (for-each
           (lambda (datum)
             (when (= (datum-id datum) num)
               (return datum)))
           (dbclass-facts dbclass))))
       #f))))

;;; get-just: 번호로 정당화(justification)를 검색
(define (get-just num . args)
  (let ((*jtre* (if (null? args) *jtre* (car args))))
    (let loop ((justs (jtms-justs (jtre-jtms *jtre*))))
      (cond ((null? justs) #f)
            ((= (just-index (car justs)) num) (car justs))
            (else (loop (cdr justs)))))))
