;;; -*- Mode: Scheme -*-

;;; Justification-based Truth Maintenance System (JTMS)
;;; Version 176.
;; Last edited 1/29/93, by KDF
;;; Translated to Scheme (R7RS + SRFI-9) from Common Lisp.

;;; Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;;; JTMS 개요
;;;
;;; JTMS는 논리적 추론 시스템에서 믿음(beliefs)을 관리하는 진리 유지 시스템입니다.
;;;
;;; 주요 개념:
;;; - 노드(Node): 명제나 사실을 표현. IN(믿어짐) 또는 OUT(믿어지지 않음) 상태를 가짐
;;; - 정당화(Justification): "선행조건들이 모두 참이면 결론도 참"이라는 추론 규칙
;;; - 가정(Assumption): 다른 근거 없이 참으로 가정할 수 있는 노드
;;; - 모순(Contradiction): IN이 되면 안 되는 노드 (모순 감지용)
;;;
;;; 사용 예:
;;; 1. JTMS 생성: (define *jtms* (create-jtms "My TMS"))
;;; 2. 노드 생성: (define n1 (tms-create-node *jtms* '(sky blue)))
;;;              (define n2 (tms-create-node *jtms* '(grass green)))
;;;              (define n3 (tms-create-node *jtms* '(day time)))
;;; 3. 정당화 추가: (justify-node 'rule-1 n3 (list n1 n2))
;;;                ; n1과 n2가 모두 IN이면 n3도 IN이 됨
;;; 4. 가정 활성화: (assume-node n1)  ; n1을 IN으로 만듦
;;;                (assume-node n2)  ; n2를 IN으로 만듦
;;;                ; 이제 n3도 자동으로 IN이 됨
;;; 5. 상태 확인: (why-node n3)  ; n3가 왜 IN인지 출력
;;; 6. 가정 철회: (retract-assumption n1)  ; n1을 OUT으로 만듦
;;;              ; n3도 자동으로 OUT이 됨 (n2만으로는 불충분)

;;; Helpers

(define (datum->string datum)
  (cond ((string? datum) datum)
        ((number? datum) (number->string datum))
        ((symbol? datum) (symbol->string datum))
        (else (let ((p (open-output-string)))
                (display datum p)
                (get-output-string p)))))

(define (every pred lst)
  (cond ((null? lst) #t)
        ((not (pred (car lst))) #f)
        (else (every pred (cdr lst)))))

(define (list-copy lst)
  (if (null? lst) '()
      (cons (car lst) (list-copy (cdr lst)))))

;;; JTMS 주 구조체
(define-record-type <jtms>
  (make-jtms-record title node-counter just-counter
                    nodes justs debugging contradictions assumptions
                    checking-contradictions node-string
                    contradiction-handler enqueue-procedure)
  jtms?
  (title jtms-title set-jtms-title!)
  (node-counter jtms-node-counter set-jtms-node-counter!)
  (just-counter jtms-just-counter set-jtms-just-counter!)
  (nodes jtms-nodes set-jtms-nodes!)
  (justs jtms-justs set-jtms-justs!)
  (debugging jtms-debugging set-jtms-debugging!)
  (contradictions jtms-contradictions set-jtms-contradictions!)
  (assumptions jtms-assumptions set-jtms-assumptions!)
  (checking-contradictions jtms-checking-contradictions set-jtms-checking-contradictions!)
  (node-string jtms-node-string set-jtms-node-string!)
  (contradiction-handler jtms-contradiction-handler set-jtms-contradiction-handler!)
  (enqueue-procedure jtms-enqueue-procedure set-jtms-enqueue-procedure!))

(define (print-jtms jtms)
  (string-append "#<JTMS: " (jtms-title jtms) ">"))

(define-record-type <tms-node>
  (make-tms-node-record index datum label support justs consequences
                        mark contradictory? assumption? in-rules out-rules jtms)
  tms-node?
  (index tms-node-index set-tms-node-index!)
  (datum tms-node-datum set-tms-node-datum!)
  (label tms-node-label set-tms-node-label!)
  (support tms-node-support set-tms-node-support!)
  (justs tms-node-justs set-tms-node-justs!)
  (consequences tms-node-consequences set-tms-node-consequences!)
  (mark tms-node-mark set-tms-node-mark!)
  (contradictory? tms-node-contradictory? set-tms-node-contradictory?!)
  (assumption? tms-node-assumption? set-tms-node-assumption?!)
  (in-rules tms-node-in-rules set-tms-node-in-rules!)
  (out-rules tms-node-out-rules set-tms-node-out-rules!)
  (jtms tms-node-jtms set-tms-node-jtms!))

(define (print-tms-node node)
  (string-append "#<Node: " (node-string node) ">"))

(define-record-type <just>
  (make-just-record index informant consequence antecedents)
  just?
  (index just-index set-just-index!)
  (informant just-informant set-just-informant!)
  (consequence just-consequence set-just-consequence!)
  (antecedents just-antecedents set-just-antecedents!))

(define (print-just j)
  (string-append "#<Just " (number->string (just-index j)) ">"))

;;; Incf helpers: increment counter and return new value
(define (jtms-node-counter-incf! jtms)
  (let ((new-val (+ 1 (jtms-node-counter jtms))))
    (set-jtms-node-counter! jtms new-val)
    new-val))

(define (jtms-just-counter-incf! jtms)
  (let ((new-val (+ 1 (jtms-just-counter jtms))))
    (set-jtms-just-counter! jtms new-val)
    new-val))

;;; Node accessors and predicates

(define (tms-node-premise? node)
  (let ((support (tms-node-support node)))
    (and support
         (not (eq? support 'enabled-assumption))
         (null? (just-antecedents support)))))

(define (node-string node)
  ((jtms-node-string (tms-node-jtms node)) node))

;;; 디버깅 출력
(define (debugging-jtms jtms msg . args)
  (when (jtms-debugging jtms)
    (display msg (current-error-port))
    (for-each (lambda (a) (display a (current-error-port))) args)
    (newline (current-error-port))))

(define (tms-error string node)
  (error (string-append string " " (node-string node))))

(define (default-node-string n)
  (let ((datum (tms-node-datum n)))
    (cond ((string? datum) datum)
          ((number? datum) (number->string datum))
          ((symbol? datum) (symbol->string datum))
          (else (let ((p (open-output-string)))
                  (display datum p)
                  (get-output-string p))))))

;;; JTMS 생성 및 변경
(define (create-jtms title . options)
  (let ((node-string-fn default-node-string)
        (debugging-flag #f)
        (checking-contradictions #t)
        (contradiction-handler ask-user-handler)
        (enqueue-proc #f))
    ;; Parse keyword-style options
    (let loop ((opts options))
      (cond ((null? opts) #t)
            ((and (pair? opts) (pair? (cdr opts)))
             (cond ((eq? (car opts) 'node-string)
                    (set! node-string-fn (cadr opts)))
                   ((eq? (car opts) 'debugging)
                    (set! debugging-flag (cadr opts)))
                   ((eq? (car opts) 'checking-contradictions)
                    (set! checking-contradictions (cadr opts)))
                   ((eq? (car opts) 'contradiction-handler)
                    (set! contradiction-handler (cadr opts)))
                   ((eq? (car opts) 'enqueue-procedure)
                    (set! enqueue-proc (cadr opts))))
             (loop (cddr opts)))
            (else (error "Odd number of keyword arguments to create-jtms"))))
    (make-jtms-record title 0 0
                      '() '() debugging-flag '() '()
                      checking-contradictions node-string-fn
                      contradiction-handler enqueue-proc)))

(define (change-jtms jtms . options)
  (let loop ((opts options))
    (cond ((null? opts) #t)
          ((and (pair? opts) (pair? (cdr opts)))
           (cond ((eq? (car opts) 'node-string)
                  (set-jtms-node-string! jtms (cadr opts)))
                 ((eq? (car opts) 'debugging)
                  (set-jtms-debugging! jtms (cadr opts)))
                 ((eq? (car opts) 'checking-contradictions)
                  (set-jtms-checking-contradictions! jtms (cadr opts)))
                 ((eq? (car opts) 'contradiction-handler)
                  (set-jtms-contradiction-handler! jtms (cadr opts)))
                 ((eq? (car opts) 'enqueue-procedure)
                  (set-jtms-enqueue-procedure! jtms (cadr opts))))
           (loop (cddr opts)))
          (else (error "Odd number of keyword arguments to change-jtms")))))

;;; 노드 상태 검사
(define (in-node? node) (eq? (tms-node-label node) 'in))
(define (out-node? node) (eq? (tms-node-label node) 'out))

;;; 노드 생성
(define (tms-create-node jtms datum . options)
  (let ((assumptionp #f)
        (contradictoryp #f))
    ;; Parse keyword-style options
    (let loop ((opts options))
      (cond ((null? opts) #t)
            ((and (pair? opts) (pair? (cdr opts)))
             (cond ((eq? (car opts) 'assumptionp)
                    (set! assumptionp (cadr opts)))
                   ((eq? (car opts) 'contradictoryp)
                    (set! contradictoryp (cadr opts))))
             (loop (cddr opts)))
            (else (error "Odd number of keyword arguments to tms-create-node"))))
    (let ((node (make-tms-node-record
                 (jtms-node-counter-incf! jtms)
                 datum 'out #f '() '()
                 #f contradictoryp assumptionp '() '() jtms)))
      (when assumptionp
        (set-jtms-assumptions! jtms (cons node (jtms-assumptions jtms))))
      (when contradictoryp
        (set-jtms-contradictions! jtms (cons node (jtms-contradictions jtms))))
      (set-jtms-nodes! jtms (cons node (jtms-nodes jtms)))
      node)))

;;; 가정 노드 활성화
(define (assume-node node)
  (let ((jtms (tms-node-jtms node)))
    (unless (tms-node-assumption? node)
      (debugging-jtms jtms
                      "\nConverting " (node-string node) " into an assumption")
      (set-tms-node-assumption?! node #t))
    (enable-assumption node)))

;;; 모순 노드로 지정
(define (make-contradiction node)
  (let ((jtms (tms-node-jtms node)))
    (unless (tms-node-contradictory? node)
      (set-tms-node-contradictory?! node #t)
      (set-jtms-contradictions! jtms (cons node (jtms-contradictions jtms)))
      (check-for-contradictions jtms))))

;;; 정당화 추가
(define (justify-node informant consequence antecedents)
  (let* ((jtms (tms-node-jtms consequence))
         (just (make-just-record (jtms-just-counter-incf! jtms)
                                 informant consequence antecedents)))
    (set-tms-node-justs! consequence (cons just (tms-node-justs consequence)))
    (for-each (lambda (node)
                (set-tms-node-consequences! node
                  (cons just (tms-node-consequences node))))
              antecedents)
    (set-jtms-justs! jtms (cons just (jtms-justs jtms)))
    (debugging-jtms jtms
                    "\nJustifying " (node-string consequence)
                    " by " (datum->string informant)
                    " using "
                    (let ((p (open-output-string)))
                      (display (map node-string antecedents) p)
                      (get-output-string p)))
    (if (or (pair? antecedents) (out-node? consequence))
        (when (check-justification just)
          (install-support consequence just))
        (set-tms-node-support! consequence just))
    (check-for-contradictions jtms)))

;;; 정당화 검증
(define (check-justification just)
  (and (out-node? (just-consequence just))
       (justification-satisfied? just)))

(define (justification-satisfied? just)
  (every in-node? (just-antecedents just)))

;;; 지지(support) 설치 및 전파
(define (install-support conseq just)
  (make-node-in conseq just)
  (propagate-inness conseq))

(define (propagate-inness node)
  (let ((jtms (tms-node-jtms node))
        (q (list node)))
    (let loop ()
      (when (pair? q)
        (let ((current (car q)))
          (set! q (cdr q))
          (debugging-jtms jtms "\n   Propagating belief in " (node-string current) ".")
          (for-each
           (lambda (justification)
             (when (check-justification justification)
               (make-node-in (just-consequence justification) justification)
               (set! q (cons (just-consequence justification) q))))
           (tms-node-consequences current))
          (loop))))))

;;; 노드를 IN 상태로 만듦
(define (make-node-in conseq reason)
  (let* ((jtms (tms-node-jtms conseq))
         (enqueuef (jtms-enqueue-procedure jtms)))
    (debugging-jtms jtms "\n     Making " (node-string conseq)
                    " in via "
                    (if (symbol? reason)
                        (symbol->string reason)
                        (let ((p (open-output-string)))
                          (display (cons (just-informant reason)
                                         (map (jtms-node-string jtms)
                                              (just-antecedents reason)))
                                   p)
                          (get-output-string p)))
                    ".")
    (set-tms-node-label! conseq 'in)
    (set-tms-node-support! conseq reason)
    (when enqueuef
      (for-each (lambda (in-rule) (enqueuef in-rule))
                (tms-node-in-rules conseq))
      (set-tms-node-in-rules! conseq '()))))

;;; 가정 철회
(define (retract-assumption node)
  (when (eq? (tms-node-support node) 'enabled-assumption)
    (let ((jtms (tms-node-jtms node)))
      (debugging-jtms jtms "\n  Retracting assumption " (node-string node) ".")
      (make-node-out node)
      (find-alternative-support jtms
        (cons node (propagate-outness node jtms))))))

;;; 가정 활성화
(define (enable-assumption node)
  (let ((jtms (tms-node-jtms node)))
    (unless (tms-node-assumption? node)
      (tms-error "Can't enable the non-assumption" node))
    (debugging-jtms jtms "\n  Enabling assumption " (node-string node) ".")
    (cond ((out-node? node)
           (make-node-in node 'enabled-assumption)
           (propagate-inness node))
          ((or (eq? (tms-node-support node) 'enabled-assumption)
               (null? (just-antecedents (tms-node-support node))))
           ;; 이미 enabled-assumption이거나 전제(premise)인 경우 아무것도 하지 않음
           #f)
          (else
           (set-tms-node-support! node 'enabled-assumption)))
    (check-for-contradictions jtms)))

;;; 노드를 OUT 상태로 만듦
(define (make-node-out node)
  (let* ((jtms (tms-node-jtms node))
         (enqueuef (jtms-enqueue-procedure jtms)))
    (debugging-jtms jtms "\n     Retracting belief in " (node-string node) ".")
    (set-tms-node-support! node #f)
    (set-tms-node-label! node 'out)
    (when enqueuef
      (for-each (lambda (out-rule) (enqueuef out-rule))
                (tms-node-out-rules node)))
    (set-tms-node-out-rules! node '())))

;;; OUT 상태 전파
(define (propagate-outness node jtms)
  (debugging-jtms jtms "\n   Propagating disbelief in " (node-string node) ".")
  (let ((out-queue '()))
    (let loop ((js (tms-node-consequences node))
               (new '()))
      (cond ((null? js)
             out-queue)
            (else
             (let ((conseq (just-consequence (car js))))
               (cond ((eq? (tms-node-support conseq) (car js))
                      (make-node-out conseq)
                      (set! out-queue (cons conseq out-queue))
                      (loop (append (cdr js) (tms-node-consequences conseq)) '()))
                     (else
                      (loop (cdr js) new)))))))))

;;; 대안적 지지 탐색
(define (find-alternative-support jtms out-queue)
  (debugging-jtms jtms "\n   Looking for alternative supports.")
  (for-each
   (lambda (node)
     (unless (in-node? node)
       (let loop ((justs (tms-node-justs node)))
         (when (pair? justs)
           (if (check-justification (car justs))
               (install-support (just-consequence (car justs)) (car justs))
               (loop (cdr justs)))))))
   out-queue))

;;; 모순 검사
(define (check-for-contradictions jtms)
  (when (jtms-checking-contradictions jtms)
    (let ((contradictions '()))
      (for-each (lambda (cnode)
                  (when (in-node? cnode)
                    (set! contradictions (cons cnode contradictions))))
                (jtms-contradictions jtms))
      (when (pair? contradictions)
        ((jtms-contradiction-handler jtms) jtms contradictions)))))

;;; 모순 검사 제어 -- dynamic-wind를 사용하여 unwind-protect를 구현
(define (without-contradiction-check jtms thunk)
  (let ((old-value (jtms-checking-contradictions jtms)))
    (dynamic-wind
      (lambda () (set-jtms-checking-contradictions! jtms #f))
      thunk
      (lambda () (set-jtms-checking-contradictions! jtms old-value)))))

(define (with-contradiction-check jtms thunk)
  (let ((old-value (jtms-checking-contradictions jtms)))
    (dynamic-wind
      (lambda () (set-jtms-checking-contradictions! jtms #t))
      thunk
      (lambda () (set-jtms-checking-contradictions! jtms old-value)))))

(define (with-contradiction-handler jtms handler thunk)
  (let ((old-handler (jtms-contradiction-handler jtms)))
    (dynamic-wind
      (lambda () (set-jtms-contradiction-handler! jtms handler))
      thunk
      (lambda () (set-jtms-contradiction-handler! jtms old-handler)))))

;;; 기본 가정 설정
(define (default-assumptions jtms)
  (with-contradiction-check jtms
    (lambda ()
      (with-contradiction-handler jtms
        (lambda (jtms contradictions) (raise 'contradiction))
        (lambda ()
          (for-each
           (lambda (assumption)
             (cond ((eq? (tms-node-support assumption) 'enabled-assumption)
                    ;; 이미 활성화된 가정은 건너뜀
                    #f)
                   ((not (eq? 'default (tms-node-assumption? assumption)))
                    ;; default 가정이 아니면 건너뜀
                    #f)
                   (else
                    (when (guard (exn
                                  ((eq? exn 'contradiction) #t)
                                  (else (raise exn)))
                            (enable-assumption assumption)
                            #f)
                      (retract-assumption assumption)))))
           (jtms-assumptions jtms)))))))

;;; 노드의 지지 정당화 조회
(define (supporting-justification-for-node node)
  (tms-node-support node))

;;; 노드의 가정들을 추적
(define (assumptions-of-node node)
  (let ((assumptions '())
        (marker (list 'mark)))  ; 고유한 마커 객체
    (let loop ((nodes (list node)))
      (cond ((null? nodes) assumptions)
            (else
             (let ((current (car nodes))
                   (rest (cdr nodes)))
               (cond ((eq? (tms-node-mark current) marker)
                      (loop rest))
                     (else
                      (set-tms-node-mark! current marker)
                      (cond ((eq? (tms-node-support current) 'enabled-assumption)
                             (set! assumptions (cons current assumptions))
                             (loop rest))
                            ((in-node? current)
                             (loop (append rest
                                           (just-antecedents
                                            (tms-node-support current)))))
                            (else
                             (loop rest)))))))))))

;;; 활성화된 가정 목록
(define (enabled-assumptions jtms)
  (let ((result '()))
    (for-each (lambda (assumption)
                (when (eq? (tms-node-support assumption) 'enabled-assumption)
                  (set! result (cons assumption result))))
              (jtms-assumptions jtms))
    result))

;;; 노드가 왜 IN/OUT인지 출력
(define (why-node node)
  (let ((justification (tms-node-support node)))
    (cond ((eq? justification 'enabled-assumption)
           (display "\n")
           (display (node-string node))
           (display " is an enabled assumption"))
          (justification
           (display "\n")
           (display (node-string node))
           (display " is IN via ")
           (display (just-informant justification))
           (display " on")
           (for-each (lambda (anode)
                       (display "\n  ")
                       (display (node-string anode)))
                     (just-antecedents justification)))
          (else
           (display "\n")
           (display (node-string node))
           (display " is OUT."))))
  node)

;;; 모든 노드에 대해 why-node 호출
(define (why-nodes jtms)
  (for-each why-node (jtms-nodes jtms)))

;;; 모순 처리를 위한 전역 변수
(define *contra-assumptions* '())

;;; 사용자에게 모순 처리를 요청하는 핸들러
(define (ask-user-handler jtms contradictions)
  (handle-one-contradiction (car contradictions))
  (check-for-contradictions jtms))

(define (handle-one-contradiction contra-node)
  (set! *contra-assumptions* (assumptions-of-node contra-node))
  (unless (pair? *contra-assumptions*)
    (tms-error "There is a flaw in the universe..." contra-node))
  (display "\nContradiction found: ")
  (display (node-string contra-node))
  (print-contra-list *contra-assumptions*)
  (display "\nCall (tms-answer <number>) to retract assumption.")
  (let ((the-answer
         (call-with-current-continuation
          (lambda (k)
            (set! *tms-contradiction-escape* k)
            ;; In Scheme we cannot replicate CL's break/continue.
            ;; The user should call (tms-answer <number>) which will
            ;; invoke the continuation.
            #f))))
    (when (and (integer? the-answer)
               (> the-answer 0)
               (<= the-answer (length *contra-assumptions*)))
      (retract-assumption (list-ref *contra-assumptions* (- the-answer 1))))))

;;; tms-contradiction-escape 연속을 저장하는 변수
(define *tms-contradiction-escape* #f)

(define (print-contra-list nodes)
  (let loop ((counter 1) (nn nodes))
    (when (pair? nn)
      (display "\n")
      (display counter)
      (display " ")
      (display (node-string (car nn)))
      (loop (+ counter 1) (cdr nn)))))

(define (tms-answer num)
  (cond ((not (integer? num))
         (display "\nIgnoring answer, must be an integer."))
        ((<= num 0)
         (display "\nIgnoring answer, too small"))
        ((> num (length *contra-assumptions*))
         (display "\nIgnoring answer, too big."))
        (else
         (when *tms-contradiction-escape*
           (*tms-contradiction-escape* num)))))

;;; 네트워크 탐색 (인터랙티브)
(define (explore-network node)
  (cond ((not (in-node? node))
         (display "\n Sorry, ")
         (display (node-string node))
         (display " not believed.")
         node)
        (else
         (call-with-current-continuation
          (lambda (return)
            (let loop ((current node) (stack '()))
              (why-node current)
              (let* ((support (tms-node-support current))
                     (options (if (just? support)
                                  (just-antecedents support)
                                  '()))
                     (olen (length options)))
                (let input-loop ()
                  (display "\n>>>")
                  (let ((choice (read)))
                    (cond ((eq? choice 'q)
                           (return current))
                          ((and (integer? choice)
                                (= choice 0))
                           (if (pair? stack)
                               (loop (car stack) (cdr stack))
                               (return current)))
                          ((and (integer? choice)
                                (> choice 0)
                                (<= choice olen))
                           (loop (list-ref options (- choice 1))
                                 (cons current stack)))
                          (else
                           (display "\n Must be q or an integer from 0 to ")
                           (display olen)
                           (display ".")
                           (input-loop))))))))))))
