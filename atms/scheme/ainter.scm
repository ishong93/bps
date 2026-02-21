;;; -*- Mode: Scheme; -*-

;;;; ATRE definitions and interface
;;; Translated from ainter.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1990-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires SRFI-9 (define-record-type) and SRFI-69 (hash-tables)

;;;; ATRE structure

(define-record-type <atre>
  (make-atre title atms dbclasses dbclass-table datum-counter
             rules rule-counter debugging queue rules-run
             in-rules focus contradiction-rules imp-rules)
  atre?
  (title atre-title set-atre-title!)                     ; Pretty name
  (atms atre-atms set-atre-atms!)                        ; Pointer to its ATMS
  (dbclasses atre-dbclasses set-atre-dbclasses!)         ; List of dbclasses
  (dbclass-table atre-dbclass-table set-atre-dbclass-table!) ; Quick index into dbclasses
  (datum-counter atre-datum-counter set-atre-datum-counter!) ; Unique ID for asserts
  (rules atre-rules set-atre-rules!)                     ; Index for rules
  (rule-counter atre-rule-counter set-atre-rule-counter!) ; Unique ID for rules
  (debugging atre-debugging set-atre-debugging!)         ; Show basic operations
  (queue atre-queue set-atre-queue!)                     ; General queue
  (rules-run atre-rules-run set-atre-rules-run!)         ; Statistics
  (in-rules atre-in-rules set-atre-in-rules!)            ; in-rules to be executed
  (focus atre-focus set-atre-focus!)                     ; State of the search, if any
  (contradiction-rules atre-contradiction-rules set-atre-contradiction-rules!) ; As in Focus paper (AAAI-88)
  (imp-rules atre-imp-rules set-atre-imp-rules!))       ; Ibid.

(define (print-atre j)
  (string-append "<ATRE: " (->string (atre-title j)) ">"))

;;;; Default ATRE

(define *atre* #f)

;;; with-atre: executes thunk within the given atre
(define (with-atre atre thunk)
  (let ((old-atre *atre*))
    (dynamic-wind
      (lambda () (set! *atre* atre))
      thunk
      (lambda () (set! *atre* old-atre)))))

(define (in-atre atre) (set! *atre* atre))

(define (debugging-atre msg . args)
  (when (atre-debugging *atre*)
    (display msg)
    (for-each display args)
    (newline)))

;;;; Dbclasses, datums, and rules

(define-record-type <dbclass>
  (make-dbclass name atre facts rules)
  dbclass?
  (name dbclass-name set-dbclass-name!)         ; Corresponding symbol
  (atre dbclass-atre set-dbclass-atre!)         ; ATRE it is part of
  (facts dbclass-facts set-dbclass-facts!)       ; Associated facts
  (rules dbclass-rules set-dbclass-rules!))      ; Associated rules

(define (print-dbclass r)
  (string-append "<Dbclass " (->string (dbclass-name r)) ">"))

(define-record-type <datum>
  (make-datum counter atre lisp-form tms-node dbclass assumption? plist)
  datum?
  (counter datum-counter set-datum-counter!)           ; Unique ID for easy lookup
  (atre datum-atre set-datum-atre!)                    ; The ATRE it is part of
  (lisp-form datum-lisp-form set-datum-lisp-form!)     ; Expression for pattern-matching
  (tms-node datum-tms-node set-datum-tms-node!)        ; Pointer into TMS
  (dbclass datum-dbclass set-datum-dbclass!)            ; Dbclass of the corresponding pattern
  (assumption? datum-assumption? set-datum-assumption?!) ; if non-#f, indicates informant
  (plist datum-plist set-datum-plist!))                 ; local property list

(define (print-datum d)
  (string-append "<Datum " (number->string (datum-counter d)) ">"))

(define-record-type <rule>
  (make-rule counter atre dbclass matcher body in-nodes imp-nodes)
  rule?
  (counter rule-counter set-rule-counter!)       ; Unique ID for easy lookup
  (atre rule-atre set-rule-atre!)                ; The ATRE it is part of
  (dbclass rule-dbclass set-rule-dbclass!)        ; Dbclass of associated pattern
  (matcher rule-matcher set-rule-matcher!)        ; Procedure that performs the match
  (body rule-body set-rule-body!)                ; Procedure that does the rules' work
  (in-nodes rule-in-nodes set-rule-in-nodes!)    ; Must have a jointly non-empty label
  (imp-nodes rule-imp-nodes set-rule-imp-nodes!)) ; Must be implied by the focus

(define (print-rule-struct r)
  (string-append "<Rule " (number->string (rule-counter r)) ">"))

;;;; Helper: convert anything to string

(define (->string x)
  (cond ((string? x) x)
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((boolean? x) (if x "#t" "#f"))
        ((null? x) "()")
        ((pair? x)
         (string-append "(" (->string-list x) ")"))
        ((atre? x) (print-atre x))
        ((dbclass? x) (print-dbclass x))
        ((datum? x) (print-datum x))
        ((rule? x) (print-rule-struct x))
        (else (let ((p (open-output-string)))
                (write x p)
                (get-output-string p)))))

(define (->string-list lst)
  (cond ((null? lst) "")
        ((not (pair? lst)) (string-append ". " (->string lst)))
        ((null? (cdr lst)) (->string (car lst)))
        (else (string-append (->string (car lst)) " "
                             (->string-list (cdr lst))))))

;;;; Setting up ATRE

(define (create-atre title . options)
  (let* ((debugging (if (and (not (null? options))
                             (memq 'debugging options))
                        (cadr (memq 'debugging options))
                        #f))
         (j (make-atre title
                       (create-atms (list 'ATMS-OF title)
                                    'stringify-node)
                       '()          ; dbclasses
                       (make-hash-table) ; dbclass-table
                       0            ; datum-counter
                       '()          ; rules
                       0            ; rule-counter
                       debugging    ; debugging
                       '()          ; queue
                       0            ; rules-run
                       '()          ; in-rules
                       #f           ; focus
                       '()          ; contradiction-rules
                       '()))        ; imp-rules
         (false-datum #f))
    (in-atre j)
    (change-atms (atre-atms j)
                 'enqueue-procedure
                 (lambda (pair) (enqueue pair j)))
    ;; Create a default contradiction
    (let ((new-counter (+ 1 (atre-datum-counter j))))
      (set-atre-datum-counter! j new-counter)
      (set! false-datum
            (make-datum new-counter    ; counter
                        j              ; atre
                        'FALSE         ; lisp-form
                        #f             ; tms-node (set below)
                        (get-dbclass 'FALSE) ; dbclass
                        #f             ; assumption?
                        '())))         ; plist
    (set-datum-tms-node! false-datum (atms-contra-node (atre-atms j)))
    (set-tms-node-datum! (datum-tms-node false-datum) false-datum)
    (set-dbclass-facts! (datum-dbclass false-datum)
                        (cons false-datum (dbclass-facts (datum-dbclass false-datum))))
    j))

(define (change-atre atre . options)
  (when (and (not (null? options))
             (memq 'debugging options))
    (set-atre-debugging! atre (cadr (memq 'debugging options)))))

;;;; Running ATRE

(define (run . args)
  (let ((atre (if (null? args) *atre* (car args))))
    (display "\n>>")
    (let loop ((form (read)))
      (cond ((memq form '(quit stop exit abort)) #f)
            (else
             (display "\n")
             (display (eval form))
             (run-rules atre)
             (display "\n>>")
             (loop (read)))))))

(define (run-forms forms . args)
  (let ((atre (if (null? args) *atre* (car args))))
    (for-each (lambda (form)
                (eval form)
                (run-rules atre))
              forms)))

(define (show . args)
  (let ((atre (if (null? args) *atre* (car args)))
        (stream (if (and (not (null? args)) (not (null? (cdr args))))
                    (cadr args)
                    (current-output-port))))
    (display (string-append "For ATRE " (->string (atre-title atre))
                            ":\n Focus = "
                            (if (env? (atre-focus atre))
                                (->string (atre-focus atre))
                                "empty")
                            ".")
             stream)
    (show-data atre stream)
    (show-rules atre stream)))

(define (solutions atre choice-sets)
  (interpretations
   (atre-atms atre)
   (map (lambda (choice-set)
          (map (lambda (f) (get-tms-node f atre))
               choice-set))
        choice-sets)))

;;;; Implied-by rules

;; The rule expansion code sets up the necessary tests for
;; seeing if the antecedent nodes are implied by the current
;; focus when the rule is on the queue.  Here we just
;; re-queue the implied-by rules which were not in the scope
;; of the previous focus for re-examination.

(define (change-focus env . args)
  (let ((atre (if (null? args) *atre* (car args))))
    (unless (atre? atre)
      (error (string-append "Must change the focus of some ATRE, not "
                            (->string atre))))
    (when (and (env? env)
               (not (env-nogood? env)))
      (set-atre-focus! atre env)  ;; change focus
      (set-atre-queue! atre      ;; re-queue implied-by rules
                       (append (atre-queue atre) (atre-imp-rules atre)))
      (set-atre-imp-rules! atre '())
      env)))  ;; return new focus to indicate switch

(define (focus-okay? atre)
  (and (atre-focus atre)
       (not (env-nogood? (atre-focus atre)))))

;;; with-focus: temporarily change focus, restore on exit
(define (with-focus focus atre thunk)
  (let ((old-focus (atre-focus atre)))
    (dynamic-wind
      (lambda () (change-focus focus atre))
      thunk
      (lambda () (change-focus old-focus atre)))))

;;;; Interface to contradiction rules in ATMS

(define (contradiction-rule env proc atre)
  (cond ((env-nogood? env)
         (enqueue (list proc (list env) #f) atre))
        (else
         (set-env-rules! env
                         (cons (list proc (list env) #f)
                               (env-rules env))))))
