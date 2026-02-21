;;; -*- Mode: Scheme; -*-

;;;; ATRE database
;;; Translated from adata.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1992, Kenneth D. Forbus, Northwestern
;;;  University, and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires: ainter.scm, atms (Scheme version), unify.scm, funify.scm
;;; Requires: SRFI-69 (hash-tables)

;;;; Assertion and assumption

(define (assert! fact just . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (let* ((datum (referent fact #t))
           (node (datum-tms-node datum))
           (just (if (list? just) just (list just))))
      (debugging-atre "    Asserting " (->string fact) " via " (->string just) ".")
      (justify-node (car just) node
                    (map (lambda (f)
                           (datum-tms-node (referent f #t)))
                         (cdr just)))
      datum)))

(define (assume! fact reason . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (let* ((datum (referent fact #t))
           (node (datum-tms-node datum)))
      (cond ((not (datum-assumption? datum))
             (set-datum-assumption?! datum reason)
             (debugging-atre "    Assuming " (->string fact) " via " (->string reason) ".")
             (assume-node node))
            ((eq? reason (datum-assumption? datum))
             ;; Same reason, do nothing
             )
            (else
             (error (string-append
                     "Fact " (show-datum datum)
                     " assumed because of " (->string (datum-assumption? datum))
                     " assumed again because of " (->string reason)))))
      datum)))

(define (already-assumed? fact)
  (tms-node-assumption? (get-tms-node fact)))

(define (assume-if-needed fact reason . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (unless (already-assumed? fact)
      (assume! fact reason))))

(define (contradiction fact . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (make-contradiction (datum-tms-node (referent fact #t)))))

;;;; Database system

(define (get-dbclass fact)
  (cond ((null? fact)
         (error "NIL can't be a dbclass."))
        ((pair? fact)
         (get-dbclass (car fact)))
        ((variable? fact)
         (error (string-append "Dbclass unbound: " (->string fact))))
        ((symbol? fact)
         (let ((dbclass (hash-table-ref/default
                         (atre-dbclass-table *atre*) fact #f)))
           (cond (dbclass dbclass)
                 (else
                  (let ((new-dbclass (make-dbclass fact *atre* '() '())))
                    (hash-table-set! (atre-dbclass-table *atre*) fact new-dbclass)
                    (set-atre-dbclasses! *atre*
                                        (cons new-dbclass (atre-dbclasses *atre*)))
                    new-dbclass)))))
        (else
         (error (string-append "Bad dbclass type: " (->string fact))))))

(define (referent fact . args)
  (let ((virtual? (if (null? args) #f (car args))))
    (if virtual? (insert fact) (referent1 fact))))

(define (referent1 fact)
  (let loop ((candidates (dbclass-facts (get-dbclass fact))))
    (cond ((null? candidates) #f)
          ((equal? (datum-lisp-form (car candidates)) fact)
           (car candidates))
          (else (loop (cdr candidates))))))

(define (insert fact)
  (let ((datum (referent1 fact)))
    (cond (datum (values datum #t))
          (else
           (let ((new-counter (+ 1 (atre-datum-counter *atre*))))
             (set-atre-datum-counter! *atre* new-counter)
             (let ((new-datum (make-datum new-counter     ; counter
                                          *atre*          ; atre
                                          fact            ; lisp-form
                                          #f              ; tms-node (set below)
                                          (get-dbclass fact) ; dbclass
                                          #f              ; assumption?
                                          '())))          ; plist
               (set-datum-tms-node! new-datum
                                    (tms-create-node (atre-atms *atre*) new-datum))
               (set-dbclass-facts! (datum-dbclass new-datum)
                                   (cons new-datum
                                         (dbclass-facts (datum-dbclass new-datum))))
               (try-rules new-datum)
               (values new-datum #f)))))))

(define (fetch pattern . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (let ((unifiers '()))
      (for-each
       (lambda (candidate)
         (let ((bindings (unify pattern (datum-lisp-form candidate))))
           (unless (eq? bindings 'FAIL)
             (set! unifiers (cons (sublis bindings pattern) unifiers)))))
       (get-candidates pattern))
      unifiers)))

(define (get-candidates pattern)
  (dbclass-facts (get-dbclass pattern)))

;;;; Interface and display of data

(define (true? fact . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (let ((r (referent fact #f)))
      (and r (true-node? (datum-tms-node r))))))

(define (in? fact env . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (let ((r (referent fact #f)))
      (and r (in-node? (datum-tms-node r) env)))))

(define (out? fact env . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (let ((r (referent fact #f)))
      (and r (out-node? (datum-tms-node r) env)))))

(define (consistent-with? fact env . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (let ((r (referent fact #f)))
      (and r (node-consistent-with? (datum-tms-node r) env)))))

(define (why? fact . args)
  (let* ((*atre* (if (null? args) *atre* (car args)))
         (stream (if (and (not (null? args)) (not (null? (cdr args))))
                     (cadr args)
                     (current-output-port))))
    (let ((r (referent fact #f)))
      (when r (why-node (datum-tms-node r) stream)))))

(define (environment-of facts . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (call-with-current-continuation
     (lambda (return)
       (let ((env (atms-empty-env (atre-atms *atre*))))
         (for-each
          (lambda (fact)
            (let ((node (get-tms-node fact *atre*)))
              (unless (tms-node-assumption? node)
                (error (string-append
                        "Non-assumption in ENVIRONMENT-OF: "
                        (->string fact))))
              (set! env (cons-env node env))
              (when (env-nogood? env)
                (return (values #f env)))))
          facts)
         env)))))

(define (environment-cons fact env)
  (cons-env (get-tms-node fact) env))

(define (view-env env)
  (map view-node (env-assumptions env)))

(define (justifications fact . args)
  (let* ((*atre* (if (null? args) *atre* (car args)))
         (stream (if (and (not (null? args)) (not (null? (cdr args))))
                     (cadr args)
                     (current-output-port))))
    (node-justifications (get-tms-node fact *atre*) stream)))

;;;; More interrogatives

(define (the-e num . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (e (atre-atms *atre*) num)))

(define (get-tms-node fact . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (datum-tms-node (referent fact #t))))

(define (view-node node)
  (datum-lisp-form (tms-node-datum node)))

(define (stringify-node node)
  (->string (view-node node)))

(define (assumptions-of fact)
  (tms-node-label (datum-tms-node (referent fact #t))))

(define (get-datum num . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (call-with-current-continuation
     (lambda (return)
       (hash-table-walk
        (atre-dbclass-table *atre*)
        (lambda (key dbclass)
          (for-each
           (lambda (datum)
             (when (= (datum-counter datum) num)
               (return datum)))
           (dbclass-facts dbclass))))
       #f))))

(define (get-just num . args)
  (let ((*atre* (if (null? args) *atre* (car args))))
    (let loop ((justs (atms-justs (atre-atms *atre*))))
      (cond ((null? justs) #f)
            ((= (just-index (car justs)) num) (car justs))
            (else (loop (cdr justs)))))))

;;;; Extra printing routines

(define (show-datum datum)
  (->string (datum-lisp-form datum)))

(define (show-data . args)
  (let* ((*atre* (if (null? args) *atre* (car args)))
         (stream (if (and (not (null? args)) (not (null? (cdr args))))
                     (cadr args)
                     (current-output-port)))
         (counter 0))
    (display (string-append "\n" (number->string (atre-datum-counter *atre*))
                            " facts total.")
             stream)
    (for-each
     (lambda (dbclass)
       (for-each
        (lambda (datum)
          (set! counter (+ counter 1))
          (display (string-append "\n" (show-datum datum) ": "
                                  (->string (assumptions-of (datum-lisp-form datum))))
                   stream))
        (dbclass-facts dbclass)))
     (atre-dbclasses *atre*))
    counter))

(define (show-context env . args)
  (let* ((*atre* (if (null? args) *atre* (car args)))
         (stream (if (and (not (null? args)) (not (null? (cdr args))))
                     (cadr args)
                     (current-output-port)))
         (counter 0))
    (for-each
     (lambda (dbclass)
       (for-each
        (lambda (datum)
          (when (in-node? (datum-tms-node datum) env)
            (set! counter (+ counter 1))
            (display (string-append "\n" (show-datum datum)) stream)))
        (dbclass-facts dbclass)))
     (atre-dbclasses *atre*))
    (display (string-append "\n" (number->string counter) " facts total.") stream)
    counter))

(define (show-dbclasses . args)
  (let* ((*atre* (if (null? args) *atre* (car args)))
         (stream (if (and (not (null? args)) (not (null? (cdr args))))
                     (cadr args)
                     (current-output-port)))
         (counter 0))
    (for-each
     (lambda (dbclass)
       (set! counter (+ counter 1))
       (display (string-append "\n " (->string (dbclass-name dbclass))
                               ": " (number->string (length (dbclass-facts dbclass)))
                               " facts, "
                               (number->string (length (dbclass-rules dbclass)))
                               " rules")
                stream))
     (atre-dbclasses *atre*))
    counter))
