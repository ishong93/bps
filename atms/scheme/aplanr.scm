;;; -*- Mode: Scheme; -*-

;;;; ATMS-based planner using ATRE + ATMS
;;; Translated from aplanr.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; Requires SRFI-9 (define-record-type)

;;;; Planning problem structure

(define-record-type <plnpr>
  (make-plnpr title atre basis-set operators plist)
  plnpr?
  (title plnpr-title set-plnpr-title!)           ; String for printing
  (atre plnpr-atre set-plnpr-atre!)              ; ATRE it uses
  (basis-set plnpr-basis-set set-plnpr-basis-set!) ; Choice sets defining states
  (operators plnpr-operators set-plnpr-operators!) ; List of allowed operators
  (plist plnpr-plist set-plnpr-plist!))            ; Cache for intermediate results (alist)

(define (print-plnpr p)
  (string-append "<PlnPr: " (plnpr-title p) ">"))

;;;; Default planner

(define *plnpr* #f)

;;; with-plnpr: executes thunk within the given plnpr
(define (with-plnpr plnpr thunk)
  (let ((old-plnpr *plnpr*))
    (dynamic-wind
      (lambda () (set! *plnpr* plnpr))
      thunk
      (lambda () (set! *plnpr* old-plnpr)))))

(define (in-plnpr x) (set! *plnpr* x))

;;; Debugging helper: prints if DEBUGGING is set in the plnpr plist
(define (debug-plnpr msg . args)
  (when (plnpr-plist-get *plnpr* 'DEBUGGING)
    (display msg)
    (for-each display args)
    (newline)))

;;;; Plist helpers (alist-based property list on plnpr)

(define (plnpr-plist-get plnpr key)
  (let ((entry (assq key (plnpr-plist plnpr))))
    (if entry (cdr entry) #f)))

(define (plnpr-plist-set! plnpr key value)
  (let ((entry (assq key (plnpr-plist plnpr))))
    (if entry
        (set-cdr! entry value)
        (set-plnpr-plist! plnpr
                          (cons (cons key value) (plnpr-plist plnpr))))))

;;;; Creating a planning problem

(define (create-planning-problem title basis-set)
  (let ((plnpr (make-plnpr title
                           (create-atre (string-append "ATRE(" title ")"))
                           basis-set
                           '()    ; operators
                           '()))) ; plist
    (in-plnpr plnpr)))

(define (setup-choice-sets . args)
  (let ((*plnpr* (if (null? args) *plnpr* (car args))))
    (let ((informant (string->symbol
                      (string-append "BASIS SET(" (plnpr-title *plnpr*) ")"))))
      (with-atre (plnpr-atre *plnpr*)
        (lambda ()
          (for-each (lambda (choice-set)
                      (for-each (lambda (choice)
                                  (assume-if-needed choice informant))
                                choice-set))
                    (plnpr-basis-set *plnpr*))
          (run-rules))))))

(define (set-debug-plnpr state . args)
  (let ((*plnpr* (if (null? args) *plnpr* (car args))))
    (plnpr-plist-set! *plnpr* 'DEBUGGING state)))

;;;; Defining operators

(define-record-type <operator>
  (make-operator form preconditions add-list delete-list)
  operator?
  (form operator-form set-operator-form!)
  (preconditions operator-preconditions set-operator-preconditions!)
  (add-list operator-add-list set-operator-add-list!)
  (delete-list operator-delete-list set-operator-delete-list!))

(define (print-operator n)
  (string-append "<Operator " (->string (operator-form n)) ">"))

;;; def-operator: registers an operator and creates a rule
;;; that determines when it is applicable.
;;; Keywords are provided as an alist:
;;;   form         - the operator form (e.g. (move ?x ?y))
;;;   preconditions - list of precondition patterns
;;;   add-list     - facts to add
;;;   delete-list  - facts to delete
;;;   test         - optional additional test (a procedure)
;;;
;;; In the original CL code this was a macro (DefOperator).
;;; In Scheme we provide it as a procedure that must be called
;;; at load time with quoted data.

(define (def-operator form preconditions add-list delete-list . rest)
  (let ((test (if (null? rest) #f (car rest))))
    (let ((entry (assoc (car form) (plnpr-operators *plnpr*)))
          (op (make-operator form preconditions add-list delete-list)))
      (if entry
          (set-cdr! entry op)
          (set-plnpr-operators! *plnpr*
                                (cons (cons (car form) op)
                                      (plnpr-operators *plnpr*)))))
    ;; Create rule that determines when the operator is applicable.
    ;; The rule checks that all preconditions hold, and if so asserts
    ;; (applicable <form>).
    ;; NOTE: In the original CL this used the `rule` macro with :INTERN.
    ;; In Scheme, we construct the rule programmatically via the ATRE
    ;; rule interface. The caller may need to set up rules separately
    ;; if the ATRE rule macro is not available as a procedure.
    ;; For now we provide a hook that can be overridden.
    (when *def-operator-rule-installer*
      (*def-operator-rule-installer* form preconditions test))))

;; Hook for installing rules; set this to a procedure that creates
;; the appropriate ATRE rule for operator applicability.
(define *def-operator-rule-installer* #f)

;;;; Finding and applying operators

(define (find-applicable-operators state . args)
  (let ((*plnpr* (if (null? args) *plnpr* (car args))))
    (let ((result '()))
      (for-each (lambda (candidate)
                  (when (in? candidate state)
                    (set! result (cons (cadr candidate) result))))
                (fetch '(applicable ?x) (plnpr-atre *plnpr*)))
      result)))

(define (fetch-operator op-name)
  (let ((entry (assoc op-name (plnpr-operators *plnpr*))))
    (if entry (cdr entry) #f)))

;;;; Applying an operator to a state

;;; sublis: substitute bindings in an expression
;;; bindings is an alist of (var . val) pairs
(define (sublis bindings expr)
  (cond ((null? expr) '())
        ((pair? expr)
         (cons (sublis bindings (car expr))
               (sublis bindings (cdr expr))))
        (else
         (let ((entry (assq expr bindings)))
           (if entry (cdr entry) expr)))))

(define (apply-operator state op-inst)
  (let ((operator (fetch-operator (car op-inst)))
        (vals (cdr op-inst))
        (assumptions (env-assumptions state))
        (atms (atre-atms *atre*)))
    ;; First substitute the values for the variables and create
    ;; the appropriate add list and delete list
    (let* ((bindings (map cons
                          (cdr (operator-form operator))
                          vals))
           (add-list (sublis bindings (operator-add-list operator)))
           (delete-list (sublis bindings (operator-delete-list operator))))
      (debug-plnpr "\n   Applying " (->string op-inst) " to "
                   (->string state) ".")
      (debug-plnpr "\n      Add list: " (->string add-list))
      (debug-plnpr "\n      Delete list: " (->string delete-list))
      ;; Remove delete-list assumptions
      (let ((assumptions
             (filter (lambda (a)
                       (not (member (datum-lisp-form (tms-node-datum a))
                                    delete-list
                                    equal?)))
                     assumptions)))
        ;; Add new assumptions from add-list
        (let loop ((new-items add-list) (assumptions assumptions))
          (if (null? new-items)
              (find-or-make-env assumptions atms)
              (loop (cdr new-items)
                    (ordered-insert (get-tms-node (car new-items))
                                   assumptions
                                   assumption-order))))))))

;;;; Examining problem-related information

(define (fetch-states facts . args)
  (let ((*plnpr* (if (null? args) *plnpr* (car args))))
    (solutions (plnpr-atre *plnpr*)
               (append (map list facts)
                       (plnpr-basis-set *plnpr*)))))

(define (satisfies-goal? state goals . args)
  (let ((*plnpr* (if (null? args) *plnpr* (car args))))
    (call-with-current-continuation
     (lambda (return)
       (with-atre (plnpr-atre *plnpr*)
         (lambda ()
           (check-goals goals state '() return)))
       (values #f #f)))))

(define (check-goals goals state bindings return)
  (cond ((null? goals)
         ;; Accept any solution
         (return (values #t bindings)))
        (else
         (for-each (lambda (candidate)
                     (when (in? candidate state)
                       (let ((new-bindings
                              (unify (car goals) candidate bindings)))
                         (when (not (eq? new-bindings ':FAIL))
                           (check-goals (cdr goals) state
                                        new-bindings return)))))
                   (fetch (car goals))))))

;;;; Showing plans

(define (show-plan plan)
  (let loop ((steps (reverse plan)))
    (when (pair? steps)
      (print-env (car steps))
      (when (and (pair? (cdr steps)) (cadr steps))
        (display "\n  then, by ")
        (display (->string (cadr steps)))
        (display ", "))
      (loop (cddr steps)))))
