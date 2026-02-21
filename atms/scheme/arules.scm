;;; -*- Mode: Scheme; -*-

;;; Tiny Rule Engine, ATMS interface: Rules module
;;; Translated to Scheme (R7RS + SRFI-9) from arules.lisp
;; Last edited: 1/29/93, KDF

;; Copyright (c) 1992, Kenneth D. Forbus, Northwestern
;; University, and Johan de Kleer, the Xerox Corporation
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;;; Global variables (dynamic/special in CL, module-level in Scheme)

(define *bound-vars* '())
(define *rule-procedures* '())
(define *in-nodes* '())
(define *imp-nodes* '())

(define *file-counter* 0)
(define *file-prefix* "")

;;;; Rule record type
;;; Fields: counter, atre, dbclass, matcher, body, in-nodes, imp-nodes

(define-record-type <rule>
  (make-rule-record counter atre dbclass matcher body in-nodes imp-nodes)
  rule?
  (counter rule-counter set-rule-counter!)
  (atre rule-atre set-rule-atre!)
  (dbclass rule-dbclass set-rule-dbclass!)
  (matcher rule-matcher set-rule-matcher!)
  (body rule-body set-rule-body!)
  (in-nodes rule-in-nodes set-rule-in-nodes!)
  (imp-nodes rule-imp-nodes set-rule-imp-nodes!))

(define (print-rule-struct r)
  (string-append "<Rule " (number->string (rule-counter r)) ">"))

;;;; Rule-File: resets the file counter and prefix for rule procedure naming.

(define (rule-file prefix)
  (set! *file-counter* 0)
  (set! *file-prefix* prefix))

;;;; Parsing triggers
;;; In CL these are macros doing compile-time code generation.
;;; In Scheme we translate them as runtime rule registration helpers.
;;; <condition> = 'INTERN | 'IN | 'IMPLIED-BY
;;; Trigger syntax is:
;;; (<pattern1> . <options for pattern1>
;;;     <pattern2> <options for pattern2> ...)
;;; and <options> can be empty, or
;;; :TEST <code> and/or :VAR <var>, where <code> must be
;;; non-nil for the match to succeed, and <var> will be
;;; bound to the whole pattern.

(define (parse-triggers trigger-list)
  (cond ((null? trigger-list) '())
        (else
         (call-with-values
          (lambda ()
            (parse-trigger-options (cdr trigger-list) #f #f))
          (lambda (var test new-triggers)
            (cons (list (car trigger-list) var test)
                  (parse-triggers new-triggers)))))))

(define (parse-trigger-options triggers var test)
  (cond ((null? triggers) (values var test triggers))
        ((eq? (car triggers) ':VAR)
         (parse-trigger-options (cddr triggers) (cadr triggers) test))
        ((eq? (car triggers) ':TEST)
         (parse-trigger-options (cddr triggers) var (cadr triggers)))
        (else (values var test triggers))))

;;;; Orchestrating rule construction
;;; In CL, do-rule / build-rule produce s-expression code at macro-expansion
;;; time that is then compiled.  In Scheme we translate the macro-based code
;;; generation into a runtime registration path.  Each "rule" call produces
;;; a matcher procedure and a body procedure, then calls insert-rule.

(define (do-rule condition triggers body)
  ;; Build one layer per trigger, innermost first.
  ;; Returns a thunk that, when called, registers the rule tree.
  (let ((*rule-procedures* '())
        (*bound-vars* '()))
    (let ((index-form
           (build-rule condition (car triggers)
                       (make-nested-rule condition (cdr triggers) body))))
      ;; In CL this returns `(progn ,@*rule-procedures* ,index-form).
      ;; Here we just evaluate: first define all generated procedures, then
      ;; call insert-rule.  Since we are at runtime the procedures are
      ;; already closures; index-form is the final insert-rule call.
      (for-each (lambda (proc) (proc)) *rule-procedures*)
      index-form)))

(define (make-nested-rule condition triggers body)
  (cond ((null? triggers) body)
        (else
         (list (lambda ()
                 (add-internal-rule condition (car triggers)
                                    (make-nested-rule condition
                                                      (cdr triggers)
                                                      body)))))))

(define (add-internal-rule condition trigger body)
  (build-rule condition trigger body))

;;;; Building rules
;;; Generates matcher and body closures, then calls insert-rule.

(define (build-rule condition trigger body)
  (let ((pattern (car trigger))
        (var (cadr trigger))
        (test (caddr trigger)))
    (let ((match-procedure
           (generate-match-procedure pattern var test condition))
          (body-procedure
           (generate-body-procedure pattern condition var body)))
      ;; In CL these were pushed onto *rule-procedures* as defun forms.
      ;; Here they are already closures.
      (insert-rule
       (get-dbclass (get-trigger-dbclass pattern))
       match-procedure
       body-procedure
       *in-nodes*
       *imp-nodes*))))

(define (get-trigger-dbclass trigger)
  (cond ((null? trigger) (error "Null trigger in ATRE rule"))
        ((variable? trigger)
         (if (member trigger *bound-vars*)
             trigger
             (error "Trigger dbclass is unbound")))
        ((symbol? trigger) trigger)
        ((pair? trigger) (get-trigger-dbclass (car trigger)))
        (else (error "ATRE rule trigger must be symbol or list"))))

;;;; Generating the body procedure
;;; In CL, generate-body-procedure emits a defun form.
;;; In Scheme we return a closure directly.

(define (generate-body-procedure pattern condition var body)
  ;; The body procedure is called with the free variables bound from
  ;; the match.  For :IN and :IMPLIED-BY conditions, the first argument
  ;; is the trigger-node; the body only fires if the trigger-node's
  ;; label is non-empty (i.e., "believed").
  ;; For :INTERN, the body fires unconditionally.
  (let* ((newly-bound (pattern-free-variables pattern))
         (newly-bound (if var (cons var newly-bound) newly-bound)))
    ;; Expand the body with the extended variable bindings.
    ;; In CL this uses with-pushed-variable-bindings + fully-expand-body;
    ;; in Scheme, body is already a list of Scheme forms (procedures/thunks),
    ;; so we just pass it through.
    (cond
     ((eq? condition 'INTERN)
      ;; No trigger node; body is called with just the match bindings.
      (lambda args
        (for-each (lambda (b) (b)) (if (pair? body) body (list body)))))
     ((or (eq? condition 'IN) (eq? condition 'IMPLIED-BY))
      ;; First argument is trigger-node; remaining are match bindings.
      (lambda (trigger-node . rest-args)
        (cond ((not (null? (tms-node-label trigger-node)))
               ;; The trigger node is believed; fire the body.
               (apply-body body trigger-node rest-args))
              (else
               ;; Not yet believed; cache this rule invocation on the node
               ;; so it fires when the node becomes believed.
               (set-tms-node-rules!
                trigger-node
                (cons (cons (generate-body-procedure pattern condition var body)
                            (cons trigger-node rest-args))
                      (tms-node-rules trigger-node)))))))
     (else (error "Bad condition in generate-body-procedure" condition)))))

;;; Helper to apply body forms.  In the CL version the body is a list of
;;; forms that get spliced into the defun.  In Scheme the body list typically
;;; contains thunks or procedures to call.
(define (apply-body body trigger-node args)
  (cond ((procedure? body) (apply body (cons trigger-node args)))
        ((pair? body)
         (for-each (lambda (form)
                     (if (procedure? form)
                         (apply form (cons trigger-node args))
                         form))
                   body))
        (else body)))

;;;; Generating the match procedure
;;; In CL, this produces a defun that returns (values T bindings condition)
;;; or (values NIL NIL NIL).  In Scheme we return a procedure that returns
;;; three values via `values`.

(define (generate-match-procedure pattern var test condition)
  (call-with-values
   (lambda ()
     (generate-match-body pattern (pattern-free-variables pattern) test))
   (lambda (tests binding-specs)
     ;; Return the matcher closure.
     ;; P is the datum's lisp-form (the pattern to test against).
     (lambda (p . bound-var-vals)
       ;; Evaluate structure/equality tests.  The tests are
       ;; s-expressions in the CL version; here they must be
       ;; closures or we use the open-coded match helpers from funify.
       ;; We rely on the evaluator: tests are predicates on P.
       (let ((match-ok? (run-match-tests tests p)))
         (if match-ok?
             (values #t
                     (if (and (not var) (null? binding-specs))
                         #f
                         (let ((bindings '()))
                           (when var (set! bindings (list p)))
                           (set! bindings
                                 (append bindings
                                         (reverse
                                          (map (lambda (spec) (spec p))
                                               binding-specs))))
                           bindings))
                     condition)
             (values #f #f #f)))))))

;;; Run the list of match tests against datum P.
;;; Each test is a procedure P -> boolean.
(define (run-match-tests tests p)
  (cond ((null? tests) #t)
        ((not ((car tests) p)) #f)
        (else (run-match-tests (cdr tests) p))))

;;;; Scratchout: remove elements of l1 from l2, non-destructive, order-preserving.

(define (scratchout l1 l2)
  (let loop ((remaining l1) (result l2))
    (if (null? remaining)
        result
        (loop (cdr remaining)
              (remove-item (car remaining) result)))))

(define (remove-item item lst)
  (cond ((null? lst) '())
        ((equal? item (car lst)) (remove-item item (cdr lst)))
        (else (cons (car lst) (remove-item item (cdr lst))))))

;;;; Generate a unique name for a rule procedure (used for debugging/display).

(define (generate-rule-procedure-name pattern)
  (set! *file-counter* (+ *file-counter* 1))
  (string->symbol
   (string-append (datum->string *file-prefix*)
                  "-"
                  (datum->string pattern)
                  "-"
                  (number->string *file-counter*))))

;;;; Recursive macro expansion
;;; In CL, fully-expand-body recursively macro-expands known macros.
;;; In Scheme there are no macros to expand at runtime; the body is
;;; already composed of procedures.  We keep the function as an identity
;;; pass-through for structural compatibility.

(define *macros-to-expand*
  '(rule internal-rule add-internal-rule with-pushed-variable-bindings
    rlet rassert! rnogood! with-focus with-ATRE))

(define (fully-expand-body body)
  ;; In the CL version this walks the body tree and expands known macros.
  ;; In Scheme rules are built at runtime with closures, so no macro
  ;; expansion is needed.  Return body unchanged.
  body)

;;;; Running rules

(define (insert-rule dbclass matcher body in-nodes imp-nodes)
  (let* ((atre (dbclass-atre dbclass))
         (counter (+ 1 (atre-rule-counter atre)))
         (rule (make-rule-record counter atre dbclass matcher body
                                 in-nodes imp-nodes)))
    (set-atre-rule-counter! atre counter)
    ;; Index the rule
    (set-atre-rules! atre (cons rule (atre-rules atre)))
    (set-dbclass-rules! dbclass (cons rule (dbclass-rules dbclass)))
    ;; Try the rule on all existing facts of this dbclass
    (for-each (lambda (candidate) (try-rule-on rule candidate))
              (dbclass-facts dbclass))
    rule))

(define (try-rules datum)
  (for-each (lambda (rule) (try-rule-on rule datum))
            (dbclass-rules (datum-dbclass datum))))

(define (try-rule-on rule datum)
  (let ((a (datum-atre datum)))
    (call-with-values
     (lambda () ((rule-matcher rule) (datum-lisp-form datum)))
     (lambda (okay? bindings condition)
       (when okay?
         (when (or (eq? condition 'IN) (eq? condition 'IMPLIED-BY))
           (set! bindings (cons (datum-tms-node datum) bindings)))
         (enqueue
          (list (rule-body rule)
                bindings
                (cond
                 ((eq? condition 'IN)
                  (cons (cons (datum-tms-node datum)
                              (rule-in-nodes rule))
                        (rule-imp-nodes rule)))
                 ((eq? condition 'IMPLIED-BY)
                  (cons (rule-in-nodes rule)
                        (cons (datum-tms-node datum)
                              (rule-imp-nodes rule))))
                 ((eq? condition 'INTERN)
                  (cons (rule-in-nodes rule)
                        (rule-imp-nodes rule)))
                 (else (error "Unknown rule condition" condition))))
          a))))))

(define (run-rules . rest)
  (let ((atre (if (pair? rest) (car rest) *atre*)))
    ;; Append in-rules to the main queue
    (set-atre-queue! atre (append (atre-queue atre) (atre-in-rules atre)))
    (set-atre-in-rules! atre '())
    ;; Process the queue
    (let loop ((form (dequeue atre))
               (counter 0))
      (cond ((not form)
             (debugging-atre-msg atre
                                 (string-append "\n    "
                                                (number->string counter)
                                                " rules run."))
             (let ((total (+ (atre-rules-run atre) counter)))
               (set-atre-rules-run! atre total)
               (values counter total)))
            (else
             (execute-rule form atre)
             (loop (dequeue atre) (+ counter 1)))))))

;;;; Executing rules, checking for appropriate conditions

(define (execute-rule queued-rule atre)
  ;; queued-rule is (<procedure> <arguments> <node-list>)
  ;; Check the node list before executing, to make sure
  ;; all the belief conditions are satisfied.
  (let ((*in-nodes* (car (caddr queued-rule)))
        (*imp-nodes* (cdr (caddr queued-rule))))
    (call-with-current-continuation
     (lambda (return)
       (unless (in-triggers-ready? *in-nodes* atre)
         ;; Re-queue under ATRE for checking.
         (set-atre-in-rules! atre (cons queued-rule (atre-in-rules atre)))
         (return #f))
       (unless (implied-by-triggers-ready? *imp-nodes* atre)
         (set-atre-imp-rules! atre (cons queued-rule (atre-imp-rules atre)))
         (return #f))
       ;; Let's do it
       (apply (car queued-rule) (cadr queued-rule))))))

(define (in-triggers-ready? nodes atre . rest)
  (let ((env (if (pair? rest) (car rest)
                 (atms-empty-env (atre-atms atre)))))
    (cond ((env-nogood? env) #f)          ; Combination was nogood
          ((null? nodes) #t)              ; Nothing else to combine
          (else
           (let loop ((label-envs (tms-node-label (car nodes))))
             (cond ((null? label-envs) #f)
                   (else
                    (let ((u (union-env (car label-envs) env)))
                      (if (in-triggers-ready? (cdr nodes) atre u)
                          #t
                          (loop (cdr label-envs)))))))))))

(define (implied-by-triggers-ready? nodes atre)
  (or (null? nodes)   ; No triggers, no problem
      (and (focus-okay? atre)
           (every (lambda (n) (in-node? n (atre-focus atre)))
                  nodes))))

(define (rules-waiting? atre) (atre-queue atre))

(define (enqueue new a)
  (set-atre-queue! a (cons new (atre-queue a))))

(define (dequeue atre)
  (if (pair? (atre-queue atre))
      (let ((item (car (atre-queue atre))))
        (set-atre-queue! atre (cdr (atre-queue atre)))
        item)
      #f))

;;;; Debugging helper

(define (debugging-atre-msg atre msg)
  (when (atre-debugging atre)
    (display msg)
    (newline)))

;;;; Display routines

(define (show-rules . rest)
  (let ((atre (if (pair? rest) (car rest) *atre*))
        (stream (if (and (pair? rest) (pair? (cdr rest)))
                    (cadr rest)
                    (current-output-port))))
    (let ((counter 0)
          (dist '()))
      (for-each
       (lambda (dbclass)
         (let ((inc (length (dbclass-rules dbclass))))
           (when (> inc 0)
             (set! dist (cons (cons (dbclass-name dbclass) inc) dist))
             (set! counter (+ counter inc)))))
       (atre-dbclasses atre))
      (let ((in (length (atre-in-rules atre)))
            (imp (length (atre-imp-rules atre)))
            (queued (length (atre-queue atre))))
        (set! counter (+ in imp counter))
        (display (string-append "\n " (datum->string (atre-title atre))
                                " has " (number->string counter)
                                " rules in all.")
                 stream)
        (display (string-append "\n  "
                                (if (> queued 0)
                                    (number->string queued)
                                    "None")
                                " queued.")
                 stream)
        (if (> (+ in imp) 0)
            (display (string-append "  Pending: "
                                    (if (> in 0) (number->string in) "No")
                                    " in, "
                                    (if (> imp 0) (number->string imp) "No")
                                    " implied-by.")
                     stream)
            (display "  None pending." stream))
        (when (pair? dist)
          (display "\n Cached under dbclasses:" stream)
          (for-each (lambda (entry)
                      (display (string-append "\n    "
                                              (datum->string (car entry))
                                              ": "
                                              (number->string (cdr entry)))
                               stream))
                    dist))
        atre))))

(define (print-rules . rest)
  (let ((atre (if (pair? rest) (car rest) *atre*))
        (stream (if (and (pair? rest) (pair? (cdr rest)))
                    (cadr rest)
                    (current-output-port))))
    (let ((counter 0))
      (display (string-append "\nThe rules in "
                              (datum->string (atre-title atre))
                              " are:")
               stream)
      (for-each (lambda (r)
                  (set! counter (+ counter 1))
                  (print-rule r stream))
                (atre-rules atre))
      counter)))

(define (print-rule rule . rest)
  (let ((stream (if (pair? rest) (car rest) (current-output-port))))
    (display (string-append "\n " (print-rule-struct rule)
                            ": " (datum->string (rule-matcher rule))
                            ", " (datum->string (rule-body rule)))
             stream)))

(define (get-rule num . rest)
  (let ((atre (if (pair? rest) (car rest) *atre*)))
    (call-with-current-continuation
     (lambda (return)
       (for-each (lambda (rule)
                   (when (= (rule-counter rule) num)
                     (return rule)))
                 (atre-rules atre))
       #f))))
