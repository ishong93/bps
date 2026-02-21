;;; -*- Mode: Scheme -*-

;;; Assumption-based truth maintenance system, version 61 of 7/21/92.
;;; Translated to Scheme (R7RS + SRFI-9) from Common Lisp.

;;; Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(define-record-type <atms>
  (make-atms-record title node-counter just-counter env-counter
                    nodes justs contradictions assumptions
                    debugging nogood-table contra-node env-table
                    empty-env node-string enqueue-procedure)
  atms?
  (title atms-title set-atms-title!)
  (node-counter atms-node-counter set-atms-node-counter!)
  (just-counter atms-just-counter set-atms-just-counter!)
  (env-counter atms-env-counter set-atms-env-counter!)
  (nodes atms-nodes set-atms-nodes!)
  (justs atms-justs set-atms-justs!)
  (contradictions atms-contradictions set-atms-contradictions!)
  (assumptions atms-assumptions set-atms-assumptions!)
  (debugging atms-debugging set-atms-debugging!)
  (nogood-table atms-nogood-table set-atms-nogood-table!)
  (contra-node atms-contra-node set-atms-contra-node!)
  (env-table atms-env-table set-atms-env-table!)
  (empty-env atms-empty-env set-atms-empty-env!)
  (node-string atms-node-string set-atms-node-string!)
  (enqueue-procedure atms-enqueue-procedure set-atms-enqueue-procedure!))

(define (print-atms atms)
  (string-append "#<ATMS: " (atms-title atms) ">"))

(define-record-type <tms-node>
  (make-tms-node-record index datum label justs consequences
                        contradictory? assumption? rules atms)
  tms-node?
  (index tms-node-index set-tms-node-index!)
  (datum tms-node-datum set-tms-node-datum!)
  (label tms-node-label set-tms-node-label!)
  (justs tms-node-justs set-tms-node-justs!)
  (consequences tms-node-consequences set-tms-node-consequences!)
  (contradictory? tms-node-contradictory? set-tms-node-contradictory?!)
  (assumption? tms-node-assumption? set-tms-node-assumption?!)
  (rules tms-node-rules set-tms-node-rules!)
  (atms tms-node-atms set-tms-node-atms!))

(define (print-tms-node node)
  (if (tms-node-assumption? node)
      (string-append "A-" (number->string (tms-node-index node)))
      (string-append "#<NODE: " (node-string node) ">")))

(define-record-type <just>
  (make-just-record index informant consequence antecedents)
  just?
  (index just-index set-just-index!)
  (informant just-informant set-just-informant!)
  (consequence just-consequence set-just-consequence!)
  (antecedents just-antecedents set-just-antecedents!))

(define (print-just j)
  (string-append "<" (just-informant j) " "
                 (number->string (just-index j)) ">"))

(define-record-type <env>
  (make-env-record index count assumptions nodes nogood? rules)
  env?
  (index env-index set-env-index!)
  (count env-count set-env-count!)
  (assumptions env-assumptions set-env-assumptions!)
  (nodes env-nodes set-env-nodes!)
  (nogood? env-nogood? set-env-nogood?!)
  (rules env-rules set-env-rules!))

(define (print-env-structure env)
  (string-append "E-" (number->string (env-index env))))

;;; Helpers

(define (node-string node)
  ((atms-node-string (tms-node-atms node)) node))

(define (debugging atms msg . args)
  (when (atms-debugging atms)
    (display msg (current-error-port))
    (for-each (lambda (a) (display a (current-error-port))) args)
    (newline (current-error-port))))

(define (default-node-string n)
  (let ((datum (tms-node-datum n)))
    (cond ((string? datum) datum)
          ((number? datum) (number->string datum))
          ((symbol? datum) (symbol->string datum))
          (else (let ((p (open-output-string)))
                  (display datum p)
                  (get-output-string p))))))

(define (datum->string datum)
  (cond ((string? datum) datum)
        ((number? datum) (number->string datum))
        ((symbol? datum) (symbol->string datum))
        (else (let ((p (open-output-string)))
                (display datum p)
                (get-output-string p)))))

(define (ordered-insert item lst test)
  (cond ((null? lst) (list item))
        ((test item (car lst)) (cons item lst))
        ((eq? item (car lst)) lst)
        (else (cons (car lst) (ordered-insert item (cdr lst) test)))))

(define (assumption-order a1 a2)
  (< (tms-node-index a1) (tms-node-index a2)))

(define (env-order e1 e2)
  (< (env-index e1) (env-index e2)))

;;; Helper: remove first occurrence of item from list using test
(define (remove-first item lst test)
  (cond ((null? lst) '())
        ((test item (car lst)) (cdr lst))
        (else (cons (car lst) (remove-first item (cdr lst) test)))))

;;; Helper: remove all occurrences matching test
(define (remove-all item lst test)
  (cond ((null? lst) '())
        ((test item (car lst)) (remove-all item (cdr lst) test))
        (else (cons (car lst) (remove-all item (cdr lst) test)))))

;;; Helper: filter out #f from a list (used for rplaca nil pattern)
(define (remove-nulls lst)
  (filter (lambda (x) x) lst))

;;; Helper: filter
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

;;; Helper: any (like CL some)
(define (any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) (pred (car lst)))
        (else (any pred (cdr lst)))))

;;; Helper: every
(define (every pred lst)
  (cond ((null? lst) #t)
        ((not (pred (car lst))) #f)
        (else (every pred (cdr lst)))))

;;; Helper: append-map (like CL mapcan)
(define (append-map f lst)
  (apply append (map f lst)))

;;; Helper: subsetp -- is every element of s1 in s2?
(define (subsetp s1 s2)
  (cond ((null? s1) #t)
        ((memq (car s1) s2) (subsetp (cdr s1) s2))
        (else #f)))

;;; Helper: list-copy
(define (list-copy lst)
  (if (null? lst) '()
      (cons (car lst) (list-copy (cdr lst)))))

;;; Helper: assoc with custom test
(define (assoc-test key alist test)
  (cond ((null? alist) #f)
        ((test key (caar alist)) (car alist))
        (else (assoc-test key (cdr alist) test))))

;;; Incf helper: increments counter and returns new value
(define (atms-node-counter-incf! atms)
  (let ((new-val (+ 1 (atms-node-counter atms))))
    (set-atms-node-counter! atms new-val)
    new-val))

(define (atms-just-counter-incf! atms)
  (let ((new-val (+ 1 (atms-just-counter atms))))
    (set-atms-just-counter! atms new-val)
    new-val))

(define (atms-env-counter-incf! atms)
  (let ((new-val (+ 1 (atms-env-counter atms))))
    (set-atms-env-counter! atms new-val)
    new-val))

;;; Basic inference engine interface.

(define (create-atms title . options)
  (let ((node-string-fn default-node-string)
        (debugging-flag #f)
        (enqueue-proc #f))
    ;; Parse keyword-style options: 'node-string fn 'debugging bool 'enqueue-procedure fn
    (let loop ((opts options))
      (cond ((null? opts) #t)
            ((and (pair? opts) (pair? (cdr opts)))
             (cond ((eq? (car opts) 'node-string)
                    (set! node-string-fn (cadr opts)))
                   ((eq? (car opts) 'debugging)
                    (set! debugging-flag (cadr opts)))
                   ((eq? (car opts) 'enqueue-procedure)
                    (set! enqueue-proc (cadr opts))))
             (loop (cddr opts)))
            (else (error "Odd number of keyword arguments to create-atms"))))
    (let ((atms (make-atms-record title 0 0 0
                                  '() '() '() '()
                                  debugging-flag '() #f '()
                                  #f node-string-fn enqueue-proc)))
      (set-atms-contra-node! atms
        (tms-create-node atms "The contradiction" 'contradictoryp #t))
      (set-atms-empty-env! atms (create-env atms '()))
      atms)))

(define (change-atms atms . options)
  (let loop ((opts options))
    (cond ((null? opts) #t)
          ((and (pair? opts) (pair? (cdr opts)))
           (cond ((eq? (car opts) 'node-string)
                  (set-atms-node-string! atms (cadr opts)))
                 ((eq? (car opts) 'debugging)
                  (set-atms-debugging! atms (cadr opts)))
                 ((eq? (car opts) 'enqueue-procedure)
                  (set-atms-enqueue-procedure! atms (cadr opts))))
           (loop (cddr opts)))
          (else (error "Odd number of keyword arguments to change-atms")))))

(define (true-node? node)
  (and (pair? (tms-node-label node))
       (eq? (car (tms-node-label node))
            (atms-empty-env (tms-node-atms node)))))

(define (in-node? n . rest)
  (let ((env (if (pair? rest) (car rest) #f)))
    (if env
        (any (lambda (le) (subset-env? le env))
             (tms-node-label n))
        (not (null? (tms-node-label n))))))

(define (out-node? n env) (not (in-node? n env)))

(define (node-consistent-with? n env)
  (any (lambda (le) (not (env-nogood? (union-env le env))))
       (tms-node-label n)))

(define (tms-create-node atms datum . options)
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
                 (atms-node-counter-incf! atms)
                 datum '() '() '()
                 contradictoryp assumptionp '() atms)))
      (set-atms-nodes! atms (cons node (atms-nodes atms)))
      (when contradictoryp
        (set-atms-contradictions! atms (cons node (atms-contradictions atms))))
      (when assumptionp
        (set-atms-assumptions! atms (cons node (atms-assumptions atms)))
        (set-tms-node-label! node (list (create-env atms (list node)))))
      node)))

(define (assume-node node)
  (when (not (tms-node-assumption? node))
    (let ((atms (tms-node-atms node)))
      (debugging atms
                 (string-append "\nConverting " (node-string node)
                                " into an assumption"))
      (set-tms-node-assumption?! node #t)
      (set-atms-assumptions! atms (cons node (atms-assumptions atms)))
      (update (list (create-env atms (list node)))
              node
              'assume-node))))

(define (make-contradiction node)
  (let ((atms (tms-node-atms node)))
    (when (not (tms-node-contradictory? node))
      (set-tms-node-contradictory?! node #t)
      (set-atms-contradictions! atms (cons node (atms-contradictions atms)))
      (let loop ()
        (let ((nogood (if (pair? (tms-node-label node))
                          (car (tms-node-label node))
                          #f)))
          (if nogood
              (begin (new-nogood atms nogood 'make-contradiction)
                     (loop))
              #f))))))

(define (justify-node informant consequence antecedents)
  (let* ((atms (tms-node-atms consequence))
         (just (make-just-record (atms-just-counter-incf! atms)
                                 informant consequence antecedents)))
    (set-tms-node-justs! consequence (cons just (tms-node-justs consequence)))
    (for-each (lambda (node)
                (set-tms-node-consequences! node
                  (cons just (tms-node-consequences node))))
              antecedents)
    (set-atms-justs! atms (cons just (atms-justs atms)))
    (debugging atms
               (string-append "\nJustifying " (node-string consequence)
                              " in terms of " (datum->string informant)
                              " on "
                              (let ((p (open-output-string)))
                                (display (map node-string antecedents) p)
                                (get-output-string p))))
    (propagate just #f (list (atms-empty-env atms)))
    just))

(define (nogood-nodes informant nodes)
  (justify-node informant
                (atms-contra-node (tms-node-atms (car nodes)))
                nodes))

;;; Label updating

(define (propagate just antecedent envs)
  (let ((new-envs (weave antecedent envs (just-antecedents just))))
    (when new-envs
      (update new-envs (just-consequence just) just))))

(define (update new-envs consequence just)
  (call-with-current-continuation
   (lambda (return)
     (let ((atms (tms-node-atms consequence)))
       (when (tms-node-contradictory? consequence)
         (for-each (lambda (env) (new-nogood atms env just)) new-envs)
         (return #f))
       (set! new-envs (update-label consequence new-envs))
       (when (null? new-envs) (return #f))
       (let ((enqueuef (atms-enqueue-procedure atms)))
         (when enqueuef
           (for-each (lambda (rule) (enqueuef rule))
                     (tms-node-rules consequence))
           (set-tms-node-rules! consequence '())))
       (for-each
        (lambda (supported-just)
          (propagate supported-just consequence new-envs)
          ;; Filter out envs no longer in the label
          (let loop ((ne new-envs))
            (when (pair? ne)
              (when (not (memq (car ne) (tms-node-label consequence)))
                (set-car! ne #f))
              (loop (cdr ne))))
          (set! new-envs (remove-nulls new-envs))
          (when (null? new-envs) (return #f)))
        (tms-node-consequences consequence))))))

(define (update-label node new-envs)
  (let ((envs (tms-node-label node)))
    ;; For each new-env, check against existing envs
    (let outer ((ne new-envs))
      (when (pair? ne)
        (let inner ((nenvs envs))
          (cond
           ((null? nenvs)
            ;; new-env survived all existing envs, add to envs
            (when (car ne)
              (set! envs (cons (car ne) envs))))
           (else
            (cond
             ((not (car nenvs))
              (inner (cdr nenvs)))
             ((not (car ne))
              ;; already nullified, stop inner loop
              #f)
             (else
              (let ((cmp (compare-env (car ne) (car nenvs))))
                (cond
                 ((or (eq? cmp ':eq) (eq? cmp ':s21))
                  (set-car! ne #f))
                 ((eq? cmp ':s12)
                  (set-env-nodes! (car nenvs)
                    (remove-first node (env-nodes (car nenvs)) eq?))
                  (set-car! nenvs #f))
                 (else
                  (inner (cdr nenvs)))))))))
        (outer (cdr ne))))
    (set! new-envs (remove-nulls new-envs))
    (for-each (lambda (new-env) (set-env-nodes! new-env (cons node (env-nodes new-env))))
              new-envs)
    (set-tms-node-label! node (remove-nulls envs))
    new-envs))

(define (weave antecedent envs antecedents)
  (let ((envs (list-copy envs)))
    (call-with-current-continuation
     (lambda (return)
       (for-each
        (lambda (node)
          (when (not (eq? node antecedent))
            (let ((new-envs '()))
              (for-each
               (lambda (env)
                 (when env
                   (for-each
                    (lambda (node-env)
                      (let ((new-env (union-env env node-env)))
                        (when (not (env-nogood? new-env))
                          ;; Check against existing new-envs for subsumption
                          (let check ((nnew-envs new-envs) (add? #t))
                            (cond
                             ((not add?)
                              ;; was subsumed, don't add
                              #f)
                             ((null? nnew-envs)
                              ;; survived, add it
                              (set! new-envs (cons new-env new-envs)))
                             ((car nnew-envs)
                              (let ((cmp (compare-env new-env (car nnew-envs))))
                                (cond
                                 ((or (eq? cmp ':eq) (eq? cmp ':s21))
                                  ;; new-env is subsumed, don't add
                                  #f)
                                 ((eq? cmp ':s12)
                                  (set-car! nnew-envs #f)
                                  (check (cdr nnew-envs) #t))
                                 (else
                                  (check (cdr nnew-envs) #t)))))
                             (else
                              (check (cdr nnew-envs) #t)))))))
                    (tms-node-label node))))
               envs)
              (set! envs (remove-nulls new-envs))
              (when (null? envs) (return #f)))))
        antecedents)
       envs))))

(define (in-antecedent? nodes)
  (or (null? nodes)
      (weave? (atms-empty-env (tms-node-atms (car nodes))) nodes)))

(define (weave? env nodes)
  (cond ((null? nodes) #t)
        (else
         (let loop ((es (tms-node-label (car nodes))))
           (cond ((null? es) #f)
                 (else
                  (let ((new-env (union-env (car es) env)))
                    (if (not (env-nogood? new-env))
                        (if (weave? new-env (cdr nodes))
                            #t
                            (loop (cdr es)))
                        (loop (cdr es))))))))))

(define (supporting-antecedent? nodes env)
  (let loop ((ns nodes))
    (cond ((null? ns) #t)
          ((not (in-node? (car ns) env)) #f)
          (else (loop (cdr ns))))))

(define (remove-node node)
  (when (pair? (tms-node-consequences node))
    (error "Can't remove node with consequences"))
  (let ((atms (tms-node-atms node)))
    (set-atms-nodes! atms
      (remove-first node (atms-nodes atms) eq?))
    (for-each
     (lambda (just)
       (for-each
        (lambda (ant)
          (set-tms-node-consequences! ant
            (remove-first just (tms-node-consequences ant) eq?)))
        (just-antecedents just)))
     (tms-node-justs node))
    (for-each
     (lambda (env)
       (set-env-nodes! env
         (remove-first node (env-nodes env) eq?)))
     (tms-node-label node))))

;;; Creating and extending environments.

(define (create-env atms assumptions)
  (let ((e (make-env-record (atms-env-counter-incf! atms)
                            assumptions
                            (length assumptions)
                            '() #f '())))
    (set-atms-env-table! atms
      (insert-in-table (atms-env-table atms) e))
    (set-env-contradictory atms e)
    e))

(define (union-env e1 e2)
  (when (> (env-count e1) (env-count e2))
    (let ((tmp e1))
      (set! e1 e2)
      (set! e2 tmp)))
  (let loop ((assumes (env-assumptions e1)) (result e2))
    (cond ((null? assumes) result)
          (else
           (let ((new-result (cons-env (car assumes) result)))
             (if (env-nogood? new-result)
                 new-result  ; return the nogood env (caller checks)
                 (loop (cdr assumes) new-result)))))))

(define (cons-env assumption env)
  (let ((nassumes (ordered-insert assumption
                                  (env-assumptions env)
                                  assumption-order)))
    (or (lookup-env nassumes)
        (create-env (tms-node-atms assumption) nassumes))))

(define (find-or-make-env assumptions atms)
  (if (null? assumptions)
      (atms-empty-env atms)
      ;; Presumes the list of assumptions is ordered properly
      (or (lookup-env assumptions)
          (create-env atms assumptions))))

;;; Env tables.

(define (insert-in-table table env)
  (let* ((count (env-count env))
         (entry (assoc-test count table =)))
    (cond (entry
           (set-cdr! entry (cons env (cdr entry)))
           table)
          (else
           (ordered-insert
            (list count env) table
            (lambda (entry1 entry2)
              (< (car entry1) (car entry2))))))))

(define (lookup-env assumes)
  (let* ((atms (tms-node-atms (car assumes)))
         (entry (assoc-test (length assumes)
                            (atms-env-table atms) =)))
    (if entry
        (let loop ((envs (cdr entry)))
          (cond ((null? envs) #f)
                ((equal? (env-assumptions (car envs)) assumes)
                 (car envs))
                (else (loop (cdr envs)))))
        #f)))

(define (subset-env? e1 e2)
  (cond ((eq? e1 e2) #t)
        ((> (env-count e1) (env-count e2)) #f)
        (else (subsetp (env-assumptions e1) (env-assumptions e2)))))

(define (compare-env e1 e2)
  (cond ((eq? e1 e2) ':eq)
        ((< (env-count e1) (env-count e2))
         (if (subsetp (env-assumptions e1) (env-assumptions e2))
             ':s12
             #f))
        ((subsetp (env-assumptions e2) (env-assumptions e1))
         ':s21)
        (else #f)))

;;; Processing nogoods

(define (new-nogood atms cenv just)
  (debugging atms (string-append "\n  " (print-env-structure cenv)
                                 " new minimal nogood."))
  (set-env-nogood?! cenv just)
  (remove-env-from-labels cenv atms)
  (set-atms-nogood-table! atms
    (insert-in-table (atms-nogood-table atms) cenv))
  (let ((count (env-count cenv)))
    (for-each
     (lambda (entry)
       (when (> (car entry) count)
         (let loop ((envs (cdr entry)))
           (when (pair? envs)
             (when (and (car envs) (subset-env? cenv (car envs)))
               (set-cdr! entry
                 (remove-first (car envs) (cdr entry) eq?)))
             (loop (cdr envs))))))
     (atms-nogood-table atms))
    (for-each
     (lambda (entry)
       (when (> (car entry) count)
         (for-each
          (lambda (old)
            (when (and (not (env-nogood? old))
                       (subset-env? cenv old))
              (set-env-nogood?! old cenv)
              (remove-env-from-labels old atms)))
          (cdr entry))))
     (atms-env-table atms))))

(define (set-env-contradictory atms env)
  (cond ((env-nogood? env) #t)
        (else
         (let ((count (env-count env)))
           (let loop ((entries (atms-nogood-table atms)))
             (cond ((null? entries) #f)
                   ((> (car (car entries)) count) #f)
                   (else
                    (let inner ((cenvs (cdr (car entries))))
                      (cond ((null? cenvs) (loop (cdr entries)))
                            ((subset-env? (car cenvs) env)
                             (set-env-nogood?! env (car cenvs))
                             #t)
                            (else (inner (cdr cenvs))))))))))))

(define (remove-env-from-labels env atms)
  (let ((enqueuef (atms-enqueue-procedure atms)))
    (when enqueuef
      (for-each (lambda (rule) (enqueuef rule))
                (env-rules env))
      (set-env-rules! env '())))
  (for-each
   (lambda (node)
     (set-tms-node-label! node
       (remove-first env (tms-node-label node) eq?)))
   (env-nodes env)))

;;; Interpretation construction

(define (interpretations atms choice-sets . rest)
  (let ((defaults (if (pair? rest) (car rest) #f))
        (solutions-box (list '())))  ; mutable box: (car solutions-box) = solutions list
    (when (atms-debugging atms)
      (display "\n Constructing interpretations depth-first..."
               (current-error-port)))
    (let ((choice-sets
           (map (lambda (alt-set)
                  (append-map (lambda (alt)
                                (list-copy (tms-node-label alt)))
                              alt-set))
                choice-sets)))
      (when (pair? choice-sets)
        (for-each
         (lambda (choice)
           (get-depth-solutions1 choice (cdr choice-sets) solutions-box))
         (car choice-sets)))
      (set-car! solutions-box (remove-nulls (car solutions-box)))
      (when (null? (car solutions-box))
        (if (pair? choice-sets)
            (set-car! solutions-box #f)  ; signal failure
            (set-car! solutions-box (list (atms-empty-env atms)))))
      (when (and defaults (car solutions-box))
        (let ((old-solutions (car solutions-box)))
          (set-car! solutions-box '())
          (for-each
           (lambda (solution)
             (extend-via-defaults solution defaults defaults solutions-box))
           old-solutions)))
      (if (car solutions-box)
          (remove-nulls (car solutions-box))
          '()))))

(define (get-depth-solutions1 solution choice-sets solutions-box)
  (cond ((null? choice-sets)
         ;; Check if subsumed by existing solution
         (let ((subsumed? #f))
           (let loop ((old (car solutions-box)))
             (when (and (pair? old) (not subsumed?))
               (when (car old)
                 (let ((cmp (compare-env (car old) solution)))
                   (cond ((or (eq? cmp ':eq) (eq? cmp ':s12))
                          (set! subsumed? #t))
                         ((eq? cmp ':s21)
                          (set-car! old #f)))))
               (loop (cdr old))))
           (unless subsumed?
             (set-car! solutions-box (cons solution (car solutions-box))))))
        ((env-nogood? solution) #f)  ; something died
        (else
         (for-each
          (lambda (choice)
            (let ((new-solution (union-env solution choice)))
              (when (not (env-nogood? new-solution))
                (get-depth-solutions1 new-solution
                                      (cdr choice-sets)
                                      solutions-box))))
          (car choice-sets)))))

(define (extend-via-defaults solution remaining original solutions-box)
  (let loop ((defaults remaining))
    (cond
     ((null? defaults)
      ;; Check if solution already in *solutions*
      (if (memq solution (car solutions-box))
          #f  ; already there
          ;; Check if all defaults are either in solution or would create nogood
          (let ((should-add? #t))
            (let check ((ds original))
              (when (and (pair? ds) should-add?)
                (if (or (memq (car ds) (env-assumptions solution))
                        (env-nogood? (cons-env (car ds) solution)))
                    (check (cdr ds))
                    (set! should-add? #f))))
            (when should-add?
              (set-car! solutions-box
                (cons solution (car solutions-box)))))))
     (else
      (let ((new-solution (cons-env (car defaults) solution)))
        (if (not (env-nogood? new-solution))
            (extend-via-defaults new-solution (cdr defaults) original solutions-box)
            (loop (cdr defaults))))))))

;;; Generating explanations

(define (explain-node node env)
  (explain-node-1 env node '() '()))

(define (explain-node-1 env node queued-nodes explanation)
  (cond
   ((memq node queued-nodes) #f)
   ((and (tms-node-assumption? node)
         (memq node (env-assumptions env)))
    (cons (cons 'assume node) explanation))
   ;; Check if already explained by a just in explanation
   ((let loop ((justs explanation))
      (cond ((null? justs) #f)
            ((if (pair? (car justs))
                 (eq? (cdr (car justs)) node)
                 (eq? (just-consequence (car justs)) node))
             explanation)
            (else (loop (cdr justs))))))
   (else
    (let ((queued-nodes (cons node queued-nodes)))
      (let loop ((justs (tms-node-justs node)))
        (cond
         ((null? justs) #f)
         (else
          ;; Check if all antecedents are in-node for this just
          (let ((ant-ok? (let check ((ants (just-antecedents (car justs))))
                           (cond ((null? ants) #t)
                                 ((not (in-node? (car ants) env)) #f)
                                 (else (check (cdr ants)))))))
            (if ant-ok?
                ;; Try to build explanation from antecedents
                (let explain-ants ((ants (just-antecedents (car justs)))
                                   (new-explanation explanation))
                  (cond
                   ((null? ants)
                    (cons (car justs) new-explanation))
                   (else
                    (let ((result (explain-node-1 env (car ants)
                                                  queued-nodes new-explanation)))
                      (if result
                          (explain-ants (cdr ants) result)
                          ;; This just failed, try next
                          (loop (cdr justs)))))))
                ;; antecedents not all in node, try next just
                (loop (cdr justs)))))))))))

;;; Printing

(define (why-node node . rest)
  (let ((stream (if (and (pair? rest) (car rest)) (car rest) (current-output-port)))
        (prefix (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) "")))
    (display "\n<" stream)
    (display prefix stream)
    (display (datum->string (tms-node-datum node)) stream)
    (display ",{" stream)
    (for-each (lambda (e) (env-string e stream)) (tms-node-label node))
    (display "}>" stream)))

(define (why-nodes atms . rest)
  (let ((stream (if (pair? rest) (car rest) (current-output-port))))
    (for-each (lambda (n) (why-node n stream))
              (reverse (atms-nodes atms)))))

(define (node-justifications node . rest)
  (let ((stream (if (pair? rest) (car rest) (current-output-port))))
    (display "\n For " stream)
    (display (node-string node) stream)
    (display ":" stream)
    (for-each (lambda (j) (print-justification j stream))
              (tms-node-justs node))))

(define (print-justification j . rest)
  (let ((stream (if (pair? rest) (car rest) (current-output-port))))
    (display "\n  " stream)
    (display (just-informant j) stream)
    (display ", " stream)
    (for-each (lambda (a) (why-node a stream "     "))
              (just-antecedents j))))

(define (e atms n)
  (call-with-current-continuation
   (lambda (return)
     (for-each
      (lambda (bucket)
        (for-each
         (lambda (env)
           (when (= (env-index env) n)
             (return env)))
         (cdr bucket)))
      (atms-env-table atms))
     #f)))

(define (print-env env . rest)
  (let ((stream (if (pair? rest) (car rest) (current-output-port))))
    (display "\n" stream)
    (display (print-env-structure env) stream)
    (display ":" stream)
    (display (if (env-nogood? env) "* " " ") stream)
    (env-string env stream)))

(define (env-string env . rest)
  (let ((stream (if (pair? rest) (car rest) (current-output-port))))
    (let ((assumptions (env-assumptions env)))
      (let ((printer (if (pair? assumptions)
                         (atms-node-string (tms-node-atms (car assumptions)))
                         #f)))
        (let ((strings (map (lambda (a) (printer a)) assumptions)))
          (let ((sorted (sort-strings strings)))
            (display "{" stream)
            (let loop ((ss sorted) (first? #t))
              (when (pair? ss)
                (unless first? (display "," stream))
                (display (car ss) stream)
                (loop (cdr ss) #f)))
            (display "}" stream)))))))

;;; Helper: sort strings lexicographically
(define (sort-strings lst)
  ;; Simple insertion sort for string lists
  (define (insert s sorted)
    (cond ((null? sorted) (list s))
          ((string<? s (car sorted)) (cons s sorted))
          (else (cons (car sorted) (insert s (cdr sorted))))))
  (let loop ((remaining lst) (result '()))
    (if (null? remaining)
        result
        (loop (cdr remaining) (insert (car remaining) result)))))

;;; Printing global data

(define (print-nogoods atms . rest)
  (let ((stream (if (pair? rest) (car rest) (current-output-port))))
    (print-env-table (atms-nogood-table atms) stream)))

(define (print-envs atms . rest)
  (let ((stream (if (pair? rest) (car rest) (current-output-port))))
    (print-env-table (atms-env-table atms) stream)))

(define (print-env-table table stream)
  (for-each
   (lambda (bucket)
     (for-each
      (lambda (env)
        (print-env env stream))
      (cdr bucket)))
   table))

(define (print-atms-statistics atms)
  (print-table "\n For env table:" (atms-env-table atms))
  (print-table "\n For nogood table:" (atms-nogood-table atms)))

(define (print-table msg table)
  (display msg)
  (for-each
   (lambda (entry)
     (display "\n   Length ")
     (display (car entry))
     (display ", ")
     (display (length (cdr entry))))
   table))
