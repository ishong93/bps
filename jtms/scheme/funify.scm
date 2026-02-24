;;; -*- Mode: Scheme -*-

;;; FTRE를 위한 추가 패턴 매칭 기능
;;; Extra pattern-matching facilities for FTRE
;;; Last edited: 1/29/93, KDF
;;; Translated to Scheme (R7RS + SRFI-9) from Common Lisp.

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;;; 개요
;;;
;;; funify는 FTRE(Forward-chaining Truth maintenance Rule Engine)에서
;;; 패턴 매칭을 위한 유틸리티를 제공합니다.
;;;
;;; 주요 기능:
;;; - quotize: 패턴을 인용(quote)된 형태로 변환
;;; - rlet: 패턴 변수를 사용한 let 확장 (매크로 → 함수로 변환)
;;; - pattern-free-variables: 패턴에서 자유 변수를 추출
;;; - generate-match-body: 패턴으로부터 매칭 테스트 코드를 생성
;;; - generate-unify-tests: 통합(unification) 테스트를 생성

;;; variable? — 심볼이 ?로 시작하면 패턴 변수로 간주
(define (variable? x)
  (and (symbol? x)
       (let ((s (symbol->string x)))
         (and (> (string-length s) 0)
              (char=? (string-ref s 0) #\?)))))

;;; *bound-vars* — 현재 바인딩된 변수 목록 (동적 변수)
(define *bound-vars* '())

;;; quotize — 패턴을 인용 형태로 변환
;;; 변수는 그대로 두고, 리터럴은 quote로 감싸고,
;;; :EVAL은 평가할 식을 추출하고, 리스트는 cons로 재구성
(define (quotize pattern)
  (cond ((null? pattern) '())
        ((variable? pattern) pattern)
        ((not (pair? pattern)) (list 'quote pattern))
        ((eq? (car pattern) ':EVAL) (cadr pattern))
        (else (list 'cons
                    (quotize (car pattern))
                    (quotize (cdr pattern))))))

;;; rlet-expand — rlet 매크로를 함수로 변환
;;; var-specs의 각 변수에 대해 값을 quotize하여 let 바인딩 생성
;;; :EVAL 태그가 붙은 값은 직접 평가
(define (rlet-expand var-specs body)
  (let ((*bound-vars*
         (append (map car var-specs) *bound-vars*)))
    (cons 'let
          (cons (map (lambda (let-clause)
                       (list (car let-clause)
                             (if (and (pair? (cadr let-clause))
                                      (eq? (car (cadr let-clause))
                                           ':EVAL))
                                 (cadr (cadr let-clause))
                                 (quotize (cadr let-clause)))))
                     var-specs)
                (fully-expand-body body)))))

;;; pattern-free-variables — 패턴에서 자유 변수 목록을 반환
;;; *bound-vars*에 있는 변수는 제외
(define (pattern-free-variables pattern)
  (pattern-free-vars1 pattern '()))

(define (pattern-free-vars1 pattern vars)
  (cond ((null? pattern) vars)
        ((variable? pattern)
         (if (or (member pattern vars)
                 (member pattern *bound-vars*))
             vars
             (cons pattern vars)))
        ((not (pair? pattern)) vars)
        (else (pattern-free-vars1
               (cdr pattern)
               (pattern-free-vars1 (car pattern) vars)))))

;;; generate-match-body — 패턴으로부터 매칭 본체(테스트 + 바인딩)를 생성
;;; 구조 테스트, 동등성 테스트, 바인딩 사양을 반환
;;; values를 사용하여 두 값(tests, binding-specs)을 반환
(define (generate-match-body pattern vars extra-test)
  (let ((structure-tests '())
        (var-alist '())
        (equal-tests '())
        (binding-specs '()))
    (for-each
     (lambda (test)
       (cond ((variable? (car test))
              (set! equal-tests
                    (append (generate-pairwise-tests (cdr test))
                            equal-tests))
              (if extra-test
                  (set! var-alist
                        (cons (cons (car test) (last-element test))
                              var-alist)))
              (set! binding-specs
                    (cons (last-element test) binding-specs)))
             (else
              (set! structure-tests
                    (cons test structure-tests)))))
     (generate-unify-tests pattern vars '() 'P))
    (set! extra-test (sublis var-alist extra-test))
    (when (not (null? (pattern-free-variables extra-test)))
      (error "Rule test includes free variable" extra-test))
    (values (append structure-tests equal-tests
                    (if extra-test (list extra-test) '()))
            binding-specs)))

;;; last-element — 리스트의 마지막 요소를 반환
(define (last-element lst)
  (if (null? (cdr lst))
      (car lst)
      (last-element (cdr lst))))

;;; sublis — 연관 리스트에 따라 트리의 원소를 치환
(define (sublis alist tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (let ((entry (assoc tree alist)))
           (if entry (cdr entry) tree)))
        (else (cons (sublis alist (car tree))
                    (sublis alist (cdr tree))))))

;;; generate-pairwise-tests — 테스트 목록에서 인접 쌍에 대해 equal 테스트를 생성
(define (generate-pairwise-tests tests)
  (cond ((or (null? tests) (null? (cdr tests))) '())
        (else (cons (list 'equal? (car tests) (cadr tests))
                    (generate-pairwise-tests (cdr tests))))))

;;; generate-unify-tests — 패턴과 경로로부터 통합 테스트를 생성
;;; 패턴의 각 요소에 대해 적절한 검사 코드를 생성
(define (generate-unify-tests pattern vars tests path)
  (cond ((null? pattern)
         (cons (list 'null? path) tests))
        ((member pattern vars)
         (let ((previous (assoc pattern tests)))
           (cond (previous
                  ;; 이전에 본 변수 — 경로를 추가
                  (set-cdr! previous
                            (append (cdr previous) (list path)))
                  tests)
                 (else (cons (list pattern path) tests)))))
        ((variable? pattern)
         (cons (list 'equal? pattern path) tests))
        ((number? pattern)
         (cons (list 'and (list 'number? path)
                     (list '= pattern path))
               tests))
        ((not (pair? pattern))
         (cons (list 'equal? (list 'quote pattern) path) tests))
        (else
         (generate-unify-tests
          (cdr pattern) vars
          (generate-unify-tests
           (car pattern) vars
           (cons (list 'pair? path) tests)
           (list 'car path))
          (list 'cdr path)))))

;;; fully-expand-body — 본체 식을 재귀적으로 확장
;;; (매크로 확장 없이 트리를 그대로 반환하는 기본 버전)
(define (fully-expand-body body)
  (cond ((null? body) '())
        ((not (pair? body)) body)
        (else (cons (if (pair? (car body))
                        (fully-expand-body (car body))
                        (car body))
                    (fully-expand-body (cdr body))))))
