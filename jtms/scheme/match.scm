;;; -*- Mode: Scheme; -*-

;;;; Pattern matcher for algebraic manipulation systems
;;; Translated from match.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; This version is inspired by one of G.J. Sussman's scheme matchers.
;;; We eschew continuation-passing, for clarity.

;;;; 개요
;;;
;;; 두 종류의 변수가 있습니다:
;;; - 요소 변수(Element variable): 리스트의 단일 요소와 매칭
;;;   형식: (? <변수명> <선택적 제한 조건>)
;;; - 세그먼트 변수(Segment variable): 리스트의 (비어있을 수 있는) 일부와 매칭
;;;   형식: (?? <변수명> <선택적 제한 조건>)
;;;
;;; 기본 진입점: match, 패턴, datum 식, 바인딩 사전을 받음

;;; 수치 허용 오차 (float 비교용)
(define *tol* 1.0e-6)

;;; match-equal?: 수치 허용 오차를 고려한 동등성 비교
(define (match-equal? a b)
  (cond ((and (number? a) (inexact? a)
              (number? b) (inexact? b))
         (< (abs (- a b)) *tol*))
        (else (equal? a b))))

;;;; Variable definitions
;;;; 변수 정의

;;; element-var?: 요소 변수인지 확인 (? name restriction?)
(define (element-var? x)
  (and (pair? x) (eq? (car x) '?)))

;;; segment-var?: 세그먼트 변수인지 확인 (?? name restriction?)
(define (segment-var? x)
  (and (pair? x) (eq? (car x) '??)))

;;; pattern-variable?: 패턴 변수인지 확인
(define (pattern-variable? x)
  (or (element-var? x) (segment-var? x)))

;;; var-name: 변수의 이름 추출
(define (var-name x) (cadr x))

;;; var-restriction: 변수의 제한 조건 추출
(define (var-restriction x)
  (if (and (pair? (cddr x)) (not (null? (cddr x))))
      (caddr x)
      #f))

;;;; Dictionary operations
;;;; 사전 연산

;;; 사전 항목 형식:
;;; (<name> <value>)           — 요소 변수용
;;; (<name> <beg> <end>)       — 세그먼트 변수용

;;; lookup-var: 사전에서 변수를 찾음
(define (lookup-var var dict)
  (assq (var-name var) dict))

;;; var-value: 사전에서 변수의 값을 가져옴
(define (var-value var dict)
  (let ((entry (lookup-var var dict)))
    (unless entry
      (error "Not bound variable" var dict))
    (cond ((= (length entry) 2) (cadr entry))  ; 요소 변수
          (else (segment->list (cadr entry) (caddr entry))))))

;;; segment-beg: 세그먼트 바인딩의 시작 위치
(define (segment-beg entry) (cadr entry))

;;; segment-end: 세그먼트 바인딩의 끝 위치
(define (segment-end entry) (caddr entry))

;;; segment->list: 세그먼트를 리스트로 변환
(define (segment->list start end)
  (let loop ((point start) (l '()))
    (cond ((eq? point end) (reverse l))
          (else (loop (cdr point) (cons (car point) l))))))

;;; bind-element-var: 요소 변수 바인딩 추가
(define (bind-element-var name dat dict)
  (cons (list name dat) dict))

;;; bind-segment-var: 세그먼트 변수 바인딩 추가
(define (bind-segment-var name beg end dict)
  (cons (list name beg end) dict))

;;;; Main match function
;;;; 메인 매칭 함수

;;; match: 패턴과 datum을 매칭
;;; 성공하면 바인딩 사전을, 실패하면 'fail을 반환
(define (match pat dat . args)
  (let ((dict (if (null? args) '() (car args))))
    (cond ((eq? dict 'fail) 'fail)           ; 실패 전파
          ((eq? pat dat) dict)                ; 쉬운 성공
          ((element-var? pat)
           (match-element-var pat dat dict))
          ((not (pair? pat))
           (if (match-equal? pat dat) dict 'fail))
          ((segment-var? (car pat))
           (match-segment-var pat dat dict))
          ((not (pair? dat)) 'fail)
          (else (match (cdr pat) (cdr dat)
                       (match (car pat) (car dat) dict))))))

;;; match-element-var: 요소 변수 매칭
(define (match-element-var pat dat dict)
  (let ((entry (lookup-var pat dict)))
    (cond (entry
           (if (match-equal? (cadr entry) dat) dict 'fail))
          (else
           (let ((pred (var-restriction pat)))
             (cond ((or (not pred) (pred dat))
                    (bind-element-var (var-name pat) dat dict))
                   (else 'fail)))))))

;;;; Finding matches for segment variables
;;;; 세그먼트 변수 매칭

;;; match-segment-var: 세그먼트 변수 매칭 (비결정적, 반복 필요)
(define (match-segment-var pat dat dict)
  (let ((entry (lookup-var (car pat) dict)))
    (cond (entry
           ;; 기존 바인딩과 일치하는지 확인
           (let ((rest (check-segment dat (segment-beg entry)
                                      (segment-end entry))))
             (if (eq? rest 'fail) 'fail
                 (match (cdr pat) rest dict))))
          (else
           ;; 대체 세그먼트 바인딩을 탐색
           (try-segment-bindings (car pat) (cdr pat) dat dict)))))

;;; check-segment: 세그먼트가 datum과 일치하는지 확인
(define (check-segment dat beg end)
  (cond ((eq? beg end) dat)
        ((not (pair? dat)) 'fail)
        ((match-equal? (car dat) (car beg))
         (check-segment (cdr dat) (cdr beg) end))
        (else 'fail)))

;;; try-segment-bindings: 가능한 세그먼트 바인딩을 탐색
(define (try-segment-bindings var pat dat dict)
  (let ((name (var-name var))
        (pred (var-restriction var))
        (beg dat))
    (call-with-current-continuation
     (lambda (return)
       (let loop ((end dat))
         (cond ((null? end)
                ;; 마지막까지 시도
                (cond ((or (not pred)
                           (pred (segment->list beg '())))
                       (return (match pat '()
                                      (bind-segment-var name beg '() dict))))
                      (else (return 'fail))))
               (else
                (when (or (not pred)
                          (pred (segment->list beg end)))
                  (let ((ndict (match pat end
                                      (bind-segment-var name beg end dict))))
                    (unless (eq? ndict 'fail)
                      (return ndict))))
                (loop (cdr end)))))))))

;;;; Performing substitutions
;;;; 치환 수행

;;; substitute-in: 사전의 바인딩에 따라 식을 치환
(define (substitute-in exp dict)
  (cond ((null? exp) '())
        ((element-var? exp)
         (var-value exp dict))
        ((pair? exp)
         (cond ((segment-var? (car exp))
                (append (var-value (car exp) dict)
                        (substitute-in (cdr exp) dict)))
               ((eq? (car exp) ':EVAL)
                (eval (substitute-in (cadr exp) dict)))
               ((and (pair? (car exp)) (eq? (caar exp) ':SPLICE))
                (append (eval (substitute-in (cadar exp) dict))
                        (substitute-in (cdr exp) dict)))
               (else (cons (substitute-in (car exp) dict)
                           (substitute-in (cdr exp) dict)))))
        (else exp)))
