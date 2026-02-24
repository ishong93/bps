;;; -*- Mode: Scheme; -*-

;;;; JTRE -- a version of TRE which uses the JTMS
;;; Translated from jtre.lisp, last edited 1/29/93, by KDF

;;; Copyright (c) 1992, Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;;; Module file listings
;;;; 모듈 파일 목록

;;; JTRE 코어 파일 목록
(define *jtre-files*
  '("jtms"       ;; JTMS
    "jinter"     ;; Interface
    "jdata"      ;; Database
    "jrules"     ;; Rule system
    "funify"))   ;; Open-coding unification

;;; N-Queens 퍼즐 파일 목록
(define *jqueens-files*
  '("jqueens"    ;; JTRE version of N-queens puzzle
    "jqrule"))   ;; Contradiction detection rule

;;; JSAINT 통합 솔버 파일 목록
(define *jsaint-files*
  '("jsaint"     ;; JSAINT main program
    "match"      ;; Math-oriented pattern matcher
    "simplify"   ;; Algebraic simplifier
    "jsrules"    ;; Bookkeeping rules
    "jsops"))    ;; Sample integration library

;;; load-jtre-files: JTRE 코어 파일들을 로드
(define (load-jtre-files . args)
  (let ((path (if (null? args) "." (car args))))
    (for-each (lambda (f)
                (load (string-append path "/" f ".scm")))
              *jtre-files*)))

;;; load-jqueens-files: N-Queens 파일들을 로드
(define (load-jqueens-files . args)
  (let ((path (if (null? args) "." (car args))))
    (unless (and *jtre* (not (eq? *jtre* #f)))
      (in-jtre (create-jtre "Dummy")))
    (for-each (lambda (f)
                (load (string-append path "/" f ".scm")))
              *jqueens-files*)))

;;; load-jsaint-files: JSAINT 파일들을 로드
(define (load-jsaint-files . args)
  (let ((path (if (null? args) "." (car args))))
    (for-each (lambda (f)
                (load (string-append path "/" f ".scm")))
              '("jsaint" "match" "simplify"))
    (unless (and (defined? '*jsaint*) *jsaint*)
      (create-jsaint "Dummy" #f))
    (for-each (lambda (f)
                (load (string-append path "/" f ".scm")))
              '("jsrules" "jsops"))))
