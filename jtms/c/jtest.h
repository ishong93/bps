/* -*- C -*- */

/* Simple shakedown procedure for JTRE */
/* jtest.lisp 에서 변환 */
/* Last edited 1/29/93, by KDF */

/* Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. */
/* All rights reserved. */

/* See the file legal.txt for a paragraph stating scope of permission */
/* and disclaimer of warranty.  The above copyright notice and that */
/* paragraph must be included in any separate copy of this file. */

/*
 * JTRE 셰이크다운 테스트
 *
 * shakedown_jtre(): JTRE 시스템의 기본 기능을 테스트합니다.
 *   - JTRE 생성
 *   - :INTERN 규칙 및 :IN 규칙 정의
 *   - referent, fetch 기능 테스트
 *   - 규칙 실행(run-rules) 테스트
 *   - uassume! 및 in? 기능 테스트
 *
 * 이 테스트는 JTRE C API가 올바르게 동작하는지 검증합니다.
 */

#ifndef JTEST_H
#define JTEST_H

#include "jtms.h"
#include "jtre.h"

/* ================================================================ */
/* JTRE 셰이크다운 테스트 함수                                       */
/* ================================================================ */

/* JTRE 시스템의 기본 기능을 테스트
 * 성공 시 0, 실패 시 -1 반환 */
int shakedown_jtre(void);

/* ================================================================ */
/* main 함수 (테스트 실행)                                           */
/* ================================================================ */

/* jtest.c에 main()이 정의됨 */

#endif /* JTEST_H */
