/* -*- C -*- */

/* JSAINT 적분 연산자 정의 */
/* jsops.lisp에서 변환 */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. All rights reserved. */

/*
 * JSAINT 연산자 개요
 *
 * 각 적분 연산자는 defIntegration 매크로로 정의되며:
 * - 트리거 패턴: 적분 문제의 형태 (예: (Integral (+ ?t1 ?t2) ?var))
 * - 테스트 조건: 추가 조건 (예: 변수가 상수에 나타나지 않는지)
 * - 하위 문제: 분해된 적분 문제들
 * - 결과: 하위 문제 풀이로부터 최종 답 구성
 *
 * C에서는 각 연산자를 IntegrationOperator 구조체로 표현하고
 * 배열로 등록합니다.
 */

#ifndef JSOPS_H
#define JSOPS_H

#include "jsaint.h"
#include "simplify.h"

/* ================================================================ */
/* 적분 연산자 구조체                                                 */
/* ================================================================ */

typedef struct {
    const char *name;            /* 연산자 이름 */
    const char *trigger;         /* 트리거 패턴 (S-expression 문자열) */
    bool (*test)(SExpr *bindings); /* 추가 조건 검사 (NULL이면 항상 참) */
    int n_subproblems;           /* 하위 문제 수 */
    const char **subproblems;    /* 하위 문제 패턴 배열 */
    const char *result;          /* 결과 패턴 (S-expression 문자열) */
} IntegrationOperator;

/* ================================================================ */
/* 전역 연산자 배열                                                   */
/* ================================================================ */

extern IntegrationOperator *integration_operators;
extern int integration_operators_count;

/* ================================================================ */
/* 연산자 등록 및 초기화                                               */
/* ================================================================ */

/* 모든 적분 연산자를 등록 */
void init_integration_operators(void);

/* 적분 연산자 해제 */
void free_integration_operators(void);

/* ================================================================ */
/* 개별 연산자 테스트 함수들                                           */
/* ================================================================ */

/* 변수가 식에 나타나는지 검사 */
bool test_not_occurs_in_var(SExpr *bindings);
bool test_occurs_in_var(SExpr *bindings);
bool test_not_constant_minus1(SExpr *bindings);

#endif /* JSOPS_H */
