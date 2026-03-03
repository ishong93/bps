/* -*- C -*- */

/* JTRE 규칙 시스템 */
/* jrules.lisp에서 변환 */

/* Copyright (c) 1989 --- 1992 Kenneth D. Forbus, Northwestern University, */
/* Johan de Kleer and Xerox Corporation. All rights reserved. */

/*
 * 규칙 시스템 개요
 *
 * JTRE의 규칙은 트리거 패턴과 실행 본문으로 구성됩니다.
 * 규칙 매칭 시 패턴 변수가 바인딩되고, 조건이 만족되면 본문이 실행됩니다.
 *
 * 원래 Lisp에서는 매크로로 컴파일 타임에 매치 프로시저와 바디 프로시저를
 * 생성했지만, C에서는 런타임 함수 포인터 기반으로 구현합니다.
 *
 * 트리거 조건:
 * - :IN   - 노드가 IN 상태일 때 실행
 * - :OUT  - 노드가 OUT 상태일 때 실행
 * - :INTERN - 사실이 데이터베이스에 삽입될 때 실행 (믿음 상태 무관)
 */

#ifndef JRULES_H
#define JRULES_H

#include "jtms.h"
#include "jtre.h"
#include "jdata.h"

/* ================================================================ */
/* 규칙 트리거 조건 열거형                                           */
/* ================================================================ */

typedef enum {
    TRIGGER_IN = 0,     /* 노드가 IN 상태일 때 */
    TRIGGER_OUT,        /* 노드가 OUT 상태일 때 */
    TRIGGER_INTERN      /* 사실 삽입 시 (믿음 상태 무관) */
} TriggerCondition;

/* ================================================================ */
/* 규칙 시스템 함수 (jdata.h에 선언된 것들의 구현)                    */
/* ================================================================ */

/* insert_rule, try_rules, try_rule_on, run_rules 등은
 * jdata.h에 이미 선언되어 있음. jrules.c에서 구현. */

/* ================================================================ */
/* 규칙 표시                                                         */
/* ================================================================ */

/* 규칙 출력 */
void print_rule(JRule *rule, FILE *stream);

/* 규칙 확장 테스트 */
void test_rule_expansion(void);

/* ================================================================ */
/* Quotize 유틸리티 (funify.lisp에서)                                */
/* ================================================================ */

/* 패턴에서 자유 변수 찾기 */
/* variable?  심볼이 '?'로 시작하는지 확인 */
int variable_p(const char *sym);

/* 패턴의 자유 변수 목록 반환 */
List *pattern_free_variables(SExpr *pattern);

/* scratchout: l1의 원소를 l2에서 제거 (비파괴적, 순서 보존) */
List *scratchout(List *l1, List *l2);

#endif /* JRULES_H */
