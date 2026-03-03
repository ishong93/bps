/* -*- C -*- */

/* JSAINT 기본 규칙 */
/* jsrules.lisp에서 변환 */

/* Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, Xerox Corporation. All Rights reserved. */

/*
 * JSAINT 기본 규칙 개요
 *
 * 이 규칙들은 AND/OR 하위 목표 관리, 부모-자식 관계 추적,
 * 풀이/실패 전파를 담당합니다.
 *
 * 규칙 목록:
 * 1. AND-SUBGOALS 규칙: AND 하위 목표들의 부모 관계 설정, 실패 전파
 * 2. OR-SUBGOALS 규칙: OR 하위 목표들의 부모 관계 설정, 성공 전파
 * 3. PARENT-OF 규칙: 자식 문제의 관련성(relevance) 설정
 * 4. SOLUTION-OF 규칙: 풀이 발견 시 SOLVED 표시
 * 5. OR-SUBGOALS nil 규칙: 메소드가 없으면 FAILED 표시
 * 6. SOLVED 규칙: 풀린 문제의 OPEN 철회
 * 7. FAILED 규칙: 실패한 문제의 OPEN 철회
 */

#ifndef JSRULES_H
#define JSRULES_H

#include "jsaint.h"
#include "jdata.h"

/* ================================================================ */
/* JSAINT 규칙 등록                                                   */
/* ================================================================ */

/* 모든 JSAINT 기본 규칙을 현재 JTRE에 등록 */
void register_jsaint_rules(JTRE *jtre);

/* ================================================================ */
/* 개별 규칙 핸들러 함수들                                             */
/* ================================================================ */

/* AND-SUBGOALS 규칙 핸들러 */
/* (AND-SUBGOALS ?parent ?children)가 IN이 되면:
 * - 각 자식에 대해 PARENT-OF 단언
 * - 각 자식의 실패 시 부모 실패 단언
 * - 모든 자식 풀이 시 부모 풀이 단언 */
void rule_and_subgoals(List *args);

/* OR-SUBGOALS 규칙 핸들러 */
/* (OR-SUBGOALS ?parent ?children)가 IN이 되면:
 * - 각 자식에 대해 PARENT-OF 단언
 * - 하나의 자식 풀이 시 부모 풀이 단언
 * - 모든 자식 실패 시 부모 실패 단언 */
void rule_or_subgoals(List *args);

/* PARENT-OF 규칙 핸들러 */
/* (PARENT-OF ?child ?parent ?type)가 IN이 되면:
 * - RELEVANT ?child 단언 */
void rule_parent_of(List *args);

/* SOLUTION-OF 규칙 핸들러 */
/* (SOLUTION-OF ?problem ?answer)가 IN이 되면:
 * - SOLVED ?problem 단언 */
void rule_solution_of(List *args);

/* OR-SUBGOALS nil 규칙 핸들러 */
/* (OR-SUBGOALS (Integrate ?expr) NIL)이 IN이 되면:
 * - FAILED (Integrate ?expr) 단언 */
void rule_or_subgoals_nil(List *args);

/* SOLVED 규칙 핸들러 */
/* (SOLVED ?problem)이 IN이 되면:
 * - (OPEN ?problem) 철회 */
void rule_solved(List *args);

/* FAILED 규칙 핸들러 */
/* (FAILED ?problem)이 IN이 되면:
 * - (OPEN ?problem) 철회 */
void rule_failed(List *args);

#endif /* JSRULES_H */
