/* -*- C -*- */

/* Justification-based TMS 예제 (Examples) */
/* jtms-ex.lisp 에서 변환 */
/* Last edited 1/29/93, by KDF */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. */
/* All rights reserved. */

/* See the file legal.txt for a paragraph stating scope of permission */
/* and disclaimer of warranty.  The above copyright notice and that */
/* paragraph must be included in any separate copy of this file. */

/*
 * JTMS 예제 함수들
 *
 * ex1(): 기본 JTMS 사용 예제
 *   - 노드 a~g를 가정(assumption)으로 생성
 *   - 정당화(justification) j1~j4 추가
 *   - 가정 a, b, c, d를 활성화
 *
 * ex2(): 모순(contradiction) 처리 테스트
 *   - ex1()을 먼저 실행한 후 사용
 *   - 모순 노드를 생성하고 정당화 추가
 *
 * ex3(): 다중 지지(multiple support) 예제
 *   - 여러 가정이 하나의 결론을 뒷받침하는 경우
 *   - 모순 발생 시 가정 철회 처리
 */

#ifndef JTMS_EX_H
#define JTMS_EX_H

#include "jtms.h"

/* ================================================================ */
/* 전역 변수 선언 (declare special 대응)                             */
/* 파일 범위 전역 변수들: 예제에서 사용하는 노드 포인터              */
/* ================================================================ */

/* ex1에서 사용하는 노드들 */
extern TmsNode *na, *nb, *nc, *nd, *ne, *nf, *ng;

/* ex2에서 사용하는 모순 노드 */
extern TmsNode *contra;

/* ex3에서 사용하는 노드들 */
extern TmsNode *assumption_a, *assumption_c, *assumption_e;
extern TmsNode *node_h, *node_g;
extern TmsNode *contradiction;

/* 현재 JTMS 포인터 (전역) */
extern JTMS *ex_jtms;

/* ================================================================ */
/* 헬퍼 함수                                                        */
/* ================================================================ */

/* datum으로 노드를 검색 */
TmsNode *get_node(const char *datum, JTMS *jtms);

/* index 번호로 정당화를 검색 */
Just *get_justification(int num, JTMS *jtms);

/* ================================================================ */
/* 예제 함수들                                                       */
/* ================================================================ */

/* ex1: 기본 JTMS 사용 예제
 * 노드 a~g 생성, 정당화 j1~j4 추가, 가정 활성화 */
void ex1(void);

/* ex2: 모순 처리 테스트 (ex1 실행 후 사용) */
void ex2(void);

/* ex3: 다중 지지 예제 */
void ex3(void);

#endif /* JTMS_EX_H */
