/* -*- C -*- */

/* JTMS/JTRE 모듈 로더 */
/* jtre.lisp에서 변환 */

/* Copyright (c) 1992, Kenneth D. Forbus, Northwestern University, */
/* Johan de Kleer and Xerox Corporation. All rights reserved. */

/*
 * 모듈 구성 개요
 *
 * JTRE 기본 모듈:
 *   jtms    - JTMS (진리 유지 시스템)
 *   jtre    - JTRE 인터페이스
 *   jdata   - 데이터베이스
 *   jrules  - 규칙 시스템
 *   match   - 패턴 매처 (JSAINT용)
 *   funify  - 오픈 코딩 유니피케이션
 *
 * N-Queens 모듈:
 *   jqueens - N-Queens 퍼즐 (JTRE 버전)
 *   jqrule  - 모순 감지 규칙
 *
 * JSAINT 모듈:
 *   jsaint   - JSAINT 메인 프로그램
 *   match    - 수학 지향 패턴 매처
 *   simplify - 대수 단순화기
 *   jsrules  - 기본 규칙
 *   jsops    - 적분 연산자 라이브러리
 */

#ifndef JTMS_LOADER_H
#define JTMS_LOADER_H

#include "jtms.h"
#include "jtre.h"
#include "jdata.h"
#include "jrules.h"
#include "jsaint.h"
#include "jsrules.h"
#include "jsops.h"

/* ================================================================ */
/* 초기화 함수                                                        */
/* ================================================================ */

/* JTRE 시스템 전체 초기화 */
void init_jtre_system(void);

/* N-Queens 시스템 초기화 */
/* 사전 조건: JTRE가 생성되어 있어야 함 */
void init_jqueens(void);

/* JSAINT 시스템 초기화 */
void init_jsaint(void);

/* ================================================================ */
/* 정리 함수                                                          */
/* ================================================================ */

/* 전체 시스템 정리 */
void cleanup_jtre_system(void);

#endif /* JTMS_LOADER_H */
