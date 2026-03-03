/* -*- C -*- */

/* JTRE (JTMS-based Tiny Rule Engine) - Definitions */
/* Converted from jinter.lisp */

/* Copyright (c) 1989 -- 1992 Kenneth D. Forbus, Northwestern University, */
/* Johan de Kleer and Xerox Corporation. All rights reserved. */

/*
 * JTRE 개요
 *
 * JTRE는 JTMS 기반의 소형 규칙 엔진(Tiny Rule Engine)입니다.
 * 패턴 매칭 규칙을 사용하여 자동 추론을 수행합니다.
 *
 * 주요 구조:
 * - JTRE: 규칙 엔진의 최상위 구조체. JTMS, 데이터베이스, 규칙 큐를 관리
 * - title: 이 JTRE의 이름
 * - jtms: 소속 JTMS 포인터
 * - dbclass_table: 기호별로 사실/규칙을 분류하는 해시 테이블
 * - datum_counter: 사실(datum) 고유 번호 생성기
 * - rule_counter: 규칙 고유 번호 생성기
 * - debugging: 디버깅 출력 활성화 플래그
 * - queue: 실행 대기 중인 규칙 큐
 * - rules_run: 실행된 규칙 수 통계
 */

#ifndef JTRE_H
#define JTRE_H

#include "jtms.h"

/* ================================================================ */
/* Hash table for dbclass-table                                     */
/* ================================================================ */

#define HASH_TABLE_SIZE 256

typedef struct HashEntry {
    const char *key;
    void       *value;
    struct HashEntry *next;
} HashEntry;

typedef struct HashTable {
    HashEntry *buckets[HASH_TABLE_SIZE];
} HashTable;

HashTable *hash_table_new(void);
void       hash_table_free(HashTable *ht);
void      *hash_table_get(HashTable *ht, const char *key);
void       hash_table_put(HashTable *ht, const char *key, void *value);
void       hash_table_map(HashTable *ht,
                          void (*fn)(const char *key, void *value,
                                     void *ctx),
                          void *ctx);

/* ================================================================ */
/* Forward declarations                                             */
/* ================================================================ */

typedef struct JTRE JTRE;
typedef struct Dbclass Dbclass;
typedef struct Datum Datum;
typedef struct JRule JRule;

/* ================================================================ */
/* JTRE structure                                                   */
/* Pretty name: JTRE                                                */
/* ================================================================ */

/* JTRE 구조체: JTMS 기반 소형 규칙 엔진의 주 구조 */
struct JTRE {
    char      *title;           /* Pretty name */
    Jtms      *jtms;            /* 소속 JTMS 포인터 */
    HashTable *dbclass_table;   /* 기호별 dbclass 해시 테이블 */
    int        datum_counter;   /* 사실(datum) 고유 번호 생성기 */
    int        rule_counter;    /* 규칙 고유 번호 생성기 */
    bool       debugging;       /* 디버깅 출력 활성화 플래그 */
    List      *queue;           /* 실행 대기 중인 규칙 큐 */
    int        rules_run;       /* 실행된 규칙 수 통계 */
};

/* ================================================================ */
/* Global JTRE pointer                                              */
/* *JTRE* 전역 변수 -> extern JTRE* current_jtre                    */
/* ================================================================ */

extern JTRE *current_jtre;

/* ================================================================ */
/* JTRE API (from jinter.lisp)                                      */
/* ================================================================ */

/* JTRE 생성 및 관리 */
JTRE *create_jtre(const char *title, bool debugging);
void  change_jtre(JTRE *jtre, int debugging);  /* -1 = don't change */
void  in_jtre(JTRE *jtre);

/* With-Jtre 매크로 대응: save/restore 패턴 */
/* 사용법:
 *   JTRE *saved = with_jtre_save(some_jtre);
 *   ... operations using current_jtre ...
 *   with_jtre_restore(saved);
 */
JTRE *with_jtre_save(JTRE *jtre);
void  with_jtre_restore(JTRE *saved);

/* JTRE 실행 */
void  uassert(SExpr *fact, SExpr *just);
void  uassume(SExpr *fact, const char *reason);
void  run_forms(List *forms);
void  show(JTRE *jtre, FILE *stream);

#endif /* JTRE_H */
