/* -*- C -*- */

/**** 대수 단순화기 ****/
/* simplify.lisp에서 변환 */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
 * and Johan de Kleer, the Xerox Corporation.
 * All rights reserved.
 *
 * See the file legal.txt for a paragraph stating scope of permission
 * and disclaimer of warranty.  The above copyright notice and that
 * paragraph must be included in any separate copy of this file. */

/* 이 버전은 G.J. Sussman의 scheme 매처에서 영감을 받았다. */

#ifndef SIMPLIFY_H
#define SIMPLIFY_H

#include "match.h"

/* ================================================================ */
/* 단순화 규칙 구조체                                               */
/* 각 규칙은 패턴, 술어(선택적), 스켈레톤으로 구성된다.             */
/* ================================================================ */

/* 규칙 술어: 바인딩 딕셔너리를 받아 bool 반환 */
typedef bool (*RulePredicate)(SExpr *bindings);

typedef struct {
    SExpr *pattern;          /* 매칭할 패턴 */
    RulePredicate predicate; /* 추가 조건 검사 (NULL이면 항상 참) */
    SExpr *skeleton;         /* 대입 스켈레톤 */
} SimplifyRule;

/* ================================================================ */
/* 해시 테이블 캐시                                                 */
/* ================================================================ */

#define SIMPLIFY_CACHE_SIZE 1024

typedef struct CacheEntry {
    SExpr *key;
    SExpr *value;
    struct CacheEntry *next;
} CacheEntry;

typedef struct {
    CacheEntry *buckets[SIMPLIFY_CACHE_SIZE];
} SimplifyCache;

SimplifyCache *simplify_cache_new(void);
void           simplify_cache_free(SimplifyCache *cache);
SExpr         *simplify_cache_get(SimplifyCache *cache, SExpr *key);
void           simplify_cache_put(SimplifyCache *cache, SExpr *key, SExpr *value);
void           simplify_cache_clear(SimplifyCache *cache);

/* ================================================================ */
/* 단순화 함수                                                      */
/* ================================================================ */

/* 메인 진입점: 식을 단순화한다 (캐시 사용) */
SExpr *simplify(SExpr *exp);

/* 캐시 초기화 */
void clear_simplify_cache(void);

/* 규칙 배열을 사용하여 단순화 (재귀적) */
SExpr *simplify_it(SExpr *exp, SimplifyRule *rules, int nrules);

/* 규칙 배열에서 매칭 시도 */
SExpr *try_matcher_rules(SExpr *exp, SimplifyRule *rules, int nrules);

/* 규칙 술어 검사 */
bool check_predicate(RulePredicate proc, SExpr *bindings);

/* ================================================================ */
/* 대수 유틸리티                                                    */
/* ================================================================ */

/* 대수식 정렬 술어 */
bool alg_lt(SExpr *e1, SExpr *e2);

/* 대수식 동등성 */
bool alg_eq(SExpr *e1, SExpr *e2);

/* 리스트가 술어에 따라 정렬되어 있는지 확인 */
bool sorted_p(SExpr *list, bool (*pred)(SExpr *, SExpr *));

/* +또는 *인지 확인 */
bool plus_or_times_p(SExpr *exp);

/* 상수 비교 */
bool same_constant_p(SExpr *exp, double constant);
bool zero_p(SExpr *exp);
bool one_p(SExpr *exp);

/* 발생 검사: exp1이 exp2 안에 나타나는지 */
bool occurs_in_p(SExpr *exp1, SExpr *exp2);

/* ================================================================ */
/* 대수 단순화 규칙                                                 */
/* ================================================================ */

/* 전역 대수 규칙 배열 */
extern SimplifyRule *algebra_rules;
extern int           algebra_rules_count;

/* 대수 규칙 초기화 */
void init_algebra_rules(void);

/* 대수 규칙 해제 */
void free_algebra_rules(void);

/* ================================================================ */
/* 단순화기 전체 초기화/해제                                        */
/* ================================================================ */

void simplify_init(void);
void simplify_cleanup(void);

/* ================================================================ */
/* eval 콜백 (대수 연산 수행)                                       */
/* simplify 규칙의 :EVAL 처리를 위해 사용                           */
/* ================================================================ */

SExpr *algebra_eval(SExpr *expr);

/* 리스트 정렬 (alg< 기준) */
SExpr *sexpr_sort(SExpr *list, bool (*pred)(SExpr *, SExpr *));

#endif /* SIMPLIFY_H */
