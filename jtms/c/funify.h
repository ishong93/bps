/* -*- C -*- */

/* S-expression 표현, 유니피케이션, 오픈 코딩 패턴 매칭 */
/* funify.lisp에서 변환됨 */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. All rights reserved. */

#ifndef FUNIFY_H
#define FUNIFY_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/* ================================================================ */
/* 일반적인 동적 배열 (리스트)                                      */
/* ================================================================ */

typedef struct {
    void **data;
    int size;
    int cap;
} List;

List *list_new(void);
void  list_free(List *l);
void  list_push(List *l, void *item);
void *list_get(List *l, int i);
void  list_set(List *l, int i, void *item);
void  list_remove(List *l, void *item);
bool  list_contains(List *l, void *item);
List *list_copy(List *l);
void  list_clear(List *l);

/* ================================================================ */
/* S-expression: Lisp와 유사한 cons 셀 표현                        */
/* ================================================================ */

typedef enum {
    SEXPR_SYMBOL,
    SEXPR_NUMBER,
    SEXPR_CONS,
    SEXPR_NIL
} SExprType;

typedef struct SExpr {
    SExprType type;
    union {
        char  *symbol;
        double number;
        struct { struct SExpr *car; struct SExpr *cdr; } cons;
    };
} SExpr;

/* S-expression 생성 */
SExpr *sexpr_symbol(const char *name);
SExpr *sexpr_number(double val);
SExpr *sexpr_cons(SExpr *car, SExpr *cdr);
SExpr *sexpr_nil(void);
SExpr *sexpr_copy(SExpr *e);
void   sexpr_free(SExpr *e);

/* S-expression 술어 */
bool sexpr_is_nil(SExpr *e);
bool sexpr_is_symbol(SExpr *e);
bool sexpr_is_number(SExpr *e);
bool sexpr_is_cons(SExpr *e);
bool sexpr_is_variable(SExpr *e);   /* ?로 시작하는 심볼인지 검사 */
bool sexpr_equal(SExpr *a, SExpr *b);

/* S-expression 접근 */
SExpr *sexpr_car(SExpr *e);
SExpr *sexpr_cdr(SExpr *e);

/* S-expression 출력 */
char *sexpr_to_string(SExpr *e);
void  sexpr_print(SExpr *e, FILE *stream);

/* "(implies ?x ?y)" 같은 간단한 문자열에서 S-expression 생성 */
SExpr *sexpr_parse(const char *str);

/* ================================================================ */
/* 바인딩: (변수 . 값) 쌍의 연결 리스트                            */
/* ================================================================ */

typedef struct Binding {
    SExpr *var;
    SExpr *val;
    struct Binding *next;
} Binding;

Binding *binding_new(SExpr *var, SExpr *val, Binding *rest);
SExpr   *binding_lookup(SExpr *var, Binding *bindings);
void     binding_free(Binding *b);

/* ================================================================ */
/* 유니피케이션                                                     */
/* ================================================================ */

#define UNIFY_FAIL NULL
#define UNIFY_SUCCESS_EMPTY ((Binding *)-1)

Binding *unify(SExpr *a, SExpr *b, Binding *bindings);
Binding *unify_variable(SExpr *var, SExpr *exp, Binding *bindings);
bool     free_in(SExpr *var, SExpr *exp, Binding *bindings);
SExpr   *subst_bindings(Binding *bindings, SExpr *pattern);

/* ================================================================ */
/* 오픈 코딩 패턴 매칭 (funify.lisp에서)                           */
/* ================================================================ */

/* *bound-vars*: 현재 바인딩된 변수 리스트 (전역) */
extern List *bound_vars;

/* quotize: 패턴을 평가 가능한 형태로 변환 */
SExpr *quotize(SExpr *pattern);

/* 패턴에서 자유 변수 찾기 */
List *pattern_free_variables(SExpr *pattern);

/* ================================================================ */
/* 유니피케이션 테스트 생성 (오픈 코딩)                             */
/* ================================================================ */

/* UnifyTest: generate-unify-tests가 생성하는 개별 테스트 */
typedef enum {
    TEST_CONSP,         /* (consp path) */
    TEST_NULL,          /* (null path)  */
    TEST_EQUAL_SYMBOL,  /* (equal 'sym path) -- 심볼 동등 비교 */
    TEST_EQUAL_NUMBER,  /* (and (numberp path) (= num path)) */
    TEST_EQUAL_VAR,     /* (equal ?bound-var path) -- 이미 바인딩된 변수 비교 */
    TEST_BIND_VAR,      /* 변수 바인딩: 변수를 path에서 추출 */
    TEST_PAIRWISE_EQUAL /* 같은 변수의 두 위치가 같은지 비교 */
} UnifyTestType;

typedef struct UnifyTest {
    UnifyTestType type;
    SExpr *symbol;       /* TEST_EQUAL_SYMBOL: 비교할 심볼 */
    double number;       /* TEST_EQUAL_NUMBER: 비교할 숫자 */
    SExpr *variable;     /* TEST_BIND_VAR, TEST_EQUAL_VAR: 관련 변수 */
    SExpr *path;         /* 패턴 내 접근 경로 (car/cdr 체인) */
    SExpr *path2;        /* TEST_PAIRWISE_EQUAL: 두 번째 경로 */
    struct UnifyTest *next;
} UnifyTest;

/* generate-match-body: 패턴 매칭 본문 생성
 * pattern: 매칭할 패턴
 * vars: 자유 변수 리스트
 * extra_test: 추가 테스트 (NULL이면 없음)
 *
 * 반환: tests (UnifyTest 연결 리스트),
 *       binding_specs (List of SExpr* 경로: 바인딩 추출 순서) */
typedef struct MatchBody {
    UnifyTest *tests;        /* 구조/동등 비교 테스트 체인 */
    List *binding_specs;     /* 바인딩 사양: SExpr* 경로 목록 */
} MatchBody;

MatchBody *generate_match_body(SExpr *pattern, List *vars,
                               SExpr *extra_test);

/* generate-unify-tests: 패턴에 대한 명시적 테스트 리스트 생성
 * pattern: 매칭할 패턴
 * vars: 자유 변수 리스트
 * tests: 기존 테스트 (NULL이면 빈 리스트)
 * path: 현재 접근 경로
 * 반환: 확장된 UnifyTest 연결 리스트 */
UnifyTest *generate_unify_tests(SExpr *pattern, List *vars,
                                UnifyTest *tests, SExpr *path);

/* generate-pairwise-tests: 같은 변수의 여러 위치 동등 비교 */
UnifyTest *generate_pairwise_tests(List *paths);

/* scratchout: l2에서 l1의 원소를 제거한 새 리스트 반환
 * (비파괴적, 순서 보존) */
List *scratchout(List *l1, List *l2);

/* ================================================================ */
/* 런타임 패턴 매칭 실행                                           */
/* ================================================================ */

/* 생성된 UnifyTest 체인을 실제 S-expression에 적용하여 매칭 수행 */
typedef struct {
    bool ok;             /* 매칭 성공 여부 */
    List *bindings;      /* 바인딩된 값 리스트 (SExpr*) */
} MatchResult;

/* execute_match: 테스트 체인을 P에 대해 실행 */
MatchResult execute_match(UnifyTest *tests, List *binding_specs,
                          SExpr *p);

/* 경로를 따라 S-expression에서 값 추출 */
SExpr *follow_path(SExpr *root, SExpr *path);

/* UnifyTest 해제 */
void unify_test_free(UnifyTest *t);

/* MatchBody 해제 */
void match_body_free(MatchBody *mb);

#endif /* FUNIFY_H */
