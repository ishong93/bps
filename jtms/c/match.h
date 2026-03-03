/* -*- C -*- */

/**** 대수 조작 시스템을 위한 패턴 매처 ****/
/* match.lisp에서 변환 */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
 * and Johan de Kleer, the Xerox Corporation.
 * All rights reserved.
 *
 * See the file legal.txt for a paragraph stating scope of permission
 * and disclaimer of warranty.  The above copyright notice and that
 * paragraph must be included in any separate copy of this file. */

/* 이 버전은 G.J. Sussman의 scheme 매처에서 영감을 받았다.
 * 명확성을 위해 continuation-passing을 사용하지 않는다. */

#ifndef MATCH_H
#define MATCH_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

/* ================================================================ */
/* S-expression: Lisp 유사 cons cell 표현                           */
/* ================================================================ */

typedef enum {
    SEXPR_SYMBOL,
    SEXPR_INT,
    SEXPR_FLOAT,
    SEXPR_CONS,
    SEXPR_NIL
} SExprType;

typedef struct SExpr {
    SExprType type;
    union {
        const char *symbol;
        int integer;
        double floating;
        struct { struct SExpr *car; struct SExpr *cdr; } cons;
    };
} SExpr;

/* :FAIL 센티넬 — 매칭 실패를 나타낸다 */
extern SExpr FAIL_SENTINEL;
#define MATCH_FAIL (&FAIL_SENTINEL)

/* 부동소수점 비교를 위한 허용 오차 */
extern double match_tolerance;

/* ================================================================ */
/* S-expression 생성 함수                                           */
/* ================================================================ */

SExpr *sexpr_symbol(const char *name);
SExpr *sexpr_int(int n);
SExpr *sexpr_float(double f);
SExpr *sexpr_cons(SExpr *car, SExpr *cdr);
SExpr *sexpr_nil(void);
SExpr *sexpr_copy(SExpr *e);
void   sexpr_free(SExpr *e);

/* ================================================================ */
/* S-expression 술어 및 접근 함수                                   */
/* ================================================================ */

bool sexpr_is_nil(SExpr *e);
bool sexpr_is_symbol(SExpr *e);
bool sexpr_is_int(SExpr *e);
bool sexpr_is_float(SExpr *e);
bool sexpr_is_number(SExpr *e);
bool sexpr_is_cons(SExpr *e);

/* sexpr_eq: 포인터 동등성 (Lisp eq) */
bool sexpr_eq(SExpr *a, SExpr *b);

/* sexpr_equal: 구조적 동등성 (Lisp equal) */
bool sexpr_equal(SExpr *a, SExpr *b);

/* equal?: 부동소수점 허용 오차를 고려한 동등성 비교 */
bool sexpr_equal_tol(SExpr *a, SExpr *b);

SExpr *sexpr_car(SExpr *e);
SExpr *sexpr_cdr(SExpr *e);
SExpr *sexpr_cadr(SExpr *e);
SExpr *sexpr_caddr(SExpr *e);
SExpr *sexpr_caar(SExpr *e);
SExpr *sexpr_cadar(SExpr *e);

/* consp, null 술어 */
#define sexpr_consp(e) sexpr_is_cons(e)
#define sexpr_null(e)  sexpr_is_nil(e)

/* S-expression 출력 */
char *sexpr_to_string(SExpr *e);
void  sexpr_print(SExpr *e, FILE *stream);

/* S-expression 파싱: 간단한 문자열 → S-expression */
SExpr *sexpr_parse(const char *str);

/* 리스트 길이 */
int sexpr_length(SExpr *e);

/* append: 두 리스트를 연결 */
SExpr *sexpr_append(SExpr *a, SExpr *b);

/* nreverse: 리스트 뒤집기 */
SExpr *sexpr_nreverse(SExpr *e);

/* mapcar: 리스트의 각 요소에 함수 적용 */
SExpr *sexpr_mapcar(SExpr *(*fn)(SExpr *), SExpr *list);

/* ================================================================ */
/* 두 종류의 변수가 있다.                                           */
/* 요소 변수(element variable)는 리스트의 단일 요소와 매칭한다.     */
/* 세그먼트 변수(segment variable)는 리스트의 (빈) 부분과 매칭한다. */
/* 요소 변수 형식: (? <변수명> <선택적 제한>)                       */
/* 세그먼트 변수 형식: (?? <변수명> <선택적 제한>)                  */
/* ================================================================ */

bool pattern_variable_p(SExpr *x);
bool element_var_p(SExpr *x);
bool segment_var_p(SExpr *x);
SExpr *var_name(SExpr *x);

/* 제한 술어: 함수 포인터 타입 */
typedef bool (*VarRestriction)(SExpr *);

/* 변수에서 제한 술어를 가져온다. 이름 기반 조회 사용. */
VarRestriction var_restriction_fn(SExpr *x);

/* ================================================================ */
/* 딕셔너리 (연관 리스트)                                           */
/* ================================================================ */
/* 딕셔너리 항목 형식:                                              */
/*   요소 변수: (name value)  — 길이 2인 리스트                     */
/*   세그먼트 변수: (name beg end)  — 길이 3인 리스트               */
/* 딕셔너리 자체는 cons cell로 구성된 연관 리스트(alist)이다.       */

SExpr *lookup_var(SExpr *var, SExpr *dict);
SExpr *var_value(SExpr *var, SExpr *dict);
SExpr *bind_element_var(SExpr *name, SExpr *dat, SExpr *dict);
SExpr *bind_segment_var(SExpr *name, SExpr *beg, SExpr *end, SExpr *dict);

SExpr *segment_beg(SExpr *entry);
SExpr *segment_end(SExpr *entry);
SExpr *segment_to_list(SExpr *start, SExpr *end);

/* ================================================================ */
/* 패턴 매칭: 기본 진입점                                           */
/* 패턴, 데이터 식, 바인딩 alist를 받는다.                          */
/* 성공 시 확장된 딕셔너리를 반환, 실패 시 MATCH_FAIL 반환.         */
/* ================================================================ */

SExpr *match(SExpr *pat, SExpr *dat, SExpr *dict);

/* ================================================================ */
/* 대입: 패턴에 바인딩을 적용한다                                   */
/* ================================================================ */

/* 제한 술어 등록/조회를 위한 콜백 */
typedef SExpr *(*EvalCallback)(SExpr *expr);

/* 전역 eval 콜백 설정 (substitute_in의 :EVAL/:SPLICE 지원) */
void match_set_eval_callback(EvalCallback cb);

SExpr *substitute_in(SExpr *exp, SExpr *dict);

/* ================================================================ */
/* 제한 술어 레지스트리                                             */
/* 이름 → 함수 포인터 맵핑                                         */
/* ================================================================ */

void match_register_restriction(const char *name, VarRestriction fn);
VarRestriction match_lookup_restriction(const char *name);

#endif /* MATCH_H */
