/* -*- C -*- */

/* JSAINT 적분 연산자 구현 */
/* jsops.lisp에서 변환 */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. All rights reserved. */

#include "jsops.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ================================================================ */
/* 전역 변수                                                         */
/* ================================================================ */

IntegrationOperator *integration_operators = NULL;
int integration_operators_count = 0;

/* ================================================================ */
/* 테스트 함수들                                                      */
/* ================================================================ */

/* 변수 ?var가 식 ?t에 나타나지 않는지 검사 */
bool test_not_occurs_in_var(SExpr *bindings) {
    SExpr *var = var_value_from_bindings("var", bindings);
    SExpr *t = var_value_from_bindings("t", bindings);
    return !occurs_in_p(var, t);
}

/* 변수 ?var가 식에 나타나는지 검사 */
bool test_occurs_in_var(SExpr *bindings) {
    SExpr *var = var_value_from_bindings("var", bindings);
    SExpr *exp = var_value_from_bindings("exp", bindings);
    return occurs_in_p(var, exp);
}

/* ?n이 -1이 아닌지 검사 */
bool test_not_constant_minus1(SExpr *bindings) {
    SExpr *n = var_value_from_bindings("n", bindings);
    return !same_constant_p(n, -1.0);
}

/* 상수가 아니고 변수가 나타나는지 검사 */
static bool test_const_and_nonconst(SExpr *bindings) {
    SExpr *var = var_value_from_bindings("var", bindings);
    SExpr *cnst = var_value_from_bindings("const", bindings);
    SExpr *nonconst = var_value_from_bindings("nonconst", bindings);
    return !occurs_in_p(var, cnst) && occurs_in_p(var, nonconst);
}

/* ?trest가 NULL이 아닌지 검사 */
static bool test_trest_not_null(SExpr *bindings) {
    SExpr *trest = var_value_from_bindings("trest", bindings);
    return trest != NULL && trest->type != SEXPR_NIL;
}

/* ?a에 ?var가 나타나지 않는지 검사 */
static bool test_a_not_occurs_var(SExpr *bindings) {
    SExpr *var = var_value_from_bindings("var", bindings);
    SExpr *a = var_value_from_bindings("a", bindings);
    return !occurs_in_p(var, a);
}

/* ?a, ?b 모두에 ?var가 나타나지 않는지 검사 */
static bool test_ab_not_occurs_var(SExpr *bindings) {
    SExpr *var = var_value_from_bindings("var", bindings);
    SExpr *a = var_value_from_bindings("a", bindings);
    SExpr *b = var_value_from_bindings("b", bindings);
    return !occurs_in_p(var, a) && !occurs_in_p(var, b);
}

/* ?var가 ?exp에 나타나고, (sin ?var)도 ?exp에 나타나는지 */
static bool test_sin_occurs(SExpr *bindings) {
    SExpr *var = var_value_from_bindings("var", bindings);
    SExpr *exp = var_value_from_bindings("exp", bindings);
    if (!occurs_in_p(var, exp)) return false;
    SExpr *sin_var = sexpr_cons(sexpr_symbol("sin"),
                                sexpr_cons(var, sexpr_nil()));
    return occurs_in_p(sin_var, exp);
}

/* ?var가 ?exp에 나타나고, (cos ?var)도 ?exp에 나타나는지 */
static bool test_cos_occurs(SExpr *bindings) {
    SExpr *var = var_value_from_bindings("var", bindings);
    SExpr *exp = var_value_from_bindings("exp", bindings);
    if (!occurs_in_p(var, exp)) return false;
    SExpr *cos_var = sexpr_cons(sexpr_symbol("cos"),
                                sexpr_cons(var, sexpr_nil()));
    return occurs_in_p(cos_var, exp);
}

/* ================================================================ */
/* 적분 연산자 정의                                                   */
/* ================================================================ */

/* 정적 연산자 배열 */
static IntegrationOperator operators_data[] = {
    /* Integral-of-Constant: (Integral ?t ?var), ?var not in ?t */
    /* 결과: (* ?t ?var) */
    {
        .name = "Integral-of-Constant",
        .trigger = "(Integral ?t ?var)",
        .test = test_not_occurs_in_var,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(* ?t ?var)"
    },

    /* Integral-of-Self: (Integral ?exp ?exp) */
    /* 결과: (/ (expt ?exp 2) 2) */
    {
        .name = "Integral-of-Self",
        .trigger = "(Integral ?exp ?exp)",
        .test = NULL,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(/ (expt ?exp 2) 2)"
    },

    /* Move-Constant-outside: (Integral (* ?const ?nonconst) ?var) */
    /* 하위문제: (Integrate (Integral ?nonconst ?var)) */
    /* 결과: (* ?const ?int) */
    {
        .name = "Move-Constant-outside",
        .trigger = "(Integral (* ?const ?nonconst) ?var)",
        .test = test_const_and_nonconst,
        .n_subproblems = 1,
        .subproblems = (const char *[]){"(Integrate (Integral ?nonconst ?var))"},
        .result = "(* ?const ?int)"
    },

    /* Integral-of-Sum: (Integral (+ ?t1 ?t2) ?var) */
    /* 하위문제: 2개 */
    /* 결과: (+ ?int1 ?int2) */
    {
        .name = "Integral-of-Sum",
        .trigger = "(Integral (+ ?t1 ?t2) ?var)",
        .test = NULL,
        .n_subproblems = 2,
        .subproblems = (const char *[]){"(Integrate (Integral ?t1 ?var))",
                                        "(Integrate (Integral ?t2 ?var))"},
        .result = "(+ ?int1 ?int2)"
    },

    /* Integral-of-Nary-sum */
    {
        .name = "Integral-of-Nary-sum",
        .trigger = "(Integral (+ ?t1 ?t2 . ?trest) ?var)",
        .test = test_trest_not_null,
        .n_subproblems = 3,
        .subproblems = (const char *[]){"(Integrate (Integral ?t1 ?var))",
                                        "(Integrate (Integral ?t2 ?var))",
                                        "(Integrate (Integral (+ . ?trest) ?var))"},
        .result = "(+ ?int1 ?int2 ?intr)"
    },

    /* Integral-of-uminus: (Integral (- ?term) ?var) */
    {
        .name = "Integral-of-uminus",
        .trigger = "(Integral (- ?term) ?var)",
        .test = NULL,
        .n_subproblems = 1,
        .subproblems = (const char *[]){"(Integrate (Integral ?term ?var))"},
        .result = "(- ?int)"
    },

    /* Integral-of-minus: (Integral (- ?t1 ?t2) ?var) */
    {
        .name = "Integral-of-minus",
        .trigger = "(Integral (- ?t1 ?t2) ?var)",
        .test = NULL,
        .n_subproblems = 2,
        .subproblems = (const char *[]){"(Integrate (Integral ?t1 ?var))",
                                        "(Integrate (Integral ?t2 ?var))"},
        .result = "(- ?int1 ?int2)"
    },

    /* Integral-of-SQR: (Integral (sqr ?var) ?var) */
    {
        .name = "Integral-of-SQR",
        .trigger = "(Integral (sqr ?var) ?var)",
        .test = NULL,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(/ (expt ?var 3) 3)"
    },

    /* Integral-of-polyterm: (Integral (expt ?var ?n) ?var), ?n != -1 */
    {
        .name = "Integral-of-polyterm",
        .trigger = "(Integral (expt ?var ?n) ?var)",
        .test = test_not_constant_minus1,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(/ (expt ?var (+ 1 ?n)) (+ 1 ?n))"
    },

    /* Simple-e-integral: (Integral (expt %e ?var) ?var) */
    {
        .name = "Simple-e-integral",
        .trigger = "(Integral (expt %e ?var) ?var)",
        .test = NULL,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(expt %e ?var)"
    },

    /* e-integral: (Integral (expt %e (* ?a ?var)) ?var) */
    {
        .name = "e-integral",
        .trigger = "(Integral (expt %e (* ?a ?var)) ?var)",
        .test = test_a_not_occurs_var,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(/ (expt %e (* ?a ?var)) ?a)"
    },

    /* non-e-power-integral */
    {
        .name = "non-e-power-integral",
        .trigger = "(Integral (expt ?b (* ?a ?var)) ?var)",
        .test = test_ab_not_occurs_var,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(/ (expt ?b (* ?a ?var)) (* ?a (log ?b %e)))"
    },

    /* Log-Integral: (Integral (log ?var %e) ?var) */
    {
        .name = "Log-Integral",
        .trigger = "(Integral (log ?var %e) ?var)",
        .test = NULL,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(- (* ?var (log ?var %e)) ?var)"
    },

    /* sin-integral: (Integral (sin (* ?a ?var)) ?var) */
    {
        .name = "sin-integral",
        .trigger = "(Integral (sin (* ?a ?var)) ?var)",
        .test = test_a_not_occurs_var,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(- (/ (cos (* ?a ?var)) ?a))"
    },

    /* cos-integral: (Integral (cos (* ?a ?var)) ?var) */
    {
        .name = "cos-integral",
        .trigger = "(Integral (cos (* ?a ?var)) ?var)",
        .test = test_a_not_occurs_var,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(/ (sin (* ?a ?var)) ?a)"
    },

    /* sin-sqr-integral: (Integral (sqr (sin ?var)) ?var) */
    {
        .name = "sin-sqr-integral",
        .trigger = "(Integral (sqr (sin ?var)) ?var)",
        .test = NULL,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(- (/ ?var 2) (/ (sin (* 2 ?var)) 4))"
    },

    /* cos-sqr-integral: (Integral (sqr (cos ?var)) ?var) */
    {
        .name = "cos-sqr-integral",
        .trigger = "(Integral (sqr (cos ?var)) ?var)",
        .test = NULL,
        .n_subproblems = 0,
        .subproblems = NULL,
        .result = "(+ (/ ?var 2) (/ (sin (* 2 ?var)) 4))"
    },

    /* SinToCosSqrSub */
    {
        .name = "SinToCosSqrSub",
        .trigger = "(Integral ?exp ?var)",
        .test = test_sin_occurs,
        .n_subproblems = 1,
        .subproblems = NULL,  /* 동적 치환 필요 */
        .result = "?Int"
    },

    /* CosToSinSqrSub */
    {
        .name = "CosToSinSqrSub",
        .trigger = "(Integral ?exp ?var)",
        .test = test_cos_occurs,
        .n_subproblems = 1,
        .subproblems = NULL,  /* 동적 치환 필요 */
        .result = "?Int"
    },

    /* SinSqrToTanCosSub */
    {
        .name = "SinSqrToTanCosSub",
        .trigger = "(Integral ?exp ?var)",
        .test = test_sin_occurs,
        .n_subproblems = 1,
        .subproblems = NULL,  /* 동적 치환 필요 */
        .result = "?Int"
    },
};

#define NUM_OPERATORS (sizeof(operators_data) / sizeof(operators_data[0]))

/* ================================================================ */
/* 연산자 초기화/해제                                                  */
/* ================================================================ */

void init_integration_operators(void) {
    integration_operators = operators_data;
    integration_operators_count = NUM_OPERATORS;
}

void free_integration_operators(void) {
    /* 정적 배열이므로 해제 불필요 */
    integration_operators = NULL;
    integration_operators_count = 0;
}
