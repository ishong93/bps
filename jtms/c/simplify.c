/* -*- C -*- */

/**** 대수 단순화기 구현 ****/
/* simplify.lisp에서 변환 */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
 * and Johan de Kleer, the Xerox Corporation.
 * All rights reserved. */

/* 이 버전은 G.J. Sussman의 scheme 매처에서 영감을 받았다. */

#include "simplify.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* ================================================================ */
/* 전역 변수                                                         */
/* ================================================================ */

static SimplifyCache *simplify_cache = NULL;
SimplifyRule *algebra_rules = NULL;
int algebra_rules_count = 0;

/* ================================================================ */
/* 해시 캐시 구현                                                     */
/* ================================================================ */

/* 간단한 SExpr 해시 함수 */
static unsigned int sexpr_hash(SExpr *exp) {
    if (exp == NULL) return 0;
    switch (exp->type) {
        case SEXPR_NIL: return 0;
        case SEXPR_SYMBOL: {
            unsigned int h = 5381;
            for (const char *p = exp->symbol; *p; p++) {
                h = ((h << 5) + h) + (unsigned char)*p;
            }
            return h;
        }
        case SEXPR_INT: return (unsigned int)exp->integer;
        case SEXPR_FLOAT: {
            unsigned int h;
            memcpy(&h, &exp->floating, sizeof(unsigned int) < sizeof(double) ?
                   sizeof(unsigned int) : sizeof(double));
            return h;
        }
        case SEXPR_CONS:
            return sexpr_hash(exp->cons.car) * 31 + sexpr_hash(exp->cons.cdr);
        default: return 0;
    }
}

SimplifyCache *simplify_cache_new(void) {
    SimplifyCache *cache = (SimplifyCache *)calloc(1, sizeof(SimplifyCache));
    return cache;
}

void simplify_cache_free(SimplifyCache *cache) {
    if (!cache) return;
    simplify_cache_clear(cache);
    free(cache);
}

SExpr *simplify_cache_get(SimplifyCache *cache, SExpr *key) {
    unsigned int idx = sexpr_hash(key) % SIMPLIFY_CACHE_SIZE;
    for (CacheEntry *e = cache->buckets[idx]; e != NULL; e = e->next) {
        if (sexpr_equal(e->key, key)) {
            return e->value;
        }
    }
    return NULL;
}

void simplify_cache_put(SimplifyCache *cache, SExpr *key, SExpr *value) {
    unsigned int idx = sexpr_hash(key) % SIMPLIFY_CACHE_SIZE;
    /* 기존 항목 확인 */
    for (CacheEntry *e = cache->buckets[idx]; e != NULL; e = e->next) {
        if (sexpr_equal(e->key, key)) {
            e->value = value;
            return;
        }
    }
    /* 새 항목 추가 */
    CacheEntry *entry = (CacheEntry *)malloc(sizeof(CacheEntry));
    entry->key = key;
    entry->value = value;
    entry->next = cache->buckets[idx];
    cache->buckets[idx] = entry;
}

void simplify_cache_clear(SimplifyCache *cache) {
    for (int i = 0; i < SIMPLIFY_CACHE_SIZE; i++) {
        CacheEntry *e = cache->buckets[i];
        while (e) {
            CacheEntry *next = e->next;
            free(e);
            e = next;
        }
        cache->buckets[i] = NULL;
    }
}

/* ================================================================ */
/* 메인 단순화 함수                                                   */
/* (defun simplify (exp) ...)                                        */
/* ================================================================ */

SExpr *simplify(SExpr *exp) {
    if (!simplify_cache) {
        simplify_cache = simplify_cache_new();
    }
    SExpr *cached = simplify_cache_get(simplify_cache, exp);
    if (cached) return cached;

    SExpr *result = simplify_it(exp, algebra_rules, algebra_rules_count);
    simplify_cache_put(simplify_cache, exp, result);
    return result;
}

/* 캐시 초기화 */
void clear_simplify_cache(void) {
    if (simplify_cache) {
        simplify_cache_clear(simplify_cache);
    }
}

/* ================================================================ */
/* 규칙 기반 단순화                                                   */
/* (defun simplify-it (exp rules &aux result) ...)                   */
/* ================================================================ */

SExpr *simplify_it(SExpr *exp, SimplifyRule *rules, int nrules) {
    SExpr *input = exp;

    /* 리스트면 하위 식을 먼저 단순화 */
    if (exp->type == SEXPR_CONS) {
        input = sexpr_cons(simplify(exp->cons.car),
                          simplify(exp->cons.cdr));
    }

    SExpr *result = try_matcher_rules(input, rules, nrules);

    if (sexpr_equal(result, exp)) {
        return result;
    }
    return simplify_it(result, rules, nrules);
}

/* ================================================================ */
/* 규칙 매칭 시도                                                     */
/* (defun try-matcher-rules (exp rules) ...)                         */
/* ================================================================ */

SExpr *try_matcher_rules(SExpr *exp, SimplifyRule *rules, int nrules) {
    for (int i = 0; i < nrules; i++) {
        SExpr *bindings = match(rules[i].pattern, exp, sexpr_nil());
        if (bindings != MATCH_FAIL) {
            if (check_predicate(rules[i].predicate, bindings)) {
                return substitute_in(rules[i].skeleton, bindings);
            }
        }
    }
    return exp;  /* 원래 식 반환 (기본값) */
}

/* ================================================================ */
/* 규칙 술어 검사                                                     */
/* (defun check-predicate (proc bindings) ...)                       */
/* ================================================================ */

bool check_predicate(RulePredicate proc, SExpr *bindings) {
    if (proc == NULL) return true;
    return proc(bindings);
}

/* ================================================================ */
/* 대수 유틸리티                                                      */
/* ================================================================ */

/* (defun alg< (e1 e2) ...) - 대수식 정렬 술어 */
bool alg_lt(SExpr *e1, SExpr *e2) {
    if (sexpr_equal(e1, e2)) return false;

    bool e1_cons = (e1->type == SEXPR_CONS);
    bool e2_cons = (e2->type == SEXPR_CONS);

    if (e1_cons) {
        if (e2_cons) {
            if (sexpr_equal(e1->cons.car, e2->cons.car)) {
                return alg_lt(e1->cons.cdr, e2->cons.cdr);
            }
            return alg_lt(e1->cons.car, e2->cons.car);
        }
        return false;
    }
    if (e2_cons) return true;

    bool e1_sym = (e1->type == SEXPR_SYMBOL);
    bool e2_sym = (e2->type == SEXPR_SYMBOL);

    if (e1_sym) {
        if (e2_sym) return strcmp(e1->symbol, e2->symbol) < 0;
        return false;
    }
    if (e2_sym) return true;

    bool e1_num = (e1->type == SEXPR_INT || e1->type == SEXPR_FLOAT);
    bool e2_num = (e2->type == SEXPR_INT || e2->type == SEXPR_FLOAT);

    if (e1_num && e2_num) {
        double v1 = (e1->type == SEXPR_INT) ? (double)e1->integer : e1->floating;
        double v2 = (e2->type == SEXPR_INT) ? (double)e2->integer : e2->floating;
        return v1 < v2;
    }

    return false;
}

/* (defun alg= (e1 e2) ...) */
bool alg_eq(SExpr *e1, SExpr *e2) {
    return !alg_lt(e1, e2) && !alg_lt(e2, e1);
}

/* (defun sorted? (list pred) ...) */
bool sorted_p(SExpr *list, bool (*pred)(SExpr *, SExpr *)) {
    if (list == NULL || list->type == SEXPR_NIL) return true;
    if (list->type != SEXPR_CONS) return true;
    SExpr *rest = list->cons.cdr;
    if (rest == NULL || rest->type == SEXPR_NIL) return true;
    if (rest->type != SEXPR_CONS) return true;

    if (pred(rest->cons.car, list->cons.car)) return false;
    return sorted_p(rest, pred);
}

/* (defun +/*? (exp) ...) */
bool plus_or_times_p(SExpr *exp) {
    if (exp->type != SEXPR_SYMBOL) return false;
    return (strcmp(exp->symbol, "+") == 0 ||
            strcmp(exp->symbol, "*") == 0);
}

/* (defun same-constant? (exp constant) ...) */
bool same_constant_p(SExpr *exp, double constant) {
    if (exp->type == SEXPR_INT) {
        return (double)exp->integer == constant;
    }
    if (exp->type == SEXPR_FLOAT) {
        return fabs(exp->floating - constant) < match_tolerance;
    }
    return false;
}

bool zero_p(SExpr *exp) { return same_constant_p(exp, 0.0); }
bool one_p(SExpr *exp) { return same_constant_p(exp, 1.0); }

/* (defun occurs-in? (exp1 exp2) ...) - 발생 검사 */
bool occurs_in_p(SExpr *exp1, SExpr *exp2) {
    if (sexpr_equal(exp1, exp2)) return true;
    if (exp2 == NULL || exp2->type == SEXPR_NIL) return false;
    if (exp2->type == SEXPR_CONS) {
        return occurs_in_p(exp1, exp2->cons.car) ||
               occurs_in_p(exp1, exp2->cons.cdr);
    }
    return false;
}

/* ================================================================ */
/* 대수 연산 평가 (:EVAL 처리)                                        */
/* ================================================================ */

SExpr *algebra_eval(SExpr *expr) {
    if (expr == NULL || expr->type == SEXPR_NIL) return expr;
    if (expr->type != SEXPR_CONS) return expr;

    SExpr *op = expr->cons.car;
    if (op->type != SEXPR_SYMBOL) return expr;

    /* 단항/이항 산술 연산 */
    SExpr *args = expr->cons.cdr;
    if (args == NULL || args->type == SEXPR_NIL) return expr;

    SExpr *a1 = args->cons.car;
    double v1;
    if (a1->type == SEXPR_INT) v1 = (double)a1->integer;
    else if (a1->type == SEXPR_FLOAT) v1 = a1->floating;
    else return expr;

    SExpr *rest = args->cons.cdr;

    /* 단항 연산 */
    if (rest == NULL || rest->type == SEXPR_NIL) {
        if (strcmp(op->symbol, "-") == 0) return sexpr_float(-v1);
        if (strcmp(op->symbol, "abs") == 0) return sexpr_float(fabs(v1));
        if (strcmp(op->symbol, "sqrt") == 0) return sexpr_float(sqrt(v1));
        return expr;
    }

    SExpr *a2 = rest->cons.car;
    double v2;
    if (a2->type == SEXPR_INT) v2 = (double)a2->integer;
    else if (a2->type == SEXPR_FLOAT) v2 = a2->floating;
    else return expr;

    /* 이항 연산 */
    if (strcmp(op->symbol, "+") == 0) return sexpr_float(v1 + v2);
    if (strcmp(op->symbol, "-") == 0) return sexpr_float(v1 - v2);
    if (strcmp(op->symbol, "*") == 0) return sexpr_float(v1 * v2);
    if (strcmp(op->symbol, "/") == 0) {
        if (v2 != 0.0) return sexpr_float(v1 / v2);
        return expr;
    }
    if (strcmp(op->symbol, "expt") == 0) return sexpr_float(pow(v1, v2));
    if (strcmp(op->symbol, "log") == 0) {
        if (v1 > 0 && v2 > 0) return sexpr_float(log(v1) / log(v2));
        return expr;
    }

    return expr;
}

/* ================================================================ */
/* SExpr 리스트 정렬 (alg< 기준)                                      */
/* ================================================================ */

/* 리스트를 배열로 변환 후 qsort */
static int alg_lt_compare(const void *a, const void *b) {
    SExpr *sa = *(SExpr **)a;
    SExpr *sb = *(SExpr **)b;
    if (alg_lt(sa, sb)) return -1;
    if (alg_lt(sb, sa)) return 1;
    return 0;
}

SExpr *sexpr_sort(SExpr *list, bool (*pred)(SExpr *, SExpr *)) {
    /* 리스트 길이 세기 */
    int len = 0;
    for (SExpr *p = list; p && p->type == SEXPR_CONS; p = p->cons.cdr) {
        len++;
    }
    if (len <= 1) return list;

    /* 배열로 변환 */
    SExpr **arr = (SExpr **)malloc(sizeof(SExpr *) * len);
    SExpr *p = list;
    for (int i = 0; i < len; i++) {
        arr[i] = p->cons.car;
        p = p->cons.cdr;
    }

    /* 정렬 */
    qsort(arr, len, sizeof(SExpr *), alg_lt_compare);

    /* 리스트로 재구성 */
    SExpr *result = sexpr_nil();
    for (int i = len - 1; i >= 0; i--) {
        result = sexpr_cons(arr[i], result);
    }
    free(arr);
    return result;
}

/* ================================================================ */
/* 대수 규칙 초기화                                                   */
/* ================================================================ */

/* 대수 규칙은 Lisp의 인용 리스트로 정의되어 있었음.
 * C에서는 각 규칙을 명시적으로 구성해야 함.
 * 여기서는 규칙의 프레임워크만 제공하고,
 * 실제 규칙은 SExpr로 패턴/스켈레톤을 구성하여 등록해야 함. */

/* 술어 함수들 */
static bool pred_zero(SExpr *bindings) {
    SExpr *e = var_value_from_bindings("e", bindings);
    return zero_p(e);
}

static bool pred_one(SExpr *bindings) {
    SExpr *e = var_value_from_bindings("e", bindings);
    return one_p(e);
}

static bool pred_numberp(SExpr *bindings) {
    SExpr *e = var_value_from_bindings("e1", bindings);
    return (e->type == SEXPR_INT || e->type == SEXPR_FLOAT);
}

static bool pred_not_sorted(SExpr *bindings) {
    SExpr *terms = var_value_from_bindings("terms", bindings);
    return !sorted_p(terms, alg_lt);
}

void init_algebra_rules(void) {
    /* 대수 규칙 배열 초기화 */
    /* 규칙 수를 미리 결정하고 배열 할당 */
    algebra_rules_count = 0;
    algebra_rules = NULL;

    /* 실제 규칙 등록은 SExpr 패턴을 구성하여 수행해야 함.
     * 이 부분은 원래 Lisp의 인용 리스트를 C로 직접 변환하기
     * 어려우므로, 필요한 규칙을 수동으로 구성하거나
     * S-expression 파서를 사용하여 로드해야 합니다. */
}

void free_algebra_rules(void) {
    if (algebra_rules) {
        free(algebra_rules);
        algebra_rules = NULL;
        algebra_rules_count = 0;
    }
}

/* ================================================================ */
/* 단순화기 전체 초기화/해제                                          */
/* ================================================================ */

void simplify_init(void) {
    simplify_cache = simplify_cache_new();
    init_algebra_rules();
}

void simplify_cleanup(void) {
    if (simplify_cache) {
        simplify_cache_free(simplify_cache);
        simplify_cache = NULL;
    }
    free_algebra_rules();
}
