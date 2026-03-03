/* -*- C -*- */

/* S-expression 표현, 유니피케이션, 오픈 코딩 패턴 매칭 */
/* funify.lisp에서 변환됨 */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. All rights reserved. */

#include "funify.h"
#include <ctype.h>

/* ================================================================ */
/* 일반적인 동적 배열 (리스트) 구현                                 */
/* ================================================================ */

List *list_new(void) {
    List *l = (List *)calloc(1, sizeof(List));
    l->cap = 4;
    l->data = (void **)calloc(l->cap, sizeof(void *));
    return l;
}

void list_free(List *l) {
    if (!l) return;
    free(l->data);
    free(l);
}

void list_push(List *l, void *item) {
    if (l->size >= l->cap) {
        l->cap *= 2;
        l->data = (void **)realloc(l->data, l->cap * sizeof(void *));
    }
    l->data[l->size++] = item;
}

void *list_get(List *l, int i) {
    if (!l || i < 0 || i >= l->size) return NULL;
    return l->data[i];
}

void list_set(List *l, int i, void *item) {
    if (!l || i < 0 || i >= l->size) return;
    l->data[i] = item;
}

void list_remove(List *l, void *item) {
    if (!l) return;
    for (int i = 0; i < l->size; i++) {
        if (l->data[i] == item) {
            memmove(&l->data[i], &l->data[i + 1],
                    (l->size - i - 1) * sizeof(void *));
            l->size--;
            i--;
        }
    }
}

bool list_contains(List *l, void *item) {
    if (!l) return false;
    for (int i = 0; i < l->size; i++)
        if (l->data[i] == item) return true;
    return false;
}

List *list_copy(List *l) {
    List *c = list_new();
    if (!l) return c;
    for (int i = 0; i < l->size; i++)
        list_push(c, l->data[i]);
    return c;
}

void list_clear(List *l) {
    if (l) l->size = 0;
}

/* ================================================================ */
/* 전역 변수: *bound-vars*                                         */
/* ================================================================ */

List *bound_vars = NULL;

/* ================================================================ */
/* S-expression 생성                                                */
/* ================================================================ */

SExpr *sexpr_symbol(const char *name) {
    SExpr *e = (SExpr *)calloc(1, sizeof(SExpr));
    e->type = SEXPR_SYMBOL;
    e->symbol = strdup(name);
    return e;
}

SExpr *sexpr_number(double val) {
    SExpr *e = (SExpr *)calloc(1, sizeof(SExpr));
    e->type = SEXPR_NUMBER;
    e->number = val;
    return e;
}

SExpr *sexpr_cons(SExpr *car, SExpr *cdr) {
    SExpr *e = (SExpr *)calloc(1, sizeof(SExpr));
    e->type = SEXPR_CONS;
    e->cons.car = car;
    e->cons.cdr = cdr;
    return e;
}

SExpr *sexpr_nil(void) {
    SExpr *e = (SExpr *)calloc(1, sizeof(SExpr));
    e->type = SEXPR_NIL;
    return e;
}

SExpr *sexpr_copy(SExpr *e) {
    if (!e) return NULL;
    switch (e->type) {
    case SEXPR_SYMBOL: return sexpr_symbol(e->symbol);
    case SEXPR_NUMBER: return sexpr_number(e->number);
    case SEXPR_CONS:   return sexpr_cons(sexpr_copy(e->cons.car),
                                         sexpr_copy(e->cons.cdr));
    case SEXPR_NIL:    return sexpr_nil();
    }
    return NULL;
}

void sexpr_free(SExpr *e) {
    if (!e) return;
    if (e->type == SEXPR_SYMBOL) free(e->symbol);
    else if (e->type == SEXPR_CONS) {
        sexpr_free(e->cons.car);
        sexpr_free(e->cons.cdr);
    }
    free(e);
}

/* ================================================================ */
/* S-expression 술어                                                */
/* ================================================================ */

bool sexpr_is_nil(SExpr *e) { return !e || e->type == SEXPR_NIL; }
bool sexpr_is_symbol(SExpr *e) { return e && e->type == SEXPR_SYMBOL; }
bool sexpr_is_number(SExpr *e) { return e && e->type == SEXPR_NUMBER; }
bool sexpr_is_cons(SExpr *e)   { return e && e->type == SEXPR_CONS; }

/* variable?: ?로 시작하는 심볼인지 검사 */
bool sexpr_is_variable(SExpr *e) {
    return sexpr_is_symbol(e) && e->symbol[0] == '?';
}

bool sexpr_equal(SExpr *a, SExpr *b) {
    if (!a && !b) return true;
    if (!a || !b) return false;
    if (a->type != b->type) return false;
    switch (a->type) {
    case SEXPR_NIL:    return true;
    case SEXPR_SYMBOL: return strcmp(a->symbol, b->symbol) == 0;
    case SEXPR_NUMBER: return a->number == b->number;
    case SEXPR_CONS:   return sexpr_equal(a->cons.car, b->cons.car) &&
                              sexpr_equal(a->cons.cdr, b->cons.cdr);
    }
    return false;
}

SExpr *sexpr_car(SExpr *e) {
    return (e && e->type == SEXPR_CONS) ? e->cons.car : NULL;
}

SExpr *sexpr_cdr(SExpr *e) {
    return (e && e->type == SEXPR_CONS) ? e->cons.cdr : NULL;
}

/* ================================================================ */
/* S-expression 출력                                                */
/* ================================================================ */

static void sexpr_to_buf(SExpr *e, char *buf, int *pos, int maxlen) {
    if (!e || *pos >= maxlen - 1) return;
    switch (e->type) {
    case SEXPR_NIL:
        *pos += snprintf(buf + *pos, maxlen - *pos, "NIL");
        break;
    case SEXPR_SYMBOL:
        *pos += snprintf(buf + *pos, maxlen - *pos, "%s", e->symbol);
        break;
    case SEXPR_NUMBER:
        if (e->number == (int)e->number)
            *pos += snprintf(buf + *pos, maxlen - *pos, "%d",
                             (int)e->number);
        else
            *pos += snprintf(buf + *pos, maxlen - *pos, "%g",
                             e->number);
        break;
    case SEXPR_CONS: {
        *pos += snprintf(buf + *pos, maxlen - *pos, "(");
        sexpr_to_buf(e->cons.car, buf, pos, maxlen);
        SExpr *rest = e->cons.cdr;
        while (rest && rest->type == SEXPR_CONS) {
            *pos += snprintf(buf + *pos, maxlen - *pos, " ");
            sexpr_to_buf(rest->cons.car, buf, pos, maxlen);
            rest = rest->cons.cdr;
        }
        if (rest && rest->type != SEXPR_NIL) {
            *pos += snprintf(buf + *pos, maxlen - *pos, " . ");
            sexpr_to_buf(rest, buf, pos, maxlen);
        }
        *pos += snprintf(buf + *pos, maxlen - *pos, ")");
        break;
    }
    }
}

static char _sexpr_buf[2048];

char *sexpr_to_string(SExpr *e) {
    int pos = 0;
    _sexpr_buf[0] = '\0';
    sexpr_to_buf(e, _sexpr_buf, &pos, sizeof(_sexpr_buf));
    return _sexpr_buf;
}

void sexpr_print(SExpr *e, FILE *stream) {
    fprintf(stream, "%s", sexpr_to_string(e));
}

/* ================================================================ */
/* S-expression 파서                                                */
/* ================================================================ */

static const char *skip_ws(const char *s) {
    while (*s && isspace((unsigned char)*s)) s++;
    return s;
}

static SExpr *parse_expr(const char **s);

static SExpr *parse_list(const char **s) {
    *s = skip_ws(*s);
    if (**s == ')') { (*s)++; return sexpr_nil(); }
    SExpr *car = parse_expr(s);
    SExpr *cdr = parse_list(s);
    return sexpr_cons(car, cdr);
}

static SExpr *parse_expr(const char **s) {
    *s = skip_ws(*s);
    if (**s == '\0') return sexpr_nil();
    if (**s == '(') {
        (*s)++;
        return parse_list(s);
    }
    /* 아톰: 공백이나 ) 까지 읽기 */
    const char *start = *s;
    while (**s && !isspace((unsigned char)**s) &&
           **s != '(' && **s != ')') (*s)++;
    int len = (int)(*s - start);
    char *tok = (char *)malloc(len + 1);
    memcpy(tok, start, len);
    tok[len] = '\0';
    /* 숫자 시도 */
    char *end;
    double num = strtod(tok, &end);
    if (*end == '\0') {
        free(tok);
        return sexpr_number(num);
    }
    /* 심볼 */
    SExpr *e = sexpr_symbol(tok);
    free(tok);
    return e;
}

SExpr *sexpr_parse(const char *str) {
    return parse_expr(&str);
}

/* ================================================================ */
/* 바인딩                                                           */
/* ================================================================ */

Binding *binding_new(SExpr *var, SExpr *val, Binding *rest) {
    Binding *b = (Binding *)calloc(1, sizeof(Binding));
    b->var = var;
    b->val = val;
    b->next = rest;
    return b;
}

SExpr *binding_lookup(SExpr *var, Binding *bindings) {
    for (Binding *b = bindings; b; b = b->next) {
        if (sexpr_equal(b->var, var)) return b->val;
    }
    return NULL;
}

void binding_free(Binding *b) {
    while (b) {
        Binding *next = b->next;
        free(b);
        b = next;
    }
}

/* ================================================================ */
/* 유니피케이션                                                     */
/* ================================================================ */

bool free_in(SExpr *var, SExpr *exp, Binding *bindings) {
    if (sexpr_is_nil(exp)) return true;
    if (sexpr_equal(var, exp)) return false;
    if (sexpr_is_variable(exp)) {
        SExpr *val = binding_lookup(exp, bindings);
        if (val) return free_in(var, val, bindings);
        return true;
    }
    if (!sexpr_is_cons(exp)) return true;
    return free_in(var, exp->cons.car, bindings) &&
           free_in(var, exp->cons.cdr, bindings);
}

Binding *unify_variable(SExpr *var, SExpr *exp, Binding *bindings) {
    SExpr *val = binding_lookup(var, bindings);
    if (val) return unify(val, exp, bindings);
    if (free_in(var, exp, bindings))
        return binding_new(var, exp, bindings);
    return UNIFY_FAIL;
}

Binding *unify(SExpr *a, SExpr *b, Binding *bindings) {
    if (sexpr_equal(a, b))
        return bindings ? bindings : UNIFY_SUCCESS_EMPTY;
    if (sexpr_is_variable(a)) return unify_variable(a, b, bindings);
    if (sexpr_is_variable(b)) return unify_variable(b, a, bindings);
    if (!sexpr_is_cons(a) || !sexpr_is_cons(b)) return UNIFY_FAIL;
    Binding *new_b = unify(a->cons.car, b->cons.car, bindings);
    if (new_b == UNIFY_FAIL) return UNIFY_FAIL;
    if (new_b == UNIFY_SUCCESS_EMPTY) new_b = NULL;
    return unify(a->cons.cdr, b->cons.cdr, new_b);
}

SExpr *subst_bindings(Binding *bindings, SExpr *pattern) {
    if (!bindings || bindings == UNIFY_SUCCESS_EMPTY)
        return sexpr_copy(pattern);
    if (sexpr_is_variable(pattern)) {
        SExpr *val = binding_lookup(pattern, bindings);
        if (val) return subst_bindings(bindings, val);
        return sexpr_copy(pattern);
    }
    if (!sexpr_is_cons(pattern)) return sexpr_copy(pattern);
    return sexpr_cons(subst_bindings(bindings, pattern->cons.car),
                      subst_bindings(bindings, pattern->cons.cdr));
}

/* ================================================================ */
/* 패턴 자유 변수 (pattern-free-variables)                          */
/* ================================================================ */

/* SExpr 리스트에서 변수가 포함되어 있는지 확인 */
static bool sexpr_list_contains(List *l, SExpr *e) {
    if (!l) return false;
    for (int i = 0; i < l->size; i++) {
        if (sexpr_equal((SExpr *)l->data[i], e)) return true;
    }
    return false;
}

/* pattern-free-vars1: 패턴에서 자유 변수를 찾아 vars에 추가 */
static void pattern_free_vars1(SExpr *pattern, List *vars) {
    if (sexpr_is_nil(pattern)) return;
    if (sexpr_is_variable(pattern)) {
        /* vars나 *bound-vars*에 이미 있으면 건너뜀 */
        if (sexpr_list_contains(vars, pattern)) return;
        if (sexpr_list_contains(bound_vars, pattern)) return;
        list_push(vars, pattern);
        return;
    }
    if (!sexpr_is_cons(pattern)) return;
    pattern_free_vars1(pattern->cons.car, vars);
    pattern_free_vars1(pattern->cons.cdr, vars);
}

List *pattern_free_variables(SExpr *pattern) {
    List *vars = list_new();
    pattern_free_vars1(pattern, vars);
    return vars;
}

/* ================================================================ */
/* quotize: 패턴을 평가 가능한 형태로 변환                         */
/* ================================================================ */

SExpr *quotize(SExpr *pattern) {
    if (sexpr_is_nil(pattern)) return sexpr_nil();
    if (sexpr_is_variable(pattern)) return sexpr_copy(pattern);
    if (!sexpr_is_cons(pattern)) return sexpr_copy(pattern);
    /* :EVAL 특수 처리 */
    if (sexpr_is_symbol(pattern->cons.car) &&
        strcmp(pattern->cons.car->symbol, ":EVAL") == 0) {
        return sexpr_copy(sexpr_car(pattern->cons.cdr));
    }
    return sexpr_cons(quotize(pattern->cons.car),
                      quotize(pattern->cons.cdr));
}

/* ================================================================ */
/* scratchout: l2에서 l1의 원소를 제거 (비파괴적, 순서 보존)       */
/* ================================================================ */

List *scratchout(List *l1, List *l2) {
    List *result = list_new();
    if (!l2) return result;
    for (int i = 0; i < l2->size; i++) {
        SExpr *el = (SExpr *)l2->data[i];
        bool found = false;
        if (l1) {
            for (int j = 0; j < l1->size; j++) {
                if (sexpr_equal(el, (SExpr *)l1->data[j])) {
                    found = true;
                    break;
                }
            }
        }
        if (!found) list_push(result, el);
    }
    return result;
}

/* ================================================================ */
/* 경로 표현                                                       */
/* car/cdr 경로를 S-expression으로 표현:                            */
/* - 심볼 "P" = 루트                                               */
/* - (car X) = X의 car                                             */
/* - (cdr X) = X의 cdr                                             */
/* ================================================================ */

/* 경로를 따라 S-expression에서 값 추출 */
SExpr *follow_path(SExpr *root, SExpr *path) {
    if (!path) return root;
    if (sexpr_is_symbol(path)) {
        /* "P" = 루트 */
        return root;
    }
    if (sexpr_is_cons(path)) {
        SExpr *op = path->cons.car;
        SExpr *arg = sexpr_car(path->cons.cdr);
        SExpr *sub = follow_path(root, arg);
        if (!sub) return NULL;
        if (sexpr_is_symbol(op)) {
            if (strcmp(op->symbol, "car") == 0)
                return sexpr_car(sub);
            if (strcmp(op->symbol, "cdr") == 0)
                return sexpr_cdr(sub);
        }
    }
    return root;
}

/* 경로 생성 헬퍼 */
static SExpr *make_path_car(SExpr *base) {
    /* (car base) */
    return sexpr_cons(sexpr_symbol("car"),
                      sexpr_cons(sexpr_copy(base), sexpr_nil()));
}

static SExpr *make_path_cdr(SExpr *base) {
    /* (cdr base) */
    return sexpr_cons(sexpr_symbol("cdr"),
                      sexpr_cons(sexpr_copy(base), sexpr_nil()));
}

/* ================================================================ */
/* UnifyTest 생성 및 해제                                           */
/* ================================================================ */

static UnifyTest *test_new(UnifyTestType type) {
    UnifyTest *t = (UnifyTest *)calloc(1, sizeof(UnifyTest));
    t->type = type;
    return t;
}

void unify_test_free(UnifyTest *t) {
    while (t) {
        UnifyTest *next = t->next;
        if (t->symbol) sexpr_free(t->symbol);
        if (t->variable) sexpr_free(t->variable);
        if (t->path) sexpr_free(t->path);
        if (t->path2) sexpr_free(t->path2);
        free(t);
        t = next;
    }
}

void match_body_free(MatchBody *mb) {
    if (!mb) return;
    unify_test_free(mb->tests);
    list_free(mb->binding_specs);
    free(mb);
}

/* UnifyTest 체인 앞에 추가 */
static UnifyTest *prepend_test(UnifyTest *test, UnifyTest *chain) {
    test->next = chain;
    return test;
}

/* ================================================================ */
/* generate-unify-tests: 패턴에 대한 명시적 테스트 리스트 생성      */
/*                                                                  */
/* Lisp 원본의 테스트 유형:                                        */
/* - (null path): 리스트 끝 확인                                   */
/* - (?var path): 변수 바인딩 (이전 바인딩 있으면 동등 비교)       */
/* - (equal ?bound-var path): 이미 바인딩된 변수와 비교            */
/* - (and (numberp path) (= num path)): 숫자 비교                  */
/* - (equal 'sym path): 심볼 비교                                  */
/* - (consp path): 리스트인지 확인                                 */
/* ================================================================ */

/* 변수 바인딩 테스트에서 이전에 같은 변수를 바인딩했는지 찾기 */
static UnifyTest *find_var_test(UnifyTest *tests, SExpr *var) {
    for (UnifyTest *t = tests; t; t = t->next) {
        if (t->type == TEST_BIND_VAR && t->variable &&
            sexpr_equal(t->variable, var))
            return t;
    }
    return NULL;
}

UnifyTest *generate_unify_tests(SExpr *pattern, List *vars,
                                UnifyTest *tests, SExpr *path) {
    if (sexpr_is_nil(pattern)) {
        /* (null path) -- 리스트 끝 확인 */
        UnifyTest *t = test_new(TEST_NULL);
        t->path = sexpr_copy(path);
        return prepend_test(t, tests);
    }

    if (sexpr_list_contains(vars, pattern)) {
        /* 자유 변수: 이전에 바인딩되었으면 동등 비교 추가,
         * 아니면 새 바인딩 생성 */
        UnifyTest *prev = find_var_test(tests, pattern);
        if (prev) {
            /* 이전 바인딩이 있으면 pairwise 테스트 생성 */
            UnifyTest *t = test_new(TEST_PAIRWISE_EQUAL);
            t->variable = sexpr_copy(pattern);
            t->path = sexpr_copy(prev->path);
            t->path2 = sexpr_copy(path);
            /* 바인딩 사양은 마지막 경로로 갱신 */
            sexpr_free(prev->path);
            prev->path = sexpr_copy(path);
            return prepend_test(t, tests);
        } else {
            /* 새 바인딩 */
            UnifyTest *t = test_new(TEST_BIND_VAR);
            t->variable = sexpr_copy(pattern);
            t->path = sexpr_copy(path);
            return prepend_test(t, tests);
        }
    }

    if (sexpr_is_variable(pattern)) {
        /* 이미 바인딩된 변수 (*bound-vars*에 있음): 동등 비교 */
        UnifyTest *t = test_new(TEST_EQUAL_VAR);
        t->variable = sexpr_copy(pattern);
        t->path = sexpr_copy(path);
        return prepend_test(t, tests);
    }

    if (sexpr_is_number(pattern)) {
        /* (and (numberp path) (= num path)) */
        UnifyTest *t = test_new(TEST_EQUAL_NUMBER);
        t->number = pattern->number;
        t->path = sexpr_copy(path);
        return prepend_test(t, tests);
    }

    if (!sexpr_is_cons(pattern)) {
        /* 아톰(심볼): (equal 'sym path) */
        UnifyTest *t = test_new(TEST_EQUAL_SYMBOL);
        t->symbol = sexpr_copy(pattern);
        t->path = sexpr_copy(path);
        return prepend_test(t, tests);
    }

    /* 리스트: 재귀 */
    /* 먼저 (consp path) 테스트 추가 */
    UnifyTest *consp = test_new(TEST_CONSP);
    consp->path = sexpr_copy(path);
    tests = prepend_test(consp, tests);

    /* car 방향으로 재귀 */
    SExpr *car_path = make_path_car(path);
    tests = generate_unify_tests(pattern->cons.car, vars,
                                 tests, car_path);
    sexpr_free(car_path);

    /* cdr 방향으로 재귀 */
    SExpr *cdr_path = make_path_cdr(path);
    tests = generate_unify_tests(pattern->cons.cdr, vars,
                                 tests, cdr_path);
    sexpr_free(cdr_path);

    return tests;
}

/* ================================================================ */
/* generate-pairwise-tests                                          */
/* ================================================================ */

UnifyTest *generate_pairwise_tests(List *paths) {
    if (!paths || paths->size < 2) return NULL;
    UnifyTest *chain = NULL;
    for (int i = 0; i < paths->size - 1; i++) {
        UnifyTest *t = test_new(TEST_PAIRWISE_EQUAL);
        t->path = sexpr_copy((SExpr *)paths->data[i]);
        t->path2 = sexpr_copy((SExpr *)paths->data[i + 1]);
        t->next = chain;
        chain = t;
    }
    return chain;
}

/* ================================================================ */
/* generate-match-body: 패턴 매칭 본문 생성                        */
/*                                                                  */
/* Lisp 원본:                                                      */
/* (defun generate-match-body (pattern vars extra-test ...)         */
/*   (dolist (test (generate-unify-tests pattern vars nil 'P))     */
/*     (cond ((variable? (car test)) ...)                          */
/*           (t (push test structure-tests))))                     */
/*   ...)                                                          */
/* ================================================================ */

MatchBody *generate_match_body(SExpr *pattern, List *vars,
                               SExpr *extra_test) {
    MatchBody *mb = (MatchBody *)calloc(1, sizeof(MatchBody));
    mb->binding_specs = list_new();

    /* 경로 루트 */
    SExpr *root_path = sexpr_symbol("P");

    /* 유니피케이션 테스트 생성 */
    UnifyTest *all_tests = generate_unify_tests(pattern, vars,
                                                NULL, root_path);
    sexpr_free(root_path);

    /* 테스트 분류: 구조 테스트와 바인딩 사양 분리 */
    /* 바인딩 테스트에서 binding_specs 추출 */
    for (UnifyTest *t = all_tests; t; t = t->next) {
        if (t->type == TEST_BIND_VAR) {
            /* 바인딩 사양에 경로 추가 */
            list_push(mb->binding_specs, sexpr_copy(t->path));
        }
    }

    /* extra_test 처리 */
    if (extra_test && !sexpr_is_nil(extra_test)) {
        /* 자유 변수가 남아있으면 오류 */
        List *free_vars = pattern_free_variables(extra_test);
        if (free_vars->size > 0) {
            fprintf(stderr,
                    "Error: Rule test includes free variable: %s\n",
                    sexpr_to_string(extra_test));
        }
        list_free(free_vars);
    }

    mb->tests = all_tests;
    return mb;
}

/* ================================================================ */
/* 런타임 패턴 매칭 실행                                           */
/*                                                                  */
/* generate-match-body가 생성한 테스트 체인을                       */
/* 실제 S-expression P에 적용하여 매칭 수행                        */
/* ================================================================ */

MatchResult execute_match(UnifyTest *tests, List *binding_specs,
                          SExpr *p) {
    MatchResult result;
    result.ok = false;
    result.bindings = list_new();

    /* 바인딩 값 저장을 위한 임시 맵 (변수명 -> 값) */
    List *bind_vars = list_new();   /* SExpr* 변수 */
    List *bind_vals = list_new();   /* SExpr* 값 */

    /* 모든 테스트 실행 */
    for (UnifyTest *t = tests; t; t = t->next) {
        SExpr *val;
        switch (t->type) {
        case TEST_CONSP:
            val = follow_path(p, t->path);
            if (!sexpr_is_cons(val)) goto fail;
            break;

        case TEST_NULL:
            val = follow_path(p, t->path);
            if (!sexpr_is_nil(val)) goto fail;
            break;

        case TEST_EQUAL_SYMBOL:
            val = follow_path(p, t->path);
            if (!sexpr_is_symbol(val)) goto fail;
            if (strcmp(val->symbol, t->symbol->symbol) != 0) goto fail;
            break;

        case TEST_EQUAL_NUMBER:
            val = follow_path(p, t->path);
            if (!sexpr_is_number(val)) goto fail;
            if (val->number != t->number) goto fail;
            break;

        case TEST_EQUAL_VAR: {
            /* 이미 바인딩된 변수와 비교 */
            val = follow_path(p, t->path);
            /* bind_vars에서 해당 변수의 값 찾기 */
            SExpr *bound_val = NULL;
            for (int i = 0; i < bind_vars->size; i++) {
                if (sexpr_equal((SExpr *)bind_vars->data[i],
                                t->variable)) {
                    bound_val = (SExpr *)bind_vals->data[i];
                    break;
                }
            }
            if (bound_val) {
                if (!sexpr_equal(val, bound_val)) goto fail;
            }
            break;
        }

        case TEST_BIND_VAR:
            val = follow_path(p, t->path);
            /* 변수에 값 바인딩 */
            list_push(bind_vars, t->variable);
            list_push(bind_vals, val);
            break;

        case TEST_PAIRWISE_EQUAL: {
            SExpr *v1 = follow_path(p, t->path);
            SExpr *v2 = follow_path(p, t->path2);
            if (!sexpr_equal(v1, v2)) goto fail;
            break;
        }
        }
    }

    /* 성공: binding_specs 순서대로 바인딩 값 수집 */
    for (int i = 0; i < binding_specs->size; i++) {
        SExpr *spec_path = (SExpr *)binding_specs->data[i];
        SExpr *val = follow_path(p, spec_path);
        list_push(result.bindings, val);
    }
    result.ok = true;
    list_free(bind_vars);
    list_free(bind_vals);
    return result;

fail:
    list_free(result.bindings);
    result.bindings = NULL;
    list_free(bind_vars);
    list_free(bind_vals);
    return result;
}
