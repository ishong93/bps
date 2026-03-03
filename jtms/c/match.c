/* -*- C -*- */

/**** 대수 조작 시스템을 위한 패턴 매처 ****/
/* match.lisp에서 변환 */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
 * and Johan de Kleer, the Xerox Corporation.
 * All rights reserved. */

/* 이 버전은 G.J. Sussman의 scheme 매처에서 영감을 받았다.
 * 명확성을 위해 continuation-passing을 사용하지 않는다. */

#include "match.h"
#include <ctype.h>

/* ================================================================ */
/* :FAIL 센티넬                                                     */
/* ================================================================ */

SExpr FAIL_SENTINEL = { SEXPR_SYMBOL, { .symbol = ":FAIL" } };

/* 부동소수점 비교 허용 오차 */
double match_tolerance = 1.0e-6;

/* 전역 eval 콜백 */
static EvalCallback g_eval_callback = NULL;

void match_set_eval_callback(EvalCallback cb) {
    g_eval_callback = cb;
}

/* ================================================================ */
/* S-expression 생성                                                */
/* ================================================================ */

SExpr *sexpr_symbol(const char *name) {
    SExpr *e = (SExpr *)calloc(1, sizeof(SExpr));
    e->type = SEXPR_SYMBOL;
    e->symbol = strdup(name);
    return e;
}

SExpr *sexpr_int(int n) {
    SExpr *e = (SExpr *)calloc(1, sizeof(SExpr));
    e->type = SEXPR_INT;
    e->integer = n;
    return e;
}

SExpr *sexpr_float(double f) {
    SExpr *e = (SExpr *)calloc(1, sizeof(SExpr));
    e->type = SEXPR_FLOAT;
    e->floating = f;
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
    case SEXPR_INT:    return sexpr_int(e->integer);
    case SEXPR_FLOAT:  return sexpr_float(e->floating);
    case SEXPR_CONS:   return sexpr_cons(sexpr_copy(e->cons.car),
                                         sexpr_copy(e->cons.cdr));
    case SEXPR_NIL:    return sexpr_nil();
    }
    return NULL;
}

void sexpr_free(SExpr *e) {
    if (!e) return;
    if (e == MATCH_FAIL) return; /* 센티넬은 해제하지 않는다 */
    if (e->type == SEXPR_SYMBOL) free((void *)e->symbol);
    else if (e->type == SEXPR_CONS) {
        sexpr_free(e->cons.car);
        sexpr_free(e->cons.cdr);
    }
    free(e);
}

/* ================================================================ */
/* S-expression 술어                                                */
/* ================================================================ */

bool sexpr_is_nil(SExpr *e)    { return !e || e->type == SEXPR_NIL; }
bool sexpr_is_symbol(SExpr *e) { return e && e->type == SEXPR_SYMBOL; }
bool sexpr_is_int(SExpr *e)    { return e && e->type == SEXPR_INT; }
bool sexpr_is_float(SExpr *e)  { return e && e->type == SEXPR_FLOAT; }
bool sexpr_is_number(SExpr *e) { return e && (e->type == SEXPR_INT || e->type == SEXPR_FLOAT); }
bool sexpr_is_cons(SExpr *e)   { return e && e->type == SEXPR_CONS; }

/* sexpr_eq: 포인터 동등성 (Lisp eq) */
bool sexpr_eq(SExpr *a, SExpr *b) {
    if (a == b) return true;
    if (!a || !b) return false;
    /* 심볼은 이름 기반 동등성 사용 (인턴된 것처럼) */
    if (a->type == SEXPR_SYMBOL && b->type == SEXPR_SYMBOL)
        return strcmp(a->symbol, b->symbol) == 0;
    return false;
}

/* sexpr_equal: 구조적 동등성 (Lisp equal) */
bool sexpr_equal(SExpr *a, SExpr *b) {
    if (a == b) return true;
    if (!a || !b) return false;
    if (a->type != b->type) {
        /* INT와 FLOAT 사이의 비교 */
        if (sexpr_is_number(a) && sexpr_is_number(b)) {
            double va = (a->type == SEXPR_INT) ? (double)a->integer : a->floating;
            double vb = (b->type == SEXPR_INT) ? (double)b->integer : b->floating;
            return va == vb;
        }
        return false;
    }
    switch (a->type) {
    case SEXPR_NIL:    return true;
    case SEXPR_SYMBOL: return strcmp(a->symbol, b->symbol) == 0;
    case SEXPR_INT:    return a->integer == b->integer;
    case SEXPR_FLOAT:  return a->floating == b->floating;
    case SEXPR_CONS:   return sexpr_equal(a->cons.car, b->cons.car) &&
                              sexpr_equal(a->cons.cdr, b->cons.cdr);
    }
    return false;
}

/* equal?: 부동소수점 허용 오차를 고려한 동등성 비교 */
/* (defun equal? (a b)
 *   (cond ((and (floatp a) (floatp b)) (< (abs (- a b)) *tol*))
 *         (t (equal a b)))) */
bool sexpr_equal_tol(SExpr *a, SExpr *b) {
    if (sexpr_is_float(a) && sexpr_is_float(b)) {
        return fabs(a->floating - b->floating) < match_tolerance;
    }
    /* float와 int 사이의 비교도 허용 */
    if (sexpr_is_number(a) && sexpr_is_number(b)) {
        double va = (a->type == SEXPR_INT) ? (double)a->integer : a->floating;
        double vb = (b->type == SEXPR_INT) ? (double)b->integer : b->floating;
        if (sexpr_is_float(a) || sexpr_is_float(b)) {
            return fabs(va - vb) < match_tolerance;
        }
        return va == vb;
    }
    return sexpr_equal(a, b);
}

/* ================================================================ */
/* S-expression 접근                                                */
/* ================================================================ */

SExpr *sexpr_car(SExpr *e) {
    return (e && e->type == SEXPR_CONS) ? e->cons.car : NULL;
}

SExpr *sexpr_cdr(SExpr *e) {
    return (e && e->type == SEXPR_CONS) ? e->cons.cdr : NULL;
}

SExpr *sexpr_cadr(SExpr *e)  { return sexpr_car(sexpr_cdr(e)); }
SExpr *sexpr_caddr(SExpr *e) { return sexpr_car(sexpr_cdr(sexpr_cdr(e))); }
SExpr *sexpr_caar(SExpr *e)  { return sexpr_car(sexpr_car(e)); }
SExpr *sexpr_cadar(SExpr *e) { return sexpr_car(sexpr_cdr(sexpr_car(e))); }

/* ================================================================ */
/* 리스트 유틸리티                                                  */
/* ================================================================ */

int sexpr_length(SExpr *e) {
    int n = 0;
    while (sexpr_is_cons(e)) { n++; e = e->cons.cdr; }
    return n;
}

SExpr *sexpr_append(SExpr *a, SExpr *b) {
    if (sexpr_is_nil(a)) return b;
    return sexpr_cons(sexpr_copy(a->cons.car),
                      sexpr_append(a->cons.cdr, b));
}

SExpr *sexpr_nreverse(SExpr *e) {
    SExpr *prev = sexpr_nil();
    while (sexpr_is_cons(e)) {
        SExpr *next = e->cons.cdr;
        e->cons.cdr = prev;
        prev = e;
        e = next;
    }
    return prev;
}

SExpr *sexpr_mapcar(SExpr *(*fn)(SExpr *), SExpr *list) {
    if (sexpr_is_nil(list)) return sexpr_nil();
    return sexpr_cons(fn(sexpr_car(list)),
                      sexpr_mapcar(fn, sexpr_cdr(list)));
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
    case SEXPR_INT:
        *pos += snprintf(buf + *pos, maxlen - *pos, "%d", e->integer);
        break;
    case SEXPR_FLOAT:
        *pos += snprintf(buf + *pos, maxlen - *pos, "%g", e->floating);
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

static char _sexpr_buf[4096];

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
    if (**s == '\0') return sexpr_nil();
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
    /* 원자: 공백이나 )까지 읽기 */
    const char *start = *s;
    while (**s && !isspace((unsigned char)**s) &&
           **s != '(' && **s != ')') (*s)++;
    int len = (int)(*s - start);
    char *tok = (char *)malloc(len + 1);
    memcpy(tok, start, len);
    tok[len] = '\0';

    /* NIL 확인 */
    if (strcmp(tok, "NIL") == 0 || strcmp(tok, "nil") == 0) {
        free(tok);
        return sexpr_nil();
    }

    /* 정수 시도 */
    char *end;
    long lval = strtol(tok, &end, 10);
    if (*end == '\0') {
        free(tok);
        return sexpr_int((int)lval);
    }

    /* 부동소수점 시도 */
    double dval = strtod(tok, &end);
    if (*end == '\0') {
        free(tok);
        return sexpr_float(dval);
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
/* 제한 술어 레지스트리                                             */
/* 이름 → 함수 포인터 맵핑                                         */
/* ================================================================ */

#define MAX_RESTRICTIONS 64

typedef struct {
    const char *name;
    VarRestriction fn;
} RestrictionEntry;

static RestrictionEntry restriction_table[MAX_RESTRICTIONS];
static int restriction_count = 0;

void match_register_restriction(const char *name, VarRestriction fn) {
    /* 기존 항목 업데이트 */
    for (int i = 0; i < restriction_count; i++) {
        if (strcmp(restriction_table[i].name, name) == 0) {
            restriction_table[i].fn = fn;
            return;
        }
    }
    if (restriction_count < MAX_RESTRICTIONS) {
        restriction_table[restriction_count].name = strdup(name);
        restriction_table[restriction_count].fn = fn;
        restriction_count++;
    }
}

VarRestriction match_lookup_restriction(const char *name) {
    for (int i = 0; i < restriction_count; i++) {
        if (strcmp(restriction_table[i].name, name) == 0)
            return restriction_table[i].fn;
    }
    return NULL;
}

/* ================================================================ */
/* 변수 정의                                                        */
/* 두 종류의 변수가 있다.                                           */
/* 요소 변수는 리스트의 단일 요소와 매칭한다.                       */
/* 세그먼트 변수는 리스트의 (빈) 부분과 매칭한다.                   */
/* 요소 변수 형식: (? <변수명> <선택적 제한>)                       */
/* 세그먼트 변수 형식: (?? <변수명> <선택적 제한>)                  */
/* ================================================================ */

/* (defun element-var? (x) (and (consp x) (eq (car x) '?))) */
bool element_var_p(SExpr *x) {
    return sexpr_is_cons(x) &&
           sexpr_is_symbol(sexpr_car(x)) &&
           strcmp(sexpr_car(x)->symbol, "?") == 0;
}

/* (defun segment-var? (x) (and (consp x) (eq (car x) '??))) */
bool segment_var_p(SExpr *x) {
    return sexpr_is_cons(x) &&
           sexpr_is_symbol(sexpr_car(x)) &&
           strcmp(sexpr_car(x)->symbol, "??") == 0;
}

/* (defun pattern-variable? (x) (or (element-var? x) (segment-var? x))) */
bool pattern_variable_p(SExpr *x) {
    return element_var_p(x) || segment_var_p(x);
}

/* (defun var-name (x) (cadr x)) */
SExpr *var_name(SExpr *x) {
    return sexpr_cadr(x);
}

/* (defun var-restriction (x) (caddr x))
 * 제한은 심볼 이름으로 지정되며, 레지스트리에서 함수 포인터를 조회한다 */
VarRestriction var_restriction_fn(SExpr *x) {
    SExpr *restriction = sexpr_caddr(x);
    if (sexpr_is_nil(restriction) || !restriction) return NULL;
    if (sexpr_is_symbol(restriction)) {
        return match_lookup_restriction(restriction->symbol);
    }
    return NULL;
}

/* ================================================================ */
/* 딕셔너리 항목                                                    */
/* 항목 형식:                                                       */
/*   (<name> <value>)  — 요소 변수 (position이 NIL)                 */
/*   (<name> <beg> <end>)  — 세그먼트 변수 (position = (beg . end)) */
/* ================================================================ */

/* (defun lookup-var (var dict) (assoc (var-name var) dict)) */
SExpr *lookup_var(SExpr *var, SExpr *dict) {
    SExpr *name = var_name(var);
    SExpr *cur = dict;
    while (sexpr_is_cons(cur)) {
        SExpr *entry = sexpr_car(cur);
        if (sexpr_is_cons(entry) && sexpr_equal(sexpr_car(entry), name)) {
            return entry;
        }
        cur = sexpr_cdr(cur);
    }
    return NULL;
}

/* (defun segment-beg (entry) (cadr entry)) */
SExpr *segment_beg(SExpr *entry) {
    return sexpr_cadr(entry);
}

/* (defun segment-end (entry) (caddr entry)) */
SExpr *segment_end(SExpr *entry) {
    return sexpr_caddr(entry);
}

/* (defun segment->list (start end)
 *   (do ((point start (cdr point)) (l nil))
 *       ((eq point end) (nreverse l))
 *     (push (car point) l))) */
SExpr *segment_to_list(SExpr *start, SExpr *end) {
    SExpr *result = sexpr_nil();
    SExpr *point = start;
    while (!sexpr_eq(point, end) && sexpr_is_cons(point)) {
        result = sexpr_cons(sexpr_copy(sexpr_car(point)), result);
        point = sexpr_cdr(point);
    }
    return sexpr_nreverse(result);
}

/* (defun var-value (var dict)
 *   (setq entry (lookup-var var dict))
 *   (unless entry (error ...))
 *   (cond ((= (length entry) 2) (cadr entry))
 *         (t (segment->list (cadr entry) (caddr entry))))) */
SExpr *var_value(SExpr *var, SExpr *dict) {
    SExpr *entry = lookup_var(var, dict);
    if (!entry) {
        fprintf(stderr, "Not bound variable: ");
        sexpr_print(var, stderr);
        fprintf(stderr, "\n");
        return sexpr_nil();
    }
    int len = sexpr_length(entry);
    if (len == 2) {
        /* 요소 변수 */
        return sexpr_cadr(entry);
    } else {
        /* 세그먼트 변수: segment->list(beg, end) */
        return segment_to_list(sexpr_cadr(entry), sexpr_caddr(entry));
    }
}

/* (defun bind-element-var (name dat dict)
 *   (cons (list name dat) dict)) */
SExpr *bind_element_var(SExpr *name, SExpr *dat, SExpr *dict) {
    SExpr *entry = sexpr_cons(sexpr_copy(name),
                              sexpr_cons(sexpr_copy(dat), sexpr_nil()));
    return sexpr_cons(entry, dict);
}

/* (defun bind-segment-var (name beg end dict)
 *   (cons (list name beg end) dict)) */
SExpr *bind_segment_var(SExpr *name, SExpr *beg, SExpr *end, SExpr *dict) {
    SExpr *entry = sexpr_cons(sexpr_copy(name),
                   sexpr_cons(beg,   /* beg와 end는 원본 리스트의 포인터 */
                   sexpr_cons(end ? end : sexpr_nil(),
                              sexpr_nil())));
    return sexpr_cons(entry, dict);
}

/* ================================================================ */
/* 패턴 매칭                                                        */
/* ================================================================ */

static SExpr *match_element_var(SExpr *pat, SExpr *dat, SExpr *dict);
static SExpr *match_segment_var(SExpr *pat, SExpr *dat, SExpr *dict);

/* (defun match (pat dat &optional (dict nil))
 *   (cond ((eq dict :FAIL) :FAIL)
 *         ((eq pat dat) dict)
 *         ((element-var? pat) (match-element-var pat dat dict))
 *         ((not (consp pat)) (if (equal? pat dat) dict :FAIL))
 *         ((segment-var? (car pat)) (match-segment-var pat dat dict))
 *         ((not (consp dat)) :FAIL)
 *         (t (match (cdr pat) (cdr dat) (match (car pat) (car dat) dict))))) */
SExpr *match(SExpr *pat, SExpr *dat, SExpr *dict) {
    /* 실패 전파 */
    if (dict == MATCH_FAIL) return MATCH_FAIL;

    /* 포인터가 같으면 즉시 성공 */
    if (pat == dat) return dict;

    /* 요소 변수 매칭 */
    if (element_var_p(pat))
        return match_element_var(pat, dat, dict);

    /* 원자이면 equal? 비교 */
    if (!sexpr_is_cons(pat)) {
        if (sexpr_equal_tol(pat, dat)) return dict;
        return MATCH_FAIL;
    }

    /* 세그먼트 변수 매칭 */
    if (segment_var_p(sexpr_car(pat)))
        return match_segment_var(pat, dat, dict);

    /* 데이터가 원자이면 실패 */
    if (!sexpr_is_cons(dat)) return MATCH_FAIL;

    /* 재귀적 매칭: car를 먼저 매칭한 후 cdr을 매칭 */
    SExpr *new_dict = match(sexpr_car(pat), sexpr_car(dat), dict);
    return match(sexpr_cdr(pat), sexpr_cdr(dat), new_dict);
}

/* (defun match-element-var (pat dat dict)
 *   (setq entry (lookup-var pat dict))
 *   (cond (entry (if (equal? (cadr entry) dat) dict :FAIL))
 *         (t (setq pred (var-restriction pat))
 *            (cond ((or (not pred) (funcall pred dat))
 *                   (bind-element-var (var-name pat) dat dict))
 *                  (t :FAIL))))) */
static SExpr *match_element_var(SExpr *pat, SExpr *dat, SExpr *dict) {
    SExpr *entry = lookup_var(pat, dict);
    if (entry) {
        /* 이미 바인딩된 변수: 값이 일치하는지 확인 */
        if (sexpr_equal_tol(sexpr_cadr(entry), dat))
            return dict;
        return MATCH_FAIL;
    }
    /* 제한 술어 확인 */
    VarRestriction pred = var_restriction_fn(pat);
    if (!pred || pred(dat)) {
        return bind_element_var(var_name(pat), dat, dict);
    }
    return MATCH_FAIL;
}

/* ================================================================ */
/* 세그먼트 변수 매칭                                               */
/* 비결정적이므로 반복이 필요하다.                                   */
/* ================================================================ */

/* (defun check-segment (dat beg end)
 *   (cond ((eq beg end) dat)
 *         ((not (consp dat)) :FAIL)
 *         ((equal? (car dat) (car beg))
 *          (check-segment (cdr dat) (cdr beg) end))
 *         (t :FAIL))) */
static SExpr *check_segment(SExpr *dat, SExpr *beg, SExpr *end) {
    if (sexpr_eq(beg, end)) return dat;
    if (!sexpr_is_cons(dat)) return MATCH_FAIL;
    if (sexpr_equal_tol(sexpr_car(dat), sexpr_car(beg)))
        return check_segment(sexpr_cdr(dat), sexpr_cdr(beg), end);
    return MATCH_FAIL;
}

/* (defun match-segment-var (pat dat dict)
 *   (setq entry (lookup-var (car pat) dict))
 *   (cond (entry  ;; 매칭 확인
 *          (setq rest (check-segment dat (segment-beg entry) (segment-end entry)))
 *          (if (eq rest :FAIL) :FAIL (match (cdr pat) rest dict)))
 *         (t  ;; 대안 세그먼트 바인딩 탐색
 *          (try-segment-bindings (car pat) (cdr pat) dat dict)))) */
static SExpr *try_segment_bindings(SExpr *var, SExpr *pat, SExpr *dat, SExpr *dict);

static SExpr *match_segment_var(SExpr *pat, SExpr *dat, SExpr *dict) {
    SExpr *entry = lookup_var(sexpr_car(pat), dict);
    if (entry) {
        /* 이미 바인딩된 세그먼트 변수: 매칭 확인 */
        SExpr *rest = check_segment(dat, segment_beg(entry), segment_end(entry));
        if (rest == MATCH_FAIL) return MATCH_FAIL;
        return match(sexpr_cdr(pat), rest, dict);
    }
    /* 대안 세그먼트 바인딩 탐색 */
    return try_segment_bindings(sexpr_car(pat), sexpr_cdr(pat), dat, dict);
}

/* (defun try-segment-bindings (var pat dat dict)
 *   (setq name (var-name var) pred (var-restriction var) beg dat)
 *   (do ((end dat (cdr end)) (ndict nil))
 *       ((null end) ...)
 *     (when (or (null pred) (funcall pred (segment->list beg end)))
 *       (setq ndict (match pat end (bind-segment-var name beg end dict)))
 *       (unless (eq ndict :FAIL) (return ndict))))) */
static SExpr *try_segment_bindings(SExpr *var, SExpr *pat, SExpr *dat, SExpr *dict) {
    SExpr *name = var_name(var);
    VarRestriction pred = var_restriction_fn(var);
    SExpr *beg = dat;

    /* end를 dat부터 끝까지 순회하며 세그먼트 바인딩을 시도 */
    SExpr *end = dat;
    while (sexpr_is_cons(end)) {
        if (!pred || pred(segment_to_list(beg, end))) {
            SExpr *ndict = match(pat, end,
                                 bind_segment_var(name, beg, end, dict));
            if (ndict != MATCH_FAIL) return ndict;
        }
        end = sexpr_cdr(end);
    }
    /* 마지막: end가 NIL일 때 (세그먼트가 나머지 전체와 매칭) */
    if (!pred || pred(segment_to_list(beg, NULL))) {
        return match(pat, sexpr_nil(),
                     bind_segment_var(name, beg, NULL, dict));
    }
    return MATCH_FAIL;
}

/* ================================================================ */
/* 대입 수행                                                        */
/* ================================================================ */

/* (defun substitute-in (exp dict)
 *   (cond ((null exp) nil)
 *         ((element-var? exp) (var-value exp dict))
 *         ((consp exp)
 *          (cond ((segment-var? (car exp))
 *                 (append (var-value (car exp) dict)
 *                         (substitute-in (cdr exp) dict)))
 *                ((eq (car exp) :EVAL)
 *                 (eval (substitute-in (cadr exp) dict)))
 *                ((and (consp (car exp)) (eq (caar exp) :SPLICE))
 *                 (append (eval (substitute-in (cadar exp) dict))
 *                         (substitute-in (cdr exp) dict)))
 *                (t (cons (substitute-in (car exp) dict)
 *                         (substitute-in (cdr exp) dict)))))
 *         (t exp))) */
SExpr *substitute_in(SExpr *exp, SExpr *dict) {
    if (sexpr_is_nil(exp)) return sexpr_nil();

    /* 요소 변수 → 값 */
    if (element_var_p(exp))
        return sexpr_copy(var_value(exp, dict));

    if (sexpr_is_cons(exp)) {
        /* 세그먼트 변수 → append(value, substitute rest) */
        if (segment_var_p(sexpr_car(exp))) {
            SExpr *val = var_value(sexpr_car(exp), dict);
            SExpr *rest = substitute_in(sexpr_cdr(exp), dict);
            return sexpr_append(val, rest);
        }
        /* :EVAL → eval 콜백 호출 */
        if (sexpr_is_symbol(sexpr_car(exp)) &&
            strcmp(sexpr_car(exp)->symbol, ":EVAL") == 0) {
            SExpr *arg = substitute_in(sexpr_cadr(exp), dict);
            if (g_eval_callback) {
                return g_eval_callback(arg);
            }
            return arg; /* 콜백이 없으면 인자를 그대로 반환 */
        }
        /* :SPLICE → eval 후 append */
        if (sexpr_is_cons(sexpr_car(exp)) &&
            sexpr_is_symbol(sexpr_caar(exp)) &&
            strcmp(sexpr_caar(exp)->symbol, ":SPLICE") == 0) {
            SExpr *arg = substitute_in(sexpr_cadar(exp), dict);
            SExpr *spliced = g_eval_callback ? g_eval_callback(arg) : arg;
            SExpr *rest = substitute_in(sexpr_cdr(exp), dict);
            return sexpr_append(spliced, rest);
        }
        /* 일반 cons: 재귀적 대입 */
        return sexpr_cons(substitute_in(sexpr_car(exp), dict),
                          substitute_in(sexpr_cdr(exp), dict));
    }

    /* 원자 → 그대로 복사 */
    return sexpr_copy(exp);
}
