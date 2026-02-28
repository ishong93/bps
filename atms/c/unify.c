/* -*- C -*- */

/* S-expression representation, unification, and open-coding */
/* Converted from unify.lisp and funify.lisp */

#include "unify.h"
#include <ctype.h>

/* ================================================================ */
/* S-expression construction                                        */
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
/* S-expression predicates                                          */
/* ================================================================ */

bool sexpr_is_nil(SExpr *e) { return !e || e->type == SEXPR_NIL; }
bool sexpr_is_symbol(SExpr *e) { return e && e->type == SEXPR_SYMBOL; }
bool sexpr_is_number(SExpr *e) { return e && e->type == SEXPR_NUMBER; }
bool sexpr_is_cons(SExpr *e)   { return e && e->type == SEXPR_CONS; }

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
/* S-expression printing                                            */
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
/* S-expression parser                                              */
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
    /* Atom: read until whitespace or ) */
    const char *start = *s;
    while (**s && !isspace((unsigned char)**s) &&
           **s != '(' && **s != ')') (*s)++;
    int len = (int)(*s - start);
    char *tok = (char *)malloc(len + 1);
    memcpy(tok, start, len);
    tok[len] = '\0';
    /* Try number */
    char *end;
    double num = strtod(tok, &end);
    if (*end == '\0') {
        free(tok);
        return sexpr_number(num);
    }
    /* Symbol */
    SExpr *e = sexpr_symbol(tok);
    free(tok);
    return e;
}

SExpr *sexpr_parse(const char *str) {
    return parse_expr(&str);
}

/* ================================================================ */
/* Bindings                                                         */
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
/* Unification                                                      */
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
    if (sexpr_equal(a, b)) return bindings ? bindings : UNIFY_SUCCESS_EMPTY;
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
/* Pattern free variables                                           */
/* ================================================================ */

static void pfv_helper(SExpr *pattern, List *vars, List *bound) {
    if (sexpr_is_nil(pattern)) return;
    if (sexpr_is_variable(pattern)) {
        /* Check if already in vars or bound */
        for (int i = 0; i < vars->size; i++) {
            SExpr *v = (SExpr *)vars->data[i];
            if (sexpr_equal(v, pattern)) return;
        }
        for (int i = 0; i < bound->size; i++) {
            SExpr *v = (SExpr *)bound->data[i];
            if (sexpr_equal(v, pattern)) return;
        }
        list_push(vars, pattern);
        return;
    }
    if (!sexpr_is_cons(pattern)) return;
    pfv_helper(pattern->cons.car, vars, bound);
    pfv_helper(pattern->cons.cdr, vars, bound);
}

List *pattern_free_variables(SExpr *pattern) {
    List *vars = list_new();
    List *bound = list_new();
    pfv_helper(pattern, vars, bound);
    list_free(bound);
    return vars;
}

/* ================================================================ */
/* Quotize                                                          */
/* ================================================================ */

SExpr *quotize(SExpr *pattern) {
    if (sexpr_is_nil(pattern)) return sexpr_nil();
    if (sexpr_is_variable(pattern)) return sexpr_copy(pattern);
    if (!sexpr_is_cons(pattern)) return sexpr_copy(pattern);
    if (sexpr_is_symbol(pattern->cons.car) &&
        strcmp(pattern->cons.car->symbol, ":EVAL") == 0) {
        return sexpr_copy(sexpr_car(pattern->cons.cdr));
    }
    return sexpr_cons(quotize(pattern->cons.car),
                      quotize(pattern->cons.cdr));
}
