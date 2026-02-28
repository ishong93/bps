/* -*- C -*- */

/* S-expression representation, unification, and open-coding */
/* Converted from unify.lisp and funify.lisp */

#ifndef UNIFY_H
#define UNIFY_H

#include "atms.h"

/* ================================================================ */
/* S-expression: Lisp-like cons cell representation                 */
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

/* S-expression construction */
SExpr *sexpr_symbol(const char *name);
SExpr *sexpr_number(double val);
SExpr *sexpr_cons(SExpr *car, SExpr *cdr);
SExpr *sexpr_nil(void);
SExpr *sexpr_copy(SExpr *e);
void   sexpr_free(SExpr *e);

/* S-expression predicates */
bool sexpr_is_nil(SExpr *e);
bool sexpr_is_symbol(SExpr *e);
bool sexpr_is_number(SExpr *e);
bool sexpr_is_cons(SExpr *e);
bool sexpr_is_variable(SExpr *e);
bool sexpr_equal(SExpr *a, SExpr *b);

/* S-expression access */
SExpr *sexpr_car(SExpr *e);
SExpr *sexpr_cdr(SExpr *e);

/* S-expression printing */
char *sexpr_to_string(SExpr *e);
void  sexpr_print(SExpr *e, FILE *stream);

/* Build an S-expression from a simple string like "(implies ?x ?y)" */
SExpr *sexpr_parse(const char *str);

/* ================================================================ */
/* Bindings: association list of (variable . value) pairs           */
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
/* Unification                                                      */
/* ================================================================ */

/* Returns non-NULL bindings on success, NULL on failure.
 * An empty Binding* (no pairs) indicates success with no bindings. */
#define UNIFY_FAIL NULL
#define UNIFY_SUCCESS_EMPTY ((Binding *)-1)

/* Unify two S-expressions.
 * Returns extended bindings on success, UNIFY_FAIL on failure.
 * Pass NULL for initial bindings. */
Binding *unify(SExpr *a, SExpr *b, Binding *bindings);
Binding *unify_variable(SExpr *var, SExpr *exp, Binding *bindings);
bool     free_in(SExpr *var, SExpr *exp, Binding *bindings);
SExpr   *subst_bindings(Binding *bindings, SExpr *pattern);

/* ================================================================ */
/* Open-coding pattern matching (from funify.lisp)                  */
/* ================================================================ */

/* Quotize: convert pattern to evaluable form */
SExpr *quotize(SExpr *pattern);

/* Pattern free variables */
List *pattern_free_variables(SExpr *pattern);

#endif /* UNIFY_H */
