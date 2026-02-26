/* -*- C -*- */

/* ATMS-based planner */
/* Converted from aplanr.lisp, plan-a.lisp, plan-e.lisp, */
/*   bcode.lisp, blocks.lisp */

#ifndef PLANNER_H
#define PLANNER_H

#include "atre.h"

/* ================================================================ */
/* Planning problem structure                                       */
/* ================================================================ */

typedef struct Plnpr {
    char  *title;
    Atre  *atre;
    List  *basis_set;        /* List of List<SExpr*> choice sets */
    List  *operators;        /* List of OpEntry* */
    bool   debugging;
    /* Cached results */
    List  *states;           /* List of Env* */
    List  *transitions;      /* List of TransEntry* */
    List  *plan;             /* Result plan (list of env/op pairs) */
} Plnpr;

/* ================================================================ */
/* Operator structure                                               */
/* ================================================================ */

typedef struct Operator {
    SExpr *form;             /* e.g., (Pickup ?x) */
    SExpr *preconditions;    /* List of precondition patterns */
    SExpr *add_list;         /* Facts to add */
    SExpr *delete_list;      /* Facts to remove */
} Operator;

typedef struct OpEntry {
    char     *name;
    Operator *op;
} OpEntry;

/* Transition: (op_inst . result_state) */
typedef struct TransPair {
    SExpr *op_inst;
    Env   *result;
} TransPair;

/* TransEntry: (state, list of TransPair) */
typedef struct TransEntry {
    Env  *state;
    List *pairs;             /* List of TransPair* */
} TransEntry;

/* ================================================================ */
/* Global planner pointer                                           */
/* ================================================================ */

extern Plnpr *current_plnpr;

/* ================================================================ */
/* Planner API                                                      */
/* ================================================================ */

/* Creation */
Plnpr *create_planning_problem(const char *title, List *basis_set);
void   in_plnpr(Plnpr *plnpr);
void   set_debug_plnpr(bool state);
void   setup_choice_sets(void);

/* Operator management */
void register_operator(SExpr *form, SExpr *preconditions,
                       SExpr *add_list, SExpr *delete_list,
                       SExpr *test_expr);
Operator *fetch_operator(const char *op_name);
List     *find_applicable_operators(Env *state);
Env      *apply_operator(Env *state, SExpr *op_inst);

/* Envisioner */
void envision(void);
void show_envisionment(FILE *stream);

/* Plan search via envisionment */
List *find_plan(List *start_facts, List *goal_facts);

/* Antecedent planner (Plan-A) */
List *plan_a(Env *start, List *goal);

/* Goal checking */
bool satisfies_goal(Env *state, List *goals);

/* Display */
void show_plan(List *plan, FILE *stream);

/* ================================================================ */
/* Blocks world                                                     */
/* ================================================================ */

List  *make_blocks_basis_set(List *blocks);
Plnpr *build_blocks_problem(const char *title, List *blocks,
                            bool debugging);
void   register_blocks_operators(void);

#endif /* PLANNER_H */
