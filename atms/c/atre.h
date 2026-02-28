/* -*- C -*- */

/* ATRE (ATMS-based Tiny Rule Engine) - Definitions */
/* Converted from ainter.lisp, adata.lisp, arules.lisp */

#ifndef ATRE_H
#define ATRE_H

#include "atms.h"
#include "unify.h"

/* ================================================================ */
/* Forward declarations                                             */
/* ================================================================ */

typedef struct Atre Atre;
typedef struct Dbclass Dbclass;
typedef struct Datum Datum;
typedef struct ARule ARule;

/* ================================================================ */
/* ATRE structure                                                   */
/* ================================================================ */

struct Atre {
    char *title;
    Atms *atms;
    List *dbclasses;          /* List of Dbclass* */
    int   datum_counter;
    List *rules;              /* List of ARule* */
    int   rule_counter;
    bool  debugging;
    List *queue;              /* General queue */
    int   rules_run;
    List *in_rules;           /* In-rules to be executed */
    Env  *focus;              /* Current search focus */
    List *contradiction_rules;
    List *imp_rules;
};

/* ================================================================ */
/* Dbclass: indexed by first symbol of pattern                      */
/* ================================================================ */

struct Dbclass {
    char *name;
    Atre *atre;
    List *facts;              /* List of Datum* */
    List *rules;              /* List of ARule* */
};

/* ================================================================ */
/* Datum: a stored fact                                              */
/* ================================================================ */

struct Datum {
    int      counter;
    Atre    *atre;
    SExpr   *lisp_form;      /* The pattern */
    TmsNode *tms_node;
    Dbclass *dbclass;
    char    *assumption_reason; /* non-NULL if assumed */
    List    *plist;           /* Property list */
};

/* ================================================================ */
/* Rule condition types                                             */
/* ================================================================ */

typedef enum {
    RULE_INTERN,
    RULE_IN,
    RULE_IMPLIED_BY
} RuleCondition;

/* ================================================================ */
/* ARule: a rule in the ATRE                                        */
/* ================================================================ */

/* Matcher: takes a Datum's lisp_form and returns match result */
typedef struct {
    bool ok;
    List *bindings;           /* List of SExpr* bound values */
    RuleCondition condition;
} MatchResult;

/* Rule body: function pointer */
typedef void (*RuleBodyFn)(List *args);

/* Matcher function pointer */
typedef MatchResult (*RuleMatcherFn)(SExpr *pattern);

struct ARule {
    int           counter;
    Atre         *atre;
    Dbclass      *dbclass;
    RuleMatcherFn matcher;
    RuleBodyFn    body;
    List         *in_nodes;   /* Must have jointly non-empty label */
    List         *imp_nodes;  /* Must be implied by focus */
};

/* ================================================================ */
/* Global ATRE pointer                                              */
/* ================================================================ */

extern Atre *current_atre;

/* ================================================================ */
/* ATRE API                                                         */
/* ================================================================ */

/* Creation and management */
Atre *create_atre(const char *title, bool debugging);
void  change_atre(Atre *atre, int debugging);
void  in_atre(Atre *atre);

/* Database operations */
Datum *assert_fact(SExpr *fact, SExpr *just);
Datum *assume_fact(SExpr *fact, const char *reason);
bool   already_assumed(SExpr *fact);
Datum *assume_if_needed(SExpr *fact, const char *reason);
void   contradiction(SExpr *fact);

/* Database lookup */
Dbclass *get_dbclass(SExpr *fact);
Datum   *referent(SExpr *fact, bool virtual_p);
Datum   *referent1(SExpr *fact);
Datum   *insert_datum(SExpr *fact);
List    *fetch(SExpr *pattern);

/* Interrogation */
bool     true_fact(SExpr *fact);
bool     in_fact(SExpr *fact, Env *env);
bool     out_fact(SExpr *fact, Env *env);
bool     consistent_with(SExpr *fact, Env *env);
void     why_fact(SExpr *fact, FILE *stream);
Env     *environment_of(List *facts);
Env     *environment_cons(SExpr *fact, Env *env);
TmsNode *get_tms_node(SExpr *fact);
SExpr   *view_node_datum(TmsNode *node);
char    *stringify_node(TmsNode *node);
List    *assumptions_of(SExpr *fact);
Datum   *get_datum_by_num(int num);
Just    *get_just_by_num(int num);

/* Solutions */
List *solutions(Atre *atre, List *choice_sets);

/* Focus management */
Env  *change_focus(Env *env);
bool  focus_okay(void);

/* Contradiction rules */
void contradiction_rule(Env *env, void (*proc)(Env *env),
                        Atre *atre);

/* Rule system */
void insert_rule(Dbclass *dbclass, RuleMatcherFn matcher,
                 RuleBodyFn body, List *in_nodes, List *imp_nodes);
void try_rules(Datum *datum);
void try_rule_on(ARule *rule, Datum *datum);
int  run_rules(void);
void enqueue(void *item, Atre *atre);
void *dequeue(Atre *atre);

/* Execution */
void execute_rule(List *queued_rule, Atre *atre);
bool in_triggers_ready(List *nodes, Atre *atre, Env *env);
bool implied_by_triggers_ready(List *nodes, Atre *atre);

/* Display */
char *show_datum(Datum *datum);
int   show_data(FILE *stream);
int   show_context(Env *env, FILE *stream);
void  show_rules_info(FILE *stream);

#endif /* ATRE_H */
