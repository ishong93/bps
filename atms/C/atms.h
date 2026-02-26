/* -*- C -*- */

/* Assumption-based Truth Maintenance System (ATMS) */
/* Converted from atms.lisp */

/* Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. All rights reserved. */

#ifndef ATMS_H
#define ATMS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/* ================================================================ */
/* Generic dynamic array                                            */
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
void  list_remove_first(List *l, void *item);
bool  list_contains(List *l, void *item);
List *list_copy(List *l);
void  list_delete_nils(List *l);
void  list_clear(List *l);

/* ================================================================ */
/* Forward declarations                                             */
/* ================================================================ */

typedef struct Atms Atms;
typedef struct TmsNode TmsNode;
typedef struct Just Just;
typedef struct Env Env;

/* ================================================================ */
/* Env table: list of buckets keyed by count                        */
/* Each bucket = (count, list of envs)                              */
/* Stored as sorted linked list by count.                           */
/* ================================================================ */

typedef struct TableEntry {
    int count;
    List *envs;                /* List of Env* */
    struct TableEntry *next;
} TableEntry;

TableEntry *table_insert(TableEntry *table, Env *env);
List       *table_get(TableEntry *table, int count);
void        table_free(TableEntry *table);

/* ================================================================ */
/* Env comparison result                                            */
/* ================================================================ */

typedef enum {
    ENV_CMP_NONE = 0,
    ENV_CMP_EQ,
    ENV_CMP_S12,
    ENV_CMP_S21
} EnvCompare;

/* ================================================================ */
/* ATMS structure                                                   */
/* ================================================================ */

struct Atms {
    char *title;
    int node_counter;
    int just_counter;
    int env_counter;
    List *nodes;                /* List of TmsNode* */
    List *justs;                /* List of Just* */
    List *contradictions;       /* List of TmsNode* */
    List *assumptions;          /* List of TmsNode* */
    bool debugging;
    TableEntry *nogood_table;
    TmsNode *contra_node;
    TableEntry *env_table;
    Env *empty_env;
    char *(*node_string)(TmsNode *node);
    void (*enqueue_procedure)(void *rule);
};

/* ================================================================ */
/* TmsNode structure                                                */
/* ================================================================ */

struct TmsNode {
    int index;
    void *datum;                /* External data */
    List *label;                /* List of Env* */
    List *justs;                /* Justifications concluding this node */
    List *consequences;         /* Justifications using this as antecedent */
    bool is_contradictory;
    bool is_assumption;
    List *rules;                /* Pending rules */
    Atms *atms;
};

/* ================================================================ */
/* Just structure                                                   */
/* ================================================================ */

struct Just {
    int index;
    char *informant;
    TmsNode *consequence;
    List *antecedents;          /* List of TmsNode* */
};

/* ================================================================ */
/* Env structure                                                    */
/* ================================================================ */

struct Env {
    int index;
    int count;
    List *assumptions;          /* List of TmsNode* (sorted by index) */
    List *nodes;                /* Nodes true in this env */
    void *nogood;               /* NULL=consistent, non-NULL=nogood */
    List *rules;
};

/* ================================================================ */
/* Utility                                                          */
/* ================================================================ */

char *default_node_string(TmsNode *node);
char *node_string_fn(TmsNode *node);
bool  assumption_order(TmsNode *a1, TmsNode *a2);
bool  env_order(Env *e1, Env *e2);
List *ordered_insert_node(TmsNode *item, List *existing);
void  debugging_atms(Atms *atms, const char *msg, TmsNode *node);

/* ================================================================ */
/* ATMS API                                                         */
/* ================================================================ */

/* Creation */
Atms *create_atms(const char *title,
                  char *(*node_string_fn)(TmsNode*),
                  bool debugging,
                  void (*enqueue_proc)(void*));
void  change_atms(Atms *atms,
                  char *(*node_string_fn)(TmsNode*),
                  void (*enqueue_proc)(void*),
                  int debugging);  /* -1 = don't change */

/* Node operations */
TmsNode *tms_create_node(Atms *atms, void *datum,
                         bool assumptionp, bool contradictoryp);
bool true_node(TmsNode *node);
bool in_node(TmsNode *node, Env *env);
bool out_node(TmsNode *node, Env *env);
bool node_consistent_with(TmsNode *node, Env *env);
void assume_node(TmsNode *node);
void make_contradiction(TmsNode *node);
void remove_node(TmsNode *node);

/* Justification */
Just *justify_node(const char *informant, TmsNode *consequence,
                   List *antecedents);
void  nogood_nodes(const char *informant, List *nodes);

/* Label updating */
void  propagate_just(Just *just, TmsNode *antecedent, List *envs);
void  update_node(List *new_envs, TmsNode *consequence, Just *just);
List *update_label(TmsNode *node, List *new_envs);
List *weave(TmsNode *antecedent, List *envs, List *antecedents);
bool  in_antecedent(List *nodes);
bool  weave_check(Env *env, List *nodes);
bool  supporting_antecedent(List *nodes, Env *env);

/* Environment operations */
Env       *create_env(Atms *atms, List *assumptions);
Env       *union_env(Env *e1, Env *e2);
Env       *cons_env(TmsNode *assumption, Env *env);
Env       *find_or_make_env(List *assumptions, Atms *atms);
Env       *lookup_env(List *assumes);
bool       subset_env(Env *e1, Env *e2);
EnvCompare compare_env(Env *e1, Env *e2);
void       set_env_contradictory(Atms *atms, Env *env);

/* Nogood processing */
void new_nogood(Atms *atms, Env *cenv, void *just);
void remove_env_from_labels(Env *env, Atms *atms);

/* Interpretation construction */
List *interpretations(Atms *atms, List *choice_sets, List *defaults);

/* Explanation */
List *explain_node(TmsNode *node, Env *env);

/* Printing */
void why_node(TmsNode *node, FILE *stream, const char *prefix);
void why_nodes(Atms *atms, FILE *stream);
void node_justifications(TmsNode *node, FILE *stream);
void print_justification(Just *j, FILE *stream);
Env *find_env_by_index(Atms *atms, int n);
void print_env(Env *e, FILE *stream);
void env_string(Env *e, FILE *stream);
void print_nogoods(Atms *atms, FILE *stream);
void print_envs(Atms *atms, FILE *stream);
void print_env_table(TableEntry *table, FILE *stream);
void print_atms_statistics(Atms *atms);

#endif /* ATMS_H */
