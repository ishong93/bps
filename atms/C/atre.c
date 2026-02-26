/* -*- C -*- */

/* ATRE (ATMS-based Tiny Rule Engine) - Implementation */
/* Converted from ainter.lisp, adata.lisp, arules.lisp */

#include "atre.h"

Atre *current_atre = NULL;

/* ================================================================ */
/* ATRE creation                                                    */
/* ================================================================ */

static void atre_enqueue_callback(void *pair) {
    if (current_atre)
        enqueue(pair, current_atre);
}

Atre *create_atre(const char *title, bool debugging) {
    Atre *a = (Atre *)calloc(1, sizeof(Atre));
    a->title = strdup(title);
    char atms_title[256];
    snprintf(atms_title, sizeof(atms_title), "ATMS-OF %s", title);
    a->atms = create_atms(atms_title, stringify_node, false, NULL);
    a->dbclasses = list_new();
    a->rules = list_new();
    a->queue = list_new();
    a->in_rules = list_new();
    a->contradiction_rules = list_new();
    a->imp_rules = list_new();
    a->debugging = debugging;
    in_atre(a);
    change_atms(a->atms, NULL, atre_enqueue_callback, -1);
    /* Create default contradiction datum */
    Datum *false_datum = (Datum *)calloc(1, sizeof(Datum));
    false_datum->counter = ++a->datum_counter;
    false_datum->atre = a;
    false_datum->lisp_form = sexpr_symbol("FALSE");
    false_datum->dbclass = get_dbclass(sexpr_symbol("FALSE"));
    false_datum->tms_node = a->atms->contra_node;
    a->atms->contra_node->datum = false_datum;
    list_push(false_datum->dbclass->facts, false_datum);
    return a;
}

void change_atre(Atre *atre, int debugging) {
    if (debugging >= 0) atre->debugging = (bool)debugging;
}

void in_atre(Atre *atre) { current_atre = atre; }

/* ================================================================ */
/* Database class management                                        */
/* ================================================================ */

Dbclass *get_dbclass(SExpr *fact) {
    if (sexpr_is_nil(fact)) {
        fprintf(stderr, "Error: NIL can't be a dbclass.\n");
        return NULL;
    }
    if (sexpr_is_cons(fact))
        return get_dbclass(fact->cons.car);
    if (!sexpr_is_symbol(fact)) {
        fprintf(stderr, "Error: Bad dbclass type\n");
        return NULL;
    }
    /* Search existing */
    for (int i = 0; i < current_atre->dbclasses->size; i++) {
        Dbclass *dc = (Dbclass *)current_atre->dbclasses->data[i];
        if (strcmp(dc->name, fact->symbol) == 0) return dc;
    }
    /* Create new */
    Dbclass *dc = (Dbclass *)calloc(1, sizeof(Dbclass));
    dc->name = strdup(fact->symbol);
    dc->atre = current_atre;
    dc->facts = list_new();
    dc->rules = list_new();
    list_push(current_atre->dbclasses, dc);
    return dc;
}

/* ================================================================ */
/* Database operations                                              */
/* ================================================================ */

Datum *referent1(SExpr *fact) {
    Dbclass *dc = get_dbclass(fact);
    if (!dc) return NULL;
    for (int i = 0; i < dc->facts->size; i++) {
        Datum *d = (Datum *)dc->facts->data[i];
        if (sexpr_equal(d->lisp_form, fact)) return d;
    }
    return NULL;
}

Datum *referent(SExpr *fact, bool virtual_p) {
    if (virtual_p) return insert_datum(fact);
    return referent1(fact);
}

Datum *insert_datum(SExpr *fact) {
    Datum *d = referent1(fact);
    if (d) return d;
    d = (Datum *)calloc(1, sizeof(Datum));
    d->counter = ++current_atre->datum_counter;
    d->atre = current_atre;
    d->lisp_form = sexpr_copy(fact);
    d->dbclass = get_dbclass(fact);
    d->tms_node = tms_create_node(current_atre->atms, d,
                                   false, false);
    d->plist = list_new();
    list_push(d->dbclass->facts, d);
    try_rules(d);
    return d;
}

Datum *assert_fact(SExpr *fact, SExpr *just) {
    Datum *datum = referent(fact, true);
    TmsNode *node = datum->tms_node;
    if (current_atre->debugging)
        fprintf(stderr, "    Asserting %s via %s.\n",
                sexpr_to_string(fact), sexpr_to_string(just));
    /* Build antecedent list from just (cdr of the just list) */
    const char *informant = "rule";
    if (sexpr_is_symbol(just))
        informant = just->symbol;
    else if (sexpr_is_cons(just) && sexpr_is_symbol(just->cons.car))
        informant = just->cons.car->symbol;
    List *antes = list_new();
    if (sexpr_is_cons(just)) {
        SExpr *rest = just->cons.cdr;
        while (sexpr_is_cons(rest)) {
            Datum *ad = referent(rest->cons.car, true);
            list_push(antes, ad->tms_node);
            rest = rest->cons.cdr;
        }
    }
    justify_node(informant, node, antes);
    return datum;
}

Datum *assume_fact(SExpr *fact, const char *reason) {
    Datum *datum = referent(fact, true);
    TmsNode *node = datum->tms_node;
    if (!datum->assumption_reason) {
        datum->assumption_reason = strdup(reason);
        if (current_atre->debugging)
            fprintf(stderr, "    Assuming %s via %s.\n",
                    sexpr_to_string(fact), reason);
        assume_node(node);
    } else if (strcmp(datum->assumption_reason, reason) != 0) {
        fprintf(stderr, "Error: Fact assumed again with different reason\n");
    }
    return datum;
}

bool already_assumed(SExpr *fact) {
    TmsNode *node = get_tms_node(fact);
    return node ? node->is_assumption : false;
}

Datum *assume_if_needed(SExpr *fact, const char *reason) {
    if (!already_assumed(fact))
        return assume_fact(fact, reason);
    return referent(fact, false);
}

void contradiction(SExpr *fact) {
    Datum *d = referent(fact, true);
    make_contradiction(d->tms_node);
}

/* ================================================================ */
/* Fetch / query                                                    */
/* ================================================================ */

List *fetch(SExpr *pattern) {
    List *result = list_new();
    Dbclass *dc = get_dbclass(pattern);
    if (!dc) return result;
    for (int i = 0; i < dc->facts->size; i++) {
        Datum *d = (Datum *)dc->facts->data[i];
        Binding *b = unify(pattern, d->lisp_form, NULL);
        if (b != UNIFY_FAIL) {
            if (b == UNIFY_SUCCESS_EMPTY) b = NULL;
            SExpr *sub = subst_bindings(b, pattern);
            list_push(result, sub);
            binding_free(b);
        }
    }
    return result;
}

/* ================================================================ */
/* Interrogation                                                    */
/* ================================================================ */

bool true_fact(SExpr *fact) {
    Datum *d = referent(fact, false);
    if (!d) return false;
    return true_node(d->tms_node);
}

bool in_fact(SExpr *fact, Env *env) {
    Datum *d = referent(fact, false);
    if (!d) return false;
    return in_node(d->tms_node, env);
}

bool out_fact(SExpr *fact, Env *env) {
    Datum *d = referent(fact, false);
    if (!d) return true;
    return out_node(d->tms_node, env);
}

bool consistent_with(SExpr *fact, Env *env) {
    Datum *d = referent(fact, false);
    if (!d) return false;
    return node_consistent_with(d->tms_node, env);
}

void why_fact(SExpr *fact, FILE *stream) {
    Datum *d = referent(fact, false);
    if (d) why_node(d->tms_node, stream, "");
}

Env *environment_of(List *facts) {
    Env *env = current_atre->atms->empty_env;
    for (int i = 0; i < facts->size; i++) {
        SExpr *fact = (SExpr *)facts->data[i];
        TmsNode *node = get_tms_node(fact);
        if (!node || !node->is_assumption) {
            fprintf(stderr, "Error: Non-assumption in environment_of\n");
            return NULL;
        }
        env = cons_env(node, env);
        if (env->nogood) return NULL;
    }
    return env;
}

Env *environment_cons(SExpr *fact, Env *env) {
    return cons_env(get_tms_node(fact), env);
}

TmsNode *get_tms_node(SExpr *fact) {
    Datum *d = referent(fact, true);
    return d ? d->tms_node : NULL;
}

SExpr *view_node_datum(TmsNode *node) {
    Datum *d = (Datum *)node->datum;
    return d ? d->lisp_form : NULL;
}

static char _stringify_buf[512];
char *stringify_node(TmsNode *node) {
    Datum *d = (Datum *)node->datum;
    if (d && d->lisp_form) {
        snprintf(_stringify_buf, sizeof(_stringify_buf), "%s",
                 sexpr_to_string(d->lisp_form));
    } else {
        snprintf(_stringify_buf, sizeof(_stringify_buf),
                 "node-%d", node->index);
    }
    return _stringify_buf;
}

List *assumptions_of(SExpr *fact) {
    Datum *d = referent(fact, true);
    return d->tms_node->label;
}

Datum *get_datum_by_num(int num) {
    for (int i = 0; i < current_atre->dbclasses->size; i++) {
        Dbclass *dc = (Dbclass *)current_atre->dbclasses->data[i];
        for (int j = 0; j < dc->facts->size; j++) {
            Datum *d = (Datum *)dc->facts->data[j];
            if (d->counter == num) return d;
        }
    }
    return NULL;
}

Just *get_just_by_num(int num) {
    for (int i = 0; i < current_atre->atms->justs->size; i++) {
        Just *j = (Just *)current_atre->atms->justs->data[i];
        if (j->index == num) return j;
    }
    return NULL;
}

/* ================================================================ */
/* Solutions                                                        */
/* ================================================================ */

List *solutions(Atre *atre, List *choice_sets) {
    /* Convert choice_sets: List of List<SExpr*> -> List of List<TmsNode*> */
    List *node_sets = list_new();
    for (int i = 0; i < choice_sets->size; i++) {
        List *cs = (List *)choice_sets->data[i];
        List *ns = list_new();
        for (int j = 0; j < cs->size; j++) {
            SExpr *f = (SExpr *)cs->data[j];
            TmsNode *n = get_tms_node(f);
            if (n) list_push(ns, n);
        }
        list_push(node_sets, ns);
    }
    List *result = interpretations(atre->atms, node_sets, NULL);
    for (int i = 0; i < node_sets->size; i++)
        list_free((List *)node_sets->data[i]);
    list_free(node_sets);
    return result;
}

/* ================================================================ */
/* Focus management                                                 */
/* ================================================================ */

Env *change_focus(Env *env) {
    if (!env || env->nogood) return NULL;
    current_atre->focus = env;
    /* Re-queue implied-by rules */
    for (int i = 0; i < current_atre->imp_rules->size; i++)
        list_push(current_atre->queue,
                  current_atre->imp_rules->data[i]);
    list_clear(current_atre->imp_rules);
    return env;
}

bool focus_okay(void) {
    return current_atre->focus &&
           !current_atre->focus->nogood;
}

/* ================================================================ */
/* Contradiction rules                                              */
/* ================================================================ */

typedef struct {
    void (*proc)(Env *env);
    List *envs;
} ContraRuleData;

void contradiction_rule(Env *env, void (*proc)(Env *env),
                        Atre *atre) {
    if (env->nogood) {
        /* Execute immediately */
        proc(env);
    } else {
        /* Register for later */
        ContraRuleData *cr = (ContraRuleData *)calloc(1,
                                        sizeof(ContraRuleData));
        cr->proc = proc;
        cr->envs = list_new();
        list_push(cr->envs, env);
        /* Store as a rule on the environment */
        list_push(env->rules, cr);
    }
}

/* ================================================================ */
/* Rule system                                                      */
/* ================================================================ */

void insert_rule(Dbclass *dbclass, RuleMatcherFn matcher,
                 RuleBodyFn body, List *in_nodes, List *imp_nodes) {
    Atre *atre = dbclass->atre;
    ARule *rule = (ARule *)calloc(1, sizeof(ARule));
    rule->counter = ++atre->rule_counter;
    rule->atre = atre;
    rule->dbclass = dbclass;
    rule->matcher = matcher;
    rule->body = body;
    rule->in_nodes = in_nodes ? in_nodes : list_new();
    rule->imp_nodes = imp_nodes ? imp_nodes : list_new();
    list_push(atre->rules, rule);
    list_push(dbclass->rules, rule);
    for (int i = 0; i < dbclass->facts->size; i++)
        try_rule_on(rule, (Datum *)dbclass->facts->data[i]);
}

void try_rules(Datum *datum) {
    for (int i = 0; i < datum->dbclass->rules->size; i++)
        try_rule_on((ARule *)datum->dbclass->rules->data[i], datum);
}

void try_rule_on(ARule *rule, Datum *datum) {
    MatchResult mr = rule->matcher(datum->lisp_form);
    if (!mr.ok) return;
    /* Build queued rule: [body_fn, bindings, (in_nodes, imp_nodes)] */
    List *queued = list_new();
    list_push(queued, (void *)rule->body);
    /* Add trigger node for :IN and :IMPLIED-BY */
    List *bindings = mr.bindings ? mr.bindings : list_new();
    if (mr.condition == RULE_IN || mr.condition == RULE_IMPLIED_BY) {
        /* Prepend trigger node to bindings */
        List *new_bindings = list_new();
        list_push(new_bindings, datum->tms_node);
        for (int i = 0; i < bindings->size; i++)
            list_push(new_bindings, bindings->data[i]);
        if (mr.bindings) list_free(bindings);
        bindings = new_bindings;
    }
    list_push(queued, bindings);
    /* Node requirements */
    List *node_reqs = list_new();
    List *in_nodes = list_new();
    List *imp_nodes_list = list_new();
    if (mr.condition == RULE_IN) {
        list_push(in_nodes, datum->tms_node);
        for (int i = 0; i < rule->in_nodes->size; i++)
            list_push(in_nodes, rule->in_nodes->data[i]);
        for (int i = 0; i < rule->imp_nodes->size; i++)
            list_push(imp_nodes_list, rule->imp_nodes->data[i]);
    } else if (mr.condition == RULE_IMPLIED_BY) {
        for (int i = 0; i < rule->in_nodes->size; i++)
            list_push(in_nodes, rule->in_nodes->data[i]);
        list_push(imp_nodes_list, datum->tms_node);
        for (int i = 0; i < rule->imp_nodes->size; i++)
            list_push(imp_nodes_list, rule->imp_nodes->data[i]);
    } else {
        for (int i = 0; i < rule->in_nodes->size; i++)
            list_push(in_nodes, rule->in_nodes->data[i]);
        for (int i = 0; i < rule->imp_nodes->size; i++)
            list_push(imp_nodes_list, rule->imp_nodes->data[i]);
    }
    list_push(node_reqs, in_nodes);
    list_push(node_reqs, imp_nodes_list);
    list_push(queued, node_reqs);
    enqueue(queued, datum->atre);
}

int run_rules(void) {
    Atre *atre = current_atre;
    /* Move in_rules to main queue */
    for (int i = 0; i < atre->in_rules->size; i++)
        list_push(atre->queue, atre->in_rules->data[i]);
    list_clear(atre->in_rules);
    int counter = 0;
    while (atre->queue->size > 0) {
        void *form = dequeue(atre);
        if (!form) break;
        execute_rule((List *)form, atre);
        counter++;
    }
    if (atre->debugging)
        fprintf(stderr, "    %d rules run.\n", counter);
    atre->rules_run += counter;
    return counter;
}

void execute_rule(List *queued_rule, Atre *atre) {
    RuleBodyFn body = (RuleBodyFn)queued_rule->data[0];
    List *args = (List *)queued_rule->data[1];
    List *node_reqs = (List *)queued_rule->data[2];
    List *in_nodes = (List *)node_reqs->data[0];
    List *imp_nodes_list = (List *)node_reqs->data[1];
    /* Check in-triggers */
    if (!in_triggers_ready(in_nodes, atre, atre->atms->empty_env)) {
        list_push(atre->in_rules, queued_rule);
        return;
    }
    /* Check implied-by triggers */
    if (!implied_by_triggers_ready(imp_nodes_list, atre)) {
        list_push(atre->imp_rules, queued_rule);
        return;
    }
    body(args);
}

bool in_triggers_ready(List *nodes, Atre *atre, Env *env) {
    if (env->nogood) return false;
    if (!nodes || nodes->size == 0) return true;
    TmsNode *first = (TmsNode *)nodes->data[0];
    /* Create rest */
    List rest;
    rest.data = nodes->data + 1;
    rest.size = nodes->size - 1;
    rest.cap = rest.size;
    for (int i = 0; i < first->label->size; i++) {
        Env *ne = (Env *)first->label->data[i];
        Env *u = union_env(ne, env);
        if (u && in_triggers_ready(&rest, atre, u))
            return true;
    }
    return false;
}

bool implied_by_triggers_ready(List *nodes, Atre *atre) {
    if (!nodes || nodes->size == 0) return true;
    if (!focus_okay()) return false;
    for (int i = 0; i < nodes->size; i++) {
        TmsNode *n = (TmsNode *)nodes->data[i];
        if (!in_node(n, atre->focus)) return false;
    }
    return true;
}

void enqueue(void *item, Atre *atre) {
    list_push(atre->queue, item);
}

void *dequeue(Atre *atre) {
    if (atre->queue->size == 0) return NULL;
    void *item = atre->queue->data[0];
    /* Shift left */
    memmove(&atre->queue->data[0], &atre->queue->data[1],
            (atre->queue->size - 1) * sizeof(void *));
    atre->queue->size--;
    return item;
}

/* ================================================================ */
/* Display                                                          */
/* ================================================================ */

char *show_datum(Datum *datum) {
    return sexpr_to_string(datum->lisp_form);
}

int show_data(FILE *stream) {
    if (!stream) stream = stdout;
    Atre *atre = current_atre;
    int counter = 0;
    fprintf(stream, "\n%d facts total.", atre->datum_counter);
    for (int i = 0; i < atre->dbclasses->size; i++) {
        Dbclass *dc = (Dbclass *)atre->dbclasses->data[i];
        for (int j = 0; j < dc->facts->size; j++) {
            Datum *d = (Datum *)dc->facts->data[j];
            counter++;
            fprintf(stream, "\n%s: ", show_datum(d));
            /* Print label */
            List *label = assumptions_of(d->lisp_form);
            fprintf(stream, "(");
            for (int k = 0; k < label->size; k++) {
                if (k > 0) fprintf(stream, " ");
                Env *e = (Env *)label->data[k];
                fprintf(stream, "E-%d", e->index);
            }
            fprintf(stream, ")");
        }
    }
    return counter;
}

int show_context(Env *env, FILE *stream) {
    if (!stream) stream = stdout;
    int counter = 0;
    for (int i = 0; i < current_atre->dbclasses->size; i++) {
        Dbclass *dc = (Dbclass *)current_atre->dbclasses->data[i];
        for (int j = 0; j < dc->facts->size; j++) {
            Datum *d = (Datum *)dc->facts->data[j];
            if (in_node(d->tms_node, env)) {
                counter++;
                fprintf(stream, "\n%s", show_datum(d));
            }
        }
    }
    fprintf(stream, "\n%d facts total.", counter);
    return counter;
}

void show_rules_info(FILE *stream) {
    if (!stream) stream = stdout;
    Atre *atre = current_atre;
    int counter = 0;
    for (int i = 0; i < atre->dbclasses->size; i++) {
        Dbclass *dc = (Dbclass *)atre->dbclasses->data[i];
        counter += dc->rules->size;
    }
    fprintf(stream, "\n %s has %d rules in all.",
            atre->title, counter);
    fprintf(stream, "\n  %d queued.",
            atre->queue->size);
}
