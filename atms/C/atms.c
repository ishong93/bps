/* -*- C -*- */

/* Assumption-based Truth Maintenance System (ATMS) */
/* Converted from atms.lisp */

#include "atms.h"

/* ================================================================ */
/* List operations                                                  */
/* ================================================================ */

List *list_new(void) {
    List *l = (List *)calloc(1, sizeof(List));
    l->cap = 4;
    l->data = (void **)calloc(l->cap, sizeof(void *));
    return l;
}

void list_free(List *l) {
    if (l) { free(l->data); free(l); }
}

void list_push(List *l, void *item) {
    if (l->size >= l->cap) {
        l->cap *= 2;
        l->data = (void **)realloc(l->data, l->cap * sizeof(void *));
    }
    l->data[l->size++] = item;
}

void *list_get(List *l, int i) {
    if (i < 0 || i >= l->size) return NULL;
    return l->data[i];
}

void list_set(List *l, int i, void *item) {
    if (i >= 0 && i < l->size) l->data[i] = item;
}

void list_remove(List *l, void *item) {
    for (int i = 0; i < l->size; i++) {
        if (l->data[i] == item) {
            memmove(&l->data[i], &l->data[i+1],
                    (l->size - i - 1) * sizeof(void *));
            l->size--;
            return;
        }
    }
}

void list_remove_first(List *l, void *item) {
    list_remove(l, item);
}

bool list_contains(List *l, void *item) {
    for (int i = 0; i < l->size; i++)
        if (l->data[i] == item) return true;
    return false;
}

List *list_copy(List *l) {
    List *c = list_new();
    for (int i = 0; i < l->size; i++)
        list_push(c, l->data[i]);
    return c;
}

void list_delete_nils(List *l) {
    int w = 0;
    for (int r = 0; r < l->size; r++)
        if (l->data[r] != NULL) l->data[w++] = l->data[r];
    l->size = w;
}

void list_clear(List *l) { l->size = 0; }

/* ================================================================ */
/* Table operations                                                 */
/* ================================================================ */

TableEntry *table_insert(TableEntry *table, Env *env) {
    int count = env->count;
    /* Find existing bucket */
    TableEntry *prev = NULL, *cur = table;
    while (cur && cur->count < count) {
        prev = cur; cur = cur->next;
    }
    if (cur && cur->count == count) {
        list_push(cur->envs, env);
        return table;
    }
    /* Create new bucket */
    TableEntry *entry = (TableEntry *)calloc(1, sizeof(TableEntry));
    entry->count = count;
    entry->envs = list_new();
    list_push(entry->envs, env);
    entry->next = cur;
    if (prev) { prev->next = entry; return table; }
    return entry;
}

List *table_get(TableEntry *table, int count) {
    for (TableEntry *e = table; e; e = e->next)
        if (e->count == count) return e->envs;
    return NULL;
}

void table_free(TableEntry *table) {
    while (table) {
        TableEntry *next = table->next;
        list_free(table->envs);
        free(table);
        table = next;
    }
}

/* ================================================================ */
/* Utility functions                                                */
/* ================================================================ */

static char _node_str_buf[256];

char *default_node_string(TmsNode *node) {
    if (node->datum) {
        snprintf(_node_str_buf, sizeof(_node_str_buf), "%s",
                 (char *)node->datum);
    } else {
        snprintf(_node_str_buf, sizeof(_node_str_buf), "node-%d",
                 node->index);
    }
    return _node_str_buf;
}

char *node_string_fn(TmsNode *node) {
    return node->atms->node_string(node);
}

bool assumption_order(TmsNode *a1, TmsNode *a2) {
    return a1->index < a2->index;
}

bool env_order(Env *e1, Env *e2) {
    return e1->index < e2->index;
}

/* Insert node into sorted list (by index), skip duplicates */
List *ordered_insert_node(TmsNode *item, List *existing) {
    List *result = list_new();
    bool inserted = false;
    for (int i = 0; i < existing->size; i++) {
        TmsNode *cur = (TmsNode *)existing->data[i];
        if (cur == item) {
            /* Already exists, copy rest and return */
            for (int j = 0; j < existing->size; j++)
                list_push(result, existing->data[j]);
            return result;
        }
        if (!inserted && assumption_order(item, cur)) {
            list_push(result, item);
            inserted = true;
        }
        list_push(result, cur);
    }
    if (!inserted) list_push(result, item);
    return result;
}

void debugging_atms(Atms *atms, const char *msg, TmsNode *node) {
    if (atms->debugging) {
        if (node)
            fprintf(stderr, "%s %s\n", msg, node_string_fn(node));
        else
            fprintf(stderr, "%s\n", msg);
    }
}

/* ================================================================ */
/* ATMS creation                                                    */
/* ================================================================ */

Atms *create_atms(const char *title,
                  char *(*ns_fn)(TmsNode*),
                  bool debugging,
                  void (*enqueue_proc)(void*)) {
    Atms *atms = (Atms *)calloc(1, sizeof(Atms));
    atms->title = strdup(title);
    atms->node_string = ns_fn ? ns_fn : default_node_string;
    atms->debugging = debugging;
    atms->enqueue_procedure = enqueue_proc;
    atms->nodes = list_new();
    atms->justs = list_new();
    atms->contradictions = list_new();
    atms->assumptions = list_new();
    atms->contra_node = tms_create_node(atms,
                            strdup("The contradiction"),
                            false, true);
    atms->empty_env = create_env(atms, list_new());
    return atms;
}

void change_atms(Atms *atms, char *(*ns_fn)(TmsNode*),
                 void (*enqueue_proc)(void*), int debugging) {
    if (ns_fn) atms->node_string = ns_fn;
    if (debugging >= 0) atms->debugging = (bool)debugging;
    if (enqueue_proc) atms->enqueue_procedure = enqueue_proc;
}

/* ================================================================ */
/* Node operations                                                  */
/* ================================================================ */

TmsNode *tms_create_node(Atms *atms, void *datum,
                         bool assumptionp, bool contradictoryp) {
    TmsNode *node = (TmsNode *)calloc(1, sizeof(TmsNode));
    node->index = ++atms->node_counter;
    node->datum = datum;
    node->is_assumption = assumptionp;
    node->is_contradictory = contradictoryp;
    node->atms = atms;
    node->label = list_new();
    node->justs = list_new();
    node->consequences = list_new();
    node->rules = list_new();
    list_push(atms->nodes, node);
    if (contradictoryp)
        list_push(atms->contradictions, node);
    if (assumptionp) {
        list_push(atms->assumptions, node);
        List *as = list_new();
        list_push(as, node);
        Env *env = create_env(atms, as);
        list_push(node->label, env);
    }
    return node;
}

bool true_node(TmsNode *node) {
    if (node->label->size == 0) return false;
    return node->label->data[0] == node->atms->empty_env;
}

bool in_node(TmsNode *node, Env *env) {
    if (env) {
        for (int i = 0; i < node->label->size; i++) {
            Env *le = (Env *)node->label->data[i];
            if (subset_env(le, env)) return true;
        }
        return false;
    }
    return node->label->size > 0;
}

bool out_node(TmsNode *node, Env *env) {
    return !in_node(node, env);
}

bool node_consistent_with(TmsNode *node, Env *env) {
    for (int i = 0; i < node->label->size; i++) {
        Env *le = (Env *)node->label->data[i];
        Env *u = union_env(le, env);
        if (u && !u->nogood) return true;
    }
    return false;
}

void assume_node(TmsNode *node) {
    if (node->is_assumption) return;
    Atms *atms = node->atms;
    debugging_atms(atms, "Converting into an assumption", node);
    node->is_assumption = true;
    list_push(atms->assumptions, node);
    List *as = list_new();
    list_push(as, node);
    Env *env = create_env(atms, as);
    List *envs = list_new();
    list_push(envs, env);
    update_node(envs, node, NULL);
    list_free(envs);
}

void make_contradiction(TmsNode *node) {
    if (node->is_contradictory) return;
    Atms *atms = node->atms;
    node->is_contradictory = true;
    list_push(atms->contradictions, node);
    while (node->label->size > 0) {
        Env *nogood = (Env *)node->label->data[0];
        new_nogood(atms, nogood, (void *)"MAKE-CONTRADICTION");
    }
}

/* ================================================================ */
/* Justification                                                    */
/* ================================================================ */

Just *justify_node(const char *informant, TmsNode *consequence,
                   List *antecedents) {
    Atms *atms = consequence->atms;
    Just *just = (Just *)calloc(1, sizeof(Just));
    just->index = ++atms->just_counter;
    just->informant = strdup(informant);
    just->consequence = consequence;
    just->antecedents = antecedents ? antecedents : list_new();
    list_push(consequence->justs, just);
    for (int i = 0; i < just->antecedents->size; i++) {
        TmsNode *ant = (TmsNode *)just->antecedents->data[i];
        list_push(ant->consequences, just);
    }
    list_push(atms->justs, just);
    if (atms->debugging) {
        fprintf(stderr, "Justifying %s by %s\n",
                node_string_fn(consequence), informant);
    }
    List *initial = list_new();
    list_push(initial, atms->empty_env);
    propagate_just(just, NULL, initial);
    list_free(initial);
    return just;
}

void nogood_nodes(const char *informant, List *nodes) {
    TmsNode *first = (TmsNode *)nodes->data[0];
    justify_node(informant, first->atms->contra_node, nodes);
}

/* ================================================================ */
/* Label updating                                                   */
/* ================================================================ */

void propagate_just(Just *just, TmsNode *antecedent, List *envs) {
    List *new_envs = weave(antecedent, envs, just->antecedents);
    if (new_envs && new_envs->size > 0)
        update_node(new_envs, just->consequence, just);
    if (new_envs) list_free(new_envs);
}

void update_node(List *new_envs, TmsNode *consequence, Just *just) {
    Atms *atms = consequence->atms;
    /* Branch 1: contradiction node */
    if (consequence->is_contradictory) {
        for (int i = 0; i < new_envs->size; i++)
            new_nogood(atms, (Env *)new_envs->data[i], just);
        return;
    }
    /* Branch 2: normal node - update label */
    List *added = update_label(consequence, new_envs);
    if (!added || added->size == 0) {
        if (added) list_free(added);
        return;
    }
    /* Enqueue rules */
    if (atms->enqueue_procedure) {
        for (int i = 0; i < consequence->rules->size; i++)
            atms->enqueue_procedure(consequence->rules->data[i]);
        list_clear(consequence->rules);
    }
    /* Propagate to dependent justifications */
    for (int i = 0; i < consequence->consequences->size; i++) {
        Just *sjust = (Just *)consequence->consequences->data[i];
        propagate_just(sjust, consequence, added);
        /* Filter out envs removed from label during propagation */
        for (int j = 0; j < added->size; j++) {
            if (added->data[j] &&
                !list_contains(consequence->label, added->data[j]))
                added->data[j] = NULL;
        }
        list_delete_nils(added);
        if (added->size == 0) break;
    }
    list_free(added);
}

List *update_label(TmsNode *node, List *new_envs) {
    /* Work on copies so we can mark with NULL */
    int new_count = new_envs->size;
    void **new_arr = (void **)calloc(new_count, sizeof(void *));
    for (int i = 0; i < new_count; i++)
        new_arr[i] = new_envs->data[i];

    List *envs = node->label;

    for (int i = 0; i < new_count; i++) {
        if (!new_arr[i]) continue;
        for (int j = 0; j < envs->size; j++) {
            if (!envs->data[j]) continue;
            if (!new_arr[i]) continue;
            EnvCompare cmp = compare_env((Env *)new_arr[i],
                                         (Env *)envs->data[j]);
            if (cmp == ENV_CMP_EQ || cmp == ENV_CMP_S21) {
                new_arr[i] = NULL;  /* new env is superset - discard */
            } else if (cmp == ENV_CMP_S12) {
                /* new env is subset - remove old */
                Env *old_env = (Env *)envs->data[j];
                list_remove_first(old_env->nodes, node);
                envs->data[j] = NULL;
            }
        }
    }
    /* Collect actually added envs */
    List *added = list_new();
    for (int i = 0; i < new_count; i++) {
        if (new_arr[i]) {
            list_push(added, new_arr[i]);
            list_push(envs, new_arr[i]);
            Env *e = (Env *)new_arr[i];
            list_push(e->nodes, node);
        }
    }
    free(new_arr);
    list_delete_nils(envs);
    return added;
}

List *weave(TmsNode *antecedent, List *envs, List *antecedents) {
    List *current = list_copy(envs);
    for (int a = 0; a < antecedents->size; a++) {
        TmsNode *node = (TmsNode *)antecedents->data[a];
        if (node == antecedent) continue;
        List *new_envs = list_new();
        for (int i = 0; i < current->size; i++) {
            Env *env = (Env *)current->data[i];
            if (!env) continue;
            for (int j = 0; j < node->label->size; j++) {
                Env *node_env = (Env *)node->label->data[j];
                Env *ne = union_env(env, node_env);
                if (!ne || ne->nogood) continue;
                /* Minimality check against new_envs */
                bool keep = true;
                for (int k = 0; k < new_envs->size; k++) {
                    if (!new_envs->data[k]) continue;
                    EnvCompare cmp = compare_env(ne,
                                        (Env *)new_envs->data[k]);
                    if (cmp == ENV_CMP_EQ || cmp == ENV_CMP_S21) {
                        keep = false; break;
                    }
                    if (cmp == ENV_CMP_S12)
                        new_envs->data[k] = NULL;
                }
                if (keep) list_push(new_envs, ne);
            }
        }
        list_delete_nils(new_envs);
        list_free(current);
        current = new_envs;
        if (current->size == 0) {
            list_free(current);
            return NULL;
        }
    }
    return current;
}

bool in_antecedent(List *nodes) {
    if (!nodes || nodes->size == 0) return true;
    TmsNode *first = (TmsNode *)nodes->data[0];
    return weave_check(first->atms->empty_env, nodes);
}

bool weave_check(Env *env, List *nodes) {
    if (!nodes || nodes->size == 0) return true;
    TmsNode *node = (TmsNode *)nodes->data[0];
    /* Create rest list */
    List rest;
    rest.data = nodes->data + 1;
    rest.size = nodes->size - 1;
    rest.cap = rest.size;
    for (int i = 0; i < node->label->size; i++) {
        Env *e = (Env *)node->label->data[i];
        Env *ne = union_env(e, env);
        if (ne && !ne->nogood) {
            if (weave_check(ne, &rest)) return true;
        }
    }
    return false;
}

bool supporting_antecedent(List *nodes, Env *env) {
    for (int i = 0; i < nodes->size; i++) {
        if (!in_node((TmsNode *)nodes->data[i], env))
            return false;
    }
    return true;
}

void remove_node(TmsNode *node) {
    if (node->consequences->size > 0) {
        fprintf(stderr, "Error: Can't remove node with consequences\n");
        return;
    }
    Atms *atms = node->atms;
    list_remove(atms->nodes, node);
    for (int i = 0; i < node->justs->size; i++) {
        Just *just = (Just *)node->justs->data[i];
        for (int j = 0; j < just->antecedents->size; j++) {
            TmsNode *ant = (TmsNode *)just->antecedents->data[j];
            list_remove_first(ant->consequences, just);
        }
    }
    for (int i = 0; i < node->label->size; i++) {
        Env *env = (Env *)node->label->data[i];
        list_remove_first(env->nodes, node);
    }
}

/* ================================================================ */
/* Environment operations                                           */
/* ================================================================ */

Env *create_env(Atms *atms, List *assumptions) {
    Env *e = (Env *)calloc(1, sizeof(Env));
    e->index = ++atms->env_counter;
    e->assumptions = assumptions;
    e->count = assumptions->size;
    e->nodes = list_new();
    e->rules = list_new();
    atms->env_table = table_insert(atms->env_table, e);
    set_env_contradictory(atms, e);
    return e;
}

Env *union_env(Env *e1, Env *e2) {
    if (e1->count > e2->count) {
        Env *tmp = e1; e1 = e2; e2 = tmp;
    }
    for (int i = 0; i < e1->assumptions->size; i++) {
        TmsNode *assume = (TmsNode *)e1->assumptions->data[i];
        e2 = cons_env(assume, e2);
        if (e2->nogood) return e2;
    }
    return e2;
}

Env *cons_env(TmsNode *assumption, Env *env) {
    List *nassumes = ordered_insert_node(assumption, env->assumptions);
    /* Check if identical to existing */
    if (nassumes->size == env->assumptions->size) {
        bool same = true;
        for (int i = 0; i < nassumes->size; i++) {
            if (nassumes->data[i] != env->assumptions->data[i]) {
                same = false; break;
            }
        }
        if (same) { list_free(nassumes); return env; }
    }
    Env *found = lookup_env(nassumes);
    if (found) { list_free(nassumes); return found; }
    return create_env(assumption->atms, nassumes);
}

Env *find_or_make_env(List *assumptions, Atms *atms) {
    if (!assumptions || assumptions->size == 0)
        return atms->empty_env;
    Env *found = lookup_env(assumptions);
    if (found) return found;
    return create_env(atms, assumptions);
}

Env *lookup_env(List *assumes) {
    if (!assumes || assumes->size == 0) return NULL;
    TmsNode *first = (TmsNode *)assumes->data[0];
    List *bucket = table_get(first->atms->env_table, assumes->size);
    if (!bucket) return NULL;
    for (int i = 0; i < bucket->size; i++) {
        Env *env = (Env *)bucket->data[i];
        if (env->assumptions->size != assumes->size) continue;
        bool match = true;
        for (int j = 0; j < assumes->size; j++) {
            if (env->assumptions->data[j] != assumes->data[j]) {
                match = false; break;
            }
        }
        if (match) return env;
    }
    return NULL;
}

bool subset_env(Env *e1, Env *e2) {
    if (e1 == e2) return true;
    if (e1->count > e2->count) return false;
    /* Check all assumptions of e1 are in e2 */
    for (int i = 0; i < e1->assumptions->size; i++) {
        if (!list_contains(e2->assumptions, e1->assumptions->data[i]))
            return false;
    }
    return true;
}

EnvCompare compare_env(Env *e1, Env *e2) {
    if (e1 == e2) return ENV_CMP_EQ;
    if (e1->count < e2->count) {
        if (subset_env(e1, e2)) return ENV_CMP_S12;
        return ENV_CMP_NONE;
    }
    if (subset_env(e2, e1)) return ENV_CMP_S21;
    return ENV_CMP_NONE;
}

void set_env_contradictory(Atms *atms, Env *env) {
    if (env->nogood) return;
    for (TableEntry *entry = atms->nogood_table; entry;
         entry = entry->next) {
        if (entry->count > env->count) return;
        for (int i = 0; i < entry->envs->size; i++) {
            Env *cenv = (Env *)entry->envs->data[i];
            if (subset_env(cenv, env)) {
                env->nogood = cenv;
                return;
            }
        }
    }
}

/* ================================================================ */
/* Nogood processing                                                */
/* ================================================================ */

void new_nogood(Atms *atms, Env *cenv, void *just) {
    if (atms->debugging)
        fprintf(stderr, "  E-%d new minimal nogood.\n", cenv->index);
    cenv->nogood = just;
    remove_env_from_labels(cenv, atms);
    atms->nogood_table = table_insert(atms->nogood_table, cenv);
    int count = cenv->count;
    /* Remove superset nogoods from nogood-table */
    for (TableEntry *entry = atms->nogood_table; entry;
         entry = entry->next) {
        if (entry->count > count) {
            for (int i = 0; i < entry->envs->size; i++) {
                Env *old = (Env *)entry->envs->data[i];
                if (old && subset_env(cenv, old))
                    entry->envs->data[i] = NULL;
            }
            list_delete_nils(entry->envs);
        }
    }
    /* Mark superset envs in env-table as nogood */
    for (TableEntry *entry = atms->env_table; entry;
         entry = entry->next) {
        if (entry->count > count) {
            for (int i = 0; i < entry->envs->size; i++) {
                Env *old = (Env *)entry->envs->data[i];
                if (old && !old->nogood && subset_env(cenv, old)) {
                    old->nogood = cenv;
                    remove_env_from_labels(old, atms);
                }
            }
        }
    }
}

void remove_env_from_labels(Env *env, Atms *atms) {
    if (atms->enqueue_procedure) {
        for (int i = 0; i < env->rules->size; i++)
            atms->enqueue_procedure(env->rules->data[i]);
        list_clear(env->rules);
    }
    for (int i = 0; i < env->nodes->size; i++) {
        TmsNode *node = (TmsNode *)env->nodes->data[i];
        list_remove_first(node->label, env);
    }
}

/* ================================================================ */
/* Interpretation construction                                      */
/* ================================================================ */

static List *_solutions = NULL;

static void get_depth_solutions1(Env *solution, List *choice_sets,
                                 int cs_index) {
    if (cs_index >= choice_sets->size) {
        /* Check minimality */
        for (int i = 0; i < _solutions->size; i++) {
            if (!_solutions->data[i]) continue;
            EnvCompare cmp = compare_env((Env *)_solutions->data[i],
                                         solution);
            if (cmp == ENV_CMP_EQ || cmp == ENV_CMP_S12) return;
            if (cmp == ENV_CMP_S21) _solutions->data[i] = NULL;
        }
        list_push(_solutions, solution);
        return;
    }
    if (solution->nogood) return;
    List *choices = (List *)choice_sets->data[cs_index];
    for (int i = 0; i < choices->size; i++) {
        Env *choice = (Env *)choices->data[i];
        Env *ns = union_env(solution, choice);
        if (ns && !ns->nogood)
            get_depth_solutions1(ns, choice_sets, cs_index + 1);
    }
}

static void extend_via_defaults(Env *solution, List *remaining,
                                int rem_start, List *original) {
    int di = rem_start;
    while (di < remaining->size) {
        TmsNode *def = (TmsNode *)remaining->data[di];
        Env *ns = cons_env(def, solution);
        if (!ns->nogood) {
            extend_via_defaults(ns, remaining, di + 1, original);
            di++;
            continue;
        }
        di++;
    }
    /* Check if already in solutions */
    if (list_contains(_solutions, solution)) return;
    /* Check if any default could still be added */
    for (int i = 0; i < original->size; i++) {
        TmsNode *def = (TmsNode *)original->data[i];
        if (list_contains(solution->assumptions, def)) continue;
        Env *test = cons_env(def, solution);
        if (!test->nogood) return;  /* Could add more */
    }
    list_push(_solutions, solution);
}

List *interpretations(Atms *atms, List *choice_sets, List *defaults) {
    if (atms->debugging)
        fprintf(stderr, " Constructing interpretations...\n");
    _solutions = list_new();
    /* Convert choice-sets: each set of nodes -> list of envs */
    List *env_choice_sets = list_new();
    for (int i = 0; i < choice_sets->size; i++) {
        List *alt_set = (List *)choice_sets->data[i];
        List *envs = list_new();
        for (int j = 0; j < alt_set->size; j++) {
            TmsNode *alt = (TmsNode *)alt_set->data[j];
            for (int k = 0; k < alt->label->size; k++)
                list_push(envs, alt->label->data[k]);
        }
        list_push(env_choice_sets, envs);
    }
    if (env_choice_sets->size > 0) {
        List *first = (List *)env_choice_sets->data[0];
        for (int i = 0; i < first->size; i++) {
            Env *choice = (Env *)first->data[i];
            get_depth_solutions1(choice, env_choice_sets, 1);
        }
    }
    list_delete_nils(_solutions);
    if (_solutions->size == 0 && choice_sets->size == 0)
        list_push(_solutions, atms->empty_env);
    if (_solutions->size == 0) {
        /* Clean up */
        for (int i = 0; i < env_choice_sets->size; i++)
            list_free((List *)env_choice_sets->data[i]);
        list_free(env_choice_sets);
        list_free(_solutions);
        return NULL;
    }
    /* Extend via defaults */
    if (defaults && defaults->size > 0) {
        List *base = _solutions;
        _solutions = list_new();
        for (int i = 0; i < base->size; i++) {
            if (base->data[i])
                extend_via_defaults((Env *)base->data[i],
                                    defaults, 0, defaults);
        }
        list_delete_nils(_solutions);
        list_free(base);
    }
    for (int i = 0; i < env_choice_sets->size; i++)
        list_free((List *)env_choice_sets->data[i]);
    list_free(env_choice_sets);
    List *result = _solutions;
    _solutions = NULL;
    return result;
}

/* ================================================================ */
/* Explanation                                                      */
/* ================================================================ */

typedef struct ExplainPair {
    bool is_assume;
    union { Just *just; TmsNode *node; };
} ExplainPair;

static List *explain_node_1(Env *env, TmsNode *node,
                            List *queued, List *explanation) {
    if (list_contains(queued, node)) return NULL;
    /* Assumption node in env */
    if (node->is_assumption &&
        list_contains(env->assumptions, node)) {
        List *result = explanation ? list_copy(explanation) : list_new();
        ExplainPair *ep = (ExplainPair *)calloc(1, sizeof(ExplainPair));
        ep->is_assume = true;
        ep->node = node;
        list_push(result, ep);
        return result;
    }
    /* Already explained */
    if (explanation) {
        for (int i = 0; i < explanation->size; i++) {
            ExplainPair *ep = (ExplainPair *)explanation->data[i];
            if (!ep->is_assume && ep->just->consequence == node)
                return explanation;
        }
    }
    /* Find justification */
    list_push(queued, node);
    for (int i = 0; i < node->justs->size; i++) {
        Just *just = (Just *)node->justs->data[i];
        bool all_in = true;
        for (int j = 0; j < just->antecedents->size; j++) {
            if (!in_node((TmsNode *)just->antecedents->data[j], env)) {
                all_in = false; break;
            }
        }
        if (!all_in) continue;
        List *new_exp = explanation ? list_copy(explanation) : list_new();
        bool failed = false;
        for (int j = 0; j < just->antecedents->size; j++) {
            TmsNode *ant = (TmsNode *)just->antecedents->data[j];
            new_exp = explain_node_1(env, ant, queued, new_exp);
            if (!new_exp) { failed = true; break; }
        }
        if (!failed) {
            ExplainPair *ep = (ExplainPair *)calloc(1, sizeof(ExplainPair));
            ep->is_assume = false;
            ep->just = just;
            list_push(new_exp, ep);
            queued->size--;  /* pop node */
            return new_exp;
        }
    }
    queued->size--;  /* pop node */
    return NULL;
}

List *explain_node(TmsNode *node, Env *env) {
    List *queued = list_new();
    List *result = explain_node_1(env, node, queued, NULL);
    list_free(queued);
    return result;
}

/* ================================================================ */
/* Printing                                                         */
/* ================================================================ */

void why_node(TmsNode *node, FILE *stream, const char *prefix) {
    if (!stream) stream = stdout;
    if (!prefix) prefix = "";
    fprintf(stream, "\n<%s%s,{", prefix,
            node->datum ? (char *)node->datum : "?");
    for (int i = 0; i < node->label->size; i++)
        env_string((Env *)node->label->data[i], stream);
    fprintf(stream, "}>");
}

void why_nodes(Atms *atms, FILE *stream) {
    if (!stream) stream = stdout;
    for (int i = atms->nodes->size - 1; i >= 0; i--)
        why_node((TmsNode *)atms->nodes->data[i], stream, "");
}

void node_justifications(TmsNode *node, FILE *stream) {
    if (!stream) stream = stdout;
    fprintf(stream, "\n For %s:", node_string_fn(node));
    for (int i = 0; i < node->justs->size; i++)
        print_justification((Just *)node->justs->data[i], stream);
}

void print_justification(Just *j, FILE *stream) {
    if (!stream) stream = stdout;
    fprintf(stream, "\n  %s, ", j->informant);
    for (int i = 0; i < j->antecedents->size; i++)
        why_node((TmsNode *)j->antecedents->data[i], stream, "     ");
}

Env *find_env_by_index(Atms *atms, int n) {
    for (TableEntry *entry = atms->env_table; entry;
         entry = entry->next) {
        for (int i = 0; i < entry->envs->size; i++) {
            Env *env = (Env *)entry->envs->data[i];
            if (env->index == n) return env;
        }
    }
    return NULL;
}

void print_env(Env *e, FILE *stream) {
    if (!stream) stream = stdout;
    fprintf(stream, "\nE-%d:%s", e->index,
            e->nogood ? "* " : " ");
    env_string(e, stream);
}

void env_string(Env *e, FILE *stream) {
    if (!stream) stream = stdout;
    fprintf(stream, "{");
    for (int i = 0; i < e->assumptions->size; i++) {
        TmsNode *a = (TmsNode *)e->assumptions->data[i];
        if (i > 0) fprintf(stream, ",");
        fprintf(stream, "%s", node_string_fn(a));
    }
    fprintf(stream, "}");
}

void print_nogoods(Atms *atms, FILE *stream) {
    print_env_table(atms->nogood_table, stream);
}

void print_envs(Atms *atms, FILE *stream) {
    print_env_table(atms->env_table, stream);
}

void print_env_table(TableEntry *table, FILE *stream) {
    if (!stream) stream = stdout;
    for (TableEntry *entry = table; entry; entry = entry->next) {
        for (int i = 0; i < entry->envs->size; i++)
            print_env((Env *)entry->envs->data[i], stream);
    }
}

void print_atms_statistics(Atms *atms) {
    printf("\n For env table:");
    for (TableEntry *e = atms->env_table; e; e = e->next)
        printf("\n   Length %d, %d", e->count, e->envs->size);
    printf("\n For nogood table:");
    for (TableEntry *e = atms->nogood_table; e; e = e->next)
        printf("\n   Length %d, %d", e->count, e->envs->size);
}
