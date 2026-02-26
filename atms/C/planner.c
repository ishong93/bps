/* -*- C -*- */

/* ATMS-based planner */
/* Converted from aplanr.lisp, plan-a.lisp, plan-e.lisp, */
/*   bcode.lisp, blocks.lisp */

#include "planner.h"

Plnpr *current_plnpr = NULL;

/* ================================================================ */
/* Planner creation                                                 */
/* ================================================================ */

Plnpr *create_planning_problem(const char *title, List *basis_set) {
    Plnpr *p = (Plnpr *)calloc(1, sizeof(Plnpr));
    p->title = strdup(title);
    char atre_title[256];
    snprintf(atre_title, sizeof(atre_title), "ATRE(%s)", title);
    p->atre = create_atre(atre_title, false);
    p->basis_set = basis_set ? basis_set : list_new();
    p->operators = list_new();
    in_plnpr(p);
    return p;
}

void in_plnpr(Plnpr *plnpr) { current_plnpr = plnpr; }

void set_debug_plnpr(bool state) {
    current_plnpr->debugging = state;
}

void setup_choice_sets(void) {
    Plnpr *p = current_plnpr;
    Atre *prev_atre = current_atre;
    in_atre(p->atre);
    char informant[256];
    snprintf(informant, sizeof(informant), "BASIS SET(%s)",
             p->title);
    for (int i = 0; i < p->basis_set->size; i++) {
        List *cs = (List *)p->basis_set->data[i];
        for (int j = 0; j < cs->size; j++) {
            SExpr *choice = (SExpr *)cs->data[j];
            assume_if_needed(choice, informant);
        }
    }
    run_rules();
    if (prev_atre) in_atre(prev_atre);
}

/* ================================================================ */
/* Operator management                                              */
/* ================================================================ */

void register_operator(SExpr *form, SExpr *preconditions,
                       SExpr *add_list, SExpr *delete_list,
                       SExpr *test_expr) {
    Operator *op = (Operator *)calloc(1, sizeof(Operator));
    op->form = sexpr_copy(form);
    op->preconditions = preconditions ? sexpr_copy(preconditions)
                                      : sexpr_nil();
    op->add_list = add_list ? sexpr_copy(add_list) : sexpr_nil();
    op->delete_list = delete_list ? sexpr_copy(delete_list)
                                  : sexpr_nil();
    OpEntry *entry = (OpEntry *)calloc(1, sizeof(OpEntry));
    entry->name = strdup(sexpr_car(form)->symbol);
    entry->op = op;
    list_push(current_plnpr->operators, entry);
    (void)test_expr;
    /* Register applicability rule: when preconditions are met,
       assert (applicable <form>) */
    /* In C, we handle applicability via find_applicable_operators */
}

Operator *fetch_operator(const char *op_name) {
    for (int i = 0; i < current_plnpr->operators->size; i++) {
        OpEntry *e = (OpEntry *)current_plnpr->operators->data[i];
        if (strcmp(e->name, op_name) == 0) return e->op;
    }
    return NULL;
}

/* Helper: substitute bindings in an S-expression */
static SExpr *sublis(List *var_names, List *vals, SExpr *expr) {
    if (sexpr_is_nil(expr)) return sexpr_nil();
    if (sexpr_is_symbol(expr) && expr->symbol[0] == '?') {
        for (int i = 0; i < var_names->size; i++) {
            SExpr *v = (SExpr *)var_names->data[i];
            if (sexpr_equal(v, expr))
                return sexpr_copy((SExpr *)vals->data[i]);
        }
        return sexpr_copy(expr);
    }
    if (!sexpr_is_cons(expr)) return sexpr_copy(expr);
    return sexpr_cons(sublis(var_names, vals, expr->cons.car),
                      sublis(var_names, vals, expr->cons.cdr));
}

/* Convert SExpr list to List of SExpr* */
static List *sexpr_to_list(SExpr *e) {
    List *l = list_new();
    while (sexpr_is_cons(e)) {
        list_push(l, sexpr_copy(e->cons.car));
        e = e->cons.cdr;
    }
    return l;
}

List *find_applicable_operators(Env *state) {
    List *result = list_new();
    Atre *prev = current_atre;
    in_atre(current_plnpr->atre);
    List *candidates = fetch(sexpr_parse("(applicable ?x)"));
    for (int i = 0; i < candidates->size; i++) {
        SExpr *cand = (SExpr *)candidates->data[i];
        if (in_fact(cand, state)) {
            /* Extract the operator instance */
            SExpr *rest = sexpr_cdr(cand);
            if (sexpr_is_cons(rest))
                list_push(result, sexpr_copy(sexpr_car(rest)));
        }
    }
    if (prev) in_atre(prev);
    list_free(candidates);
    return result;
}

Env *apply_operator(Env *state, SExpr *op_inst) {
    /* op_inst = (OpName val1 val2 ...) */
    const char *op_name = sexpr_car(op_inst)->symbol;
    Operator *op = fetch_operator(op_name);
    if (!op) return NULL;
    /* Build bindings from operator form variables to values */
    List *var_names = list_new();
    List *vals = list_new();
    SExpr *form_rest = sexpr_cdr(op->form);
    SExpr *inst_rest = sexpr_cdr(op_inst);
    while (sexpr_is_cons(form_rest) && sexpr_is_cons(inst_rest)) {
        list_push(var_names, sexpr_car(form_rest));
        list_push(vals, sexpr_car(inst_rest));
        form_rest = sexpr_cdr(form_rest);
        inst_rest = sexpr_cdr(inst_rest);
    }
    SExpr *add_list = sublis(var_names, vals, op->add_list);
    SExpr *del_list = sublis(var_names, vals, op->delete_list);
    if (current_plnpr->debugging) {
        printf("\n   Applying %s to E-%d.",
               sexpr_to_string(op_inst), state->index);
    }
    /* Start with current assumptions */
    List *assumptions = list_copy(state->assumptions);
    /* Remove delete-list assumptions */
    List *del_items = sexpr_to_list(del_list);
    for (int i = 0; i < del_items->size; i++) {
        SExpr *d = (SExpr *)del_items->data[i];
        for (int j = assumptions->size - 1; j >= 0; j--) {
            TmsNode *node = (TmsNode *)assumptions->data[j];
            Datum *datum = (Datum *)node->datum;
            if (datum && sexpr_equal(datum->lisp_form, d)) {
                /* Remove from assumptions */
                memmove(&assumptions->data[j],
                        &assumptions->data[j+1],
                        (assumptions->size - j - 1) * sizeof(void *));
                assumptions->size--;
                break;
            }
        }
    }
    /* Add add-list items */
    Atre *prev = current_atre;
    in_atre(current_plnpr->atre);
    List *add_items = sexpr_to_list(add_list);
    for (int i = 0; i < add_items->size; i++) {
        SExpr *a = (SExpr *)add_items->data[i];
        TmsNode *n = get_tms_node(a);
        if (n) assumptions = ordered_insert_node(n, assumptions);
    }
    Env *result = find_or_make_env(assumptions,
                                    current_plnpr->atre->atms);
    if (prev) in_atre(prev);
    list_free(del_items);
    list_free(add_items);
    list_free(var_names);
    list_free(vals);
    sexpr_free(add_list);
    sexpr_free(del_list);
    return result;
}

/* ================================================================ */
/* Envisioner                                                       */
/* ================================================================ */

void envision(void) {
    Plnpr *p = current_plnpr;
    Atre *prev = current_atre;
    in_atre(p->atre);
    p->states = solutions(p->atre, p->basis_set);
    if (!p->states) p->states = list_new();
    /* Build transition table */
    p->transitions = list_new();
    for (int i = 0; i < p->states->size; i++) {
        Env *state = (Env *)p->states->data[i];
        TransEntry *te = (TransEntry *)calloc(1, sizeof(TransEntry));
        te->state = state;
        te->pairs = list_new();
        List *ops = find_applicable_operators(state);
        for (int j = 0; j < ops->size; j++) {
            SExpr *op_inst = (SExpr *)ops->data[j];
            Env *result = apply_operator(state, op_inst);
            if (result) {
                TransPair *tp = (TransPair *)calloc(1,
                                    sizeof(TransPair));
                tp->op_inst = op_inst;
                tp->result = result;
                list_push(te->pairs, tp);
            }
        }
        list_push(p->transitions, te);
        list_free(ops);
    }
    if (prev) in_atre(prev);
}

void show_envisionment(FILE *stream) {
    if (!stream) stream = stdout;
    Plnpr *p = current_plnpr;
    if (!p->states || p->states->size == 0) {
        fprintf(stream, "\nThe state space is empty.");
        return;
    }
    fprintf(stream, "\n %d states have been generated:",
            p->states->size);
    for (int i = 0; i < p->states->size; i++)
        print_env((Env *)p->states->data[i], stream);
    fprintf(stream, "\nTransition Table:");
    if (!p->transitions || p->transitions->size == 0) {
        fprintf(stream, " empty.");
        return;
    }
    for (int i = 0; i < p->transitions->size; i++) {
        TransEntry *te = (TransEntry *)p->transitions->data[i];
        fprintf(stream, "\n  E-%d: ", te->state->index);
        for (int j = 0; j < te->pairs->size; j++) {
            TransPair *tp = (TransPair *)te->pairs->data[j];
            fprintf(stream, "\n   %s -> E-%d",
                    sexpr_to_string(tp->op_inst), tp->result->index);
        }
    }
}

/* ================================================================ */
/* Plan search via envisionment                                     */
/* ================================================================ */

/* Helper: fetch states matching given facts */
static List *fetch_states(List *facts) {
    return solutions(current_plnpr->atre,
                     current_plnpr->basis_set);
}

List *find_plan(List *start_facts, List *goal_facts) {
    Plnpr *p = current_plnpr;
    List *goal_states = fetch_states(goal_facts);
    List *start_states = fetch_states(start_facts);
    if (!goal_states || !start_states) return NULL;
    /* BFS */
    typedef struct QEntry { List *path; } QEntry;
    List *queue = list_new();
    for (int i = 0; i < start_states->size; i++) {
        List *path = list_new();
        list_push(path, start_states->data[i]);
        list_push(queue, path);
    }
    while (queue->size > 0) {
        /* Dequeue */
        List *current = (List *)queue->data[0];
        memmove(&queue->data[0], &queue->data[1],
                (queue->size - 1) * sizeof(void *));
        queue->size--;
        Env *head = (Env *)current->data[0];
        /* Check if goal */
        if (list_contains(goal_states, head)) {
            p->plan = current;
            list_free(queue);
            return current;
        }
        /* Find transitions */
        for (int i = 0; i < p->transitions->size; i++) {
            TransEntry *te = (TransEntry *)p->transitions->data[i];
            if (te->state != head) continue;
            for (int j = 0; j < te->pairs->size; j++) {
                TransPair *tp = (TransPair *)te->pairs->data[j];
                if (!list_contains(current, tp->result)) {
                    List *new_path = list_new();
                    list_push(new_path, tp->result);
                    list_push(new_path, tp->op_inst);
                    for (int k = 0; k < current->size; k++)
                        list_push(new_path, current->data[k]);
                    list_push(queue, new_path);
                }
            }
        }
    }
    list_free(queue);
    return NULL;
}

/* ================================================================ */
/* Antecedent planner (Plan-A)                                      */
/* ================================================================ */

bool satisfies_goal(Env *state, List *goals) {
    Atre *prev = current_atre;
    in_atre(current_plnpr->atre);
    for (int i = 0; i < goals->size; i++) {
        SExpr *goal = (SExpr *)goals->data[i];
        if (!in_fact(goal, state)) {
            if (prev) in_atre(prev);
            return false;
        }
    }
    if (prev) in_atre(prev);
    return true;
}

List *plan_a(Env *start, List *goal) {
    Plnpr *p = current_plnpr;
    Atre *prev = current_atre;
    in_atre(p->atre);
    /* BFS queue: each entry is a path (list of state/op pairs) */
    List *queue = list_new();
    List *initial = list_new();
    list_push(initial, start);
    list_push(queue, initial);
    int examined = 0;
    List *found = NULL;
    while (queue->size > 0 && !found) {
        List *current = (List *)queue->data[0];
        memmove(&queue->data[0], &queue->data[1],
                (queue->size - 1) * sizeof(void *));
        queue->size--;
        examined++;
        Env *head = (Env *)current->data[0];
        if (satisfies_goal(head, goal)) {
            found = current;
            break;
        }
        List *ops = find_applicable_operators(head);
        for (int i = 0; i < ops->size; i++) {
            SExpr *op_inst = (SExpr *)ops->data[i];
            Env *result = apply_operator(head, op_inst);
            if (result && !list_contains(current, result)) {
                if (p->debugging)
                    printf("\n  Reaching E-%d via %s on E-%d.",
                           result->index,
                           sexpr_to_string(op_inst),
                           head->index);
                List *new_path = list_new();
                list_push(new_path, result);
                list_push(new_path, op_inst);
                for (int k = 0; k < current->size; k++)
                    list_push(new_path, current->data[k]);
                list_push(queue, new_path);
            }
        }
        list_free(ops);
    }
    p->plan = found;
    if (prev) in_atre(prev);
    list_free(queue);
    return found;
}

/* ================================================================ */
/* Display plan                                                     */
/* ================================================================ */

void show_plan(List *plan, FILE *stream) {
    if (!stream) stream = stdout;
    if (!plan || plan->size == 0) {
        fprintf(stream, "\nNo plan found.");
        return;
    }
    /* Plan is stored as: [state_n, op_n, ..., state_1, op_1, state_0] */
    for (int i = plan->size - 1; i >= 0; i -= 2) {
        Env *state = (Env *)plan->data[i];
        print_env(state, stream);
        if (i > 0) {
            SExpr *op = (SExpr *)plan->data[i - 1];
            fprintf(stream, "\n  then, by %s, ",
                    sexpr_to_string(op));
        }
    }
}

/* ================================================================ */
/* Blocks world                                                     */
/* ================================================================ */

List *make_blocks_basis_set(List *blocks) {
    List *basis = list_new();
    for (int i = 0; i < blocks->size; i++) {
        SExpr *block = (SExpr *)blocks->data[i];
        /* What the block can be on */
        List *on_choices = list_new();
        /* (Holding block) */
        list_push(on_choices,
            sexpr_cons(sexpr_symbol("Holding"),
             sexpr_cons(sexpr_copy(block), sexpr_nil())));
        /* (On block Table) */
        list_push(on_choices,
            sexpr_cons(sexpr_symbol("On"),
             sexpr_cons(sexpr_copy(block),
              sexpr_cons(sexpr_symbol("Table"), sexpr_nil()))));
        /* (On block other) for each other block */
        for (int j = 0; j < blocks->size; j++) {
            if (i == j) continue;
            SExpr *other = (SExpr *)blocks->data[j];
            list_push(on_choices,
                sexpr_cons(sexpr_symbol("On"),
                 sexpr_cons(sexpr_copy(block),
                  sexpr_cons(sexpr_copy(other), sexpr_nil()))));
        }
        list_push(basis, on_choices);
        /* What can be on the block */
        List *top_choices = list_new();
        /* (Holding block) */
        list_push(top_choices,
            sexpr_cons(sexpr_symbol("Holding"),
             sexpr_cons(sexpr_copy(block), sexpr_nil())));
        /* (Clear block) */
        list_push(top_choices,
            sexpr_cons(sexpr_symbol("Clear"),
             sexpr_cons(sexpr_copy(block), sexpr_nil())));
        /* (On other block) */
        for (int j = 0; j < blocks->size; j++) {
            if (i == j) continue;
            SExpr *other = (SExpr *)blocks->data[j];
            list_push(top_choices,
                sexpr_cons(sexpr_symbol("ON"),
                 sexpr_cons(sexpr_copy(other),
                  sexpr_cons(sexpr_copy(block), sexpr_nil()))));
        }
        list_push(basis, top_choices);
    }
    /* Hand status */
    List *hand_choices = list_new();
    list_push(hand_choices,
        sexpr_cons(sexpr_symbol("HAND-EMPTY"), sexpr_nil()));
    for (int i = 0; i < blocks->size; i++) {
        SExpr *block = (SExpr *)blocks->data[i];
        list_push(hand_choices,
            sexpr_cons(sexpr_symbol("HOLDING"),
             sexpr_cons(sexpr_copy(block), sexpr_nil())));
    }
    /* Prepend hand choices */
    List *result = list_new();
    list_push(result, hand_choices);
    for (int i = 0; i < basis->size; i++)
        list_push(result, basis->data[i]);
    list_free(basis);
    return result;
}

void register_blocks_operators(void) {
    /* Pickup ?x: pre=(On ?x Table)(Clear ?x)(Hand-empty)
                   del=(On ?x Table)(Clear ?x)(Hand-empty)
                   add=(Holding ?x) */
    register_operator(
        sexpr_parse("(Pickup ?x)"),
        sexpr_parse("((On ?x Table) (Clear ?x) (Hand-empty))"),
        sexpr_parse("((Holding ?x))"),
        sexpr_parse("((On ?x Table) (Clear ?x) (Hand-empty))"),
        NULL);
    /* Putdown ?x */
    register_operator(
        sexpr_parse("(Putdown ?x)"),
        sexpr_parse("((Holding ?x))"),
        sexpr_parse("((On ?x Table) (Clear ?x) (Hand-empty))"),
        sexpr_parse("((Holding ?x))"),
        NULL);
    /* Stack ?x ?y */
    register_operator(
        sexpr_parse("(Stack ?x ?y)"),
        sexpr_parse("((Holding ?x) (Clear ?y))"),
        sexpr_parse("((Hand-empty) (On ?x ?y) (Clear ?x))"),
        sexpr_parse("((Holding ?x) (Clear ?y))"),
        NULL);
    /* Unstack ?x ?y */
    register_operator(
        sexpr_parse("(Unstack ?x ?y)"),
        sexpr_parse("((Hand-empty) (Clear ?x) (On ?x ?y))"),
        sexpr_parse("((Holding ?x) (Clear ?y))"),
        sexpr_parse("((Hand-empty) (Clear ?x) (On ?x ?y))"),
        NULL);
}

Plnpr *build_blocks_problem(const char *title, List *blocks,
                            bool debugging) {
    List *basis = make_blocks_basis_set(blocks);
    Plnpr *plnpr = create_planning_problem(title, basis);
    in_plnpr(plnpr);
    set_debug_plnpr(debugging);
    Atre *prev = current_atre;
    in_atre(plnpr->atre);
    /* Assert block definitions */
    for (int i = 0; i < blocks->size; i++) {
        SExpr *block = (SExpr *)blocks->data[i];
        SExpr *fact = sexpr_cons(sexpr_symbol("block"),
                       sexpr_cons(sexpr_copy(block), sexpr_nil()));
        assert_fact(fact, sexpr_symbol("Definition"));
    }
    run_rules();
    register_blocks_operators();
    setup_choice_sets();
    if (prev) in_atre(prev);
    return plnpr;
}
