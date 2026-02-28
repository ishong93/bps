/* -*- C -*- */

/* ATRE test code */
/* Converted from atret.lisp */

#include "atre.h"
#include <stdio.h>

/* ================================================================ */
/* Helper: build S-expression lists from C strings                  */
/* ================================================================ */

/* Build (implies (sentient-robot Robbie) (Human Robbie)) etc. */
static SExpr *make_list2(const char *a, const char *b) {
    return sexpr_cons(sexpr_symbol(a),
            sexpr_cons(sexpr_symbol(b), sexpr_nil()));
}

static SExpr *make_implies(SExpr *ante, SExpr *conse) {
    return sexpr_cons(sexpr_symbol("implies"),
            sexpr_cons(ante, sexpr_cons(conse, sexpr_nil())));
}

static SExpr *make_just1(const char *inf) {
    return sexpr_cons(sexpr_symbol(inf), sexpr_nil());
}

static SExpr *make_just2(const char *inf, SExpr *f1, SExpr *f2) {
    return sexpr_cons(sexpr_symbol(inf),
            sexpr_cons(sexpr_copy(f1),
             sexpr_cons(sexpr_copy(f2), sexpr_nil())));
}

/* ================================================================ */
/* ATRE test 1: INTERN rules with assume!                           */
/* ================================================================ */

/* For test rules, we use procedural registration */

static RuleMatcherFn implies_matcher;
static RuleBodyFn implies_intern_body;

static MatchResult match_implies_intern(SExpr *p) {
    MatchResult mr = {false, NULL, RULE_INTERN};
    /* Match (implies ?ante ?conse) */
    if (!sexpr_is_cons(p)) return mr;
    SExpr *head = sexpr_car(p);
    if (!sexpr_is_symbol(head) ||
        strcmp(head->symbol, "implies") != 0) return mr;
    SExpr *rest = sexpr_cdr(p);
    if (!sexpr_is_cons(rest)) return mr;
    SExpr *ante = sexpr_car(rest);
    SExpr *rest2 = sexpr_cdr(rest);
    if (!sexpr_is_cons(rest2)) return mr;
    SExpr *conse = sexpr_car(rest2);
    mr.ok = true;
    mr.bindings = list_new();
    list_push(mr.bindings, sexpr_copy(p));     /* ?f1 = whole form */
    list_push(mr.bindings, sexpr_copy(ante));  /* ?ante */
    list_push(mr.bindings, sexpr_copy(conse)); /* ?conse */
    mr.condition = RULE_INTERN;
    return mr;
}

/* Body: assert! ?conse (:CE ?f1 ?ante) */
static void body_implies_intern(List *args) {
    SExpr *f1 = (SExpr *)args->data[0];
    SExpr *ante = (SExpr *)args->data[1];
    SExpr *conse = (SExpr *)args->data[2];
    /* Build just = (:CE ?f1 ?ante) -> ("CE" f1 ante) */
    SExpr *just = sexpr_cons(sexpr_symbol("CE"),
                   sexpr_cons(sexpr_copy(f1),
                    sexpr_cons(sexpr_copy(ante), sexpr_nil())));
    assert_fact(conse, just);
}

void atre_test1(bool debugging) {
    Atre *atre = create_atre("Test ATRE", debugging);
    in_atre(atre);
    /* Register the implies rule */
    insert_rule(get_dbclass(sexpr_symbol("implies")),
                match_implies_intern, body_implies_intern,
                NULL, NULL);
    run_rules();
    /* assume! (implies (sentient-robot Robbie) (Human Robbie)) */
    SExpr *sr = make_list2("sentient-robot", "Robbie");
    SExpr *hr = make_list2("Human", "Robbie");
    SExpr *imp1 = make_implies(sr, hr);
    assume_fact(imp1, "no-bias");
    run_rules();
    /* assume! (implies (human Robbie) (mortal Robbie)) */
    SExpr *hr2 = make_list2("human", "Robbie");
    SExpr *mr = make_list2("mortal", "Robbie");
    SExpr *imp2 = make_implies(hr2, mr);
    assume_fact(imp2, "sigh");
    run_rules();
    /* assume! (sentient-robot Robbie) */
    assume_fact(sexpr_copy(sr), "sort-of");
    run_rules();
    printf("\n=== atre_test1 results ===\n");
    show_data(stdout);
    printf("\n");
}

/* ================================================================ */
/* ATRE test 2: INTERN rules with assert!                           */
/* ================================================================ */

void atre_test2(bool debugging) {
    Atre *atre = create_atre("Test ATRE", debugging);
    in_atre(atre);
    insert_rule(get_dbclass(sexpr_symbol("implies")),
                match_implies_intern, body_implies_intern,
                NULL, NULL);
    run_rules();
    SExpr *sr = make_list2("sentient-robot", "Robbie");
    SExpr *hr = make_list2("Human", "Robbie");
    SExpr *imp1 = make_implies(sr, hr);
    assert_fact(imp1, make_just1("no-bias"));
    run_rules();
    SExpr *hr2 = make_list2("human", "Robbie");
    SExpr *mr = make_list2("mortal", "Robbie");
    SExpr *imp2 = make_implies(hr2, mr);
    assert_fact(imp2, make_just1("sigh"));
    run_rules();
    assume_fact(sexpr_copy(sr), "sort-of");
    run_rules();
    printf("\n=== atre_test2 results ===\n");
    show_data(stdout);
    printf("\n");
}

/* ================================================================ */
/* ATRE test 3: IN rules                                            */
/* ================================================================ */

static MatchResult match_implies_in(SExpr *p) {
    MatchResult mr = match_implies_intern(p);
    mr.condition = RULE_IN;
    return mr;
}

/* Body for :IN rule - same logic but with trigger node check */
static void body_implies_in(List *args) {
    /* args[0] = trigger_node, args[1] = f1, args[2] = ante, args[3] = conse */
    TmsNode *trigger = (TmsNode *)args->data[0];
    SExpr *f1 = (SExpr *)args->data[1];
    SExpr *ante = (SExpr *)args->data[2];
    SExpr *conse = (SExpr *)args->data[3];
    if (trigger->label->size > 0) {
        SExpr *just = sexpr_cons(sexpr_symbol("CE"),
                       sexpr_cons(sexpr_copy(f1),
                        sexpr_cons(sexpr_copy(ante), sexpr_nil())));
        assert_fact(conse, just);
    } else {
        /* Defer: push to node rules */
        /* Simplified: store callback info */
        List *rule_info = list_new();
        list_push(rule_info, f1);
        list_push(rule_info, ante);
        list_push(rule_info, conse);
        list_push(trigger->rules, rule_info);
    }
}

void atre_test3(bool debugging) {
    Atre *atre = create_atre("Test ATRE", debugging);
    in_atre(atre);
    insert_rule(get_dbclass(sexpr_symbol("implies")),
                match_implies_in, body_implies_in,
                NULL, NULL);
    run_rules();
    SExpr *sr = make_list2("sentient-robot", "Robbie");
    SExpr *hr = make_list2("Human", "Robbie");
    SExpr *imp1 = make_implies(sr, hr);
    assert_fact(imp1, make_just1("no-bias"));
    run_rules();
    SExpr *hr2 = make_list2("human", "Robbie");
    SExpr *mr = make_list2("mortal", "Robbie");
    SExpr *imp2 = make_implies(hr2, mr);
    assert_fact(imp2, make_just1("sigh"));
    run_rules();
    assume_fact(sexpr_copy(sr), "sort-of");
    run_rules();
    printf("\n=== atre_test3 results ===\n");
    show_data(stdout);
    printf("\n");
}

/* ================================================================ */
/* ATRE test 4: IMPLIED-BY rules                                    */
/* ================================================================ */

static MatchResult match_implies_implied_by(SExpr *p) {
    MatchResult mr = match_implies_intern(p);
    mr.condition = RULE_IMPLIED_BY;
    return mr;
}

void atre_test4(bool debugging) {
    Atre *atre = create_atre("Test ATRE", debugging);
    in_atre(atre);
    insert_rule(get_dbclass(sexpr_symbol("implies")),
                match_implies_implied_by, body_implies_in,
                NULL, NULL);
    run_rules();
    SExpr *sr = make_list2("sentient-robot", "Robbie");
    SExpr *hr = make_list2("Human", "Robbie");
    SExpr *imp1 = make_implies(sr, hr);
    assume_fact(imp1, "no-bias");
    run_rules();
    SExpr *hr2 = make_list2("human", "Robbie");
    SExpr *mr = make_list2("mortal", "Robbie");
    SExpr *imp2 = make_implies(hr2, mr);
    assert_fact(imp2, make_just1("sigh"));
    run_rules();
    assume_fact(sexpr_copy(sr), "sort-of");
    run_rules();
    printf("\n=== atre_test4 results ===\n");
    show_data(stdout);
    print_envs(atre->atms, stdout);
    printf("\n");
}

/* ================================================================ */
/* ATRE test 5: Contradiction rules                                 */
/* ================================================================ */

static MatchResult match_mortal(SExpr *p) {
    MatchResult mr = {false, NULL, RULE_INTERN};
    if (!sexpr_is_cons(p)) return mr;
    SExpr *head = sexpr_car(p);
    if (!sexpr_is_symbol(head) ||
        strcmp(head->symbol, "mortal") != 0) return mr;
    SExpr *rest = sexpr_cdr(p);
    if (!sexpr_is_cons(rest)) return mr;
    mr.ok = true;
    mr.bindings = list_new();
    list_push(mr.bindings, sexpr_copy(p));
    list_push(mr.bindings, sexpr_copy(sexpr_car(rest)));
    return mr;
}

static MatchResult match_immortal(SExpr *p) {
    MatchResult mr = {false, NULL, RULE_INTERN};
    if (!sexpr_is_cons(p)) return mr;
    SExpr *head = sexpr_car(p);
    if (!sexpr_is_symbol(head) ||
        strcmp(head->symbol, "immortal") != 0) return mr;
    SExpr *rest = sexpr_cdr(p);
    if (!sexpr_is_cons(rest)) return mr;
    mr.ok = true;
    mr.bindings = list_new();
    list_push(mr.bindings, sexpr_copy(p));
    list_push(mr.bindings, sexpr_copy(sexpr_car(rest)));
    return mr;
}

static void contra_callback(Env *env) {
    printf("\n Poor Robbie!  --> E-%d.", env->index);
}

void atre_test5(bool debugging) {
    Atre *atre = create_atre("Test ATRE", debugging);
    in_atre(atre);
    /* Register implies rule */
    insert_rule(get_dbclass(sexpr_symbol("implies")),
                match_implies_intern, body_implies_intern,
                NULL, NULL);
    run_rules();
    /* assume! (sentient Robbie) */
    SExpr *sent = make_list2("sentient", "Robbie");
    assume_fact(sent, "sort-of");
    run_rules();
    /* assume! (immortal Robbie) */
    SExpr *imm = make_list2("immortal", "Robbie");
    assume_fact(imm, "why-not");
    run_rules();
    /* Register mortal/immortal nogood rule (simplified) */
    /* In the original, this is a nested rule. Here we set up
       the contradiction rule on the environment */
    /* Setup contradiction-rule on env of (sentient, immortal) */
    List *facts = list_new();
    list_push(facts, sexpr_copy(sent));
    list_push(facts, sexpr_copy(imm));
    Env *env = environment_of(facts);
    if (env)
        contradiction_rule(env, contra_callback, atre);
    list_free(facts);
    run_rules();
    /* assert! (implies (sentient Robbie) (Human Robbie)) */
    SExpr *sent2 = make_list2("sentient", "Robbie");
    SExpr *hr = make_list2("Human", "Robbie");
    SExpr *imp1 = make_implies(sent2, hr);
    assert_fact(imp1, make_just1("no-bias"));
    run_rules();
    /* assert! (implies (human Robbie) (mortal Robbie)) */
    SExpr *hr2 = make_list2("human", "Robbie");
    SExpr *mort = make_list2("mortal", "Robbie");
    SExpr *imp2 = make_implies(hr2, mort);
    assert_fact(imp2, make_just1("sigh"));
    run_rules();
    /* Now the mortal/immortal nogood should trigger */
    /* Manually create the nogood between mortal and immortal */
    SExpr *mortal_r = make_list2("mortal", "Robbie");
    SExpr *immortal_r = make_list2("immortal", "Robbie");
    TmsNode *mn = get_tms_node(mortal_r);
    TmsNode *in = get_tms_node(immortal_r);
    if (mn && in) {
        List *nn = list_new();
        list_push(nn, mn); list_push(nn, in);
        nogood_nodes("DEFINITION", nn);
    }
    run_rules();
    printf("\n=== atre_test5 results ===\n");
    print_nogoods(atre->atms, stdout);
    printf("\n");
}
