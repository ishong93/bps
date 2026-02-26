/* -*- C -*- */

/* ATMS test code */
/* Converted from atest.lisp */

#include "atms.h"
#include <stdio.h>

static Atms *test_atms;
static TmsNode *a, *b, *c, *d, *e, *f;

/* ================================================================ */
/* atms_test1                                                       */
/* ================================================================ */

void atms_test1(void) {
    test_atms = create_atms("atms-test1", NULL, true, NULL);
    a = tms_create_node(test_atms, strdup("A"), false, false);
    b = tms_create_node(test_atms, strdup("B"), false, false);
    c = tms_create_node(test_atms, strdup("C"), false, false);
    d = tms_create_node(test_atms, strdup("D"), false, false);
    e = tms_create_node(test_atms, strdup("E"), false, false);
    f = tms_create_node(test_atms, strdup("F"), false, false);
    assume_node(a);
    assume_node(b);
    assume_node(c);
    List *ab = list_new(); list_push(ab, a); list_push(ab, b);
    justify_node("J1", d, ab);
    List *bc = list_new(); list_push(bc, b); list_push(bc, c);
    justify_node("J2", e, bc);
    List *de = list_new(); list_push(de, d); list_push(de, e);
    justify_node("J3", f, de);
    printf("\n=== atms_test1 results ===\n");
    why_nodes(test_atms, stdout);
    printf("\n");
    print_envs(test_atms, stdout);
    printf("\n");
}

/* ================================================================ */
/* atms_test2: simpler justification for d                          */
/* ================================================================ */

void atms_test2(void) {
    List *al = list_new(); list_push(al, a);
    justify_node("simpler", d, al);
    printf("\n=== atms_test2 results ===\n");
    why_nodes(test_atms, stdout);
    printf("\n");
    print_envs(test_atms, stdout);
    printf("\n");
}

/* ================================================================ */
/* atms_test3: declare {A,B} as nogood                              */
/* ================================================================ */

void atms_test3(void) {
    List *ab = list_new(); list_push(ab, a); list_push(ab, b);
    nogood_nodes("atms-test3", ab);
    printf("\n=== atms_test3 results ===\n");
    why_nodes(test_atms, stdout);
    printf("\n");
    print_envs(test_atms, stdout);
    printf("\n");
}

/* ================================================================ */
/* step_1: de Kleer example                                         */
/* ================================================================ */

void step_1(void) {
    static TmsNode *sa, *sb, *sc, *x1, *yx, *xz, *y1, *z1;
    Atms *atms = create_atms("Step-1", NULL, false, NULL);
    sa = tms_create_node(atms, strdup("A"), false, false);
    sb = tms_create_node(atms, strdup("B"), false, false);
    sc = tms_create_node(atms, strdup("C"), false, false);
    x1 = tms_create_node(atms, strdup("x=1"), false, false);
    yx = tms_create_node(atms, strdup("y=x"), false, false);
    xz = tms_create_node(atms, strdup("x=z"), false, false);
    y1 = tms_create_node(atms, strdup("y=1"), false, false);
    z1 = tms_create_node(atms, strdup("z=1"), false, false);

    assume_node(sa);
    assume_node(sb);
    assume_node(sc);

    List *la = list_new(); list_push(la, sa);
    justify_node("j1", x1, la);
    List *lb = list_new(); list_push(lb, sb);
    justify_node("j2", yx, lb);
    List *lc = list_new(); list_push(lc, sc);
    justify_node("j3", xz, lc);

    why_nodes(atms, stdout);
    print_envs(atms, stdout);

    printf("\n\nNow register nogood{A,B}");
    List *ab = list_new(); list_push(ab, sa); list_push(ab, sb);
    nogood_nodes("NOGOOD", ab);
    why_nodes(atms, stdout);
    print_envs(atms, stdout);

    printf("\n\nx=1, y=x => y=1");
    List *x1yx = list_new();
    list_push(x1yx, x1); list_push(x1yx, yx);
    justify_node("j4", y1, x1yx);
    why_nodes(atms, stdout);
    print_envs(atms, stdout);

    printf("\n\nWe have a premise z=1");
    justify_node("Premise", z1, list_new());
    why_nodes(atms, stdout);
    print_envs(atms, stdout);

    printf("\n\nz=1, x=z => x=1");
    List *z1xz = list_new();
    list_push(z1xz, z1); list_push(z1xz, xz);
    justify_node("j5", x1, z1xz);
    why_nodes(atms, stdout);
    print_envs(atms, stdout);
    printf("\n");
}
