/* -*- C -*- */

/* N-Queens 규칙: JTRE 버전 */
/* jqrule.lisp에서 변환 */

/* Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. All rights reserved. */

/*
 * 원본 Lisp:
 *
 *   (contradiction 'Queens-capture *jtre*)
 *
 *   (rule ((:IN (Queen ?column1 ?row1) :VAR ?Q1)
 *          (:IN (Queen ?column2 ?row2) :VAR ?Q2
 *            :TEST (not (or (= ?column1 ?column2)
 *                           (queens-okay? ?column1 ?row1
 *                                         ?column2 ?row2)))))
 *         (rassert! Queens-capture (Death ?Q1 ?Q2)))
 *
 * 두 퀸이 같은 열이 아니면서 서로 잡을 수 있으면(같은 행 또는 대각선)
 * Queens-capture를 assert하여 모순을 발생시킨다.
 */

#include "jqrule.h"

/* ================================================================ */
/* 퀸 배치에서 열/행 추출 유틸리티                                   */
/* ================================================================ */

/* (Queen col row) S-expression에서 col, row를 추출한다.
 * 성공 시 true 반환, fact가 올바른 형식이 아니면 false. */
static bool extract_queen_col_row(SExpr *fact, int *col_out,
                                  int *row_out) {
    /* fact = (Queen col row) = cons("Queen", cons(col, cons(row, nil))) */
    if (!sexpr_is_cons(fact)) return false;
    SExpr *head = sexpr_car(fact);
    if (!sexpr_is_symbol(head)) return false;
    if (strcmp(head->symbol, "Queen") != 0) return false;

    SExpr *rest1 = sexpr_cdr(fact);
    if (!sexpr_is_cons(rest1)) return false;
    SExpr *col_expr = sexpr_car(rest1);
    if (!sexpr_is_number(col_expr)) return false;

    SExpr *rest2 = sexpr_cdr(rest1);
    if (!sexpr_is_cons(rest2)) return false;
    SExpr *row_expr = sexpr_car(rest2);
    if (!sexpr_is_number(row_expr)) return false;

    *col_out = (int)col_expr->number;
    *row_out = (int)row_expr->number;
    return true;
}

/* ================================================================ */
/* 퀸 충돌 규칙 본체                                                 */
/* ================================================================ */

/*
 * queen_capture_rule_body: 두 퀸 팩트가 충돌하면 Queens-capture를 assert
 *
 * Lisp의 (rassert! Queens-capture (Death ?Q1 ?Q2)) 에 해당.
 * rassert!는 assert!와 같지만, justification에 두 퀸 팩트를 포함한다.
 */
void queen_capture_rule_body(SExpr *q1_fact, SExpr *q2_fact,
                             Jtre *jtre) {
    /* (Death q1 q2) justification 생성 */
    /* assert! Queens-capture (Death q1 q2)
     * => justify "Queens-capture" node with antecedents [q1, q2] */
    SExpr *capture = sexpr_symbol("Queens-capture");

    /* justification: (Death q1 q2) 형태 */
    SExpr *death = sexpr_cons(sexpr_symbol("Death"),
                   sexpr_cons(sexpr_copy(q1_fact),
                   sexpr_cons(sexpr_copy(q2_fact),
                   sexpr_nil())));

    /* assert! Queens-capture with justification (Death q1 q2)
     * 이는 JTRE의 assert_fact을 호출하여 Queens-capture 노드를
     * q1_fact, q2_fact의 TMS 노드로 정당화한다. */
    assert_fact(capture, death, jtre);

    sexpr_free(death);
    sexpr_free(capture);
}

/* ================================================================ */
/* 규칙 검사: 모든 IN 상태 퀸 쌍에 대해 충돌 검사                    */
/* ================================================================ */

/*
 * check_queen_captures: Queen dbclass의 모든 IN 팩트 쌍을 검사한다.
 *
 * Lisp의 rule 매크로는 새 팩트가 IN이 될 때마다 트리거되지만,
 * C 버전에서는 enqueue 콜백을 통해 호출된다.
 * 새 Queen 팩트(datum)가 IN이 되면, 기존 IN Queen 팩트들과 비교한다.
 */
static void check_queen_capture_against(Datum *new_datum,
                                        Jtre *jtre) {
    int col1, row1;
    if (!extract_queen_col_row(new_datum->lisp_form, &col1, &row1))
        return;

    /* Queen dbclass의 모든 팩트를 순회 */
    Dbclass *dc = new_datum->dbclass;
    for (int i = 0; i < dc->facts->size; i++) {
        Datum *other = (Datum *)list_get(dc->facts, i);
        if (other == new_datum) continue;

        /* 다른 팩트가 IN 상태인지 확인 */
        if (!in_node(other->tms_node)) continue;

        int col2, row2;
        if (!extract_queen_col_row(other->lisp_form, &col2, &row2))
            continue;

        /* 같은 열이면 무시 (다른 열의 퀸끼리만 검사) */
        if (col1 == col2) continue;

        /* 충돌 검사: 같은 행 또는 대각선 */
        if (!queens_okay(col1, row1, col2, row2)) {
            queen_capture_rule_body(new_datum->lisp_form,
                                    other->lisp_form, jtre);
        }
    }
}

/* ================================================================ */
/* 규칙 등록 콜백                                                    */
/* ================================================================ */

/*
 * queen_rule_trigger: enqueue 콜백을 통해 호출되는 규칙 트리거.
 *
 * JTRE의 enqueue_procedure로 등록되어, 노드가 IN이 될 때
 * 해당 노드의 in_rules에 등록된 콜백이 큐에 추가되고,
 * run_rules 시 실행된다.
 *
 * 이 함수는 datum을 인수로 받아 충돌 검사를 수행한다.
 */
static void queen_rule_trigger(void *arg) {
    Datum *datum = (Datum *)arg;
    if (!datum || !datum->jtre) return;

    /* Queen 팩트인지 확인 */
    if (!sexpr_is_cons(datum->lisp_form)) return;
    SExpr *head = sexpr_car(datum->lisp_form);
    if (!sexpr_is_symbol(head)) return;
    if (strcmp(head->symbol, "Queen") != 0) return;

    check_queen_capture_against(datum, datum->jtre);
}

/* ================================================================ */
/* 규칙 로드                                                         */
/* ================================================================ */

/*
 * load_queen_rules: Queens-capture를 모순으로 선언하고,
 *                   퀸 충돌 규칙을 JTRE에 등록한다.
 *
 * Lisp의 (load *queen-rules-file*)에 해당.
 *
 * 1. (contradiction 'Queens-capture *jtre*) 호출
 * 2. Queen dbclass에 IN 규칙 등록
 */
void load_queen_rules(Jtre *jtre) {
    /* Queens-capture를 모순으로 선언 */
    SExpr *capture_sym = sexpr_symbol("Queens-capture");
    contradiction(capture_sym, jtre);
    sexpr_free(capture_sym);

    /* Queen dbclass에 규칙 등록:
     * 새 Queen 팩트가 IN이 되면 queen_rule_trigger가 호출된다. */
    SExpr *queen_sym = sexpr_symbol("Queen");
    Dbclass *dc = get_dbclass(queen_sym, jtre);
    sexpr_free(queen_sym);

    /* 규칙 등록: IN 트리거로 queen_rule_trigger를 등록 */
    insert_rule(dc, NULL, queen_rule_trigger, jtre);
}
