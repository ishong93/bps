/* -*- C -*- */

/* JSAINT 기본 규칙 구현 */
/* jsrules.lisp에서 변환 */

/* Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, Xerox Corporation. All Rights reserved. */

#include "jsrules.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ================================================================ */
/* 규칙 핸들러 구현                                                   */
/* ================================================================ */

/* AND-SUBGOALS 규칙 핸들러
 * 원래 Lisp:
 * (rule ((:IN (AND-SUBGOALS ?parent ?children) :VAR ?def))
 *   (dolist (child ?children)
 *     (rassert! (PARENT-OF ?child ?parent :AND) (:DEF-OF-AND ?def))
 *     (rule ((:IN (failed ?child) :VAR ?delinquent))
 *       (rassert! (failed ?parent) (:AND-FAILURE ?def ?delinquent))))
 *   (assert! `(solved ,?parent) ...))
 */
void rule_and_subgoals(List *args) {
    if (args == NULL) return;

    /* args: trigger-node, parent, children 등의 바인딩 */
    /* 실제 구현에서는 바인딩에서 parent와 children을 추출하여
     * assert_fact으로 PARENT-OF, failed, solved를 단언해야 함 */

    /* 프레임워크: 바인딩에서 값을 추출하여 처리 */
    /* SExpr *parent = ...; */
    /* SExpr *children = ...; */

    /* 각 자식에 대해: */
    /* assert_fact(sexpr_list3(sexpr_symbol("PARENT-OF"),
     *                         child, parent), ...); */

    /* 모든 자식의 solved를 선행조건으로 하는 정당화 추가: */
    /* assert_fact(sexpr_list2(sexpr_symbol("solved"), parent), ...); */
}

/* OR-SUBGOALS 규칙 핸들러
 * 원래 Lisp:
 * (rule ((:IN (OR-SUBGOALS ?parent ?children) :VAR ?def :TEST ?children))
 *   (dolist (child ?children)
 *     (rassert! (PARENT-OF ?child ?parent :OR) (:DEF-OF-OR ?def))
 *     (rule ((:IN (SOLVED ?child) :VAR ?winner))
 *       (rassert! (SOLVED ?parent) (:OR-SUCCESS ?winner ?def))))
 *   (assert! `(FAILED ,?parent) ...))
 */
void rule_or_subgoals(List *args) {
    if (args == NULL) return;
    /* 각 자식에 대해 PARENT-OF 단언 */
    /* 하나의 자식 SOLVED 시 부모 SOLVED */
    /* 모든 자식 FAILED 시 부모 FAILED */
}

/* PARENT-OF 규칙 핸들러
 * 원래 Lisp:
 * (rule ((:IN (PARENT-OF ?child ?parent ?type) :VAR ?lineage))
 *   (rassert! (RELEVANT ?child) (:STILL-WORKING-ON (OPEN ?parent) ?lineage)))
 */
void rule_parent_of(List *args) {
    if (args == NULL) return;
    /* RELEVANT ?child를 단언 */
    /* 정당화: OPEN ?parent와 lineage */
}

/* SOLUTION-OF 규칙 핸들러
 * 원래 Lisp:
 * (rule ((:IN (SOLUTION-OF ?problem ?answer) :VAR ?found))
 *   (rassert! (SOLVED ?problem) (:FOUND-ANSWER ?found)))
 */
void rule_solution_of(List *args) {
    if (args == NULL) return;
    /* SOLVED ?problem을 단언 */
}

/* OR-SUBGOALS nil 규칙 핸들러
 * 원래 Lisp:
 * (rule ((:IN (OR-SUBGOALS (Integrate ?expr) NIL) :VAR ?no-ideas))
 *   (rassert! (FAILED (Integrate ?expr)) (:NO-METHODS ?no-ideas)))
 */
void rule_or_subgoals_nil(List *args) {
    if (args == NULL) return;
    /* FAILED (Integrate ?expr)를 단언 */
}

/* SOLVED 규칙 핸들러
 * 원래 Lisp:
 * (rule ((:IN (SOLVED ?problem)))
 *   (retract! `(OPEN ,?problem) :EXPAND-AGENDA-ITEM t))
 */
void rule_solved(List *args) {
    if (args == NULL) return;
    /* (OPEN ?problem)을 철회 */
    /* retract_fact(open_expr, "EXPAND-AGENDA-ITEM", true); */
}

/* FAILED 규칙 핸들러
 * 원래 Lisp:
 * (rule ((:IN (FAILED ?problem)))
 *   (retract! `(OPEN ,?problem) :EXPAND-AGENDA-ITEM t))
 */
void rule_failed(List *args) {
    if (args == NULL) return;
    /* (OPEN ?problem)을 철회 */
    /* retract_fact(open_expr, "EXPAND-AGENDA-ITEM", true); */
}

/* ================================================================ */
/* JSAINT 규칙 등록                                                   */
/* ================================================================ */

/* 매칭 함수 스텁들 */
static MatchResult match_and_subgoals(SExpr *pattern) {
    MatchResult mr = { false, NULL, false };
    /* (AND-SUBGOALS ?parent ?children) 패턴 매칭 */
    if (pattern == NULL || pattern->type != SEXPR_CONS) return mr;
    if (pattern->cons.car->type != SEXPR_SYMBOL) return mr;
    if (strcmp(pattern->cons.car->symbol, "AND-SUBGOALS") == 0) {
        mr.ok = true;
        mr.node = true;
        /* 바인딩: parent, children */
        SExpr *rest = pattern->cons.cdr;
        if (rest && rest->type == SEXPR_CONS) {
            mr.bindings = list_prepend(NULL, rest->cons.car); /* parent */
            if (rest->cons.cdr && rest->cons.cdr->type == SEXPR_CONS) {
                mr.bindings = list_prepend(mr.bindings,
                                           rest->cons.cdr->cons.car); /* children */
            }
        }
    }
    return mr;
}

static MatchResult match_or_subgoals(SExpr *pattern) {
    MatchResult mr = { false, NULL, false };
    if (pattern == NULL || pattern->type != SEXPR_CONS) return mr;
    if (pattern->cons.car->type != SEXPR_SYMBOL) return mr;
    if (strcmp(pattern->cons.car->symbol, "OR-SUBGOALS") == 0) {
        mr.ok = true;
        mr.node = true;
        SExpr *rest = pattern->cons.cdr;
        if (rest && rest->type == SEXPR_CONS) {
            mr.bindings = list_prepend(NULL, rest->cons.car);
            if (rest->cons.cdr && rest->cons.cdr->type == SEXPR_CONS) {
                SExpr *children = rest->cons.cdr->cons.car;
                /* ?children이 nil이 아닌지 검사 */
                if (children != NULL && children->type != SEXPR_NIL) {
                    mr.bindings = list_prepend(mr.bindings, children);
                } else {
                    mr.ok = false;
                }
            }
        }
    }
    return mr;
}

static MatchResult match_parent_of(SExpr *pattern) {
    MatchResult mr = { false, NULL, false };
    if (pattern == NULL || pattern->type != SEXPR_CONS) return mr;
    if (pattern->cons.car->type != SEXPR_SYMBOL) return mr;
    if (strcmp(pattern->cons.car->symbol, "PARENT-OF") == 0) {
        mr.ok = true;
        mr.node = true;
    }
    return mr;
}

static MatchResult match_solution_of(SExpr *pattern) {
    MatchResult mr = { false, NULL, false };
    if (pattern == NULL || pattern->type != SEXPR_CONS) return mr;
    if (pattern->cons.car->type != SEXPR_SYMBOL) return mr;
    if (strcmp(pattern->cons.car->symbol, "SOLUTION-OF") == 0) {
        mr.ok = true;
        mr.node = true;
    }
    return mr;
}

static MatchResult match_or_subgoals_nil(SExpr *pattern) {
    MatchResult mr = { false, NULL, false };
    if (pattern == NULL || pattern->type != SEXPR_CONS) return mr;
    if (pattern->cons.car->type != SEXPR_SYMBOL) return mr;
    if (strcmp(pattern->cons.car->symbol, "OR-SUBGOALS") == 0) {
        SExpr *rest = pattern->cons.cdr;
        if (rest && rest->type == SEXPR_CONS) {
            SExpr *first_arg = rest->cons.car;
            /* (Integrate ?expr) 형태인지 확인 */
            if (first_arg && first_arg->type == SEXPR_CONS &&
                first_arg->cons.car->type == SEXPR_SYMBOL &&
                strcmp(first_arg->cons.car->symbol, "Integrate") == 0) {
                /* 두 번째 인자가 NIL인지 확인 */
                if (rest->cons.cdr && rest->cons.cdr->type == SEXPR_CONS) {
                    SExpr *second_arg = rest->cons.cdr->cons.car;
                    if (second_arg == NULL || second_arg->type == SEXPR_NIL) {
                        mr.ok = true;
                        mr.node = true;
                    }
                }
            }
        }
    }
    return mr;
}

static MatchResult match_solved(SExpr *pattern) {
    MatchResult mr = { false, NULL, false };
    if (pattern == NULL || pattern->type != SEXPR_CONS) return mr;
    if (pattern->cons.car->type != SEXPR_SYMBOL) return mr;
    if (strcmp(pattern->cons.car->symbol, "SOLVED") == 0) {
        mr.ok = true;
        mr.node = false;
    }
    return mr;
}

static MatchResult match_failed(SExpr *pattern) {
    MatchResult mr = { false, NULL, false };
    if (pattern == NULL || pattern->type != SEXPR_CONS) return mr;
    if (pattern->cons.car->type != SEXPR_SYMBOL) return mr;
    if (strcmp(pattern->cons.car->symbol, "FAILED") == 0) {
        mr.ok = true;
        mr.node = false;
    }
    return mr;
}

/* 모든 JSAINT 기본 규칙을 현재 JTRE에 등록 */
void register_jsaint_rules(JTRE *jtre) {
    JTRE *saved = current_jtre;
    current_jtre = jtre;

    /* AND-SUBGOALS 규칙 등록 */
    Dbclass *dbc;

    dbc = get_dbclass(sexpr_symbol("AND-SUBGOALS"));
    insert_rule(dbc, match_and_subgoals, rule_and_subgoals);

    /* OR-SUBGOALS 규칙 등록 (children이 있는 경우) */
    dbc = get_dbclass(sexpr_symbol("OR-SUBGOALS"));
    insert_rule(dbc, match_or_subgoals, rule_or_subgoals);

    /* PARENT-OF 규칙 등록 */
    dbc = get_dbclass(sexpr_symbol("PARENT-OF"));
    insert_rule(dbc, match_parent_of, rule_parent_of);

    /* SOLUTION-OF 규칙 등록 */
    dbc = get_dbclass(sexpr_symbol("SOLUTION-OF"));
    insert_rule(dbc, match_solution_of, rule_solution_of);

    /* OR-SUBGOALS nil 규칙 등록 */
    dbc = get_dbclass(sexpr_symbol("OR-SUBGOALS"));
    insert_rule(dbc, match_or_subgoals_nil, rule_or_subgoals_nil);

    /* SOLVED 규칙 등록 */
    dbc = get_dbclass(sexpr_symbol("SOLVED"));
    insert_rule(dbc, match_solved, rule_solved);

    /* FAILED 규칙 등록 */
    dbc = get_dbclass(sexpr_symbol("FAILED"));
    insert_rule(dbc, match_failed, rule_failed);

    current_jtre = saved;
}
