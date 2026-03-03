/* -*- C -*- */

/* JTRE 규칙 시스템 구현 */
/* jrules.lisp에서 변환 */

/* Copyright (c) 1989 --- 1992 Kenneth D. Forbus, Northwestern University, */
/* Johan de Kleer and Xerox Corporation. All rights reserved. */

#include "jrules.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ================================================================ */
/* variable? - 심볼이 패턴 변수인지 확인                              */
/* 변수는 '?'로 시작하는 심볼                                         */
/* ================================================================ */

int variable_p(const char *sym) {
    return (sym != NULL && sym[0] == '?');
}

/* ================================================================ */
/* 패턴에서 자유 변수 찾기                                            */
/* ================================================================ */

/* 내부 재귀 함수 */
static List *pattern_free_vars1(SExpr *pattern, List *vars, List *bound_vars) {
    if (pattern == NULL || pattern->type == SEXPR_NIL) {
        return vars;
    }
    if (pattern->type == SEXPR_SYMBOL && variable_p(pattern->symbol)) {
        /* 이미 변수 목록에 있거나 바운드 변수인지 확인 */
        for (List *p = vars; p != NULL; p = p->next) {
            if (strcmp((const char *)p->data, pattern->symbol) == 0) {
                return vars;
            }
        }
        for (List *p = bound_vars; p != NULL; p = p->next) {
            if (strcmp((const char *)p->data, pattern->symbol) == 0) {
                return vars;
            }
        }
        return list_prepend(vars, (void *)pattern->symbol);
    }
    if (pattern->type != SEXPR_CONS) {
        return vars;
    }
    /* 재귀: (car pattern)과 (cdr pattern) */
    vars = pattern_free_vars1(pattern->cons.car, vars, bound_vars);
    vars = pattern_free_vars1(pattern->cons.cdr, vars, bound_vars);
    return vars;
}

List *pattern_free_variables(SExpr *pattern) {
    return pattern_free_vars1(pattern, NULL, NULL);
}

/* ================================================================ */
/* scratchout - l1의 원소를 l2에서 제거 (비파괴적, 순서 보존)          */
/* ================================================================ */

List *scratchout(List *l1, List *l2) {
    List *result = list_copy(l2);
    for (List *p = l1; p != NULL; p = p->next) {
        /* l2에서 해당 원소를 제거 */
        List *prev = NULL;
        List *curr = result;
        while (curr != NULL) {
            if (curr->data == p->data) {
                if (prev == NULL) {
                    result = curr->next;
                } else {
                    prev->next = curr->next;
                }
                List *tmp = curr;
                curr = curr->next;
                free(tmp);
            } else {
                prev = curr;
                curr = curr->next;
            }
        }
    }
    return result;
}

/* ================================================================ */
/* 규칙을 dbclass에 등록                                              */
/* (defun insert-rule (dbclass matcher body &aux rule) ...)           */
/* ================================================================ */

void insert_rule(Dbclass *dbclass, RuleMatcherFn matcher, RuleBodyFn body) {
    JTRE *jtre = dbclass->jtre;
    JRule *rule = (JRule *)malloc(sizeof(JRule));
    rule->matcher = matcher;
    rule->body = body;
    rule->dbclass = dbclass;
    rule->id = ++(jtre->rule_counter);
    rule->jtre = jtre;

    /* 규칙을 dbclass의 규칙 리스트에 추가 */
    dbclass->rules = list_prepend(dbclass->rules, rule);

    /* 기존 사실들에 대해 규칙 시도 */
    for (List *p = dbclass->facts; p != NULL; p = p->next) {
        Datum *candidate = (Datum *)p->data;
        try_rule_on(rule, candidate);
    }
}

/* ================================================================ */
/* datum에 대해 모든 관련 규칙 시도                                    */
/* (defun try-rules (datum) ...)                                     */
/* ================================================================ */

void try_rules(Datum *datum) {
    Dbclass *dbc = datum->dbclass;
    for (List *p = dbc->rules; p != NULL; p = p->next) {
        JRule *rule = (JRule *)p->data;
        try_rule_on(rule, datum);
    }
}

/* ================================================================ */
/* 특정 규칙을 datum에 시도                                           */
/* (defun try-rule-on (rule datum) ...)                               */
/* ================================================================ */

void try_rule_on(JRule *rule, Datum *datum) {
    JTRE *jtre = datum->dbclass->jtre;
    MatchResult mr = rule->matcher(datum->lisp_form);

    if (mr.ok) {
        List *bindings = mr.bindings;
        if (mr.node) {
            /* 트리거 노드를 바인딩에 추가 */
            bindings = list_prepend(bindings, datum->tms_node);
        }
        /* (cons (rule-body rule) bindings) 를 큐에 추가 */
        /* 큐 항목: 첫 원소가 body 함수 포인터, 나머지가 인자 */
        List *queue_item = list_prepend(bindings, (void *)rule->body);
        enqueue(queue_item, jtre);
    }
}

/* ================================================================ */
/* 큐에 있는 규칙들을 모두 실행                                       */
/* (defun run-rules (&optional (*JTRE* *JTRE*)) ...)                  */
/* ================================================================ */

int run_rules(void) {
    JTRE *jtre = current_jtre;
    int counter = 0;

    while (1) {
        List *form = (List *)dequeue(jtre);
        if (form == NULL) break;

        /* form의 car가 body 함수 포인터, cdr가 인자 */
        RuleBodyFn body = (RuleBodyFn)form->data;
        List *args = form->next;
        body(args);
        counter++;

        /* form의 첫 노드만 해제 (args는 body에서 사용) */
        List *tmp = form;
        form = form->next;
        free(tmp);
    }

    if (jtre->debugging) {
        fprintf(stderr, "\n    %d rules run.", counter);
    }
    jtre->rules_run += counter;
    return counter;
}

/* ================================================================ */
/* 대기 중인 규칙이 있는지 확인                                       */
/* ================================================================ */

bool rules_waiting(JTRE *jtre) {
    return jtre->queue != NULL;
}

/* ================================================================ */
/* 큐에 항목 추가                                                     */
/* (defun enqueue (new j) (push new (jtre-queue j)))                  */
/* ================================================================ */

void enqueue(void *item, JTRE *jtre) {
    jtre->queue = list_prepend(jtre->queue, item);
}

/* ================================================================ */
/* 큐에서 항목 제거                                                   */
/* (defun dequeue (jtre) (pop (jtre-queue jtre)))                     */
/* ================================================================ */

void *dequeue(JTRE *jtre) {
    return list_pop(&(jtre->queue));
}

/* ================================================================ */
/* 규칙 표시                                                          */
/* ================================================================ */

/* (defun show-rules (&optional (*JTRE* *JTRE*) (stream *standard-output*)) */
void show_rules(FILE *stream) {
    JTRE *jtre = current_jtre;
    fprintf(stream, "\nThere are %d rules in %s:",
            jtre->rule_counter, jtre->title);
    fprintf(stream, "\n %s queued.",
            jtre->queue == NULL ? "None" : "Some");

    /* 모든 dbclass의 규칙 표시 */
    /* map_dbclass를 통해 표시 */
}

/* (defun print-rule (rule &optional (stream *standard-output*)) */
void print_rule(JRule *rule, FILE *stream) {
    fprintf(stream, "\n <Rule %d>: matcher=%p, body=%p",
            rule->id, (void *)rule->matcher, (void *)rule->body);
}

/* ================================================================ */
/* 규칙 조회                                                          */
/* ================================================================ */

/* get_rule은 jdata.h에 선언되어 있음 */
/* map_dbclass를 통해 모든 dbclass를 순회하며 규칙 ID 검색 */

/* 내부 콜백 구조체 */
typedef struct {
    int target_id;
    JRule *found;
} GetRuleCtx;

static void get_rule_callback(Dbclass *dbclass, void *ctx) {
    GetRuleCtx *gctx = (GetRuleCtx *)ctx;
    if (gctx->found) return;
    for (List *p = dbclass->rules; p != NULL; p = p->next) {
        JRule *rule = (JRule *)p->data;
        if (rule->id == gctx->target_id) {
            gctx->found = rule;
            return;
        }
    }
}

JRule *get_rule(int num) {
    GetRuleCtx ctx = { num, NULL };
    map_dbclass(get_rule_callback, &ctx);
    return ctx.found;
}

/* ================================================================ */
/* 규칙 확장 테스트 (디버깅용)                                        */
/* ================================================================ */

void test_rule_expansion(void) {
    printf("\ntest_rule_expansion: In C, rules are registered "
           "at runtime via insert_rule().\n"
           "See the original jrules.lisp for the macro-based approach.\n");
}
