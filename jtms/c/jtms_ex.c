/* -*- C -*- */

/* Justification-based TMS 예제 (Examples) */
/* jtms-ex.lisp 에서 변환 */
/* Last edited 1/29/93, by KDF */

/* Copyright (c) 1993, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. */
/* All rights reserved. */

/* See the file legal.txt for a paragraph stating scope of permission */
/* and disclaimer of warranty.  The above copyright notice and that */
/* paragraph must be included in any separate copy of this file. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "jtms.h"
#include "jtms_ex.h"

/* ================================================================ */
/* 전역 변수 정의 (declare special 대응)                             */
/* ================================================================ */

/* ex1에서 사용하는 노드들 */
TmsNode *na = NULL, *nb = NULL, *nc = NULL, *nd = NULL;
TmsNode *ne = NULL, *nf = NULL, *ng = NULL;

/* ex2에서 사용하는 모순 노드 */
TmsNode *contra = NULL;

/* ex3에서 사용하는 노드들 */
TmsNode *assumption_a = NULL, *assumption_c = NULL, *assumption_e = NULL;
TmsNode *node_h = NULL, *node_g = NULL;
TmsNode *contradiction = NULL;

/* 현재 JTMS 포인터 (전역) */
JTMS *ex_jtms = NULL;

/* ================================================================ */
/* 헬퍼 함수                                                        */
/* ================================================================ */

/* datum으로 노드를 검색
 * (dolist (node (jtms-nodes jtms))
 *   (if (equal datum (tms-node-datum node)) (return node))) */
TmsNode *get_node(const char *datum, JTMS *jtms) {
    for (List *p = jtms->nodes; p != NULL; p = p->next) {
        TmsNode *node = (TmsNode *)p->data;
        if (strcmp(datum, node->datum) == 0) {
            return node;
        }
    }
    return NULL;
}

/* index 번호로 정당화를 검색
 * (dolist (just (jtms-justs jtms))
 *   (if (= num (just-index just)) (return just))) */
Just *get_justification(int num, JTMS *jtms) {
    for (List *p = jtms->justs; p != NULL; p = p->next) {
        Just *just = (Just *)p->data;
        if (just->index == num) {
            return just;
        }
    }
    return NULL;
}

/* ================================================================ */
/* ex1: 기본 JTMS 사용 예제                                          */
/* ================================================================ */

/* (defun ex1 ()
 *   (setq *jtms* (create-jtms "Simple Example" :debugging T)
 *         na (tms-create-node *jtms* 'a :assumptionp T)
 *         ...
 *   (justify-node 'j1 nf (list na nb))
 *   ...
 *   (enable-assumption na)
 *   ...) */
void ex1(void) {
    /* JTMS 생성: "Simple Example", 디버깅 활성화 */
    ex_jtms = create_jtms("Simple Example",
                          NULL,  /* node_string: 기본값 사용 */
                          1,     /* debugging: T */
                          1,     /* checking_contradictions */
                          NULL,  /* contradiction_handler: 기본값 */
                          NULL); /* enqueue_procedure */

    /* 노드 a~g를 가정(assumption)으로 생성 */
    na = tms_create_node(ex_jtms, "a", ASSUMPTION_TRUE, 0);
    nb = tms_create_node(ex_jtms, "b", ASSUMPTION_TRUE, 0);
    nc = tms_create_node(ex_jtms, "c", ASSUMPTION_TRUE, 0);
    nd = tms_create_node(ex_jtms, "d", ASSUMPTION_TRUE, 0);
    ne = tms_create_node(ex_jtms, "e", ASSUMPTION_TRUE, 0);
    nf = tms_create_node(ex_jtms, "f", ASSUMPTION_TRUE, 0);
    ng = tms_create_node(ex_jtms, "g", ASSUMPTION_TRUE, 0);

    /* 정당화 추가 */
    List *antes;

    /* j1: f <- a, b */
    antes = NULL;
    antes = list_prepend(antes, nb);
    antes = list_prepend(antes, na);
    justify_node("j1", nf, antes);

    /* j2: e <- b, c */
    antes = NULL;
    antes = list_prepend(antes, nc);
    antes = list_prepend(antes, nb);
    justify_node("j2", ne, antes);

    /* j3: g <- a, e */
    antes = NULL;
    antes = list_prepend(antes, ne);
    antes = list_prepend(antes, na);
    justify_node("j3", ng, antes);

    /* j4: g <- d, e */
    antes = NULL;
    antes = list_prepend(antes, ne);
    antes = list_prepend(antes, nd);
    justify_node("j4", ng, antes);

    /* 가정 활성화 */
    enable_assumption(na);
    enable_assumption(nb);
    enable_assumption(nc);
    enable_assumption(nd);

    printf("ex1 completed.\n");
    printf("  Node states after enabling assumptions a, b, c, d:\n");
    printf("    na(a): %s\n", in_node(na) ? "IN" : "OUT");
    printf("    nb(b): %s\n", in_node(nb) ? "IN" : "OUT");
    printf("    nc(c): %s\n", in_node(nc) ? "IN" : "OUT");
    printf("    nd(d): %s\n", in_node(nd) ? "IN" : "OUT");
    printf("    ne(e): %s\n", in_node(ne) ? "IN" : "OUT");
    printf("    nf(f): %s\n", in_node(nf) ? "IN" : "OUT");
    printf("    ng(g): %s\n", in_node(ng) ? "IN" : "OUT");
}

/* ================================================================ */
/* ex2: 모순 처리 테스트                                              */
/* ================================================================ */

/* ex1의 결과를 사용하여 모순(contradiction) 테스트
 * (defun ex2 ()
 *   (setq contra (tms-create-node *jtms* 'Loser :contradictoryp T))
 *   (justify-node 'j5 contra (list ne nf))) */
void ex2(void) {
    if (ex_jtms == NULL) {
        printf("Error: ex1()을 먼저 실행하세요.\n");
        return;
    }

    /* 모순 노드 생성 */
    contra = tms_create_node(ex_jtms, "Loser", 0, 1);

    /* j5: Loser <- e, f (모순을 일으키는 정당화) */
    List *antes = NULL;
    antes = list_prepend(antes, nf);
    antes = list_prepend(antes, ne);
    justify_node("j5", contra, antes);

    printf("ex2 completed.\n");
    printf("  Contradiction node 'Loser' created and justified by e, f.\n");
}

/* ================================================================ */
/* ex3: 다중 지지(multiple support) 예제                              */
/* ================================================================ */

/* (defun ex3 ()
 *   (setq *jtms* (create-jtms "Multiple support example")
 *         assumption-a (tms-create-node *jtms* 'A :assumptionp T)
 *         ...
 *   (justify-node 'R1 node-h (list assumption-c assumption-e))
 *   ...
 *   (justify-node 'R3 contradiction (list node-g))) */
void ex3(void) {
    /* 새로운 JTMS 생성 */
    ex_jtms = create_jtms("Multiple support example",
                          NULL,  /* node_string */
                          0,     /* debugging: NIL */
                          1,     /* checking_contradictions */
                          NULL,  /* contradiction_handler */
                          NULL); /* enqueue_procedure */

    /* 가정 노드 생성 */
    assumption_a = tms_create_node(ex_jtms, "A", ASSUMPTION_TRUE, 0);
    assumption_c = tms_create_node(ex_jtms, "C", ASSUMPTION_TRUE, 0);
    assumption_e = tms_create_node(ex_jtms, "E", ASSUMPTION_TRUE, 0);

    /* 일반 노드 생성 */
    node_h = tms_create_node(ex_jtms, "h", 0, 0);

    /* 가정 활성화 */
    enable_assumption(assumption_a);
    enable_assumption(assumption_c);
    enable_assumption(assumption_e);

    /* R1: h <- C, E */
    List *antes = NULL;
    antes = list_prepend(antes, assumption_e);
    antes = list_prepend(antes, assumption_c);
    justify_node("R1", node_h, antes);

    /* 추가 노드 생성 */
    node_g = tms_create_node(ex_jtms, "g", 0, 0);

    /* R2: g <- A, C */
    antes = NULL;
    antes = list_prepend(antes, assumption_c);
    antes = list_prepend(antes, assumption_a);
    justify_node("R2", node_g, antes);

    /* 모순 노드 생성 */
    contradiction = tms_create_node(ex_jtms, "CONTRADICTION", 0, 1);

    /* R3: CONTRADICTION <- g (모순을 일으키는 정당화) */
    antes = NULL;
    antes = list_prepend(antes, node_g);
    justify_node("R3", contradiction, antes);

    printf("ex3 completed.\n");
    printf("  Multiple support example with contradiction.\n");
    printf("  Node states:\n");
    printf("    A: %s\n", in_node(assumption_a) ? "IN" : "OUT");
    printf("    C: %s\n", in_node(assumption_c) ? "IN" : "OUT");
    printf("    E: %s\n", in_node(assumption_e) ? "IN" : "OUT");
    printf("    h: %s\n", in_node(node_h) ? "IN" : "OUT");
    printf("    g: %s\n", in_node(node_g) ? "IN" : "OUT");
}
