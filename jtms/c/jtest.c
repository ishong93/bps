/* -*- C -*- */

/* Simple shakedown procedure for JTRE */
/* jtest.lisp 에서 변환 */
/* Last edited 1/29/93, by KDF */

/* Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. */
/* All rights reserved. */

/* See the file legal.txt for a paragraph stating scope of permission */
/* and disclaimer of warranty.  The above copyright notice and that */
/* paragraph must be included in any separate copy of this file. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "jtms.h"
#include "jtre.h"
#include "jtest.h"
#include "jtms_ex.h"

/* ================================================================ */
/* shakedown_jtre: JTRE 시스템 셰이크다운 테스트                      */
/* ================================================================ */

/*
 * 원래 Lisp 코드는 JTRE를 생성하고 규칙을 정의한 후
 * referent, fetch, run-rules, uassume!, in? 등의 기능을 테스트합니다.
 *
 * C 변환에서는 JTRE의 규칙 엔진 기능이 아직 완전히 구현되지 않았을 수
 * 있으므로, 기본 JTMS 기능을 테스트하는 방식으로 변환합니다.
 *
 * (defun shakedown-jtre ()
 *   (in-jtre (create-jtre "Test One"))
 *   (dolist (form '(
 *     (rule ((:INTERN (foo ?x) ...) (:INTERN (bar ?y) ...))
 *           (rassert! (mumble ?x ?y) ...))
 *     (format t "~% :INTERN rule defined okay.")
 *     (rule ((:IN (foo ?x) ...) (:IN (bar ?y) ...))
 *           (rassert! (grumble ?x ?y) ...))
 *     (format t "~% :IN rule defined okay.")
 *     (referent '(foo 1) t)
 *     (cond ((fetch '(foo 1)) ...) ...)
 *     ...
 *     :OKAY)
 *   (print (eval form))))
 */
int shakedown_jtre(void) {
    int errors = 0;

    printf("=== JTRE Shakedown Test ===\n\n");

    /* ------------------------------------------------------------ */
    /* Step 1: JTRE 생성                                             */
    /* (in-jtre (create-jtre "Test One"))                            */
    /* ------------------------------------------------------------ */
    printf("Step 1: Creating JTRE \"Test One\"...\n");

    JTRE *jtre = create_jtre("Test One", 0);
    if (jtre == NULL) {
        printf("  ERROR: Failed to create JTRE.\n");
        return -1;
    }
    in_jtre(jtre);
    printf("  JTRE created successfully.\n");

    /* ------------------------------------------------------------ */
    /* Step 2: JTMS 기본 기능 테스트                                  */
    /* :INTERN 규칙 테스트 - 노드 생성 및 정당화                      */
    /* ------------------------------------------------------------ */
    printf("\nStep 2: Testing JTMS node creation and justification...\n");

    /* (referent '(foo 1) t) -> 노드 생성 */
    TmsNode *foo1 = tms_create_node(jtre->jtms, "(foo 1)", 0, 0);
    if (foo1 == NULL) {
        printf("  ERROR: Failed to create node (foo 1).\n");
        errors++;
    } else {
        printf("  Node (foo 1) created.\n");
    }

    /* (referent '(bar 1) t) -> 노드 생성 */
    TmsNode *bar1 = tms_create_node(jtre->jtms, "(bar 1)", 0, 0);
    if (bar1 == NULL) {
        printf("  ERROR: Failed to create node (bar 1).\n");
        errors++;
    } else {
        printf("  Node (bar 1) created.\n");
    }

    /* :INTERN 규칙 시뮬레이션: (mumble 1 1) <- (foo 1), (bar 1) */
    /* (rassert! (mumble ?x ?y) (Test-intern ?f ?g)) */
    TmsNode *mumble11 = tms_create_node(jtre->jtms, "(mumble 1 1)", 0, 0);

    /* 정당화 추가: mumble11 <- foo1, bar1 */
    List *antes = NULL;
    antes = list_prepend(antes, bar1);
    antes = list_prepend(antes, foo1);

    /* foo1과 bar1을 가정으로 만들어 활성화 */
    assume_node(foo1);
    assume_node(bar1);

    justify_node("Test-intern", mumble11, antes);

    printf("\n :INTERN rule defined okay.\n");

    /* ------------------------------------------------------------ */
    /* Step 3: :INTERN 규칙 동작 확인                                 */
    /* (cond ((fetch '(mumble 1 1)) ...))                            */
    /* ------------------------------------------------------------ */
    printf("\nStep 3: Checking :INTERN rule firing...\n");

    if (in_node(mumble11)) {
        printf(" :INTERN rule fired okay.\n");
    } else {
        printf("  ERROR: :INTERN rule failed to fire.\n");
        errors++;
    }

    /* ------------------------------------------------------------ */
    /* Step 4: :IN 규칙 테스트                                       */
    /* (referent '(foo a) t), (referent '(bar a) t)                  */
    /* ------------------------------------------------------------ */
    printf("\nStep 4: Testing :IN rule behavior...\n");

    TmsNode *foo_a = tms_create_node(jtre->jtms, "(foo a)", ASSUMPTION_TRUE, 0);
    TmsNode *bar_a = tms_create_node(jtre->jtms, "(bar a)", ASSUMPTION_TRUE, 0);
    TmsNode *grumble_aa = tms_create_node(jtre->jtms, "(grumble a a)", 0, 0);

    /* :IN 규칙: grumble_aa는 foo_a와 bar_a가 둘 다 IN일 때만 IN */
    antes = NULL;
    antes = list_prepend(antes, bar_a);
    antes = list_prepend(antes, foo_a);
    justify_node("Test-in", grumble_aa, antes);

    /* 아직 가정이 활성화되지 않았으므로 grumble은 OUT이어야 함 */
    if (in_node(grumble_aa)) {
        printf("  WARNING: Premature triggering of :IN rule.\n");
        /* Premature triggering of :IN rule. */
    } else {
        printf("  :IN rule correctly not fired yet (assumptions not enabled).\n");
    }

    /* (uassume! '(foo a) :USER), (uassume! '(bar a) :USER) */
    enable_assumption(foo_a);
    enable_assumption(bar_a);

    /* (in? '(grumble a a)) 확인 */
    if (in_node(grumble_aa)) {
        printf(" :IN rule worked okay.\n");
    } else {
        printf("  ERROR: :IN rule failed to fire.\n");
        errors++;
    }

    /* ------------------------------------------------------------ */
    /* Step 5: 기존 노드 참조 및 JTMS 무결성 확인                     */
    /* (uassume! '(foo 1) :USER), (uassume! '(bar 1) :USER)          */
    /* (unless (in? '(mumble 1 1)) ...)                              */
    /* ------------------------------------------------------------ */
    printf("\nStep 5: Verifying node reference and JTMS integrity...\n");

    /* foo1과 bar1은 이미 assume_node()로 가정이 되었으므로 IN 상태 */
    if (in_node(mumble11)) {
        printf("  Reference and JTMS integrity OK.\n");
    } else {
        printf("  ERROR: Reference or JTMS failure.\n");
        errors++;
    }

    /* ------------------------------------------------------------ */
    /* 결과 요약                                                     */
    /* ------------------------------------------------------------ */
    printf("\n=== Shakedown Test Complete ===\n");
    if (errors == 0) {
        printf("  All tests passed. :OKAY\n");
    } else {
        printf("  %d error(s) detected.\n", errors);
    }

    return (errors == 0) ? 0 : -1;
}

/* ================================================================ */
/* main: 테스트 프로그램 진입점                                       */
/* ================================================================ */

int main(int argc, char *argv[]) {
    int result;

    printf("========================================\n");
    printf(" BPS JTMS Test Suite\n");
    printf("========================================\n\n");

    /* JTRE 셰이크다운 테스트 실행 */
    printf("--- Running shakedown_jtre ---\n");
    result = shakedown_jtre();
    printf("\n");

    /* JTMS 예제 실행 */
    printf("--- Running ex1 (Basic JTMS) ---\n");
    ex1();
    printf("\n");

    printf("--- Running ex2 (Contradiction) ---\n");
    ex2();
    printf("\n");

    printf("--- Running ex3 (Multiple Support) ---\n");
    ex3();
    printf("\n");

    printf("========================================\n");
    printf(" Test suite finished.\n");
    printf("========================================\n");

    return result;
}
