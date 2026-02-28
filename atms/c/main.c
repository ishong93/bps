/* -*- C -*- */

/* ATMS C - Main entry point */
/* Equivalent to atre.lisp loader plus test dispatcher */

#include "atms.h"
#include "atre.h"
#include "unify.h"
#include "planner.h"
#include <stdio.h>
#include <string.h>

/* Forward declarations from atest.c */
void atms_test1(void);
void atms_test2(void);
void atms_test3(void);
void step_1(void);

/* Forward declarations from atret.c */
void atre_test1(bool debugging);
void atre_test2(bool debugging);
void atre_test3(bool debugging);
void atre_test4(bool debugging);
void atre_test5(bool debugging);

static void print_usage(void) {
    printf("ATMS C - Assumption-based Truth Maintenance System\n"
           "Based on 'Building Problem Solvers' by Forbus & de Kleer\n\n"
           "Usage: atms_test <command>\n\n"
           "Commands:\n"
           "  atms1          ATMS test 1: basic justification network\n"
           "  atms2          ATMS test 2: simpler justification\n"
           "  atms3          ATMS test 3: nogood declaration\n"
           "  step1          de Kleer example\n"
           "  atre1          ATRE test 1: INTERN rules with assume!\n"
           "  atre2          ATRE test 2: INTERN rules with assert!\n"
           "  atre3          ATRE test 3: IN rules\n"
           "  atre4          ATRE test 4: IMPLIED-BY rules\n"
           "  atre5          ATRE test 5: contradiction rules\n"
           "  blocks         Blocks world planning\n"
           "  all            Run all tests\n");
}

static void run_blocks_test(void) {
    printf("\n=== Blocks World Planning ===\n");
    List *blocks = list_new();
    list_push(blocks, sexpr_symbol("A"));
    list_push(blocks, sexpr_symbol("B"));
    list_push(blocks, sexpr_symbol("C"));
    Plnpr *plnpr = build_blocks_problem("Sussman Anomaly",
                                         blocks, true);
    in_plnpr(plnpr);
    envision();
    show_envisionment(stdout);
    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        print_usage();
        return 0;
    }

    const char *cmd = argv[1];

    if (strcmp(cmd, "atms1") == 0) {
        printf("=== ATMS Test 1 ===\n");
        atms_test1();
    }
    else if (strcmp(cmd, "atms2") == 0) {
        printf("=== ATMS Tests 1 + 2 ===\n");
        atms_test1();
        atms_test2();
    }
    else if (strcmp(cmd, "atms3") == 0) {
        printf("=== ATMS Tests 1 + 3 ===\n");
        atms_test1();
        atms_test3();
    }
    else if (strcmp(cmd, "step1") == 0) {
        printf("=== Step-1 (de Kleer example) ===\n");
        step_1();
    }
    else if (strcmp(cmd, "atre1") == 0) {
        atre_test1(true);
    }
    else if (strcmp(cmd, "atre2") == 0) {
        atre_test2(true);
    }
    else if (strcmp(cmd, "atre3") == 0) {
        atre_test3(true);
    }
    else if (strcmp(cmd, "atre4") == 0) {
        atre_test4(true);
    }
    else if (strcmp(cmd, "atre5") == 0) {
        atre_test5(true);
    }
    else if (strcmp(cmd, "blocks") == 0) {
        run_blocks_test();
    }
    else if (strcmp(cmd, "all") == 0) {
        printf("=== ATMS Test 1 ===\n");
        atms_test1();
        printf("\n\n=== Step-1 (de Kleer) ===\n");
        step_1();
        printf("\n\n");
        atre_test1(true);
        printf("\n\n");
        atre_test2(true);
        printf("\n\n");
        atre_test3(true);
        printf("\n\n");
        atre_test5(true);
    }
    else {
        fprintf(stderr, "Unknown command: %s\n", cmd);
        print_usage();
        return 1;
    }

    printf("\n");
    return 0;
}
