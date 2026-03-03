/* -*- C -*- */

/* JTMS/JTRE 모듈 로더 구현 */
/* jtre.lisp에서 변환 */

/* Copyright (c) 1992, Kenneth D. Forbus, Northwestern University, */
/* Johan de Kleer and Xerox Corporation. All rights reserved. */

#include "jtms_loader.h"
#include <stdio.h>
#include <stdlib.h>

/* ================================================================ */
/* JTRE 모듈 목록 (문자열)                                            */
/* ================================================================ */

/* JTRE 기본 모듈 */
static const char *jtre_modules[] = {
    "jtms",     /* JTMS */
    "jtre",     /* 인터페이스 */
    "jdata",    /* 데이터베이스 */
    "jrules",   /* 규칙 시스템 */
    "funify",   /* 오픈 코딩 유니피케이션 */
    NULL
};

/* N-Queens 모듈 */
static const char *jqueens_modules[] = {
    "jqueens",  /* JTRE 버전 N-Queens 퍼즐 */
    "jqrule",   /* 모순 감지 규칙 */
    NULL
};

/* JSAINT 모듈 */
static const char *jsaint_modules[] = {
    "jsaint",   /* JSAINT 메인 프로그램 */
    "match",    /* 수학 지향 패턴 매처 */
    "simplify", /* 대수 단순화기 */
    "jsrules",  /* 기본 규칙 */
    "jsops",    /* 적분 연산자 라이브러리 */
    NULL
};

/* ================================================================ */
/* 초기화 함수                                                        */
/* ================================================================ */

/* JTRE 시스템 전체 초기화
 * (defun compile-jqueens ()
 *   (unless (and (boundp '*jtre*) (not (null *jtre*)))
 *     (in-jtre (create-jtre "Dummy")))
 *   (compile-load-files *jqueens-files* *jtre-path*))
 */
void init_jtre_system(void) {
    printf("JTRE system modules:\n");
    for (const char **mod = jtre_modules; *mod != NULL; mod++) {
        printf("  %s\n", *mod);
    }
    printf("System initialized.\n");
}

/* N-Queens 시스템 초기화 */
void init_jqueens(void) {
    /* JTRE가 없으면 더미 생성 */
    if (current_jtre == NULL) {
        in_jtre(create_jtre("Dummy", 0));
    }

    printf("N-Queens modules:\n");
    for (const char **mod = jqueens_modules; *mod != NULL; mod++) {
        printf("  %s\n", *mod);
    }
    printf("N-Queens initialized.\n");
}

/* JSAINT 시스템 초기화
 * (defun compile-jsaint ()
 *   (compile-load-files '("jsaint" "match" "simplify") *jtre-path*)
 *   (unless (and (boundp '*jsaint*) (not (null *jsaint*)))
 *     (create-jsaint "Dummy" nil))
 *   (compile-load-files '("jsrules" "jsops") *jtre-path*))
 */
void init_jsaint(void) {
    /* 단순화기 초기화 */
    simplify_init();

    /* 적분 연산자 초기화 */
    init_integration_operators();

    printf("JSAINT modules:\n");
    for (const char **mod = jsaint_modules; *mod != NULL; mod++) {
        printf("  %s\n", *mod);
    }
    printf("JSAINT initialized.\n");
}

/* ================================================================ */
/* 정리 함수                                                          */
/* ================================================================ */

void cleanup_jtre_system(void) {
    simplify_cleanup();
    free_integration_operators();
    printf("JTRE system cleaned up.\n");
}
