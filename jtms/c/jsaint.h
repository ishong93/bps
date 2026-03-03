/* -*- C -*- */

/* JSAINT: A rational reconstruction of Slagel's SAINT program */
/* Converted from jsaint.lisp */
/* Last edited 1/29/92, by KDF */

/* Copyright (c) 1991 -- 1992, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, Xerox Corporation */
/* All rights reserved. */

/* See the file legal.txt for a paragraph stating scope of permission */
/* and disclaimer of warranty.  The above copyright notice and that */
/* paragraph must be included in any separate copy of this file. */

/*
 * JSAINT 개요
 *
 * JSAINT는 Slagle의 SAINT 기호 적분 프로그램을 합리적으로 재구성한 것입니다.
 * JTMS 기반 진리 유지 시스템 위에서 AND/OR 그래프 탐색을 수행하여
 * 기호 적분 문제를 해결합니다.
 *
 * 주요 개념:
 * - 아젠다(Agenda): 우선순위 큐로 관리되는 하위 문제들의 리스트
 * - 문제(Problem): 적분 식 (예: (Integrate (Integral x x)))
 * - 연산자(Operator): 적분 규칙 (예: 합의 적분, 상수의 적분)
 * - AND/OR 그래프: 하위 문제들의 분해 구조
 *
 * 사용 예:
 * 1. JSAINT 생성: Jsaint* js = create_jsaint("test", problem, 0, 20);
 * 2. 문제 풀기: SolveResult r = solve_integral(problem, "test", 0, 20);
 * 3. 결과 설명: explain_result(js);
 */

#ifndef JSAINT_H
#define JSAINT_H

#include "jtms.h"
#include "jtre.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

/* ================================================================ */
/* Forward declarations                                             */
/* ================================================================ */

typedef struct Jsaint Jsaint;
typedef struct AgendaEntry AgendaEntry;

/* ================================================================ */
/* 풀이 결과 열거형                                                 */
/* ================================================================ */

typedef enum {
    SOLVE_NOT_YET = 0,       /* 아직 풀리지 않음 */
    SOLVE_SOLVED,            /* 풀이 완료 */
    SOLVE_FAILED_PROBLEM,    /* 원래 문제가 실패로 표시됨 */
    SOLVE_FAILED_EMPTY,      /* 아젠다가 비어서 실패 */
    SOLVE_TIME_OUT           /* 자원 한계 초과 */
} SolveResult;

/* ================================================================ */
/* 아젠다 항목 (우선순위 큐 원소)                                    */
/* ================================================================ */

/* 큐 항목: (난이도, 하위 문제) */
/* 난이도가 낮을수록 우선순위가 높음 */
struct AgendaEntry {
    int difficulty;          /* 난이도 추정값 */
    const char *subproblem;  /* 하위 문제 S-expression 문자열 */
};

/* ================================================================ */
/* Jsaint 구조체                                                    */
/* ================================================================ */

/*
 * Jsaint 구조체
 * title: 인쇄용 이름
 * jtre: 연결된 JTRE
 * agenda: 큐에 들어간 하위 문제들의 리스트 (AgendaEntry*의 리스트)
 * problem: 이것이 풀리면 완료
 * solution: 캐시된 답
 * solution_result: 풀이 결과 상태
 * n_subproblems: 통계
 * max_tasks: 자원 한계
 * debugging: 디버깅 플래그
 */
struct Jsaint {
    const char  *title;          /* 인쇄용 이름 */
    JTRE        *jtre;           /* 연결된 JTRE */
    List        *agenda;         /* AgendaEntry*의 정렬 리스트 */
    const char  *problem;        /* 원래 문제 (S-expression 문자열) */
    const char  *solution;       /* 캐시된 답 (NULL이면 미해결) */
    SolveResult  solution_result; /* 풀이 결과 상태 */
    int          n_subproblems;  /* 생성된 하위 문제 수 */
    int          max_tasks;      /* 자원 한계 */
    int          debugging;      /* 디버깅 플래그 */
};

/* ================================================================ */
/* 전역 JSAINT 포인터                                               */
/* ================================================================ */

extern Jsaint *current_jsaint;

/* ================================================================ */
/* 디버깅 매크로                                                    */
/* ================================================================ */

/* JSAINT의 debugging 플래그가 켜져 있을 때만 메시지 출력 */
#define DEBUGGING_JSAINT(js, fmt, ...) \
    do { if ((js)->debugging) fprintf(stderr, fmt, ##__VA_ARGS__); } while(0)

/* ================================================================ */
/* JSAINT 생성 및 관리                                              */
/* ================================================================ */

/* 새 JSAINT 인스턴스 생성 */
Jsaint *create_jsaint(const char *title, const char *problem,
                      int debugging, int max_tasks);

/* 현재 JSAINT 설정 변경 */
/* -1을 전달하면 해당 항목을 변경하지 않음 */
void change_jsaint(Jsaint *js, int debugging, const char *problem,
                   int max_tasks);

/* 현재 사용 중인 JSAINT를 설정 */
void use_jsaint(Jsaint *js);

/* ================================================================ */
/* 사용자 진입점                                                    */
/* ================================================================ */

/* 적분 문제를 풀기 */
/* integral: 적분 식 (S-expression 문자열) */
/* title: 인쇄용 이름 */
/* debugging: 디버깅 플래그 */
/* max_tasks: 최대 작업 수 */
/* 반환: 풀이 결과 */
SolveResult solve_integral(const char *integral, const char *title,
                           int debugging, int max_tasks);

/* 풀이 결과 설명 출력 */
void explain_result(Jsaint *js);

/* ================================================================ */
/* 기본 알고리즘                                                    */
/* ================================================================ */

/* JSAINT 실행: 아젠다에서 하위 문제를 꺼내 처리 */
/* 반환: 풀이 결과 */
SolveResult run_jsaint(Jsaint *js);

/* 하위 문제 처리 */
void process_subproblem(const char *item, Jsaint *js);

/* 하위 문제 열기 (expanded, open 단언) */
void open_subproblem(const char *item, Jsaint *js);

/* ================================================================ */
/* 큐 관리                                                          */
/* ================================================================ */

/* 문제를 아젠다에 추가 (우선순위 큐에 삽입) */
/* problem: 하위 문제 S-expression 문자열 */
/* parent: 부모 문제 (NULL 가능) */
void queue_problem(const char *problem, const char *parent, Jsaint *js);

/* 난이도 추정 */
int estimate_difficulty(const char *problem);

/* S-expression 문자열에서 심볼 수 세기 */
int count_symbols(const char *pr);

/* S-expression 문자열에서 최대 깊이 계산 */
int max_depth(const char *pr);

/* ================================================================ */
/* 보조 루틴                                                        */
/* ================================================================ */

/* 문제의 풀이를 검색 */
/* 반환: 풀이 S-expression 문자열 또는 NULL */
const char *fetch_solution(const char *problem, Jsaint *js);

/* JSAINT 모순 처리 핸들러 */
void jsaint_contradiction_handler(JTMS *jtms, List *contradictions);

/* ================================================================ */
/* 질의/표시                                                        */
/* ================================================================ */

/* 문제의 상세 정보 출력 */
void show_problem(const char *pr, Jsaint *js);

/* AND/OR 그래프의 텍스트 표시 */
void show_ao_graph(Jsaint *js);

/* AND/OR 깊이 테이블 갱신 (내부용) */
/* depth_table: (problem, depth) 쌍의 리스트 */
List *update_ao_depth_table(const char *now, int depth,
                            List *depths, List *path);

/* 특정 문제의 자식 문제들 반환 */
List *get_children(const char *gp, Jsaint *js);

/* 모든 확장된 문제들 반환 */
List *get_problems(Jsaint *js);

/* ================================================================ */
/* 디버깅 편의 함수                                                 */
/* ================================================================ */

/* 문제를 디버깅 모드로 풀기 */
SolveResult try_jsaint(const char *problem, const char *title);

/* 현재 JSAINT에서 패턴으로 검색 */
List *jfetch(const char *pattern);

/* ================================================================ */
/* 테스트 문제들                                                    */
/* ================================================================ */

extern const char *problem1;
extern const char *problem2;
extern const char *problem3;
extern const char *problem4;

/* ================================================================ */
/* 아젠다 항목 생성/해제                                            */
/* ================================================================ */

AgendaEntry *agenda_entry_new(int difficulty, const char *subproblem);
void agenda_entry_free(AgendaEntry *entry);

/* 아젠다에 항목을 난이도 순으로 삽입 (정렬된 삽입) */
List *agenda_insert_sorted(List *agenda, AgendaEntry *entry);

#endif /* JSAINT_H */
