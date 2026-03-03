/* -*- C -*- */

/* N-Queens 퍼즐: 의존성 기반 탐색을 사용하는 JTRE 예제 */
/* jqueens.lisp에서 변환 */

/* Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. All rights reserved. */

#ifndef JQUEENS_H
#define JQUEENS_H

#include "jtms.h"
#include "jtre.h"
#include "jdata.h"
#include <setjmp.h>

/* ================================================================ */
/* 퀸 배치 구조체                                                    */
/* ================================================================ */

/* (Queen column row) 형태의 배치를 나타내는 구조체 */
typedef struct QueenPlacement {
    int column;
    int row;
} QueenPlacement;

/* ================================================================ */
/* 선택 집합: 각 열에 가능한 퀸 배치 목록                            */
/* ================================================================ */

typedef struct QueenChoiceSet {
    int              n_choices;
    QueenPlacement  *choices;     /* 배열: n_choices 개 */
} QueenChoiceSet;

/* ================================================================ */
/* try-in-context 결과 구조체                                        */
/* ================================================================ */

typedef struct TryResult {
    bool   nogood;               /* 모순 발견 여부 */
    List  *assumptions;          /* 관련 가정 목록 (SExpr*) */
} TryResult;

/* ================================================================ */
/* 전역 변수                                                         */
/* ================================================================ */

/* 통계 */
extern int   jqueens_n_assumptions;     /* 시도한 가정 수 */
extern List *jqueens_placements;        /* 찾은 해 목록 (List of List<SExpr*>) */

/* setjmp/longjmp 용 모순 핸들링 */
extern jmp_buf jqueens_try_jmp;
extern bool    jqueens_contradiction_found;
extern List   *jqueens_contra_asns;     /* 모순 시 가정 목록 */

/* ================================================================ */
/* N-Queens API                                                      */
/* ================================================================ */

/* 퍼즐 실행: n개의 퀸 배치, 해의 개수 반환 */
int n_queens(int n, bool debugging);

/* n부터 to까지 테스트 */
void test_queens(int from, int to);

/* 퍼즐 초기 설정: JTRE 생성, 규칙 로드 */
void setup_queens_puzzle(int n, bool debugging);

/* 선택 집합 생성: 각 열에 가능한 퀸 배치 목록 */
QueenChoiceSet *make_queens_choice_sets(int n, int *n_sets_out);

/* 선택 집합 해제 */
void free_queens_choice_sets(QueenChoiceSet *sets, int n_sets);

/* 퍼즐 탐색: 모든 해를 찾는다 */
void solve_queens_puzzle(QueenChoiceSet *sets, int n_sets,
                         int current_set);

/* ================================================================ */
/* try-in-context: 가정을 시도하고 모순 처리                         */
/* ================================================================ */

/* catch/throw 대신 setjmp/longjmp 사용 */
TryResult try_in_context(SExpr *assumption, QueenChoiceSet *sets,
                         int n_sets, int next_set, Jtre *jtre);

/* 모순 핸들러: with-contradiction-handler 콜백 */
void try_contradiction_handler(Jtms *jtms, List *contras,
                               SExpr *assumption, SExpr *marker,
                               Jtre *jtre);

/* ================================================================ */
/* 유틸리티                                                          */
/* ================================================================ */

/* 두 퀸이 서로 공격하지 않는지 검사 */
bool queens_okay(int x1, int y1, int x2, int y2);

/* 현재 IN 상태인 퀸 배치를 수집하여 해로 기록 */
void gather_queens_solution(void);

/* 해 출력 */
void show_queens_solution(List *solution, int n);

/* ================================================================ */
/* SExpr 헬퍼: (Queen col row) 형태 생성/분해                        */
/* ================================================================ */

/* (Queen column row) S-expression 생성 */
SExpr *make_queen_sexpr(int column, int row);

/* (not (Queen column row)) S-expression 생성 */
SExpr *make_not_queen_sexpr(int column, int row);

/* (Queen ?c ?r) 패턴 S-expression 생성 */
SExpr *make_queen_pattern(void);

/* QueenPlacement에서 SExpr 변환 */
SExpr *queen_placement_to_sexpr(QueenPlacement *qp);

#endif /* JQUEENS_H */
