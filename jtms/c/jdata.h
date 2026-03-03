/* -*- C -*- */

/* Database for Tiny Rule Engine using JTMS */
/* Converted from jdata.lisp */

/* Copyright (c) 1989, 1990, 1991 Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer and the Xerox Corporation. All rights reserved. */

/*
 * JTRE 데이터베이스 구조 및 내용
 *
 * jdata는 JTRE의 데이터베이스를 관리합니다.
 * 사실(fact)의 저장, 검색, assert, assume, retract 등을 담당합니다.
 *
 * 주요 구조:
 * - Dbclass: 기호별로 사실과 규칙을 분류하는 클래스
 *   - name: 대응하는 기호
 *   - jtre: 소속 JTRE
 *   - facts: 연관된 사실 리스트
 *   - rules: 연관된 규칙 리스트
 *
 * - Datum: 데이터베이스에 저장된 하나의 사실
 *   - id: 쉬운 조회를 위한 고유 ID
 *   - lisp_form: 패턴 매칭을 위한 표현식
 *   - tms_node: TMS 노드 포인터
 *   - dbclass: 대응하는 패턴의 Dbclass
 *   - assumption_reason: NULL이 아니면 가정의 이유를 나타냄
 *   - plist: 로컬 속성 리스트
 *
 * - JRule: JTRE 규칙
 *   - id: 규칙 고유 ID
 *   - jtre: 소속 JTRE
 *   - dbclass: 연관된 패턴의 Dbclass
 *   - matcher: 매칭을 수행하는 함수 포인터
 *   - body: 실제 작업을 수행하는 함수 포인터
 */

#ifndef JDATA_H
#define JDATA_H

#include "jtre.h"

/* ================================================================ */
/* Dbclass structure                                                */
/* 기호별로 사실과 규칙을 분류하는 클래스                           */
/* ================================================================ */

/* Dbclass 구조체 */
struct Dbclass {
    char  *name;       /* 대응하는 기호 */
    JTRE  *jtre;       /* 소속 JTRE */
    List  *facts;      /* 연관된 사실 리스트 (List of Datum*) */
    List  *rules;      /* 연관된 규칙 리스트 (List of JRule*) */
};

/* ================================================================ */
/* Datum structure                                                  */
/* 데이터베이스에 저장된 하나의 사실                                */
/* ================================================================ */

/* Datum 구조체 */
struct Datum {
    int       id;                /* 고유 ID */
    SExpr    *lisp_form;         /* 패턴 매칭을 위한 표현식 */
    TmsNode  *tms_node;          /* TMS 노드 포인터 */
    Dbclass  *dbclass;           /* 대응하는 패턴의 Dbclass */
    char     *assumption_reason; /* NULL이 아니면 가정의 이유 (informant) */
    List     *plist;             /* 로컬 속성 리스트 */
};

/* ================================================================ */
/* Rule match result                                                */
/* ================================================================ */

typedef struct {
    bool  ok;          /* 매칭 성공 여부 */
    List *bindings;    /* 바인딩 리스트 (List of void*) */
    bool  node;        /* true이면 트리거 노드를 바인딩에 추가 */
} MatchResult;

/* Rule matcher function pointer */
/* 매치 함수 포인터: datum의 lisp_form을 받아 매칭 결과 반환 */
typedef MatchResult (*RuleMatcherFn)(SExpr *pattern);

/* Rule body function pointer */
/* 바디 함수 포인터: 바인딩 리스트를 받아 규칙 본문 실행 */
typedef void (*RuleBodyFn)(List *args);

/* ================================================================ */
/* JRule structure                                                  */
/* JTRE 규칙 구조체                                                 */
/* ================================================================ */

struct JRule {
    int           id;       /* 규칙 고유 ID */
    JTRE         *jtre;     /* 소속 JTRE */
    Dbclass      *dbclass;  /* 연관된 패턴의 Dbclass */
    RuleMatcherFn matcher;  /* 매칭을 수행하는 함수 포인터 */
    RuleBodyFn    body;     /* 실제 작업을 수행하는 함수 포인터 */
};

/* ================================================================ */
/* Making statements (사실 선언)                                    */
/* ================================================================ */

/* (defun assert! (fact just &optional (*JTRE* *JTRE*) ...))
 * 사실을 assert (정당화 부여) */
Datum *assert_fact(SExpr *fact, SExpr *just);

/* (defun quiet-assert! (fact just &optional (*JTRE* *JTRE*)))
 * 모순 검사 없이 assert */
Datum *quiet_assert(SExpr *fact, SExpr *just);

/* (defun assume! (fact reason &optional (*JTRE* *JTRE*) ...))
 * 사실을 가정(assume)으로 선언 */
Datum *assume_fact(SExpr *fact, const char *reason);

/* (defun already-assumed? (fact))
 * 이미 가정된 사실인지 확인 */
bool already_assumed(SExpr *fact);

/* ================================================================ */
/* Retraction (철회)                                                */
/* ================================================================ */

/* (defun retract! (fact &optional (just 'user) (quiet? nil) ...))
 * 가정을 철회 */
TmsNode *retract_fact(SExpr *fact, const char *just, bool quiet);

/* (defun contradiction (fact &optional (*JTRE* *JTRE*)))
 * 사실을 모순으로 선언 */
void contradiction(SExpr *fact);

/* ================================================================ */
/* Interface and display of data (인터페이스 및 표시)               */
/* ================================================================ */

/* (defun in? (fact ...)) - 사실이 IN 상태인지 확인 */
bool in_fact(SExpr *fact);

/* (defun out? (fact ...)) - 사실이 OUT 상태인지 확인 */
bool out_fact(SExpr *fact);

/* (defun why? (fact ...)) - 사실의 지지 이유 출력 */
void why_fact(SExpr *fact);

/* (defun assumptions-of (fact ...)) - 사실의 가정 목록 반환 */
List *assumptions_of(SExpr *fact);

/* (defun fetch (pattern ...)) - 패턴에 매칭되는 사실 검색 */
List *fetch(SExpr *pattern);

/* ================================================================ */
/* More display-intensive procedures                                */
/* ================================================================ */

/* (defun wfs (fact ...)) - 잘 설립된 지지(well-founded support) 표시 */
void wfs(SExpr *fact);

/* (defun say-datum-belief (pr ...)) - datum의 믿음 상태 출력 */
void say_datum_belief(SExpr *pr, const char *indent);

/* (defun show-justifications (fact ...)) - 사실의 모든 정당화 표시 */
void show_justifications(SExpr *fact);

/* (defun show-data (...)) - 모든 데이터 표시 */
void show_data(FILE *stream);

/* ================================================================ */
/* Database system (데이터베이스 시스템)                            */
/* ================================================================ */

/* (defun get-dbclass (fact ...)) - 사실의 dbclass 조회 또는 생성 */
Dbclass *get_dbclass(SExpr *fact);

/* (defun referent (fact &optional (virtual? nil) ...))
 * 사실의 datum 조회 (virtual이면 없을 때 생성) */
Datum *referent(SExpr *fact, bool virtual_p);

/* (defun referent1 (fact)) - 기존 datum만 조회 (생성하지 않음) */
Datum *referent1(SExpr *fact);

/* (defun insert (fact ...)) - 사실을 데이터베이스에 삽입 */
Datum *insert_datum(SExpr *fact);

/* (defun get-candidates (pattern)) - 패턴의 후보 datum 리스트 반환 */
List *get_candidates(SExpr *pattern);

/* (defun map-dbclass (proc ...)) - 모든 dbclass에 대해 함수 적용 */
void map_dbclass(void (*proc)(Dbclass *dbclass, void *ctx), void *ctx);

/* (defun get-tms-node (fact ...)) - 사실의 TMS 노드 반환 */
TmsNode *get_tms_node(SExpr *fact);

/* (defun view-node (node)) - TMS 노드의 datum lisp_form 반환 */
SExpr *view_node(TmsNode *node);

/* view_node의 문자열 버전 (JTMS의 node-string으로 사용) */
char *view_node_string(TmsNode *node);

/* ================================================================ */
/* More query routines (추가 질의 루틴)                            */
/* ================================================================ */

/* (defun show-datum (datum)) - datum의 문자열 표현 반환 */
char *show_datum(Datum *datum);

/* (defun get-datum (num ...)) - ID로 datum 조회 */
Datum *get_datum(int num);

/* (defun get-just (num ...)) - ID로 정당화 조회 */
Just *get_just(int num);

/* ================================================================ */
/* Rule system (규칙 시스템) - from jrules.lisp                    */
/* ================================================================ */

/* (defun insert-rule (dbclass matcher body ...))
 * 규칙을 dbclass에 등록 */
void insert_rule(Dbclass *dbclass, RuleMatcherFn matcher,
                 RuleBodyFn body);

/* (defun try-rules (datum)) - datum에 대해 모든 관련 규칙 시도 */
void try_rules(Datum *datum);

/* (defun try-rule-on (rule datum)) - 특정 규칙을 datum에 시도 */
void try_rule_on(JRule *rule, Datum *datum);

/* (defun run-rules (...)) - 큐에 있는 규칙들을 모두 실행 */
int run_rules(void);

/* (defun rules-waiting? (jtre)) - 대기 중인 규칙이 있는지 확인 */
bool rules_waiting(JTRE *jtre);

/* (defun enqueue (new j)) - 규칙을 큐에 추가 */
void enqueue(void *item, JTRE *jtre);

/* (defun dequeue (jtre)) - 큐에서 규칙 제거 */
void *dequeue(JTRE *jtre);

/* (defun show-rules (...)) - 모든 규칙 표시 */
void show_rules(FILE *stream);

/* (defun get-rule (num ...)) - ID로 규칙 조회 */
JRule *get_rule(int num);

#endif /* JDATA_H */
