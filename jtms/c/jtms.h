/*-*- Mode: C; -*-*/

/* Justification-based Truth Maintenance System (JTMS) */
/* Version 176. */
/* Last edited 1/29/93, by KDF */

/* Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. */
/* All rights reserved. */

/* See the file legal.txt for a paragraph stating scope of permission */
/* and disclaimer of warranty.  The above copyright notice and that */
/* paragraph must be included in any separate copy of this file. */

/**** JTMS 개요
 *
 * JTMS는 논리적 추론 시스템에서 믿음(beliefs)을 관리하는 진리 유지 시스템입니다.
 *
 * 주요 개념:
 * - 노드(Node): 명제나 사실을 표현. IN(믿어짐) 또는 OUT(믿어지지 않음) 상태를 가짐
 * - 정당화(Justification): "선행조건들이 모두 참이면 결론도 참"이라는 추론 규칙
 * - 가정(Assumption): 다른 근거 없이 참으로 가정할 수 있는 노드
 * - 모순(Contradiction): IN이 되면 안 되는 노드 (모순 감지용)
 *
 * 사용 예:
 * 1. JTMS 생성: JTMS* jtms = create_jtms("My TMS", ...);
 * 2. 노드 생성: TmsNode* n1 = tms_create_node(jtms, "sky blue", 0, 0);
 * 3. 정당화 추가: List* antes = list_prepend(NULL, n1);
 *                justify_node("rule-1", n3, antes);
 * 4. 가정 활성화: assume_node(n1);
 * 5. 상태 확인: why_node(n3);
 * 6. 가정 철회: retract_assumption(n1);
 */

#ifndef JTMS_H
#define JTMS_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ============================================================ */
/* Generic linked list utility                                   */
/* ============================================================ */

typedef struct List {
    void* data;
    struct List* next;
} List;

/* 리스트 앞에 원소 추가 (push) */
static inline List* list_prepend(List* lst, void* data) {
    List* node = (List*)malloc(sizeof(List));
    node->data = data;
    node->next = lst;
    return node;
}

/* 리스트 길이 반환 */
static inline int list_length(List* lst) {
    int count = 0;
    for (List* p = lst; p != NULL; p = p->next) count++;
    return count;
}

/* 리스트에서 n번째 원소 (0-based) */
static inline void* list_nth(List* lst, int n) {
    List* p = lst;
    for (int i = 0; i < n && p != NULL; i++) p = p->next;
    return p ? p->data : NULL;
}

/* 리스트에 특정 포인터가 있는지 확인 (eq) */
static inline int list_member(List* lst, void* data) {
    for (List* p = lst; p != NULL; p = p->next) {
        if (p->data == data) return 1;
    }
    return 0;
}

/* 리스트의 첫 원소 팝 (car + cdr) */
static inline void* list_pop(List** lst) {
    if (*lst == NULL) return NULL;
    List* head = *lst;
    void* data = head->data;
    *lst = head->next;
    free(head);
    return data;
}

/* 리스트 뒤에 다른 리스트를 연결 (append) - 파괴적 */
static inline List* list_append_destructive(List* a, List* b) {
    if (a == NULL) return b;
    List* p = a;
    while (p->next != NULL) p = p->next;
    p->next = b;
    return a;
}

/* 리스트 복사 */
static inline List* list_copy(List* lst) {
    if (lst == NULL) return NULL;
    List* result = NULL;
    List* tail = NULL;
    for (List* p = lst; p != NULL; p = p->next) {
        List* node = (List*)malloc(sizeof(List));
        node->data = p->data;
        node->next = NULL;
        if (tail == NULL) {
            result = node;
            tail = node;
        } else {
            tail->next = node;
            tail = node;
        }
    }
    return result;
}

/* 리스트 메모리 해제 (데이터는 해제하지 않음) */
static inline void list_free(List* lst) {
    while (lst != NULL) {
        List* next = lst->next;
        free(lst);
        lst = next;
    }
}

/* ============================================================ */
/* :IN/:OUT → enum                                              */
/* ============================================================ */

typedef enum { OUT = 0, IN = 1 } NodeLabel;

/* ============================================================ */
/* Forward declarations                                         */
/* ============================================================ */

typedef struct JTMS JTMS;
typedef struct TmsNode TmsNode;
typedef struct Just Just;

/* ============================================================ */
/* Function pointer types                                       */
/* ============================================================ */

/* 노드를 문자열로 변환하는 함수: node -> 문자열 */
typedef const char* (*NodeStringFunc)(TmsNode*);

/* 모순 처리 함수: (jtms, contradictions list) -> void */
typedef void (*ContradictionHandlerFunc)(JTMS*, List*);

/* 규칙을 큐에 넣는 함수: rule -> void */
typedef void (*EnqueueFunc)(void*);

/* ============================================================ */
/* Struct definitions                                           */
/* ============================================================ */

/**** JTMS 주 구조체
 * title: JTMS의 이름
 * node_counter: 노드 고유 번호 생성기
 * just_counter: 정당화 고유 번호 생성기
 * nodes: 모든 TMS 노드들의 리스트
 * justs: 모든 정당화들의 리스트
 * debugging: 디버깅 출력 활성화 플래그
 * contradictions: 모순 노드들의 리스트
 * assumptions: 가정 노드들의 리스트
 * checking_contradictions: 모순 검사 활성화 플래그
 * node_string: 노드를 문자열로 변환하는 함수
 * contradiction_handler: 모순 발생 시 호출될 함수
 * enqueue_procedure: 규칙을 큐에 넣는 함수
 */
struct JTMS {
    const char* title;
    int node_counter;              /* unique namer for nodes. */
    int just_counter;              /* unique namer for justifications. */
    List* nodes;                   /* list of all tms nodes. */
    List* justs;                   /* list of all justifications */
    int debugging;                 /* debugging flag */
    List* contradictions;          /* list of contradiction nodes. */
    List* assumptions;             /* list of assumption nodes. */
    int checking_contradictions;   /* For external systems */
    NodeStringFunc node_string;
    ContradictionHandlerFunc contradiction_handler;
    EnqueueFunc enqueue_procedure;
};

/**** TMS 노드 구조체
 * index: 노드의 고유 번호
 * datum: 외부 문제 해결자에 대한 포인터 (노드가 표현하는 명제/사실)
 * label: IN (믿어짐) 또는 OUT (믿어지지 않음)
 * support: 현재 정당화 또는 전제 마커 (ENABLED_ASSUMPTION 또는 Just*)
 * justs: 이 노드에 대한 가능한 정당화들의 리스트
 * consequences: 이 노드를 선행조건으로 사용하는 정당화들
 * mark: 순회 알고리즘을 위한 마커
 * contradictory: 모순 노드임을 표시하는 플래그
 * is_assumption: 가정 노드임을 표시하는 플래그
 * in_rules: 노드가 IN이 될 때 실행될 규칙들
 * out_rules: 노드가 OUT이 될 때 실행될 규칙들
 * jtms: 이 노드가 속한 JTMS
 */

/* support가 :ENABLED-ASSUMPTION인지 구분하기 위한 특별한 마커 */
#define ENABLED_ASSUMPTION ((void*)1)

/* assumption?가 :DEFAULT인지 구분하기 위한 특별한 마커 값 */
#define ASSUMPTION_DEFAULT 2
#define ASSUMPTION_TRUE    1
#define ASSUMPTION_NONE    0

struct TmsNode {
    int index;
    const char* datum;             /* pointer to external problem solver */
    NodeLabel label;               /* IN means believed, OUT means disbelieved */
    void* support;                 /* Current justification or ENABLED_ASSUMPTION */
    List* justs;                   /* Possible justifications */
    List* consequences;            /* Justifications in which it is an antecedent */
    void* mark;                    /* Marker for sweep algorithms */
    int contradictory;             /* Flag marking it as contradictory */
    int is_assumption;             /* Flag marking it as an assumption (0, 1, or ASSUMPTION_DEFAULT) */
    List* in_rules;                /* Rules that should be triggered when node goes in */
    List* out_rules;               /* Rules that should be triggered when node goes out */
    JTMS* jtms;                    /* The JTMS in which this node appears. */
};

/**** 정당화(Justification) 구조체
 * index: 정당화의 고유 번호
 * informant: 정당화의 출처 (규칙 이름, 추론 엔진 정보 등)
 * consequence: 이 정당화가 지지하는 결론 노드
 * antecedents: 결론을 뒷받침하는 선행조건 노드들의 리스트
 *              (모든 선행조건이 IN이면 결론도 IN이 됨)
 */
struct Just {
    int index;
    const char* informant;
    TmsNode* consequence;
    List* antecedents;
};

/* ============================================================ */
/* Function declarations                                        */
/* ============================================================ */

/* JTMS 객체를 사람이 읽을 수 있는 형식으로 출력 */
/* 예: #<JTMS: My-TMS> */
void print_jtms(JTMS* jtms, FILE* stream);

/* TMS 노드를 사람이 읽을 수 있는 형식으로 출력 */
/* 예: #<Node: (temperature hot)> */
void print_tms_node(TmsNode* node, FILE* stream);

/* 정당화(justification) 객체를 사람이 읽을 수 있는 형식으로 출력 */
/* 예: #<Just 5> */
void print_just(Just* just, FILE* stream);

/* 노드가 전제(premise)인지 확인 */
int tms_node_premise(TmsNode* node);

/* 노드를 문자열로 변환 */
const char* node_string(TmsNode* node);

/* TMS 에러를 발생시킴 */
void tms_error(const char* string, TmsNode* node);

/* 기본 노드 문자열 변환 함수 */
const char* default_node_string(TmsNode* n);

/* 새로운 JTMS 인스턴스 생성 */
JTMS* create_jtms(const char* title,
                  NodeStringFunc node_string_func,
                  int debugging,
                  int checking_contradictions,
                  ContradictionHandlerFunc contradiction_handler,
                  EnqueueFunc enqueue_procedure);

/* 기존 JTMS의 설정 변경 */
void change_jtms(JTMS* jtms,
                 ContradictionHandlerFunc contradiction_handler,
                 NodeStringFunc node_string_func,
                 EnqueueFunc enqueue_procedure,
                 int debugging,
                 int checking_contradictions);

/* 노드가 믿어지는 상태(IN)인지 확인 */
int in_node(TmsNode* node);

/* 노드가 믿어지지 않는 상태(OUT)인지 확인 */
int out_node(TmsNode* node);

/* JTMS에 새로운 노드 생성 */
TmsNode* tms_create_node(JTMS* jtms, const char* datum, int assumptionp, int contradictoryp);

/* 일반 노드를 가정(assumption)으로 변환하고 활성화 */
void assume_node(TmsNode* node);

/* 노드를 모순(contradiction) 노드로 만듦 */
void make_contradiction(TmsNode* node);

/* 노드에 정당화(justification) 추가 */
void justify_node(const char* informant, TmsNode* consequence, List* antecedents);

/* 정당화가 설치될 수 있는지 확인 */
int check_justification(Just* just);

/* 정당화의 모든 선행조건이 IN 상태인지 확인 */
int justification_satisfied(Just* just);

/* 정당화를 노드의 지지(support)로 설치하고 전파 */
void install_support(TmsNode* conseq, Just* just);

/* 노드가 IN이 된 것을 의존하는 다른 노드들에 전파 */
void propagate_inness(TmsNode* node);

/* 노드를 IN 상태로 만듦 */
void make_node_in(TmsNode* conseq, void* reason);

/* 활성화된 가정을 철회(retract) */
void retract_assumption(TmsNode* node);

/* 가정 노드를 활성화 */
void enable_assumption(TmsNode* node);

/* 노드를 OUT 상태로 만듦 */
void make_node_out(TmsNode* node);

/* 노드가 OUT이 된 것을 의존하는 다른 노드들에 전파 */
List* propagate_outness(TmsNode* node, JTMS* jtms);

/* OUT이 된 노드들에 대해 대체 지지를 찾음 */
void find_alternative_support(JTMS* jtms, List* out_queue);

/* 모순 검사 수행 */
void check_for_contradictions(JTMS* jtms);

/* 모순 검사를 비활성화하고 코드 실행 (save/restore 패턴) */
/* 예: without_contradiction_check(jtms);
 *     ... code ...
 *     restore_contradiction_check(jtms, saved_value); */
int without_contradiction_check(JTMS* jtms);

/* 모순 검사를 활성화하고 코드 실행 (save/restore 패턴) */
int with_contradiction_check(JTMS* jtms);

/* 모순 검사 플래그를 복원 */
void restore_contradiction_check(JTMS* jtms, int old_value);

/* 모순 처리 핸들러를 임시로 변경 (save/restore 패턴) */
ContradictionHandlerFunc with_contradiction_handler(JTMS* jtms, ContradictionHandlerFunc handler);

/* 모순 처리 핸들러를 복원 */
void restore_contradiction_handler(JTMS* jtms, ContradictionHandlerFunc old_handler);

/* 기본(default) 가정들을 활성화 */
void default_assumptions(JTMS* jtms);

/* 노드를 지지하는 정당화 반환 */
void* supporting_justification_for_node(TmsNode* node);

/* 노드가 의존하는 모든 가정들을 찾음 */
List* assumptions_of_node(TmsNode* node);

/* 현재 활성화된 모든 가정들의 리스트 반환 */
List* enabled_assumptions(JTMS* jtms);

/* 노드가 왜 IN 또는 OUT 상태인지 설명 출력 */
void why_node(TmsNode* node);

/* JTMS의 모든 노드에 대해 why_node 실행 */
void why_nodes(JTMS* jtms);

/* 기본 모순 처리 핸들러 */
void ask_user_handler(JTMS* jtms, List* contradictions);

/* 하나의 모순 처리 */
void handle_one_contradiction(TmsNode* contra_node);

/* 모순을 일으킨 가정들의 번호 매긴 리스트 출력 */
void print_contra_list(List* nodes);

/* 모순 처리 시 사용자의 응답 처리 */
void tms_answer(int num);

/* 정당화 네트워크를 대화형으로 탐색 */
void explore_network(TmsNode* node);

/* ============================================================ */
/* Helper macros                                                */
/* ============================================================ */

/* 디버깅 메시지 출력 매크로 */
/* JTMS의 debugging 플래그가 켜져 있을 때만 메시지 출력 */
#define DEBUGGING_JTMS(jtms, fmt, ...) \
    do { if ((jtms)->debugging) fprintf(stderr, fmt, ##__VA_ARGS__); } while(0)

/* support가 Just*인지 확인 (ENABLED_ASSUMPTION이 아닌 경우) */
#define IS_JUST_SUPPORT(support) ((support) != NULL && (support) != ENABLED_ASSUMPTION)

#endif /* JTMS_H */
