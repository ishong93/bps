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

#include "jtms.h"
#include <setjmp.h>

/* ============================================================ */
/* Global variables (special variables from Lisp)               */
/* ============================================================ */

/* 모순 처리에 사용되는 전역 변수 (proclaim '(special *contra-assumptions*)) */
static List* contra_assumptions = NULL;

/* setjmp/longjmp for catch/throw emulation */
static jmp_buf contradiction_jmp;
static int contradiction_jmp_active = 0;
static int contradiction_jmp_value = 0;

static jmp_buf tms_contradiction_handler_jmp;
static int tms_contradiction_handler_jmp_active = 0;
static int tms_contradiction_handler_jmp_value = 0;

/* ============================================================ */
/* Print functions                                              */
/* ============================================================ */

/* JTMS 객체를 사람이 읽을 수 있는 형식으로 출력 */
/* 예: #<JTMS: My-TMS> */
void print_jtms(JTMS* jtms, FILE* stream) {
    fprintf(stream, "#<JTMS: %s>", jtms->title);
}

/* TMS 노드를 사람이 읽을 수 있는 형식으로 출력 */
/* 예: #<Node: (temperature hot)> */
void print_tms_node(TmsNode* node, FILE* stream) {
    fprintf(stream, "#<Node: %s>", node_string(node));
}

/* 정당화(justification) 객체를 사람이 읽을 수 있는 형식으로 출력 */
/* 예: #<Just 5> */
void print_just(Just* just, FILE* stream) {
    fprintf(stream, "#<Just %d>", just->index);
}

/* ============================================================ */
/* Simple utilities                                             */
/* ============================================================ */

/* 노드가 전제(premise)인지 확인 */
/* 전제는 선행조건(antecedents)이 없는 정당화로 지원되는 노드 */
/* 예: tms_node_premise(my_node) => 1 또는 0 */
int tms_node_premise(TmsNode* node) {
    void* support = node->support;
    if (support == NULL) return 0;
    if (support == ENABLED_ASSUMPTION) return 0;
    /* support is a Just* */
    Just* just = (Just*)support;
    return (just->antecedents == NULL) ? 1 : 0;
}

/* 노드를 문자열로 변환 */
/* JTMS에 등록된 node_string 함수를 사용하여 노드를 표현 */
/* 예: node_string(my_node) => "(temperature hot)" */
const char* node_string(TmsNode* node) {
    return node->jtms->node_string(node);
}

/* TMS 에러를 발생시킴 */
/* 예: tms_error("Node %s is invalid", bad_node) */
void tms_error(const char* string, TmsNode* node) {
    fprintf(stderr, string, node_string(node));
    fprintf(stderr, "\n");
    exit(1);
}

/* 기본 노드 문자열 변환 함수 */
/* 노드의 datum 필드를 문자열로 변환 */
/* 예: default_node_string(node) => "(is-hot coffee)" */
const char* default_node_string(TmsNode* n) {
    return n->datum;
}

/* ============================================================ */
/* JTMS creation and configuration                              */
/* ============================================================ */

/* 새로운 JTMS 인스턴스 생성 */
/* title: JTMS의 이름 */
/* node_string_func: 노드를 문자열로 변환하는 함수 (NULL이면 default_node_string 사용) */
/* debugging: 디버깅 모드 활성화 여부 */
/* checking_contradictions: 모순 검사 활성화 여부 */
/* contradiction_handler: 모순 처리 함수 (NULL이면 ask_user_handler 사용) */
/* enqueue_procedure: 규칙을 큐에 넣는 함수 */
/* 예: create_jtms("My TMS", NULL, 1, 1, NULL, NULL) */
JTMS* create_jtms(const char* title,
                  NodeStringFunc node_string_func,
                  int debugging,
                  int checking_contradictions,
                  ContradictionHandlerFunc contradiction_handler,
                  EnqueueFunc enqueue_procedure) {
    JTMS* jtms = (JTMS*)malloc(sizeof(JTMS));
    jtms->title = title;
    jtms->node_counter = 0;
    jtms->just_counter = 0;
    jtms->nodes = NULL;
    jtms->justs = NULL;
    jtms->debugging = debugging;
    jtms->contradictions = NULL;
    jtms->assumptions = NULL;
    jtms->checking_contradictions = checking_contradictions;
    jtms->node_string = node_string_func ? node_string_func : default_node_string;
    jtms->contradiction_handler = contradiction_handler ? contradiction_handler : ask_user_handler;
    jtms->enqueue_procedure = enqueue_procedure;
    return jtms;
}

/* 기존 JTMS의 설정 변경 */
/* 제공된 인자에 해당하는 설정만 업데이트 */
/* 예: change_jtms(my_jtms, NULL, NULL, NULL, 1, -1) */
/* Note: -1 for checking_contradictions means "don't change" */
void change_jtms(JTMS* jtms,
                 ContradictionHandlerFunc contradiction_handler,
                 NodeStringFunc node_string_func,
                 EnqueueFunc enqueue_procedure,
                 int debugging,
                 int checking_contradictions) {
    if (node_string_func) jtms->node_string = node_string_func;
    if (debugging >= 0) jtms->debugging = debugging;
    if (checking_contradictions >= 0)
        jtms->checking_contradictions = checking_contradictions;
    if (contradiction_handler)
        jtms->contradiction_handler = contradiction_handler;
    if (enqueue_procedure)
        jtms->enqueue_procedure = enqueue_procedure;
}

/* ============================================================ */
/* Basic inference-engine interface                             */
/* ============================================================ */

/* 노드가 믿어지는 상태(IN)인지 확인 */
/* 예: in_node(my_node) => 1 또는 0 */
int in_node(TmsNode* node) {
    return (node->label == IN) ? 1 : 0;
}

/* 노드가 믿어지지 않는 상태(OUT)인지 확인 */
/* 예: out_node(my_node) => 1 또는 0 */
int out_node(TmsNode* node) {
    return (node->label == OUT) ? 1 : 0;
}

/* JTMS에 새로운 노드 생성 */
/* datum: 노드가 표현하는 명제나 사실 */
/* assumptionp: 가정(assumption) 노드로 생성할지 여부 */
/* contradictoryp: 모순(contradiction) 노드로 생성할지 여부 */
/* 예: tms_create_node(my_jtms, "(temperature hot)", 1, 0) */
TmsNode* tms_create_node(JTMS* jtms, const char* datum, int assumptionp, int contradictoryp) {
    TmsNode* node = (TmsNode*)malloc(sizeof(TmsNode));
    node->index = ++(jtms->node_counter);
    node->datum = datum;
    node->label = OUT;
    node->support = NULL;
    node->justs = NULL;
    node->consequences = NULL;
    node->mark = NULL;
    node->contradictory = contradictoryp ? 1 : 0;
    node->is_assumption = assumptionp ? ASSUMPTION_TRUE : ASSUMPTION_NONE;
    node->in_rules = NULL;
    node->out_rules = NULL;
    node->jtms = jtms;

    if (assumptionp)
        jtms->assumptions = list_prepend(jtms->assumptions, node);
    if (contradictoryp)
        jtms->contradictions = list_prepend(jtms->contradictions, node);
    jtms->nodes = list_prepend(jtms->nodes, node);
    return node;
}

/* ============================================================ */
/* Node state manipulation                                      */
/* ============================================================ */

/* 일반 노드를 가정(assumption)으로 변환하고 활성화 */
/* 노드가 이미 가정이 아니면 가정으로 만들고, 가정을 활성화함 */
/* 예: assume_node(my_node) */
void assume_node(TmsNode* node) {
    JTMS* jtms = node->jtms;
    if (!node->is_assumption) {
        DEBUGGING_JTMS(jtms, "\nConverting %s into an assumption", node_string(node));
        node->is_assumption = ASSUMPTION_TRUE;
        jtms->assumptions = list_prepend(jtms->assumptions, node);
    }
    enable_assumption(node);
}

/* 노드를 모순(contradiction) 노드로 만듦 */
/* 이 노드가 IN 상태가 되면 모순이 발생한 것으로 간주 */
/* 예: make_contradiction(my_node) */
void make_contradiction(TmsNode* node) {
    JTMS* jtms = node->jtms;
    if (!node->contradictory) {
        node->contradictory = 1;
        jtms->contradictions = list_prepend(jtms->contradictions, node);
        check_for_contradictions(jtms);
    }
}

/* ============================================================ */
/* Justification support                                        */
/* ============================================================ */

/* 정당화가 설치될 수 있는지 확인 */
/* 결론이 OUT 상태이고 모든 선행조건이 만족되면 1 */
/* 예: check_justification(my_just) => 1 또는 0 */
int check_justification(Just* just) {
    return out_node(just->consequence) && justification_satisfied(just);
}

/* 정당화의 모든 선행조건이 IN 상태인지 확인 */
/* 예: justification_satisfied(my_just) => 1 또는 0 */
int justification_satisfied(Just* just) {
    for (List* p = just->antecedents; p != NULL; p = p->next) {
        if (!in_node((TmsNode*)p->data)) return 0;
    }
    return 1;
}

/* 노드에 정당화(justification) 추가 */
/* informant: 정당화의 출처나 규칙 이름 */
/* consequence: 정당화되는 결론 노드 */
/* antecedents: 결론을 뒷받침하는 선행조건 노드들의 리스트 */
/* 모든 선행조건이 IN이면 결론도 IN이 됨 */
/* 예: justify_node("modus-ponens", conclusion_node, antecedent_list) */
void justify_node(const char* informant, TmsNode* consequence, List* antecedents) {
    JTMS* jtms = consequence->jtms;
    Just* just = (Just*)malloc(sizeof(Just));
    just->index = ++(jtms->just_counter);
    just->informant = informant;
    just->consequence = consequence;
    just->antecedents = antecedents;

    consequence->justs = list_prepend(consequence->justs, just);

    for (List* p = antecedents; p != NULL; p = p->next) {
        TmsNode* anode = (TmsNode*)p->data;
        anode->consequences = list_prepend(anode->consequences, just);
    }

    jtms->justs = list_prepend(jtms->justs, just);

    if (jtms->debugging) {
        fprintf(stderr, "\nJustifying %s by %s using",
                node_string(consequence), informant);
        for (List* p = antecedents; p != NULL; p = p->next) {
            fprintf(stderr, " %s", node_string((TmsNode*)p->data));
        }
        fprintf(stderr, ".");
    }

    if (antecedents != NULL || out_node(consequence)) {
        if (check_justification(just))
            install_support(consequence, just);
    } else {
        consequence->support = (void*)just;
    }
    check_for_contradictions(jtms);
}

/* 정당화를 노드의 지지(support)로 설치하고 전파 */
/* 노드를 IN 상태로 만들고, 이 변화를 의존하는 노드들에 전파 */
/* 예: install_support(conclusion, my_just) */
void install_support(TmsNode* conseq, Just* just) {
    make_node_in(conseq, (void*)just);
    propagate_inness(conseq);
}

/* 노드가 IN이 된 것을 의존하는 다른 노드들에 전파 */
/* 큐를 사용하여 영향받는 모든 노드를 IN으로 만듦 */
/* 예: propagate_inness(my_node) */
void propagate_inness(TmsNode* node) {
    JTMS* jtms = node->jtms;
    List* q = list_prepend(NULL, node);

    while (q != NULL) {
        TmsNode* current = (TmsNode*)list_pop(&q);
        DEBUGGING_JTMS(jtms, "\n   Propagating belief in %s.", node_string(current));
        for (List* p = current->consequences; p != NULL; p = p->next) {
            Just* justification = (Just*)p->data;
            if (check_justification(justification)) {
                make_node_in(justification->consequence, (void*)justification);
                q = list_prepend(q, justification->consequence);
            }
        }
    }
}

/* 노드를 IN 상태로 만듦 */
/* reason: 노드가 IN이 되는 이유 (Just* 또는 ENABLED_ASSUMPTION) */
/* IN 규칙들을 큐에 넣어 실행하도록 함 */
/* 예: make_node_in(my_node, (void*)my_just) */
void make_node_in(TmsNode* conseq, void* reason) {
    JTMS* jtms = conseq->jtms;
    EnqueueFunc enqueuef = jtms->enqueue_procedure;

    if (jtms->debugging) {
        if (reason == ENABLED_ASSUMPTION) {
            fprintf(stderr, "\n     Making %s in via ENABLED-ASSUMPTION.",
                    node_string(conseq));
        } else {
            Just* just = (Just*)reason;
            fprintf(stderr, "\n     Making %s in via %s on",
                    node_string(conseq), just->informant);
            for (List* p = just->antecedents; p != NULL; p = p->next) {
                fprintf(stderr, " %s", jtms->node_string((TmsNode*)p->data));
            }
            fprintf(stderr, ".");
        }
    }

    conseq->label = IN;
    conseq->support = reason;

    if (enqueuef) {
        for (List* p = conseq->in_rules; p != NULL; p = p->next) {
            enqueuef(p->data);
        }
        list_free(conseq->in_rules);
        conseq->in_rules = NULL;
    }
}

/* ============================================================ */
/* Assumption Manipulation                                      */
/* ============================================================ */

/* 활성화된 가정을 철회(retract) */
/* 가정이 철회되면 OUT 상태가 되고, 의존하는 노드들도 OUT이 됨 */
/* 그 후 대체 지지(alternative support)를 찾음 */
/* 예: retract_assumption(my_assumption) */
void retract_assumption(TmsNode* node) {
    if (node->support == ENABLED_ASSUMPTION) {
        JTMS* jtms = node->jtms;
        DEBUGGING_JTMS(jtms, "\n  Retracting assumption %s.", node_string(node));
        make_node_out(node);
        List* out_list = propagate_outness(node, jtms);
        List* combined = list_prepend(out_list, node);
        find_alternative_support(jtms, combined);
    }
}

/* 가정 노드를 활성화 */
/* 가정이 아닌 노드에 대해서는 에러 발생 */
/* OUT 상태의 가정을 IN으로 만들고 전파함 */
/* 예: enable_assumption(my_assumption) */
void enable_assumption(TmsNode* node) {
    JTMS* jtms = node->jtms;

    if (!node->is_assumption) {
        tms_error("Can't enable the non-assumption %s", node);
    }

    DEBUGGING_JTMS(jtms, "\n  Enabling assumption %s.", node_string(node));

    if (out_node(node)) {
        make_node_in(node, ENABLED_ASSUMPTION);
        propagate_inness(node);
    } else if (node->support == ENABLED_ASSUMPTION) {
        /* 이미 활성화된 가정 - 아무것도 하지 않음 */
    } else if (IS_JUST_SUPPORT(node->support) &&
               ((Just*)node->support)->antecedents == NULL) {
        /* 선행조건 없는 정당화로 지지됨 (전제) - 아무것도 하지 않음 */
    } else {
        node->support = ENABLED_ASSUMPTION;
    }

    check_for_contradictions(jtms);
}

/* 노드를 OUT 상태로 만듦 */
/* 지지(support)를 제거하고 레이블을 OUT으로 설정 */
/* OUT 규칙들을 큐에 넣어 실행하도록 함 */
/* 예: make_node_out(my_node) */
void make_node_out(TmsNode* node) {
    JTMS* jtms = node->jtms;
    EnqueueFunc enqueuef = jtms->enqueue_procedure;

    DEBUGGING_JTMS(jtms, "\n     retracting belief in %s.", node_string(node));

    node->support = NULL;
    node->label = OUT;

    if (enqueuef) {
        for (List* p = node->out_rules; p != NULL; p = p->next) {
            enqueuef(p->data);
        }
    }
    list_free(node->out_rules);
    node->out_rules = NULL;
}

/* 노드가 OUT이 된 것을 의존하는 다른 노드들에 전파 */
/* 이 노드를 사용하는 정당화들을 찾아서, 그것으로 지지받던 노드들을 OUT으로 만듦 */
/* OUT이 된 노드들의 리스트를 반환하여 대체 지지를 찾을 수 있게 함 */
/* 예: propagate_outness(my_node, my_jtms) => 영향 받은 노드들 리스트 */
List* propagate_outness(TmsNode* node, JTMS* jtms) {
    List* out_queue = NULL;
    DEBUGGING_JTMS(jtms, "\n   Propagating disbelief in %s.", node_string(node));

    /* js: 처리할 정당화 리스트 - 초기값은 node의 consequences 복사본 */
    List* js = list_copy(node->consequences);

    while (js != NULL) {
        Just* just = (Just*)list_pop(&js);
        /* For each justification using the node, check to see if */
        /* it supports some other node.  If so, forget that node, */
        /* queue up the node to look for other support, and recurse */
        TmsNode* conseq = just->consequence;
        if (conseq->support == (void*)just) {
            make_node_out(conseq);
            out_queue = list_prepend(out_queue, conseq);
            /* append consequences of conseq to js */
            List* new_consequences = list_copy(conseq->consequences);
            js = list_append_destructive(js, new_consequences);
        }
    }
    return out_queue;
}

/* OUT이 된 노드들에 대해 대체 지지를 찾음 */
/* 각 노드의 다른 정당화들을 검사하여 만족되는 것이 있으면 설치 */
/* 예: find_alternative_support(my_jtms, out_queue) */
void find_alternative_support(JTMS* jtms, List* out_queue) {
    DEBUGGING_JTMS(jtms, "\n   Looking for alternative supports.");
    for (List* p = out_queue; p != NULL; p = p->next) {
        TmsNode* node = (TmsNode*)p->data;
        if (!in_node(node)) {
            for (List* jp = node->justs; jp != NULL; jp = jp->next) {
                Just* just = (Just*)jp->data;
                if (check_justification(just)) {
                    install_support(just->consequence, just);
                    break;  /* return just - 내부 루프만 탈출 */
                }
            }
        }
    }
}

/* ============================================================ */
/* Contradiction handling interface                             */
/* ============================================================ */

/* 모순 검사 수행 */
/* 모순 노드들 중 IN 상태인 것이 있으면 모순 처리 함수 호출 */
/* 예: check_for_contradictions(my_jtms) */
void check_for_contradictions(JTMS* jtms) {
    if (jtms->checking_contradictions) {
        List* contradictions = NULL;
        for (List* p = jtms->contradictions; p != NULL; p = p->next) {
            TmsNode* cnode = (TmsNode*)p->data;
            if (in_node(cnode)) {
                contradictions = list_prepend(contradictions, cnode);
            }
        }
        if (contradictions) {
            jtms->contradiction_handler(jtms, contradictions);
            list_free(contradictions);
        }
    }
}

/* 모순 검사를 비활성화하고 이전 값을 반환 (save/restore 패턴) */
/* 예: int saved = without_contradiction_check(jtms);
 *     ... code ...
 *     restore_contradiction_check(jtms, saved); */
int without_contradiction_check(JTMS* jtms) {
    int old_value = jtms->checking_contradictions;
    jtms->checking_contradictions = 0;
    return old_value;
}

/* 모순 검사를 활성화하고 이전 값을 반환 (save/restore 패턴) */
/* 예: int saved = with_contradiction_check(jtms);
 *     ... code ...
 *     restore_contradiction_check(jtms, saved); */
int with_contradiction_check(JTMS* jtms) {
    int old_value = jtms->checking_contradictions;
    jtms->checking_contradictions = 1;
    return old_value;
}

/* 모순 검사 플래그를 복원 */
void restore_contradiction_check(JTMS* jtms, int old_value) {
    jtms->checking_contradictions = old_value;
}

/* 모순 처리 핸들러를 임시로 변경하고 이전 핸들러를 반환 (save/restore 패턴) */
/* 예: ContradictionHandlerFunc saved = with_contradiction_handler(jtms, my_handler);
 *     ... code ...
 *     restore_contradiction_handler(jtms, saved); */
ContradictionHandlerFunc with_contradiction_handler(JTMS* jtms, ContradictionHandlerFunc handler) {
    ContradictionHandlerFunc old_handler = jtms->contradiction_handler;
    jtms->contradiction_handler = handler;
    return old_handler;
}

/* 모순 처리 핸들러를 복원 */
void restore_contradiction_handler(JTMS* jtms, ContradictionHandlerFunc old_handler) {
    jtms->contradiction_handler = old_handler;
}

/* ============================================================ */
/* Default assumptions                                          */
/* ============================================================ */

/* default_assumptions 용 모순 핸들러 - longjmp로 throw 에뮬레이션 */
static void default_assumptions_contradiction_handler(JTMS* jtms, List* contradictions) {
    (void)jtms;
    (void)contradictions;
    if (contradiction_jmp_active) {
        longjmp(contradiction_jmp, 1);
    }
}

/* 기본(default) 가정들을 활성화 */
/* :DEFAULT로 표시된 가정들을 활성화하되, 모순이 발생하면 철회 */
/* 모순 없이 활성화할 수 있는 최대한의 기본 가정들을 활성화 */
/* 예: default_assumptions(my_jtms) */
void default_assumptions(JTMS* jtms) {
    int saved_check = with_contradiction_check(jtms);
    ContradictionHandlerFunc saved_handler =
        with_contradiction_handler(jtms, default_assumptions_contradiction_handler);

    for (List* p = jtms->assumptions; p != NULL; p = p->next) {
        TmsNode* assumption = (TmsNode*)p->data;
        if (assumption->support == ENABLED_ASSUMPTION) {
            /* 이미 활성화됨 - 건너뜀 */
            continue;
        }
        if (assumption->is_assumption != ASSUMPTION_DEFAULT) {
            /* :DEFAULT가 아님 - 건너뜀 */
            continue;
        }
        /* catch 'CONTRADICTION (enable-assumption assumption) 에뮬레이션 */
        contradiction_jmp_active = 1;
        if (setjmp(contradiction_jmp) != 0) {
            /* 모순 발생 - 가정 철회 */
            contradiction_jmp_active = 0;
            retract_assumption(assumption);
        } else {
            enable_assumption(assumption);
            contradiction_jmp_active = 0;
        }
    }

    restore_contradiction_handler(jtms, saved_handler);
    restore_contradiction_check(jtms, saved_check);
}

/* ============================================================ */
/* Well-founded support inquiries                               */
/* ============================================================ */

/* 노드를 지지하는 정당화 반환 */
/* 예: supporting_justification_for_node(my_node) => Just* 또는 ENABLED_ASSUMPTION */
void* supporting_justification_for_node(TmsNode* node) {
    return node->support;
}

/* 노드가 의존하는 모든 가정들을 찾음 */
/* 노드의 지지를 역추적하여 근거가 되는 활성화된 가정들의 리스트 반환 */
/* 예: assumptions_of_node(my_node) => 가정 노드들의 리스트 */
List* assumptions_of_node(TmsNode* node) {
    List* assumptions = NULL;
    /* 유일한 마커 생성 (힙 할당된 포인터를 마커로 사용) */
    void* marker = malloc(1);

    List* nodes = list_prepend(NULL, node);

    while (nodes != NULL) {
        TmsNode* current = (TmsNode*)list_pop(&nodes);
        if (current->mark == marker) {
            continue;  /* 이미 방문함 */
        }
        if (current->support == ENABLED_ASSUMPTION) {
            assumptions = list_prepend(assumptions, current);
        } else if (in_node(current) && IS_JUST_SUPPORT(current->support)) {
            /* support의 antecedents를 nodes에 추가 */
            Just* just = (Just*)current->support;
            for (List* p = just->antecedents; p != NULL; p = p->next) {
                nodes = list_prepend(nodes, p->data);
            }
        }
        current->mark = marker;
    }

    free(marker);
    return assumptions;
}

/* 현재 활성화된 모든 가정들의 리스트 반환 */
/* 예: enabled_assumptions(my_jtms) => 활성화된 가정 노드들의 리스트 */
List* enabled_assumptions(JTMS* jtms) {
    List* result = NULL;
    for (List* p = jtms->assumptions; p != NULL; p = p->next) {
        TmsNode* assumption = (TmsNode*)p->data;
        if (assumption->support == ENABLED_ASSUMPTION) {
            result = list_prepend(result, assumption);
        }
    }
    return result;
}

/* ============================================================ */
/* Inference engine stub                                        */
/* ============================================================ */

/* 노드가 왜 IN 또는 OUT 상태인지 설명 출력 */
/* 활성화된 가정이면 그렇게 표시하고, */
/* 정당화로 IN이면 informant와 선행조건들을 표시 */
/* 예: why_node(my_node) */
/*     출력: "node1 is IN via modus-ponens on */
/*            premise1 */
/*            premise2" */
void why_node(TmsNode* node) {
    void* justification = node->support;

    if (justification == ENABLED_ASSUMPTION) {
        printf("\n%s is an enabled assumption", node_string(node));
    } else if (justification != NULL) {
        Just* just = (Just*)justification;
        printf("\n%s is IN via %s on", node_string(node), just->informant);
        for (List* p = just->antecedents; p != NULL; p = p->next) {
            printf("\n  %s", node_string((TmsNode*)p->data));
        }
    } else {
        printf("\n%s is OUT.", node_string(node));
    }
}

/* JTMS의 모든 노드에 대해 why_node 실행 */
/* 전체 네트워크의 상태를 출력 */
/* 예: why_nodes(my_jtms) */
void why_nodes(JTMS* jtms) {
    for (List* p = jtms->nodes; p != NULL; p = p->next) {
        why_node((TmsNode*)p->data);
    }
}

/* ============================================================ */
/* Contradiction handling - user interaction                     */
/* ============================================================ */

/* 기본 모순 처리 핸들러 */
/* 첫 번째 모순을 처리하고 다시 모순 검사 */
/* 예: ask_user_handler(my_jtms, list_of_contradictions) */
void ask_user_handler(JTMS* jtms, List* contradictions) {
    handle_one_contradiction((TmsNode*)contradictions->data);
    check_for_contradictions(jtms);
}

/* 하나의 모순 처리 */
/* 모순을 일으킨 가정들을 찾아 사용자에게 제시하고, */
/* 사용자가 선택한 가정을 철회 */
/* 예: handle_one_contradiction(my_contradiction_node) */
/*     출력: "Contradiction found: (false) */
/*            1 (premise-a) */
/*            2 (premise-b) */
/*            Call tms_answer(<number>) to retract assumption." */
void handle_one_contradiction(TmsNode* contra_node) {
    contra_assumptions = assumptions_of_node(contra_node);

    if (contra_assumptions == NULL) {
        tms_error("\nThere is a flaw in the universe...%s", contra_node);
    }

    printf("\nContradiction found: %s", node_string(contra_node));
    print_contra_list(contra_assumptions);
    printf("\nCall tms_answer(<number>) to retract assumption.");

    /* catch 'tms-contradiction-handler (break ...) 에뮬레이션 */
    /* C에서는 사용자 입력을 직접 받음 */
    int the_answer = 0;
    tms_contradiction_handler_jmp_active = 1;
    if (setjmp(tms_contradiction_handler_jmp) != 0) {
        the_answer = tms_contradiction_handler_jmp_value;
    } else {
        printf("\nJTMS contradiction break. Enter answer number: ");
        if (scanf("%d", &the_answer) != 1) {
            the_answer = 0;
        }
    }
    tms_contradiction_handler_jmp_active = 0;

    int len = list_length(contra_assumptions);
    if (the_answer > 0 && the_answer <= len) {
        TmsNode* to_retract = (TmsNode*)list_nth(contra_assumptions, the_answer - 1);
        retract_assumption(to_retract);
    }
}

/* 모순을 일으킨 가정들의 번호 매긴 리스트 출력 */
/* 예: print_contra_list(nodes) */
/*     출력: "1 (assumption-a) */
/*            2 (assumption-b)" */
void print_contra_list(List* nodes) {
    int counter = 1;
    for (List* p = nodes; p != NULL; p = p->next, counter++) {
        printf("\n%d %s", counter, node_string((TmsNode*)p->data));
    }
}

/* 모순 처리 시 사용자의 응답 처리 */
/* 유효한 번호면 해당 가정을 철회 */
/* 예: tms_answer(2) */
void tms_answer(int num) {
    int len = list_length(contra_assumptions);
    if (num > 0) {
        if (num <= len) {
            if (tms_contradiction_handler_jmp_active) {
                tms_contradiction_handler_jmp_value = num;
                longjmp(tms_contradiction_handler_jmp, 1);
            }
        } else {
            printf("\nIgnoring answer, too big.");
        }
    } else {
        printf("\nIgnoring answer, too small");
    }
}

/* ============================================================ */
/* Network exploration                                          */
/* ============================================================ */

/* 정당화 네트워크를 대화형으로 탐색 */
/* 노드의 지지 구조를 따라가며 어떻게 믿음이 형성되었는지 탐색 */
/* 사용자가 선행조건 노드들을 선택하여 깊이 들어가고, 0으로 뒤로 가며, q로 종료 */
/* 예: explore_network(my_node) */
void explore_network(TmsNode* node) {
    if (!in_node(node)) {
        printf("\n Sorry, %s not believed.", node_string(node));
        return;
    }

    List* stack = NULL;
    TmsNode* current = node;

    for (;;) {
        why_node(current);
        List* options = NULL;
        int olen = 0;

        if (IS_JUST_SUPPORT(current->support)) {
            options = ((Just*)current->support)->antecedents;
        }
        olen = list_length(options);

        /* 사용자 입력 루프 */
        for (;;) {
            printf("\n>>>");
            char input[64];
            if (fgets(input, sizeof(input), stdin) == NULL) {
                list_free(stack);
                return;
            }

            /* 'q'를 입력하면 종료 */
            if (input[0] == 'q' || input[0] == 'Q') {
                list_free(stack);
                return;
            }

            int choice = atoi(input);

            if (choice == 0) {
                /* 0: 뒤로 가기 */
                if (stack != NULL) {
                    current = (TmsNode*)list_pop(&stack);
                    break;
                } else {
                    list_free(stack);
                    return;
                }
            } else if (choice > 0 && choice <= olen) {
                /* 유효한 선택: 해당 선행조건으로 이동 */
                stack = list_prepend(stack, current);
                current = (TmsNode*)list_nth(options, choice - 1);
                break;
            } else {
                printf("\n Must be q or an integer from 0 to %d.", olen);
            }
        }
    }
}
