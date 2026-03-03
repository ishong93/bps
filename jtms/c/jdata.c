/* -*- C -*- */

/* Database for Tiny Rule Engine using JTMS - Implementation */
/* Converted from jdata.lisp and jrules.lisp */

/* Copyright (c) 1989, 1990, 1991 Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer and the Xerox Corporation. All rights reserved. */

#include "jdata.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* unify()가 외부에서 선언되어 있다고 가정 */
extern List *unify(SExpr *pattern, SExpr *datum);
extern SExpr *subst_bindings(List *bindings, SExpr *pattern);

/* ================================================================ */
/* Dbclass printer                                                  */
/* (defun jtre-dbclass-printer (r st ignore)                        */
/*   (format st "<Dbclass ~A>" (dbclass-name r)))                   */
/* ================================================================ */

static void dbclass_printer(Dbclass *dc, FILE *stream) {
    fprintf(stream, "<Dbclass %s>", dc->name);
}

/* ================================================================ */
/* Datum printer                                                    */
/* (defun jtre-datum-printer (d st ignore)                          */
/*   (format st "<Datum ~D>" (datum-id d)))                         */
/* ================================================================ */

static void datum_printer(Datum *d, FILE *stream) {
    fprintf(stream, "<Datum %d>", d->id);
}

/* ================================================================ */
/* Making statements (사실 선언)                                    */
/* ================================================================ */

/* (defun assert! (fact just &optional (*JTRE* *JTRE*) &aux datum node)
 *   (setq datum (referent fact t)
 *         node (datum-tms-node datum))
 *   (unless (listp just) (setq just (list just)))
 *   (debugging-jtre "~%    Asserting ~A via ~A." fact just)
 *   (justify-node (car just) node
 *     (mapcar #'(lambda (f) (datum-tms-node (referent f t)))
 *             (cdr just)))
 *   datum)
 *
 * 사실을 assert: 정당화(justification)를 부여하여 사실을 참으로 만듦
 * just는 (informant ante1 ante2 ...) 형태의 리스트 또는 단일 기호
 */
Datum *assert_fact(SExpr *fact, SExpr *just) {
    Datum *datum = referent(fact, true);
    TmsNode *node = datum->tms_node;

    /* just가 리스트가 아니면 리스트로 변환 */
    /* (unless (listp just) (setq just (list just))) */
    const char *informant = "user";
    List *antes = list_new();  /* 선행조건 TMS 노드 리스트 */

    if (sexpr_is_symbol(just)) {
        /* 단일 기호: informant만 있고 선행조건 없음 */
        informant = just->symbol;
    } else if (sexpr_is_cons(just)) {
        /* 리스트: car가 informant, cdr가 선행조건 */
        if (sexpr_is_symbol(just->cons.car))
            informant = just->cons.car->symbol;
        /* (mapcar #'(lambda (f) (datum-tms-node (referent f t))) (cdr just)) */
        SExpr *rest = just->cons.cdr;
        while (sexpr_is_cons(rest)) {
            Datum *ante_datum = referent(rest->cons.car, true);
            list_push(antes, ante_datum->tms_node);
            rest = rest->cons.cdr;
        }
    }

    /* 디버깅 출력 */
    if (current_jtre->debugging)
        fprintf(stderr, "\n    Asserting %s via %s.",
                sexpr_to_string(fact), sexpr_to_string(just));

    /* (justify-node (car just) node antes) */
    justify_node(informant, node, antes);

    return datum;
}

/* (defun quiet-assert! (fact just &optional (*JTRE* *JTRE*))
 *   (without-contradiction-check (jtre-jtms *JTRE*) (assert! fact just)))
 *
 * 모순 검사를 비활성화한 채로 assert 수행 */
Datum *quiet_assert(SExpr *fact, SExpr *just) {
    Jtms *jtms = current_jtre->jtms;
    /* 모순 검사를 임시로 비활성화 */
    bool saved_checking = jtms->checking_contradictions;
    jtms->checking_contradictions = false;

    Datum *result = assert_fact(fact, just);

    /* 모순 검사 복원 */
    jtms->checking_contradictions = saved_checking;
    return result;
}

/* (defun assume! (fact reason &optional (*JTRE* *JTRE*) &aux datum node)
 *   (setq datum (referent fact t)
 *         node (datum-tms-node datum))
 *   (cond ((not (datum-assumption? datum))
 *          (setf (datum-assumption? datum) reason)
 *          (debugging-jtre "~%    Assuming ~A via ~A." fact reason)
 *          (assume-node node)
 *          (enable-assumption node))
 *         ((eq reason (datum-assumption? datum)))
 *         (t (error ...)))
 *   datum)
 *
 * 사실을 가정(assumption)으로 선언
 * 이미 같은 이유로 가정되었으면 무시, 다른 이유면 오류 */
Datum *assume_fact(SExpr *fact, const char *reason) {
    Datum *datum = referent(fact, true);
    TmsNode *node = datum->tms_node;

    if (!datum->assumption_reason) {
        /* 아직 가정되지 않은 경우: 새로운 가정 설정 */
        datum->assumption_reason = strdup(reason);
        if (current_jtre->debugging)
            fprintf(stderr, "\n    Assuming %s via %s.",
                    sexpr_to_string(fact), reason);
        assume_node(node);
        enable_assumption(node);
    } else if (strcmp(reason, datum->assumption_reason) == 0) {
        /* 같은 이유로 이미 가정됨: 무시 */
    } else {
        /* 다른 이유로 다시 가정하려 함: 오류 */
        fprintf(stderr,
                "\nError: Fact %s assumed because of %s "
                "assumed again because of %s\n",
                show_datum(datum), datum->assumption_reason, reason);
    }
    return datum;
}

/* (defun already-assumed? (fact) (datum-assumption? (referent fact t)))
 * 사실이 이미 가정되었는지 확인 */
bool already_assumed(SExpr *fact) {
    Datum *d = referent(fact, true);
    return d && d->assumption_reason != NULL;
}

/* ================================================================ */
/* Retraction (철회)                                                */
/* ================================================================ */

/* (defun retract! (fact &optional (just 'user) (quiet? nil)
 *                      (*JTRE* *JTRE*) &aux datum node)
 *   ...)
 *
 * 가정을 철회
 * just: 철회 요청자 (기본값 "user")
 * quiet: true이면 오류 메시지를 출력하지 않음 */
TmsNode *retract_fact(SExpr *fact, const char *just, bool quiet) {
    if (!just) just = "user";

    Datum *datum = referent(fact, true);
    TmsNode *node = datum->tms_node;

    if (!node->assumption) {
        /* (not (tms-node-assumption? node)) - 가정이 아닌 노드 */
        if (!quiet)
            fprintf(stdout, "\n%s isn't an assumption.",
                    show_datum(datum));
    } else if (!in_node(node)) {
        /* (not (in-node? node)) - 현재 IN이 아닌 가정 */
        if (!quiet)
            fprintf(stdout,
                    "\nThe assumption %s is not currently in.",
                    sexpr_to_string(fact));
    } else if (datum->assumption_reason &&
               strcmp(just, datum->assumption_reason) == 0) {
        /* 철회 요청자가 원래 가정 이유와 일치 */
        if (current_jtre->debugging)
            fprintf(stderr, "\n    Retracting %s via %s.",
                    sexpr_to_string(fact), just);
        free(datum->assumption_reason);
        datum->assumption_reason = NULL;
        retract_assumption(node);
    } else {
        /* 철회 요청자가 원래 가정 이유와 불일치 */
        if (!quiet)
            fprintf(stdout, "\n%s not source of assumption for %s",
                    just, sexpr_to_string(fact));
    }
    return node;
}

/* (defun contradiction (fact &optional (*JTRE* *JTRE*))
 *   (make-contradiction (datum-tms-node (referent fact t))))
 *
 * 사실을 모순으로 선언 */
void contradiction(SExpr *fact) {
    make_contradiction(get_tms_node(fact));
}

/* ================================================================ */
/* Interface and display of data (인터페이스 및 표시)               */
/* ================================================================ */

/* (defun in? (fact &optional (*JTRE* *JTRE*) &aux r)
 *   (when (setq r (referent fact))
 *         (in-node? (datum-tms-node r))))
 *
 * 사실이 IN 상태인지 확인 */
bool in_fact(SExpr *fact) {
    Datum *r = referent(fact, false);
    if (!r) return false;
    return in_node(r->tms_node);
}

/* (defun out? (fact &optional (*JTRE* *JTRE*) &aux r)
 *   (when (setq r (referent fact))
 *         (out-node? (datum-tms-node r))))
 *
 * 사실이 OUT 상태인지 확인 */
bool out_fact(SExpr *fact) {
    Datum *r = referent(fact, false);
    if (!r) return false;
    return out_node(r->tms_node);
}

/* (defun why? (fact &optional (*JTRE* *JTRE*) &aux r)
 *   (when (setq r (referent fact))
 *         (why-node (datum-tms-node r))))
 *
 * 사실의 지지 이유 출력 */
void why_fact(SExpr *fact) {
    Datum *r = referent(fact, false);
    if (r)
        why_node(r->tms_node);
}

/* (defun assumptions-of (fact &optional (*JTRE* *JTRE*))
 *   (mapcar #'view-node
 *     (assumptions-of-node
 *       (datum-tms-node (referent fact *jtre* t)))))
 *
 * 사실의 가정 목록을 view-node로 변환하여 반환 */
List *assumptions_of(SExpr *fact) {
    Datum *d = referent(fact, true);
    List *assumption_nodes = assumptions_of_node(d->tms_node);
    List *result = list_new();
    for (int i = 0; i < assumption_nodes->size; i++) {
        TmsNode *n = (TmsNode *)assumption_nodes->data[i];
        list_push(result, view_node(n));
    }
    return result;
}

/* (defun fetch (pattern &optional (*JTRE* *JTRE*) &aux bindings unifiers)
 *   (dolist (candidate (get-candidates pattern) unifiers)
 *     (setq bindings (unify pattern (datum-lisp-form candidate)))
 *     (unless (eq bindings :FAIL)
 *       (push (sublis bindings pattern) unifiers))))
 *
 * 패턴에 매칭되는 사실 검색 */
List *fetch(SExpr *pattern) {
    List *unifiers = list_new();
    List *candidates = get_candidates(pattern);
    for (int i = 0; i < candidates->size; i++) {
        Datum *candidate = (Datum *)candidates->data[i];
        List *bindings = unify(pattern, candidate->lisp_form);
        if (bindings != NULL) {  /* NULL은 :FAIL에 대응 */
            SExpr *substituted = subst_bindings(bindings, pattern);
            list_push(unifiers, substituted);
        }
    }
    return unifiers;
}

/* ================================================================ */
/* More display-intensive procedures                                */
/* ================================================================ */

/* (defun wfs (fact &optional (*JTRE* *JTRE*))
 *   ;; 잘 설립된 지지(well-founded support) 표시
 *   ...)
 *
 * 사실의 well-founded support를 BFS로 표시 */
void wfs(SExpr *fact) {
    if (out_fact(fact)) {
        fprintf(stdout, "\n %s is OUT.", sexpr_to_string(fact));
        return;
    }

    /* BFS 큐와 방문 집합 */
    List *queue = list_new();
    List *so_far = list_new();
    TmsNode *start = get_tms_node(fact);
    list_push(queue, start);
    list_push(so_far, start);

    while (queue->size > 0) {
        /* dequeue: 큐의 첫 번째 원소 제거 */
        TmsNode *current = (TmsNode *)queue->data[0];
        memmove(&queue->data[0], &queue->data[1],
                (queue->size - 1) * sizeof(void *));
        queue->size--;

        why_node(current);

        /* OUT이거나 가정인 노드는 더 탐색하지 않음 */
        if (out_node(current) || current->assumption)
            continue;

        /* 현재 노드의 지지 정당화의 선행조건들을 큐에 추가 */
        /* (just-antecedents (tms-node-support (car queue))) */
        Just *support = tms_node_support(current);
        if (!support) continue;
        List *antes = support->antecedents;
        for (int i = 0; i < antes->size; i++) {
            TmsNode *ante = (TmsNode *)antes->data[i];
            if (!list_contains(so_far, ante)) {
                list_push(so_far, ante);
                list_push(queue, ante);
            }
        }
    }

    fprintf(stdout, "\n--------");
    list_free(queue);
    list_free(so_far);
}

/* (defun say-datum-belief (pr &optional (*jtre* *jtre*) (indent ""))
 *   (format t "~%~A~A: ~A" indent pr
 *     (if (in-node? (get-tms-node pr *jtre*)) "IN" "OUT")))
 *
 * datum의 믿음 상태를 인덴트와 함께 출력 */
void say_datum_belief(SExpr *pr, const char *indent) {
    if (!indent) indent = "";
    fprintf(stdout, "\n%s%s: %s", indent, sexpr_to_string(pr),
            in_node(get_tms_node(pr)) ? "IN" : "OUT");
}

/* (defun show-justifications (fact &optional (*jtre* *jtre*))
 *   ...)
 *
 * 사실의 모든 정당화를 표시 */
void show_justifications(SExpr *fact) {
    fprintf(stdout, "\n %s::", sexpr_to_string(fact));
    TmsNode *node = get_tms_node(fact);
    List *justs = node->justs;

    if (!justs || justs->size == 0) {
        fprintf(stdout, " No justifications.");
        return;
    }

    for (int i = 0; i < justs->size; i++) {
        Just *j = (Just *)justs->data[i];
        fprintf(stdout, "\n %s", j->informant);
        if (j->antecedents && j->antecedents->size > 0) {
            fprintf(stdout, ", on:");
            for (int k = 0; k < j->antecedents->size; k++) {
                TmsNode *ante = (TmsNode *)j->antecedents->data[k];
                SExpr *ante_form = view_node(ante);
                say_datum_belief(ante_form, "  ");
            }
            fprintf(stdout, ".");
        } else {
            fprintf(stdout, ".");
        }
    }
}

/* (defun show-data (&optional (*JTRE* *JTRE*) (stream *standard-output*))
 *   (format stream "~%~D facts total." (jtre-datum-counter *JTRE*))
 *   (map-dbclass
 *    #'(lambda (dbclass) ...)))
 *
 * 모든 데이터를 표시 */
static void show_data_callback(Dbclass *dbclass, void *ctx) {
    FILE *stream = (FILE *)ctx;
    for (int i = 0; i < dbclass->facts->size; i++) {
        Datum *datum = (Datum *)dbclass->facts->data[i];
        fprintf(stream, "\n%s: %s", show_datum(datum),
                in_node(datum->tms_node) ? "IN" : "OUT");
    }
}

void show_data(FILE *stream) {
    if (!stream) stream = stdout;
    fprintf(stream, "\n%d facts total.", current_jtre->datum_counter);
    map_dbclass(show_data_callback, stream);
}

/* ================================================================ */
/* Database system (데이터베이스 시스템)                            */
/* ================================================================ */

/* (defun get-dbclass (fact &optional (*JTRE* *JTRE*) &aux dbclass)
 *   (cond ((null fact) (error "~% NIL can't be a dbclass."))
 *         ((listp fact) (get-dbclass (car fact) *JTRE*))
 *         ((variable? fact) ...)
 *         ((symbolp fact) ...)
 *         (t (error "Bad dbclass type: ~A" fact))))
 *
 * 사실의 dbclass를 조회하거나 새로 생성
 * 리스트이면 car의 dbclass를, 기호이면 해시 테이블에서 조회 */
Dbclass *get_dbclass(SExpr *fact) {
    if (sexpr_is_nil(fact)) {
        fprintf(stderr, "\nError: NIL can't be a dbclass.\n");
        return NULL;
    }

    /* (listp fact) -> (get-dbclass (car fact) *JTRE*) */
    if (sexpr_is_cons(fact))
        return get_dbclass(fact->cons.car);

    /* (variable? fact) -> 변수 처리 */
    if (sexpr_is_variable(fact)) {
        fprintf(stderr, "\nError: Dbclass unbound: %s\n",
                sexpr_to_string(fact));
        return NULL;
    }

    /* (symbolp fact) -> 해시 테이블에서 조회 또는 생성 */
    if (sexpr_is_symbol(fact)) {
        Dbclass *dbclass = (Dbclass *)hash_table_get(
            current_jtre->dbclass_table, fact->symbol);
        if (dbclass)
            return dbclass;

        /* 새 dbclass 생성 */
        dbclass = (Dbclass *)calloc(1, sizeof(Dbclass));
        dbclass->name = strdup(fact->symbol);
        dbclass->facts = list_new();
        dbclass->rules = list_new();
        dbclass->jtre = current_jtre;
        hash_table_put(current_jtre->dbclass_table,
                       dbclass->name, dbclass);
        return dbclass;
    }

    fprintf(stderr, "\nError: Bad dbclass type: %s\n",
            sexpr_to_string(fact));
    return NULL;
}

/* (defun referent (fact &optional (virtual? nil) (*JTRE* *JTRE*))
 *   (if virtual? (insert fact) (referent1 fact)))
 *
 * 사실의 datum 조회
 * virtual_p가 true이면 없을 때 새로 생성 */
Datum *referent(SExpr *fact, bool virtual_p) {
    if (virtual_p) return insert_datum(fact);
    return referent1(fact);
}

/* (defun referent1 (fact)
 *   (dolist (candidate (dbclass-facts (get-dbclass fact)))
 *     (when (equal (datum-lisp-form candidate) fact)
 *           (return candidate))))
 *
 * 기존 datum만 조회 (생성하지 않음) */
Datum *referent1(SExpr *fact) {
    Dbclass *dc = get_dbclass(fact);
    if (!dc) return NULL;
    for (int i = 0; i < dc->facts->size; i++) {
        Datum *candidate = (Datum *)dc->facts->data[i];
        if (sexpr_equal(candidate->lisp_form, fact))
            return candidate;
    }
    return NULL;
}

/* (defun insert (fact &aux datum)
 *   (setq datum (referent1 fact))
 *   (cond (datum (values datum t))
 *         (t (setq datum (make-datum ...))
 *            (setf (datum-tms-node datum) (tms-create-node ...))
 *            (push datum (dbclass-facts ...))
 *            (try-rules datum)
 *            (values datum nil))))
 *
 * 사실을 데이터베이스에 삽입
 * 이미 존재하면 기존 datum 반환, 아니면 새로 생성 */
Datum *insert_datum(SExpr *fact) {
    Datum *datum = referent1(fact);
    if (datum) return datum;

    /* 새 datum 생성 */
    datum = (Datum *)calloc(1, sizeof(Datum));
    datum->id = ++current_jtre->datum_counter;
    datum->lisp_form = sexpr_copy(fact);
    datum->dbclass = get_dbclass(fact);

    /* TMS 노드 생성 */
    /* (tms-create-node (jtre-jtms *JTRE*) datum) */
    datum->tms_node = tms_create_node(current_jtre->jtms, datum,
                                       false, false);

    /* dbclass의 facts 리스트에 추가 (push) */
    list_push(datum->dbclass->facts, datum);

    /* 관련 규칙 시도 */
    try_rules(datum);

    return datum;
}

/* (defun get-candidates (pattern)
 *   (dbclass-facts (get-dbclass pattern)))
 *
 * 패턴의 후보 datum 리스트 반환 */
List *get_candidates(SExpr *pattern) {
    Dbclass *dc = get_dbclass(pattern);
    if (!dc) return list_new();
    return dc->facts;
}

/* (defun map-dbclass (proc &optional (*JTRE* *JTRE*))
 *   (maphash #'(lambda (name dbclass) ...) (jtre-dbclass-table *JTRE*)))
 *
 * 모든 dbclass에 대해 함수 적용 */
typedef struct {
    void (*proc)(Dbclass *dbclass, void *ctx);
    void *ctx;
} MapDbclassData;

static void map_dbclass_hash_callback(const char *key, void *value,
                                       void *ctx) {
    MapDbclassData *data = (MapDbclassData *)ctx;
    Dbclass *dbclass = (Dbclass *)value;
    data->proc(dbclass, data->ctx);
}

void map_dbclass(void (*proc)(Dbclass *dbclass, void *ctx), void *ctx) {
    MapDbclassData data;
    data.proc = proc;
    data.ctx = ctx;
    hash_table_map(current_jtre->dbclass_table,
                   map_dbclass_hash_callback, &data);
}

/* (defun get-tms-node (fact &optional (*JTRE* *JTRE*))
 *   (datum-tms-node (referent fact t)))
 *
 * 사실의 TMS 노드 반환 */
TmsNode *get_tms_node(SExpr *fact) {
    Datum *d = referent(fact, true);
    return d ? d->tms_node : NULL;
}

/* (defun view-node (node)
 *   (datum-lisp-form (tms-node-datum node)))
 *
 * TMS 노드의 datum lisp_form 반환 */
SExpr *view_node(TmsNode *node) {
    Datum *d = (Datum *)node->datum;
    return d ? d->lisp_form : NULL;
}

/* view-node의 문자열 버전 (JTMS의 node-string으로 사용) */
static char _view_node_buf[512];
char *view_node_string(TmsNode *node) {
    SExpr *form = view_node(node);
    if (form) {
        snprintf(_view_node_buf, sizeof(_view_node_buf), "%s",
                 sexpr_to_string(form));
    } else {
        snprintf(_view_node_buf, sizeof(_view_node_buf),
                 "node-%d", node->index);
    }
    return _view_node_buf;
}

/* ================================================================ */
/* More query routines (추가 질의 루틴)                            */
/* ================================================================ */

/* (defun show-datum (datum)
 *   (format nil "~A" (datum-lisp-form datum)))
 *
 * datum의 문자열 표현 반환 */
char *show_datum(Datum *datum) {
    return sexpr_to_string(datum->lisp_form);
}

/* (defun get-datum (num &optional (*JTRE* *JTRE*))
 *   (map-dbclass
 *    #'(lambda (dbclass)
 *        (dolist (datum (dbclass-facts dbclass))
 *          (when (= (datum-id datum) num)
 *                (return-from GET-DATUM datum))))))
 *
 * ID로 datum 조회 (return-from을 플래그로 대체) */
typedef struct {
    int num;
    Datum *result;
} GetDatumCtx;

static void get_datum_callback(Dbclass *dbclass, void *ctx) {
    GetDatumCtx *gd = (GetDatumCtx *)ctx;
    if (gd->result) return;  /* 이미 찾았으면 종료 */
    for (int i = 0; i < dbclass->facts->size; i++) {
        Datum *datum = (Datum *)dbclass->facts->data[i];
        if (datum->id == gd->num) {
            gd->result = datum;
            return;
        }
    }
}

Datum *get_datum(int num) {
    GetDatumCtx ctx;
    ctx.num = num;
    ctx.result = NULL;
    map_dbclass(get_datum_callback, &ctx);
    return ctx.result;
}

/* (defun get-just (num &optional (*JTRE* *JTRE*))
 *   (dolist (just (jtms-justs (jtre-jtms *JTRE*)))
 *     (when (= (just-index just) num)
 *           (return-from GET-just just))))
 *
 * ID로 정당화 조회 */
Just *get_just(int num) {
    List *justs = current_jtre->jtms->justs;
    for (int i = 0; i < justs->size; i++) {
        Just *j = (Just *)justs->data[i];
        if (j->index == num)
            return j;
    }
    return NULL;
}

/* ================================================================ */
/* Rule system (규칙 시스템) - from jrules.lisp                    */
/* ================================================================ */

/* (defun insert-rule (dbclass matcher body &aux rule)
 *   (let ((*JTRE* (dbclass-jtre dbclass)))
 *     (setq rule (make-rule ...))
 *     (push rule (dbclass-rules dbclass))
 *     (dolist (candidate (dbclass-facts dbclass))
 *       (try-rule-on rule candidate))))
 *
 * 규칙을 dbclass에 등록하고 기존 사실에 대해 시도 */
void insert_rule(Dbclass *dbclass, RuleMatcherFn matcher,
                 RuleBodyFn body) {
    JTRE *saved = with_jtre_save(dbclass->jtre);

    JRule *rule = (JRule *)calloc(1, sizeof(JRule));
    rule->id = ++current_jtre->rule_counter;
    rule->jtre = current_jtre;
    rule->dbclass = dbclass;
    rule->matcher = matcher;
    rule->body = body;

    /* (push rule (dbclass-rules dbclass)) */
    list_push(dbclass->rules, rule);

    /* 기존 사실에 대해 규칙 시도 */
    /* (dolist (candidate (dbclass-facts dbclass))
     *   (try-rule-on rule candidate)) */
    for (int i = 0; i < dbclass->facts->size; i++) {
        Datum *candidate = (Datum *)dbclass->facts->data[i];
        try_rule_on(rule, candidate);
    }

    with_jtre_restore(saved);
}

/* (defun try-rules (datum)
 *   (dolist (rule (dbclass-rules (datum-dbclass datum)))
 *     (try-rule-on rule datum)))
 *
 * datum에 대해 모든 관련 규칙 시도 */
void try_rules(Datum *datum) {
    List *rules = datum->dbclass->rules;
    for (int i = 0; i < rules->size; i++) {
        JRule *rule = (JRule *)rules->data[i];
        try_rule_on(rule, datum);
    }
}

/* (defun try-rule-on (rule datum)
 *   (let ((*JTRE* (dbclass-jtre (datum-dbclass datum))))
 *     (multiple-value-bind (okay? bindings node?)
 *      (funcall (rule-matcher rule) (datum-lisp-form datum))
 *      (when okay?
 *        (when node? (push (datum-tms-node datum) bindings))
 *        (enqueue (cons (rule-body rule) bindings) *JTRE*)))))
 *
 * 특정 규칙을 datum에 시도
 * 매칭에 성공하면 큐에 (body . bindings) 추가 */
void try_rule_on(JRule *rule, Datum *datum) {
    JTRE *saved = with_jtre_save(datum->dbclass->jtre);

    /* (funcall (rule-matcher rule) (datum-lisp-form datum)) */
    MatchResult mr = rule->matcher(datum->lisp_form);
    if (mr.ok) {
        List *bindings = mr.bindings ? mr.bindings : list_new();
        /* node?가 true이면 TMS 노드를 바인딩 앞에 추가 */
        if (mr.node) {
            /* 바인딩 리스트 앞에 TMS 노드 삽입 */
            List *new_bindings = list_new();
            list_push(new_bindings, datum->tms_node);
            for (int i = 0; i < bindings->size; i++)
                list_push(new_bindings, bindings->data[i]);
            if (mr.bindings) list_free(bindings);
            bindings = new_bindings;
        }
        /* (enqueue (cons (rule-body rule) bindings) *JTRE*)
         * 큐에 추가: [body, bindings] 쌍 */
        List *queued = list_new();
        list_push(queued, (void *)rule->body);
        list_push(queued, bindings);
        enqueue(queued, current_jtre);
    }

    with_jtre_restore(saved);
}

/* (defun run-rules (&optional (*JTRE* *JTRE*))
 *   (do ((form (dequeue *JTRE*) (dequeue *JTRE*))
 *        (counter 0 (1+ counter)))
 *       ((null form)
 *        (debugging-jtre "~%    ~A rules run." counter)
 *        (incf (jtre-rules-run *JTRE*) counter))
 *     (apply (car form) (cdr form))))
 *
 * 큐에 있는 규칙들을 모두 실행 */
int run_rules(void) {
    int counter = 0;
    void *form;
    while ((form = dequeue(current_jtre)) != NULL) {
        /* form은 [body, bindings] 리스트 */
        List *queued = (List *)form;
        RuleBodyFn body = (RuleBodyFn)queued->data[0];
        List *bindings = (List *)queued->data[1];
        /* (apply (car form) (cdr form)) */
        body(bindings);
        counter++;
    }

    if (current_jtre->debugging)
        fprintf(stderr, "\n    %d rules run.", counter);

    current_jtre->rules_run += counter;
    return counter;
}

/* (defun rules-waiting? (jtre) (jtre-queue jtre))
 * 대기 중인 규칙이 있는지 확인 */
bool rules_waiting(JTRE *jtre) {
    return jtre->queue && jtre->queue->size > 0;
}

/* (defun enqueue (new j) (push new (jtre-queue j)))
 * 규칙을 큐에 추가 (push = prepend) */
void enqueue(void *item, JTRE *jtre) {
    /* push는 앞에 추가하지만, 큐 동작을 위해 뒤에 추가 */
    /* Lisp의 push는 스택처럼 동작하므로 앞에 추가 */
    /* 원래 Lisp에서 enqueue는 push이고 dequeue는 pop이므로
     * LIFO(스택) 동작임 */
    List *q = jtre->queue;
    /* 앞에 삽입: 배열을 한 칸 뒤로 밀고 앞에 넣기 */
    if (q->size >= q->cap) {
        int new_cap = q->cap == 0 ? 8 : q->cap * 2;
        q->data = (void **)realloc(q->data, new_cap * sizeof(void *));
        q->cap = new_cap;
    }
    memmove(&q->data[1], &q->data[0], q->size * sizeof(void *));
    q->data[0] = item;
    q->size++;
}

/* (defun dequeue (jtre) (pop (jtre-queue jtre)))
 * 큐에서 규칙 제거 (pop = 앞에서 제거) */
void *dequeue(JTRE *jtre) {
    List *q = jtre->queue;
    if (!q || q->size == 0) return NULL;
    void *item = q->data[0];
    memmove(&q->data[0], &q->data[1],
            (q->size - 1) * sizeof(void *));
    q->size--;
    return item;
}

/* ================================================================ */
/* Rule display (규칙 표시)                                        */
/* ================================================================ */

/* (defun show-rules (&optional (*JTRE* *JTRE*) (stream *standard-output*))
 *   (format t "~%There are ~D rules in ~A:"
 *     (jtre-rule-counter *JTRE*) (jtre-title *JTRE*))
 *   (format stream "~% ~A queued." ...)
 *   (map-dbclass #'(lambda (dbclass) ...)))
 *
 * 모든 규칙 표시 */
static void show_rules_callback(Dbclass *dbclass, void *ctx) {
    FILE *stream = (FILE *)ctx;
    for (int i = 0; i < dbclass->rules->size; i++) {
        JRule *rule = (JRule *)dbclass->rules->data[i];
        fprintf(stream, "\n <Rule %d>", rule->id);
    }
}

void show_rules(FILE *stream) {
    if (!stream) stream = stdout;
    fprintf(stream, "\nThere are %d rules in %s:",
            current_jtre->rule_counter, current_jtre->title);
    if (!current_jtre->queue || current_jtre->queue->size == 0)
        fprintf(stream, "\n None queued.");
    else
        fprintf(stream, "\n %d queued.", current_jtre->queue->size);
    map_dbclass(show_rules_callback, stream);
}

/* (defun get-rule (num &optional (*JTRE* *JTRE*))
 *   (map-dbclass #'(lambda (dbclass) ...)))
 *
 * ID로 규칙 조회 (return-from을 플래그로 대체) */
typedef struct {
    int num;
    JRule *result;
} GetRuleCtx;

static void get_rule_callback(Dbclass *dbclass, void *ctx) {
    GetRuleCtx *gr = (GetRuleCtx *)ctx;
    if (gr->result) return;
    for (int i = 0; i < dbclass->rules->size; i++) {
        JRule *rule = (JRule *)dbclass->rules->data[i];
        if (rule->id == gr->num) {
            gr->result = rule;
            return;
        }
    }
}

JRule *get_rule(int num) {
    GetRuleCtx ctx;
    ctx.num = num;
    ctx.result = NULL;
    map_dbclass(get_rule_callback, &ctx);
    return ctx.result;
}
