/* -*- C -*- */

/* JTRE (JTMS-based Tiny Rule Engine) - Implementation */
/* Converted from jinter.lisp */

/* Copyright (c) 1989 -- 1992 Kenneth D. Forbus, Northwestern University, */
/* Johan de Kleer and Xerox Corporation. All rights reserved. */

#include "jtre.h"
#include "jdata.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ================================================================ */
/* *JTRE* 전역 변수                                                 */
/* ================================================================ */

JTRE *current_jtre = NULL;

/* ================================================================ */
/* Hash table implementation                                        */
/* 간단한 배열 기반 해시 맵 (체이닝 사용)                           */
/* ================================================================ */

static unsigned int hash_string(const char *str) {
    unsigned int hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + (unsigned int)c;  /* hash * 33 + c */
    return hash % HASH_TABLE_SIZE;
}

HashTable *hash_table_new(void) {
    HashTable *ht = (HashTable *)calloc(1, sizeof(HashTable));
    return ht;
}

void hash_table_free(HashTable *ht) {
    if (!ht) return;
    for (int i = 0; i < HASH_TABLE_SIZE; i++) {
        HashEntry *e = ht->buckets[i];
        while (e) {
            HashEntry *next = e->next;
            free(e);
            e = next;
        }
    }
    free(ht);
}

void *hash_table_get(HashTable *ht, const char *key) {
    unsigned int idx = hash_string(key);
    HashEntry *e = ht->buckets[idx];
    while (e) {
        if (strcmp(e->key, key) == 0)
            return e->value;
        e = e->next;
    }
    return NULL;
}

void hash_table_put(HashTable *ht, const char *key, void *value) {
    unsigned int idx = hash_string(key);
    /* 기존 엔트리가 있으면 값 갱신 */
    HashEntry *e = ht->buckets[idx];
    while (e) {
        if (strcmp(e->key, key) == 0) {
            e->value = value;
            return;
        }
        e = e->next;
    }
    /* 새 엔트리 추가 (체이닝) */
    HashEntry *ne = (HashEntry *)calloc(1, sizeof(HashEntry));
    ne->key = key;  /* key는 외부에서 관리 (strdup된 문자열) */
    ne->value = value;
    ne->next = ht->buckets[idx];
    ht->buckets[idx] = ne;
}

/* hash_table_map: 모든 엔트리에 대해 함수 호출 */
/* maphash 에 대응 */
void hash_table_map(HashTable *ht,
                    void (*fn)(const char *key, void *value, void *ctx),
                    void *ctx) {
    for (int i = 0; i < HASH_TABLE_SIZE; i++) {
        HashEntry *e = ht->buckets[i];
        while (e) {
            fn(e->key, e->value, ctx);
            e = e->next;
        }
    }
}

/* ================================================================ */
/* JTRE printer                                                     */
/* (defun jtre-printer (j st ignore)                                */
/*   (format st "<JTRE: ~A>" (jtre-title j)))                      */
/* ================================================================ */

static void jtre_printer(JTRE *j, FILE *stream) {
    fprintf(stream, "<JTRE: %s>", j->title);
}

/* ================================================================ */
/* enqueue callback for JTMS                                        */
/* JTMS의 enqueue-procedure에 설정되는 콜백                        */
/* 규칙이 트리거되면 JTRE의 큐에 추가                              */
/* ================================================================ */

static JTRE *_enqueue_target_jtre = NULL;

static void jtre_enqueue_callback(void *rule) {
    if (_enqueue_target_jtre)
        enqueue(rule, _enqueue_target_jtre);
}

/* ================================================================ */
/* JTRE creation                                                    */
/* (defun create-jtre (title &key debugging) ...)                   */
/* JTRE 생성: JTMS와 dbclass 해시 테이블을 초기화                  */
/* ================================================================ */

JTRE *create_jtre(const char *title, bool debugging) {
    JTRE *j = (JTRE *)calloc(1, sizeof(JTRE));
    j->title = strdup(title);

    /* JTMS 생성: (create-jtms (list :JTMS-OF title) :NODE-STRING 'view-node) */
    char jtms_title[256];
    snprintf(jtms_title, sizeof(jtms_title), "JTMS-OF %s", title);
    j->jtms = create_jtms(jtms_title, view_node_string, false, true, NULL);

    /* dbclass 해시 테이블 생성 */
    j->dbclass_table = hash_table_new();
    j->debugging = debugging;
    j->queue = list_new();
    j->datum_counter = 0;
    j->rule_counter = 0;
    j->rules_run = 0;

    /* JTMS의 enqueue-procedure를 설정 */
    /* (change-jtms (jtre-jtms j) :ENQUEUE-PROCEDURE
     *   #'(lambda (rule) (enqueue rule j))) */
    _enqueue_target_jtre = j;
    change_jtms(j->jtms, NULL, jtre_enqueue_callback, -1, -1, NULL);

    return j;
}

/* ================================================================ */
/* JTRE configuration                                               */
/* (defun change-jtre (jtre &key (debugging :NADA)) ...)            */
/* debugging이 -1이면 변경하지 않음                                */
/* ================================================================ */

void change_jtre(JTRE *jtre, int debugging) {
    if (debugging >= 0)
        jtre->debugging = (bool)debugging;
}

/* ================================================================ */
/* In-Jtre: 현재 JTRE를 설정                                       */
/* (defun In-Jtre (jtre) (setq *JTRE* jtre))                       */
/* ================================================================ */

void in_jtre(JTRE *jtre) {
    current_jtre = jtre;
}

/* ================================================================ */
/* With-Jtre save/restore 패턴                                     */
/* (defmacro With-Jtre (jtre &rest forms)                           */
/*   `(let ((*JTRE* ,jtre)) ,@ forms))                             */
/* C에서는 save/restore 패턴으로 구현                              */
/* ================================================================ */

JTRE *with_jtre_save(JTRE *jtre) {
    JTRE *saved = current_jtre;
    current_jtre = jtre;
    return saved;
}

void with_jtre_restore(JTRE *saved) {
    current_jtre = saved;
}

/* ================================================================ */
/* debugging-jtre 매크로 대응                                       */
/* (defmacro debugging-jtre (msg &rest args)                        */
/*   `(when (jtre-debugging *JTRE*) (format t ,msg ,@args)))       */
/* ================================================================ */

/* 디버깅 메시지는 각 함수 내에서 직접 호출:
 *   if (current_jtre->debugging)
 *       fprintf(stderr, ...);
 */

/* ================================================================ */
/* Running JTRE                                                     */
/* ================================================================ */

/* (defun uassert! (fact &optional (just 'user))
 *   (assert! fact just)
 *   (run-rules *JTRE*))
 * 사실을 assert하고 규칙을 실행 */
void uassert(SExpr *fact, SExpr *just) {
    /* just가 NULL이면 기본값 "user" 사용 */
    if (!just)
        just = sexpr_symbol("user");
    assert_fact(fact, just);    /* 내부 연산 수행 */
    run_rules();                /* 규칙 실행 */
}

/* (defun uassume! (fact reason)
 *   (assume! fact reason *JTRE*)
 *   (run-rules *JTRE*))
 * 사실을 assume하고 규칙을 실행 */
void uassume(SExpr *fact, const char *reason) {
    assume_fact(fact, reason);
    run_rules();
}

/* (defun run-forms (forms &optional (*JTRE* *JTRE*))
 *   (dolist (form forms) (eval form) (run-rules *JTRE*)))
 * 폼 리스트를 실행하고 각 폼 후 규칙 실행
 * C에서는 함수 포인터 리스트로 구현 */
void run_forms(List *forms) {
    /* forms는 void (*)(void) 함수 포인터의 리스트로 가정 */
    for (int i = 0; i < forms->size; i++) {
        void (*fn)(void) = (void (*)(void))forms->data[i];
        if (fn) fn();
        run_rules();
    }
}

/* (defun show (&optional (*JTRE* *JTRE*) (stream *standard-output*))
 *   (show-data *JTRE* stream) (show-rules *JTRE* stream))
 * 데이터와 규칙 모두 출력 */
void show(JTRE *jtre, FILE *stream) {
    JTRE *saved = NULL;
    if (jtre) saved = with_jtre_save(jtre);
    if (!stream) stream = stdout;

    show_data(stream);
    show_rules(stream);

    if (jtre) with_jtre_restore(saved);
}
