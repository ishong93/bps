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

#include "jsaint.h"
#include "jsops.h"
#include "jsrules.h"

/* ================================================================ */
/* 전역 JSAINT 포인터                                               */
/* ================================================================ */

Jsaint *current_jsaint = NULL;

/* ================================================================ */
/* 테스트 문제들                                                    */
/* ================================================================ */

const char *problem1 = "(Integrate (Integral 1 x))";
const char *problem2 = "(Integrate (Integral (+ x 5) x))";
const char *problem3 = "(Integrate (Integral (* 46 (log x %e)) x))";
const char *problem4 = "(Integrate (Integral (+ 0.63 (* 3.2 (sin (* 1.7 x))) (* 4 (expt %e (* 2 x)))) x))";

/* ================================================================ */
/* 아젠다 항목 생성/해제                                            */
/* ================================================================ */

/* 새 아젠다 항목 생성 */
AgendaEntry *agenda_entry_new(int difficulty, const char *subproblem) {
    AgendaEntry *entry = (AgendaEntry *)malloc(sizeof(AgendaEntry));
    entry->difficulty = difficulty;
    entry->subproblem = subproblem;
    return entry;
}

/* 아젠다 항목 해제 */
void agenda_entry_free(AgendaEntry *entry) {
    free(entry);
}

/* 아젠다에 항목을 난이도 순으로 삽입 (정렬된 삽입) */
/* (merge 'list (list entry) agenda #'(lambda (a b) (< (car a) (car b)))) */
List *agenda_insert_sorted(List *agenda, AgendaEntry *entry) {
    /* 빈 아젠다이거나 새 항목이 첫 항목보다 쉬운 경우 */
    if (agenda == NULL ||
        entry->difficulty < ((AgendaEntry *)agenda->data)->difficulty) {
        return list_prepend(agenda, entry);
    }

    /* 적절한 위치 찾기 */
    List *prev = agenda;
    List *curr = agenda->next;
    while (curr != NULL &&
           ((AgendaEntry *)curr->data)->difficulty <= entry->difficulty) {
        prev = curr;
        curr = curr->next;
    }

    /* prev 뒤에 삽입 */
    List *node = (List *)malloc(sizeof(List));
    node->data = entry;
    node->next = curr;
    prev->next = node;
    return agenda;
}

/* ================================================================ */
/* JSAINT 생성 및 관리                                              */
/* ================================================================ */

/* 새 JSAINT 인스턴스 생성 */
/*
 * (defun create-jsaint (title problem &key (debugging nil) (max-tasks nil))
 *   (let ((ag (make-jsaint ...)))
 *     (in-jtre (jsaint-jtre ag))
 *     (change-jtms (jtre-jtms (jsaint-jtre ag))
 *                  :CONTRADICTION-HANDLER #'jsaint-contradiction-handler)
 *     (use-jsaint ag)))
 */
Jsaint *create_jsaint(const char *title, const char *problem,
                      int debugging, int max_tasks) {
    Jsaint *js = (Jsaint *)malloc(sizeof(Jsaint));
    js->title = title;
    js->problem = problem;
    js->solution = NULL;
    js->solution_result = SOLVE_NOT_YET;
    js->agenda = NULL;
    js->n_subproblems = 0;
    js->max_tasks = (max_tasks > 0) ? max_tasks : 20;
    js->debugging = debugging;

    /* 연결된 JTRE 생성 */
    /* (concatenate 'string "JTRE of " title) */
    char jtre_title[256];
    snprintf(jtre_title, sizeof(jtre_title), "JTRE of %s", title);
    js->jtre = create_jtre(jtre_title, 0);

    /* JTRE를 현재로 설정 */
    in_jtre(js->jtre);

    /* 모순 처리 핸들러 설정 */
    change_jtms(js->jtre->jtms,
                jsaint_contradiction_handler,
                NULL, NULL, -1, -1);

    use_jsaint(js);
    return js;
}

/* 현재 사용 중인 JSAINT를 설정 */
/* (defun use-jsaint (js) (setq *jsaint* js)) */
void use_jsaint(Jsaint *js) {
    current_jsaint = js;
}

/* JSAINT 설정 변경 */
/* -1을 전달하면 해당 항목을 변경하지 않음 */
/*
 * (defun change-jsaint (js &key (debugging :NADA) (problem :NADA) (max-tasks :NADA))
 *   (unless (eq debugging :NADA) (setf (jsaint-debugging js) debugging))
 *   (unless (eq problem :NADA) (setf (jsaint-problem js) problem))
 *   (unless (eq max-tasks :NADA) (setf (jsaint-max-tasks js) max-tasks)))
 */
void change_jsaint(Jsaint *js, int debugging, const char *problem,
                   int max_tasks) {
    if (debugging >= 0) js->debugging = debugging;
    if (problem != NULL) js->problem = problem;
    if (max_tasks >= 0) js->max_tasks = max_tasks;
}

/* ================================================================ */
/* 사용자 진입점                                                    */
/* ================================================================ */

/*
 * (defun solve-integral (integral &key ...)
 *   (setq integral (eval (quotize (simplifying-form-of integral))))
 *   (use-jsaint (create-jsaint title integral ...))
 *   (queue-problem (jsaint-problem *jsaint*) nil)
 *   (with-JTRE (jsaint-jtre *jsaint*)
 *     (load *jsaint-rules*)
 *     (load *jsaint-operators*))
 *   (run-jsaint *jsaint*))
 */
SolveResult solve_integral(const char *integral, const char *title,
                           int debugging, int max_tasks) {
    /* 중복 제거 및 정규화 입력 */
    /* 원래 Lisp: (eval (quotize (simplifying-form-of integral))) */
    /* C에서는 입력이 이미 문자열이므로 그대로 사용 */
    const char *canonical = integral;

    /* JSAINT 생성 */
    use_jsaint(create_jsaint(title, canonical, debugging, max_tasks));

    /* 초기 문제를 큐에 넣기 */
    queue_problem(current_jsaint->problem, NULL, current_jsaint);

    /* JTRE 컨텍스트에서 규칙과 연산자 로드 */
    {
        JTRE *saved = with_jtre_save(current_jsaint->jtre);
        load_jsaint_rules(current_jsaint);
        load_jsaint_operators(current_jsaint);
        with_jtre_restore(saved);
    }

    /* 실행 */
    return run_jsaint(current_jsaint);
}

/* 풀이 결과 설명 출력 */
/*
 * (defun explain-result (&optional (*jsaint* *jsaint*))
 *   (cond ((null (jsaint-solution *jsaint*)) ...)
 *         ((eq ... :FAILED-PROBLEM) ...)
 *         ((eq ... :FAILED-EMPTY) ...)
 *         (t ...)))
 */
void explain_result(Jsaint *js) {
    if (js == NULL) js = current_jsaint;
    if (js == NULL) return;

    switch (js->solution_result) {
    case SOLVE_NOT_YET:
        printf("\n Problem not solved yet.");
        break;
    case SOLVE_FAILED_PROBLEM: {
        /* (explore-network (get-tms-node `(failed ,(jsaint-problem *jsaint*)) ...)) */
        char buf[512];
        snprintf(buf, sizeof(buf), "(failed %s)", js->problem);
        TmsNode *node = tms_create_node(js->jtre->jtms, buf, 0, 0);
        explore_network(node);
        printf("\n Failed to find a solution.");
        break;
    }
    case SOLVE_FAILED_EMPTY: {
        printf("\n Ran out of things to do.");
        char buf[512];
        snprintf(buf, sizeof(buf), "(failed %s)", js->problem);
        TmsNode *node = tms_create_node(js->jtre->jtms, buf, 0, 0);
        explore_network(node);
        break;
    }
    case SOLVE_SOLVED: {
        printf("\n Solved the problem:");
        char buf[512];
        snprintf(buf, sizeof(buf), "(solution-of %s %s)",
                 js->problem, js->solution);
        TmsNode *node = tms_create_node(js->jtre->jtms, buf, 0, 0);
        explore_network(node);
        break;
    }
    case SOLVE_TIME_OUT:
        printf("\n Timed out.");
        break;
    }
}

/* ================================================================ */
/* 기본 알고리즘                                                    */
/* ================================================================ */

/*
 * (defun run-jsaint (*jsaint*)
 *   (when (jsaint-solution *jsaint*)
 *     (return-from run-jsaint (values (jsaint-solution *jsaint*) *jsaint*)))
 *   (when (> (jsaint-n-subproblems *jsaint*) (jsaint-max-tasks *jsaint*))
 *     (return-from run-jsaint (values :TIME-OUT *jsaint*)))
 *   (do ((done? nil)
 *        (solution ...) (failure-signal ...))
 *       (done? (values (jsaint-solution *jsaint*) *jsaint*))
 *     (cond (solution ...)
 *           ((in? failure-signal ...) ...)
 *           ((null (jsaint-agenda *jsaint*)) ...)
 *           (t (process-subproblem (cdr (pop (jsaint-agenda *jsaint*))))))))
 */
SolveResult run_jsaint(Jsaint *js) {
    const char *solution;
    char failure_signal[512];

    /* return-from: 이미 풀렸으면 재풀이하지 않음 */
    if (js->solution != NULL) {
        return js->solution_result;
    }

    /* return-from: 자원 한계 존중 */
    if (js->n_subproblems > js->max_tasks) {
        js->solution_result = SOLVE_TIME_OUT;
        return SOLVE_TIME_OUT;
    }

    /* 실패 신호 패턴: (Failed (Integrate <problem>)) */
    snprintf(failure_signal, sizeof(failure_signal),
             "(Failed %s)", js->problem);

    for (;;) {
        solution = fetch_solution(js->problem, js);

        if (solution != NULL) {
            /* 풀이를 찾음 */
            js->solution = solution;
            js->solution_result = SOLVE_SOLVED;
            DEBUGGING_JSAINT(js, "\n %s: Solved original problem.",
                             js->title);
            return SOLVE_SOLVED;
        }

        /* (in? failure-signal (jsaint-jtre *jsaint*)) */
        /* 실패 신호가 IN인지 확인 */
        /* TODO: JTRE의 in? 함수를 호출하여 확인 */
        /* 여기서는 fetch를 사용하여 확인 */
        /*
         * if (in_fact(failure_signal, js->jtre)) {
         */
        {
            /* 간단한 근사: 실패 신호를 fetch하여 확인 */
            int failed = 0;
            /* TODO: 실제 in? 확인 구현 필요 */
            if (failed) {
                DEBUGGING_JSAINT(js, "\n %s: Failed on original problem.",
                                 js->title);
                js->solution_result = SOLVE_FAILED_PROBLEM;
                return SOLVE_FAILED_PROBLEM;
            }
        }

        if (js->agenda == NULL) {
            DEBUGGING_JSAINT(js, "\n %s: Agenda empty.", js->title);
            js->solution_result = SOLVE_FAILED_EMPTY;
            return SOLVE_FAILED_EMPTY;
        }

        /* (process-subproblem (cdr (pop (jsaint-agenda *jsaint*)))) */
        {
            AgendaEntry *entry = (AgendaEntry *)list_pop(&js->agenda);
            process_subproblem(entry->subproblem, js);
            agenda_entry_free(entry);
        }
    }
}

/* 하위 문제 처리 */
/*
 * (defun process-subproblem (item &aux (jtre (jsaint-jtre *jsaint*))
 *                            (suggestions nil))
 *   (debugging-jsaint *jsaint* "~%  Trying to solve ~A." item)
 *   (open-subproblem item)
 *   (when (fetch-solution item *jsaint*)
 *         (return-from process-subproblem T))
 *   (when (some ...) ;; Already expanded
 *         (return-from process-subproblem T))
 *   (dolist (suggestion (fetch `(SUGGEST-FOR ,item ?operator) jtre))
 *     (when (in? suggestion jtre)
 *       (queue-problem `(try ,(third suggestion)) item)
 *       (push `(try ,(third suggestion)) suggestions)))
 *   (assert! `(OR-SUBGOALS ,item ,suggestions) :OR-SUBGOALS jtre)
 *   (run-rules jtre))
 */
void process_subproblem(const char *item, Jsaint *js) {
    JTRE *jtre = js->jtre;
    List *suggestions = NULL;
    char buf[512];

    DEBUGGING_JSAINT(js, "\n  Trying to solve %s.", item);

    /* 하위 문제 열기 */
    open_subproblem(item, js);

    /* 이미 풀렸는지 확인 */
    /* (when (fetch-solution item *jsaint*) (return-from ...)) */
    if (fetch_solution(item, js) != NULL) {
        DEBUGGING_JSAINT(js, "\n    ..already solved.");
        return;
    }

    /* 이미 확장되었는지 확인 */
    /* (when (some #'(lambda (f) (in? f jtre))
     *       (fetch `(AND-SUBGOALS ,item ?subproblems) jtre)) ...) */
    snprintf(buf, sizeof(buf), "(AND-SUBGOALS %s ?subproblems)", item);
    {
        /* TODO: fetch 및 in? 확인 구현 */
        /* List *and_subgoals = fetch(buf, jtre); */
        /* ... */
        DEBUGGING_JSAINT(js, "\n   ..checking for existing expansions.");
    }

    /* 제안된 연산자들 처리 */
    /* (dolist (suggestion (fetch `(SUGGEST-FOR ,item ?operator) jtre)) ...) */
    snprintf(buf, sizeof(buf), "(SUGGEST-FOR %s ?operator)", item);
    {
        /* TODO: fetch 구현 후 실제 제안 처리 */
        /* List *sugg_list = fetch(buf, jtre); */
        /* for (List *p = sugg_list; p; p = p->next) { */
        /*     ... */
        /* } */
    }

    /* (assert! `(OR-SUBGOALS ,item ,suggestions) :OR-SUBGOALS jtre) */
    /* TODO: assert! 호출 */

    /* (run-rules jtre) */
    /* TODO: run_rules(jtre); */
}

/* 하위 문제 열기 */
/*
 * (defun open-subproblem (item &aux (jtre (jsaint-jtre *jsaint*)))
 *   (assert! `(expanded ,item) :EXPAND-AGENDA-ITEM jtre)
 *   (assume! `(open ,item) :EXPAND-AGENDA-ITEM jtre)
 *   (run-rules jtre))
 */
void open_subproblem(const char *item, Jsaint *js) {
    JTRE *jtre = js->jtre;
    char buf[512];

    /* (assert! `(expanded ,item) :EXPAND-AGENDA-ITEM jtre) */
    snprintf(buf, sizeof(buf), "(expanded %s)", item);
    /* TODO: assert_fact(buf, "EXPAND-AGENDA-ITEM", jtre); */

    /* (assume! `(open ,item) :EXPAND-AGENDA-ITEM jtre) */
    snprintf(buf, sizeof(buf), "(open %s)", item);
    /* TODO: assume_fact(buf, "EXPAND-AGENDA-ITEM", jtre); */

    /* 빠른 승리, 추가 결과 탐색 */
    /* (run-rules jtre) */
    /* TODO: run_rules(jtre); */
}

/* ================================================================ */
/* 큐 관리                                                          */
/* ================================================================ */

/*
 * (defun queue-problem (problem parent &aux entry)
 *   (setq entry (cons (estimate-difficulty problem) problem))
 *   (debugging-jsaint ...)
 *   (setf (jsaint-agenda *jsaint*)
 *         (merge 'list (list entry) (jsaint-agenda *jsaint*)
 *                #'(lambda (a b) (< (car a) (car b))))))
 */
void queue_problem(const char *problem, const char *parent, Jsaint *js) {
    int difficulty = estimate_difficulty(problem);
    AgendaEntry *entry = agenda_entry_new(difficulty, problem);

    DEBUGGING_JSAINT(js, "\n   Queueing %s, difficulty = %d",
                     problem, difficulty);

    js->agenda = agenda_insert_sorted(js->agenda, entry);
    js->n_subproblems++;
}

/*
 * (defun estimate-difficulty (problem)
 *   (+ (max-depth problem) (count-symbols problem)))
 */
int estimate_difficulty(const char *problem) {
    return max_depth(problem) + count_symbols(problem);
}

/*
 * (defun count-symbols (pr)
 *   (cond ((null pr) 0)
 *         ((listp pr) (reduce #'+ (mapcar #'count-symbols pr) :INITIAL-VALUE 0))
 *         (t 1)))
 *
 * S-expression 문자열에서 심볼(토큰) 수를 세기
 * 괄호와 공백은 세지 않고, 심볼/숫자 토큰만 셈
 */
int count_symbols(const char *pr) {
    if (pr == NULL) return 0;

    int count = 0;
    int in_token = 0;
    for (const char *p = pr; *p != '\0'; p++) {
        if (*p == '(' || *p == ')' || *p == ' ' ||
            *p == '\t' || *p == '\n') {
            if (in_token) {
                count++;
                in_token = 0;
            }
        } else {
            in_token = 1;
        }
    }
    if (in_token) count++;
    return count;
}

/*
 * (defun max-depth (pr)
 *   (cond ((not (listp pr)) 1)
 *         (t (1+ (reduce #'max (mapcar #'max-depth pr) :INITIAL-VALUE 0)))))
 *
 * S-expression 문자열에서 최대 괄호 깊이 계산
 */
int max_depth(const char *pr) {
    if (pr == NULL) return 1;

    int max_d = 0;
    int current_d = 0;
    for (const char *p = pr; *p != '\0'; p++) {
        if (*p == '(') {
            current_d++;
            if (current_d > max_d) max_d = current_d;
        } else if (*p == ')') {
            current_d--;
        }
    }
    return (max_d > 0) ? max_d : 1;
}

/* ================================================================ */
/* 보조 루틴                                                        */
/* ================================================================ */

/*
 * (defun fetch-solution (problem &optional (*jsaint* *jsaint*) ...)
 *   (dolist (solution (fetch `(SOLUTION-OF ,problem ?answer) jtre))
 *     (when (in? solution jtre)
 *       (return-from fetch-solution (third solution)))))
 */
const char *fetch_solution(const char *problem, Jsaint *js) {
    if (js == NULL) js = current_jsaint;
    if (js == NULL) return NULL;

    /* (fetch `(SOLUTION-OF ,problem ?answer) jtre) */
    char pattern[512];
    snprintf(pattern, sizeof(pattern), "(SOLUTION-OF %s ?answer)", problem);

    /* TODO: JTRE의 fetch 함수 호출 후 in? 확인 */
    /* List *solutions = fetch(pattern, js->jtre); */
    /* for (List *p = solutions; p; p = p->next) { */
    /*     if (in_fact(p->data, js->jtre)) { */
    /*         return third(p->data); */
    /*     } */
    /* } */

    return NULL;
}

/*
 * (defun jsaint-contradiction-handler (contradictions jtms)
 *   (ask-user-hander contradictions jtms))
 */
void jsaint_contradiction_handler(JTMS *jtms, List *contradictions) {
    ask_user_handler(jtms, contradictions);
}

/* ================================================================ */
/* 질의/표시                                                        */
/* ================================================================ */

/*
 * (defun show-problem (pr &optional (*jsaint* *jsaint*) ...)
 *   ... 문제의 부모, 확장 여부, 열림/닫힘, 관련성, 풀이/실패 상태,
 *       AND/OR 하위 목표들을 출력 ...)
 */
void show_problem(const char *pr, Jsaint *js) {
    if (js == NULL) js = current_jsaint;
    if (js == NULL) return;

    printf("\n%s:: (%d)", pr, estimate_difficulty(pr));

    JTRE *jtre = js->jtre;

    /* (setq stuff (fetch `(parent-of ,pr ?x ?type))) */
    char buf[512];
    snprintf(buf, sizeof(buf), "(parent-of %s ?x ?type)", pr);
    /* TODO: List *stuff = fetch(buf, jtre); */
    /* 부모 정보 출력 */
    printf("\n No parents found.");

    /* 확장 여부 */
    snprintf(buf, sizeof(buf), "(expanded %s)", pr);
    /* TODO: if (fetch(buf, jtre)) ... */
    printf("\n Not expanded,");

    /* 열림/닫힘 여부 */
    snprintf(buf, sizeof(buf), "(open %s)", pr);
    /* TODO: if (fetch(buf, jtre) && in_fact(buf, jtre)) ... */
    printf(" not opened,");

    /* 관련성 */
    snprintf(buf, sizeof(buf), "(relevant %s)", pr);
    /* TODO: if (in_fact(buf, jtre)) ... */
    printf(" not relevant.");

    /* 풀이/실패 상태 */
    const char *sol = fetch_solution(pr, js);
    if (sol != NULL) {
        printf("\n Solved, solution = %s", sol);
    } else {
        /* 실패 확인 */
        snprintf(buf, sizeof(buf), "(failed %s)", pr);
        /* TODO: if (in_fact(buf, jtre)) ... */
        printf("\n Neither solved nor failed.");
    }

    /* AND 하위 목표들 */
    snprintf(buf, sizeof(buf), "(and-subgoals %s ?ands)", pr);
    /* TODO: List *ands = fetch(buf, jtre); */

    /* OR 하위 목표들 */
    snprintf(buf, sizeof(buf), "(or-subgoals %s ?ors)", pr);
    /* TODO: List *ors = fetch(buf, jtre); */
}

/* ================================================================ */
/* AND/OR 그래프 텍스트 표시                                        */
/* ================================================================ */

/* 깊이 테이블 항목 */
typedef struct DepthEntry {
    const char *problem;
    int depth;
} DepthEntry;

/* 깊이 테이블에서 문제 검색 */
static DepthEntry *find_depth_entry(List *depths, const char *problem) {
    for (List *p = depths; p != NULL; p = p->next) {
        DepthEntry *entry = (DepthEntry *)p->data;
        if (strcmp(entry->problem, problem) == 0) return entry;
    }
    return NULL;
}

/* 경로에 문제가 있는지 확인 (순환 검사) */
static int in_path(List *path, const char *problem) {
    for (List *p = path; p != NULL; p = p->next) {
        if (strcmp((const char *)p->data, problem) == 0) return 1;
    }
    return 0;
}

/*
 * (defun update-ao-depth-table (now depth depths path)
 *   (incf depth)
 *   (dolist (child (get-children now) depths)
 *    (unless (member child path :TEST 'equal)
 *     (let ((entry (assoc child depths :TEST 'equal)))
 *       (unless entry
 *               (push (setq entry (cons child 0)) depths))
 *       (when (> depth (cdr entry))
 *             (setf (cdr entry) depth)
 *             (setq depths (update-ao-depth-table
 *                           child depth depths (cons child path))))))))
 */
List *update_ao_depth_table(const char *now, int depth,
                            List *depths, List *path) {
    depth++;
    List *children = get_children(now, current_jsaint);

    for (List *p = children; p != NULL; p = p->next) {
        const char *child = (const char *)p->data;
        if (in_path(path, child)) continue; /* 순환 방지 */

        DepthEntry *entry = find_depth_entry(depths, child);
        if (entry == NULL) {
            entry = (DepthEntry *)malloc(sizeof(DepthEntry));
            entry->problem = child;
            entry->depth = 0;
            depths = list_prepend(depths, entry);
        }
        if (depth > entry->depth) {
            entry->depth = depth;
            List *new_path = list_prepend(list_copy(path), (void *)child);
            depths = update_ao_depth_table(child, depth, depths, new_path);
            list_free(new_path);
        }
    }

    list_free(children);
    return depths;
}

/* 깊이 비교 함수 (qsort용) */
static int depth_compare(const void *a, const void *b) {
    DepthEntry *ea = *(DepthEntry **)a;
    DepthEntry *eb = *(DepthEntry **)b;
    return ea->depth - eb->depth;
}

/*
 * (defun show-ao-graph (&optional (*jsaint* *jsaint*))
 *   (let* ((problems (get-problems))
 *          (depth-table (update-ao-depth-table ...)))
 *     (setq depth-table (sort depth-table ...))
 *     (dolist (pair depth-table) ...)))
 */
void show_ao_graph(Jsaint *js) {
    if (js == NULL) js = current_jsaint;
    if (js == NULL) return;

    /* 초기 깊이 테이블 */
    DepthEntry *root_entry = (DepthEntry *)malloc(sizeof(DepthEntry));
    root_entry->problem = js->problem;
    root_entry->depth = 0;
    List *depths = list_prepend(NULL, root_entry);
    List *path = list_prepend(NULL, (void *)js->problem);

    depths = update_ao_depth_table(js->problem, 0, depths, path);
    list_free(path);

    /* 리스트를 배열로 변환하여 정렬 */
    int n = list_length(depths);
    if (n == 0) return;

    DepthEntry **arr = (DepthEntry **)malloc(n * sizeof(DepthEntry *));
    int i = 0;
    for (List *p = depths; p != NULL; p = p->next) {
        arr[i++] = (DepthEntry *)p->data;
    }

    qsort(arr, n, sizeof(DepthEntry *), depth_compare);

    for (i = 0; i < n; i++) {
        printf("\n %d:", arr[i]->depth);
        show_problem(arr[i]->problem, js);
    }

    free(arr);
    /* 참고: depths 리스트와 DepthEntry 메모리는 프로그램 종료 시 해제됨 */
}

/*
 * (defun get-children (gp &optional (*jsaint* *jsaint*) &aux children)
 *   (dolist (maybe-kid (fetch `(parent-of ?x ,gp ?type) ...) children)
 *     (if (in? maybe-kid ...) (push (cadr maybe-kid) children))))
 */
List *get_children(const char *gp, Jsaint *js) {
    if (js == NULL) js = current_jsaint;
    if (js == NULL) return NULL;

    List *children = NULL;
    char buf[512];
    snprintf(buf, sizeof(buf), "(parent-of ?x %s ?type)", gp);

    /* TODO: JTRE fetch 호출 후 in? 확인하여 children 구성 */
    /* List *results = fetch(buf, js->jtre); */
    /* for (List *p = results; p; p = p->next) { */
    /*     if (in_fact(p->data, js->jtre)) { */
    /*         children = list_prepend(children, cadr(p->data)); */
    /*     } */
    /* } */

    return children;
}

/*
 * (defun get-problems (&optional (*jsaint* *jsaint*))
 *   (mapcar 'cadr (fetch '(expanded ?x) (jsaint-jtre *jsaint*))))
 */
List *get_problems(Jsaint *js) {
    if (js == NULL) js = current_jsaint;
    if (js == NULL) return NULL;

    /* TODO: JTRE fetch 호출 */
    /* List *expanded = fetch("(expanded ?x)", js->jtre); */
    /* return mapcar(cadr, expanded); */

    return NULL;
}

/* ================================================================ */
/* 디버깅 편의 함수                                                 */
/* ================================================================ */

/*
 * (defun try-jsaint (problem &optional (title "JSAINT Test"))
 *   (solve-integral problem :DEBUGGING t :TITLE title))
 */
SolveResult try_jsaint(const char *problem, const char *title) {
    if (title == NULL) title = "JSAINT Test";
    return solve_integral(problem, title, 1, 20);
}

/*
 * (defun jfetch (pattern) (fetch pattern (jsaint-jtre *jsaint*)))
 */
List *jfetch(const char *pattern) {
    if (current_jsaint == NULL) return NULL;
    /* TODO: return fetch(pattern, current_jsaint->jtre); */
    return NULL;
}
