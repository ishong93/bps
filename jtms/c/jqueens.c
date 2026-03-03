/* -*- C -*- */

/* N-Queens 퍼즐: 의존성 기반 탐색을 사용하는 JTRE 예제 */
/* jqueens.lisp에서 변환 */

/* Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. All rights reserved. */

/*
 * 원본 Lisp에서의 핵심 구조:
 *
 * - n-queens(n): N-Queens 퍼즐을 풀고 해의 개수를 반환
 * - setup-queens-puzzle: JTRE 생성, 규칙 로드
 * - make-queens-choice-sets: 각 열에 가능한 퀸 배치 생성
 * - solve-queens-puzzle: 의존성 기반 백트래킹 탐색
 * - try-in-context: 가정을 시도하고 모순을 처리 (catch/throw → setjmp/longjmp)
 * - queens-okay?: 두 퀸이 공격하지 않는지 검사
 * - gather-queens-solution: IN 상태 퀸 배치를 수집
 */

#include "jqueens.h"
#include "jqrule.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

/* ================================================================ */
/* 전역 변수 (통계)                                                  */
/* ================================================================ */

int   jqueens_n_assumptions = 0;    /* *n-assumptions* */
List *jqueens_placements    = NULL; /* *placements* */

/* setjmp/longjmp 용 모순 핸들링 전역 */
jmp_buf jqueens_try_jmp;
bool    jqueens_contradiction_found = false;
List   *jqueens_contra_asns         = NULL;

/* try-in-context에서 사용하는 현재 가정 정보 */
static SExpr *current_try_assumption = NULL;
static SExpr *current_try_marker     = NULL;

/* ================================================================ */
/* SExpr 헬퍼                                                        */
/* ================================================================ */

/* (Queen column row) S-expression 생성 */
SExpr *make_queen_sexpr(int column, int row) {
    return sexpr_cons(sexpr_symbol("Queen"),
           sexpr_cons(sexpr_number((double)column),
           sexpr_cons(sexpr_number((double)row),
           sexpr_nil())));
}

/* (not (Queen column row)) S-expression 생성 */
SExpr *make_not_queen_sexpr(int column, int row) {
    SExpr *queen = make_queen_sexpr(column, row);
    return sexpr_cons(sexpr_symbol("not"),
           sexpr_cons(queen,
           sexpr_nil()));
}

/* (Queen ?c ?r) 패턴 S-expression 생성 */
SExpr *make_queen_pattern(void) {
    return sexpr_cons(sexpr_symbol("Queen"),
           sexpr_cons(sexpr_symbol("?c"),
           sexpr_cons(sexpr_symbol("?r"),
           sexpr_nil())));
}

/* QueenPlacement에서 SExpr 변환 */
SExpr *queen_placement_to_sexpr(QueenPlacement *qp) {
    return make_queen_sexpr(qp->column, qp->row);
}

/* ================================================================ */
/* 퀸 안전성 검사                                                    */
/* ================================================================ */

/*
 * queens_okay: 두 퀸이 서로 공격하지 않는지 검사.
 *
 * (defun queens-okay? (x1 y1 x2 y2)
 *   (not (or (= y1 y2) (= (abs (- x1 x2)) (abs (- y1 y2))))))
 *
 * 같은 행이거나 대각선이면 false(공격 가능), 아니면 true(안전).
 */
bool queens_okay(int x1, int y1, int x2, int y2) {
    if (y1 == y2) return false;                    /* 같은 행 */
    if (abs(x1 - x2) == abs(y1 - y2)) return false; /* 대각선 */
    return true;
}

/* ================================================================ */
/* 퍼즐 초기 설정                                                    */
/* ================================================================ */

/*
 * setup_queens_puzzle: JTRE를 생성하고 퀸 규칙을 로드한다.
 *
 * (defun setup-queens-puzzle (n &optional (debugging? nil))
 *   (in-JTRE (create-jtre (format nil "~D-Queens JTRE" n)
 *                          :DEBUGGING debugging?))
 *   (setq *placements* nil *n-assumptions* 0)
 *   (load *queen-rules-file*))
 */
void setup_queens_puzzle(int n, bool debugging) {
    /* JTRE 생성 */
    char title[64];
    snprintf(title, sizeof(title), "%d-Queens JTRE", n);
    Jtre *jtre = create_jtre(title, debugging);
    in_jtre(jtre);

    /* 전역 변수 초기화 */
    if (jqueens_placements) {
        /* 이전 해 목록 해제 */
        for (int i = 0; i < jqueens_placements->size; i++) {
            List *sol = (List *)list_get(jqueens_placements, i);
            /* 각 해 안의 SExpr들은 JTRE 데이터베이스 소유이므로
             * 여기서는 List 구조체만 해제 */
            list_free(sol);
        }
        list_free(jqueens_placements);
    }
    jqueens_placements = list_new();
    jqueens_n_assumptions = 0;

    /* 퀸 규칙 로드 (jqrule.c) */
    load_queen_rules(jtre);
}

/* ================================================================ */
/* 선택 집합 생성                                                    */
/* ================================================================ */

/*
 * make_queens_choice_sets: 각 열에 가능한 퀸 배치 목록을 생성.
 *
 * (defun make-queens-choice-sets (n)
 *   (do ((column 1 (1+ column))
 *        (column-queens nil nil)
 *        (choice-sets nil))
 *       ((> column n) (nreverse choice-sets))
 *     (dotimes (row n)
 *       (push `(Queen ,column ,(1+ row)) column-queens))
 *     (push (nreverse column-queens) choice-sets)))
 *
 * column: 1..n, row: 1..n
 */
QueenChoiceSet *make_queens_choice_sets(int n, int *n_sets_out) {
    QueenChoiceSet *sets = (QueenChoiceSet *)calloc(
        n, sizeof(QueenChoiceSet));
    *n_sets_out = n;

    for (int col = 1; col <= n; col++) {
        QueenChoiceSet *cs = &sets[col - 1];
        cs->n_choices = n;
        cs->choices = (QueenPlacement *)calloc(
            n, sizeof(QueenPlacement));
        for (int row = 1; row <= n; row++) {
            cs->choices[row - 1].column = col;
            cs->choices[row - 1].row = row;
        }
    }
    return sets;
}

/* 선택 집합 해제 */
void free_queens_choice_sets(QueenChoiceSet *sets, int n_sets) {
    for (int i = 0; i < n_sets; i++) {
        free(sets[i].choices);
    }
    free(sets);
}

/* ================================================================ */
/* 모순 핸들러 (try-contradiction-handler)                           */
/* ================================================================ */

/*
 * try_contradiction_handler: with-contradiction-handler 콜백.
 *
 * (defun try-contradiction-handler (contras jtms asn marker *JTRE*
 *                                   &aux node)
 *   (unless (eq jtms (jtre-jtms *JTRE*))
 *     (error ...))
 *   (unless contras (return-from ... nil))
 *   (unless asn (return-from ... nil))
 *   (setq node (get-tms-node asn))
 *   (dolist (cnode contras)
 *     (let ((asns (assumptions-of-node cnode)))
 *       (when (member node asns)
 *         (retract! asn marker)
 *         (throw 'TRY-CONTRADICTION-FOUND (cons :ASNS asns))))))
 *
 * setjmp/longjmp로 변환: 모순이 현재 가정을 포함하면
 * 가정을 철회하고 longjmp로 점프한다.
 */
void try_contradiction_handler(Jtms *jtms, List *contras,
                               SExpr *assumption, SExpr *marker,
                               Jtre *jtre) {
    if (jtms != jtre->jtms) {
        fprintf(stderr,
                "Error: High Contradiction Weirdness: "
                "jtms mismatch for jtre!\n");
        return;
    }
    if (!contras || contras->size == 0) return;
    if (!assumption) return;

    TmsNode *asn_node = get_tms_node(assumption, jtre);
    if (!asn_node) return;

    for (int i = 0; i < contras->size; i++) {
        TmsNode *cnode = (TmsNode *)list_get(contras, i);
        List *asns = assumptions_of_node(cnode);

        /* 현재 가정이 모순의 가정 집합에 포함되는지 확인 */
        bool found = false;
        for (int j = 0; j < asns->size; j++) {
            TmsNode *a = (TmsNode *)list_get(asns, j);
            if (a == asn_node) {
                found = true;
                break;
            }
        }

        if (found) {
            /* 가정 철회 */
            retract_fact(assumption, marker, false, jtre);

            /* 가정 목록을 SExpr 형태로 변환하여 저장 */
            if (jqueens_contra_asns) {
                list_free(jqueens_contra_asns);
            }
            jqueens_contra_asns = list_new();
            for (int j = 0; j < asns->size; j++) {
                TmsNode *a = (TmsNode *)list_get(asns, j);
                SExpr *view = view_node(a);
                list_push(jqueens_contra_asns, view);
            }

            list_free(asns);

            /* throw 'TRY-CONTRADICTION-FOUND 에 해당 */
            jqueens_contradiction_found = true;
            longjmp(jqueens_try_jmp, 1);
        }

        list_free(asns);
    }
}

/* ================================================================ */
/* 모순 핸들러 래퍼 (with-contradiction-handler 콜백)                */
/* ================================================================ */

/*
 * JTMS의 contradiction_handler로 등록되는 콜백.
 * 현재 try-in-context 세션의 가정/마커를 사용하여
 * try_contradiction_handler를 호출한다.
 */
static void queens_contra_callback(Jtms *jtms, List *contras) {
    if (current_try_assumption && current_jtre) {
        try_contradiction_handler(jtms, contras,
                                  current_try_assumption,
                                  current_try_marker,
                                  current_jtre);
    }
}

/* ================================================================ */
/* try-in-context 구현                                               */
/* ================================================================ */

/*
 * try_in_context: 가정을 시도하고, 모순이 발생하면 처리한다.
 *
 * Lisp 원본 (catch/throw, with-contradiction-handler, unwind-protect):
 *
 * (defun try-in-context (asn thunk jtre &aux try-marker result)
 *   (setq try-marker (cons 'TRY asn))
 *   (with-contradiction-handler (jtre-jtms jtre)
 *     #'(lambda (jtms contras)
 *         (try-contradiction-handler contras jtms asn try-marker jtre))
 *     (unwind-protect
 *       (progn
 *         (unless (in? asn jtre)
 *           (setq result (catch 'TRY-CONTRADICTION-FOUND
 *                          (assume! asn try-marker jtre)))
 *           (when (and (listp result) (eq (car result) :ASNS))
 *             (return-from TRY-IN-CONTEXT (values t (mapcar #'view-node (cdr result)))))
 *           (setq result (catch 'TRY-CONTRADICTION-FOUND
 *                          (run-rules jtre)))
 *           (when (and (listp result) (eq (car result) :ASNS))
 *             (return-from TRY-IN-CONTEXT (values t (mapcar #'view-node (cdr result)))))
 *           (eval thunk)
 *           (progn (retract! asn try-marker t)
 *                  (return-from TRY-IN-CONTEXT (values nil nil))))))))
 *
 * C에서는 setjmp/longjmp로 catch/throw를 구현한다.
 * thunk는 solve_queens_puzzle의 재귀 호출로 대체한다.
 */
TryResult try_in_context(SExpr *assumption, QueenChoiceSet *sets,
                         int n_sets, int next_set, Jtre *jtre) {
    TryResult result;
    result.nogood = false;
    result.assumptions = NULL;

    /* try-marker = (TRY . asn) */
    SExpr *try_marker = sexpr_cons(sexpr_symbol("TRY"),
                        sexpr_cons(sexpr_copy(assumption),
                        sexpr_nil()));

    /* 이전 모순 핸들러 저장 (with-contradiction-handler) */
    void (*old_handler)(Jtms*, List*) =
        jtre->jtms->contradiction_handler;
    SExpr *old_assumption = current_try_assumption;
    SExpr *old_marker = current_try_marker;

    /* 새 모순 핸들러 설정 */
    current_try_assumption = assumption;
    current_try_marker = try_marker;
    jtre->jtms->contradiction_handler = queens_contra_callback;

    /* 이미 IN이면 thunk만 실행하고 반환 */
    if (in_fact(assumption, jtre)) {
        /* 핸들러 복원 (unwind-protect) */
        jtre->jtms->contradiction_handler = old_handler;
        current_try_assumption = old_assumption;
        current_try_marker = old_marker;
        sexpr_free(try_marker);
        return result;
    }

    /* setjmp: catch 'TRY-CONTRADICTION-FOUND 에 해당 */
    jqueens_contradiction_found = false;

    if (setjmp(jqueens_try_jmp) != 0) {
        /* longjmp로 돌아옴: 모순 발견됨 */
        result.nogood = true;
        result.assumptions = jqueens_contra_asns;
        jqueens_contra_asns = NULL;

        /* 핸들러 복원 (unwind-protect) */
        jtre->jtms->contradiction_handler = old_handler;
        current_try_assumption = old_assumption;
        current_try_marker = old_marker;
        sexpr_free(try_marker);
        return result;
    }

    /* assume! asn try-marker jtre */
    assume_fact(assumption, sexpr_to_string(try_marker), jtre);

    /* assume! 중 모순이 발생하면 longjmp로 위의 setjmp로 점프했을 것.
     * 여기 도달했으면 assume! 성공. */

    /* run-rules: 규칙 실행 */
    run_rules(jtre);

    /* run-rules 중 모순이 발생하면 longjmp로 점프했을 것.
     * 여기 도달했으면 규칙 실행 성공. */

    /* thunk 실행: (solve-queens-puzzle (cdr choice-sets))
     * 나머지 열에 대해 재귀적으로 탐색 */
    solve_queens_puzzle(sets, n_sets, next_set);

    /* 가정 철회 (unwind-protect 정리 코드) */
    retract_fact(assumption, try_marker, true, jtre);

    /* 핸들러 복원 */
    jtre->jtms->contradiction_handler = old_handler;
    current_try_assumption = old_assumption;
    current_try_marker = old_marker;
    sexpr_free(try_marker);

    result.nogood = false;
    result.assumptions = NULL;
    return result;
}

/* ================================================================ */
/* 해 수집                                                           */
/* ================================================================ */

/*
 * gather_queens_solution: 현재 IN 상태인 퀸 배치를 수집.
 *
 * (defun gather-queens-solution ()
 *   (push (remove-if #'(lambda (q) (out? q *jtre*))
 *                    (fetch `(Queen ?c ?r) *jtre*))
 *         *placements*))
 *
 * fetch (Queen ?c ?r)로 모든 Queen 팩트를 가져오고,
 * OUT인 것을 제거하여 해 목록에 추가한다.
 */
void gather_queens_solution(void) {
    SExpr *pattern = make_queen_pattern();
    List *all_queens = fetch(pattern, current_jtre);
    sexpr_free(pattern);

    List *solution = list_new();

    for (int i = 0; i < all_queens->size; i++) {
        SExpr *q = (SExpr *)list_get(all_queens, i);
        /* IN 상태인 것만 해에 포함 */
        if (!out_fact(q, current_jtre)) {
            list_push(solution, sexpr_copy(q));
        }
    }

    /* fetch 결과 해제 */
    for (int i = 0; i < all_queens->size; i++) {
        SExpr *q = (SExpr *)list_get(all_queens, i);
        sexpr_free(q);
    }
    list_free(all_queens);

    list_push(jqueens_placements, solution);
}

/* ================================================================ */
/* 퍼즐 탐색                                                        */
/* ================================================================ */

/*
 * solve_queens_puzzle: 의존성 기반 백트래킹으로 퀸 배치를 탐색.
 *
 * (defun solve-queens-puzzle (choice-sets)
 *   (cond ((null choice-sets) (gather-queens-solution))
 *         (t (dolist (choice (car choice-sets))
 *             (unless (in? `(not ,choice) *jtre*)
 *               ;; nogood 정보를 존중
 *               (multiple-value-bind (nogood? asns)
 *                 (try-in-context choice
 *                   `(solve-queens-puzzle ',(cdr choice-sets))
 *                   *jtre*)
 *                 (incf *n-assumptions*)
 *                 (when nogood?
 *                   ;; 이 가정은 실패했으므로, 다른 관련 가정들을 근거로
 *                   ;; 부정을 정당화한다.
 *                   (assert! `(not ,choice)
 *                            `(Nogood ,@(remove choice asns))))))))))
 */
void solve_queens_puzzle(QueenChoiceSet *sets, int n_sets,
                         int current_set) {
    /* 모든 열을 처리했으면 해 수집 */
    if (current_set >= n_sets) {
        gather_queens_solution();
        return;
    }

    QueenChoiceSet *cs = &sets[current_set];

    for (int i = 0; i < cs->n_choices; i++) {
        QueenPlacement *qp = &cs->choices[i];
        SExpr *choice = queen_placement_to_sexpr(qp);

        /* (not choice)가 IN이면 이미 nogood으로 판명된 것이므로 건너뜀 */
        SExpr *not_choice = sexpr_cons(sexpr_symbol("not"),
                            sexpr_cons(sexpr_copy(choice),
                            sexpr_nil()));

        if (in_fact(not_choice, current_jtre)) {
            /* nogood 정보 존중: 이 선택 건너뜀 */
            sexpr_free(not_choice);
            sexpr_free(choice);
            continue;
        }
        sexpr_free(not_choice);

        /* try-in-context로 이 선택을 시도 */
        TryResult tr = try_in_context(choice, sets, n_sets,
                                      current_set + 1,
                                      current_jtre);
        jqueens_n_assumptions++;

        if (tr.nogood && tr.assumptions) {
            /*
             * 이 가정은 모순을 일으켰으므로, 부정을 정당화한다.
             *
             * (assert! `(not ,choice)
             *          `(Nogood ,@(remove choice asns)))
             *
             * justification: (Nogood asn1 asn2 ...) 에서
             * choice 자신은 제외한다.
             */
            SExpr *neg = sexpr_cons(sexpr_symbol("not"),
                         sexpr_cons(sexpr_copy(choice),
                         sexpr_nil()));

            /* justification 구성: (Nogood asn1 asn2 ...) */
            SExpr *just_list = sexpr_nil();
            /* 역순으로 빌드 후 뒤집기 위해 먼저 수집 */
            int n_asns = 0;
            SExpr **asn_arr = NULL;
            if (tr.assumptions) {
                /* choice를 제외한 가정들을 수집 */
                asn_arr = (SExpr **)calloc(tr.assumptions->size,
                                           sizeof(SExpr *));
                for (int j = 0; j < tr.assumptions->size; j++) {
                    SExpr *asn = (SExpr *)list_get(
                        tr.assumptions, j);
                    if (!sexpr_equal(asn, choice)) {
                        asn_arr[n_asns++] = asn;
                    }
                }
            }
            /* (Nogood asn1 asn2 ...) 리스트 구성 (역순 cons) */
            just_list = sexpr_nil();
            for (int j = n_asns - 1; j >= 0; j--) {
                just_list = sexpr_cons(sexpr_copy(asn_arr[j]),
                                       just_list);
            }
            just_list = sexpr_cons(sexpr_symbol("Nogood"),
                                    just_list);

            assert_fact(neg, just_list, current_jtre);

            sexpr_free(neg);
            sexpr_free(just_list);
            if (asn_arr) free(asn_arr);

            /* 가정 목록 해제 */
            list_free(tr.assumptions);
        }

        sexpr_free(choice);
    }
}

/* ================================================================ */
/* N-Queens 메인 함수                                                */
/* ================================================================ */

/*
 * n_queens: N-Queens 퍼즐을 풀고 해의 개수를 반환.
 *
 * (defun n-queens (n &optional (debugging? nil))
 *   (setup-queens-puzzle n debugging?)
 *   (solve-queens-puzzle (make-queens-choice-sets n))
 *   (length *placements*))
 */
int n_queens(int n, bool debugging) {
    setup_queens_puzzle(n, debugging);

    int n_sets = 0;
    QueenChoiceSet *sets = make_queens_choice_sets(n, &n_sets);

    solve_queens_puzzle(sets, n_sets, 0);

    free_queens_choice_sets(sets, n_sets);

    return jqueens_placements->size;
}

/* ================================================================ */
/* 테스트: from부터 to까지 N-Queens 실행                             */
/* ================================================================ */

/*
 * test_queens: from부터 to까지 n-queens를 실행하고 결과를 출력.
 *
 * (defun test-queens (from to)
 *   (do ((n from (1+ n)))
 *       ((> n to))
 *     (gc)
 *     (time (n-queens n))
 *     (format t "~% For n=~D, ~D solutions, ~D assumptions."
 *            n (length *placements*) *n-assumptions*)))
 */
void test_queens(int from, int to) {
    for (int n = from; n <= to; n++) {
        clock_t start = clock();
        int n_solutions = n_queens(n, false);
        clock_t end = clock();
        double elapsed = (double)(end - start) / CLOCKS_PER_SEC;

        printf("\n For n=%d, %d solutions, %d assumptions. (%.3f sec)",
               n, n_solutions, jqueens_n_assumptions, elapsed);
    }
    printf("\n");
}

/* ================================================================ */
/* 해 출력                                                           */
/* ================================================================ */

/*
 * show_queens_solution: 해를 보드 형태로 출력.
 *
 * (defun show-queens-solution (solution &aux n)
 *   (setq n (length solution))
 *   (dotimes (i n)
 *     (terpri)
 *     (dotimes (j n)
 *       (format t "~A"
 *               (if (member `(queen ,i ,j) solution
 *                           :TEST #'equal) "Q" "-")))))
 *
 * 주의: 원본은 0-indexed (queen i j)를 사용하지만,
 * make-queens-choice-sets는 1-indexed (Queen col row)를 사용.
 * 여기서는 1-indexed로 통일한다.
 */
void show_queens_solution(List *solution, int n) {
    for (int row = 1; row <= n; row++) {
        printf("\n");
        for (int col = 1; col <= n; col++) {
            SExpr *q = make_queen_sexpr(col, row);
            bool found = false;
            for (int k = 0; k < solution->size; k++) {
                SExpr *s = (SExpr *)list_get(solution, k);
                if (sexpr_equal(q, s)) {
                    found = true;
                    break;
                }
            }
            printf("%s", found ? "Q" : "-");
            sexpr_free(q);
        }
    }
    printf("\n");
}
