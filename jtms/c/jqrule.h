/* -*- C -*- */

/* N-Queens 규칙: JTRE 버전 */
/* jqrule.lisp에서 변환 */

/* Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University, */
/* and Johan de Kleer, the Xerox Corporation. All rights reserved. */

#ifndef JQRULE_H
#define JQRULE_H

#include "jtms.h"
#include "jtre.h"
#include "jdata.h"
#include "jqueens.h"

/* ================================================================ */
/* N-Queens 규칙 등록                                                */
/* ================================================================ */

/* 퀸 충돌 모순 규칙을 JTRE에 등록한다.
 *
 * Lisp 원본:
 *   (contradiction 'Queens-capture *jtre*)
 *   (rule ((:IN (Queen ?column1 ?row1) :VAR ?Q1)
 *          (:IN (Queen ?column2 ?row2) :VAR ?Q2
 *            :TEST (not (or (= ?column1 ?column2)
 *                           (queens-okay? ...)))))
 *         (rassert! Queens-capture (Death ?Q1 ?Q2)))
 *
 * 두 퀸이 같은 행에 있거나 대각선에 있으면 모순(contradiction)을 발생시킨다.
 */
void load_queen_rules(Jtre *jtre);

/* Queens-capture 모순 검사 콜백:
 * 두 퀸 배치를 받아서 충돌 여부를 확인하고,
 * 충돌이면 Queens-capture를 assert하여 모순을 일으킨다. */
void queen_capture_rule_body(SExpr *q1_fact, SExpr *q2_fact,
                             Jtre *jtre);

#endif /* JQRULE_H */
