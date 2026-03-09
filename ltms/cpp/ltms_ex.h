#pragma once

/// LTMS Examples and Tests
/// Converted from ltms-ex.lisp
/// Copyright (c) 1986-1990, Kenneth D. Forbus, University of Illinois,
/// Johan de Kleer and Xerox Corporation. All rights reserved.

#include "ltms.h"
#include "cltms.h"

// Core tests
void test_explain();
void test_formula(bool complete = false);
void run_ltms_tests();
void test_ask();
void test_avoid_all();
void test1(bool complete = true);
void test_bug();
void test_bug1(bool complete = true);
void test_tax(int n, bool complete = true);
void test_tax1(int n);
void test_e(bool complete = true);
void test_remove();
void test_delay();

// Utility
void dirty_clauses_check(LTMS* ltms);
void print_statistics(LTMS* ltms = nullptr);
