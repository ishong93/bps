#pragma once

/// Dependency-Directed Search Facility
/// Converted from dds.lisp
/// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ldata.h"
#include "linter.h"
#include <vector>
#include <string>
#include <functional>

extern bool debug_dds;

void dd_search(const std::vector<std::vector<std::any>>& choice_sets,
               std::function<void()> end_fn,
               LTRE* ltre = nullptr);

// Test
void test_dd_search(bool debugging = true);
void show_dd_test_solution(LTRE* ltre);
