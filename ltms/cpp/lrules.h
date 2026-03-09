#pragma once

/// LTRE Rule System - Definitions
/// Converted from lrules.lisp
/// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ldata.h"
#include "linter.h"
#include <any>
#include <functional>
#include <iostream>
#include <string>
#include <vector>

// ---------------------------------------------------------------------------
// LRule struct
// ---------------------------------------------------------------------------

struct LRule {
    int counter = 0;
    LTRE* ltre = nullptr;
    DbClass* dbclass = nullptr;

    // Matcher: (lisp_form, out_bindings, out_needs_node) -> success?
    std::function<bool(const std::string&,
                       std::vector<std::any>&,
                       bool&)> matcher;

    // Body: called with the bindings produced by the matcher
    std::function<void(const std::vector<std::any>&)> body;
};

// ---------------------------------------------------------------------------
// Rule management functions
// ---------------------------------------------------------------------------

void insert_rule(DbClass* dbclass,
                 std::function<bool(const std::string&,
                                    std::vector<std::any>&,
                                    bool&)> matcher,
                 std::function<void(const std::vector<std::any>&)> body);

void try_rule_on(LRule* rule, Datum* datum);
void print_rule(LRule* rule, std::ostream& stream = std::cout);
bool rules_waiting(LTRE* ltre = nullptr);
LRule* get_rule(int num, LTRE* ltre = nullptr);

// Note: enqueue, run_rules, try_rules, show_rules are declared in
// linter.h and implemented in linter.cpp.
