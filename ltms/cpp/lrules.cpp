/// LTRE Rule System - Implementation
/// Converted from lrules.lisp
/// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "lrules.h"
#include <functional>
#include <iostream>

// ================================================================
// insert_rule
// ================================================================

void insert_rule(DbClass* dbclass,
                 std::function<bool(const std::string&,
                                    std::vector<std::any>&,
                                    bool&)> matcher,
                 std::function<void(const std::vector<std::any>&)> body) {
    LTRE* ltre = dbclass->ltre;

    auto* rule = new LRule();
    rule->counter = ++(ltre->rule_counter);
    rule->ltre = ltre;
    rule->dbclass = dbclass;
    rule->matcher = std::move(matcher);
    rule->body = std::move(body);

    dbclass->rules.push_back(rule);

    for (Datum* candidate : dbclass->facts) {
        try_rule_on(rule, candidate);
    }
}

// ================================================================
// try_rule_on
// ================================================================

void try_rule_on(LRule* rule, Datum* datum) {
    LTRE* ltre = datum->ltre;

    std::vector<std::any> bindings;
    bool needs_node = false;

    bool ok = rule->matcher(datum->lisp_form, bindings, needs_node);
    if (!ok) return;

    if (needs_node) {
        bindings.insert(bindings.begin(), std::any(datum->tms_node));
    }

    auto body_fn = rule->body;
    auto bound_args = std::move(bindings);

    std::function<void()> closure = [body_fn, bound_args]() {
        body_fn(bound_args);
    };

    enqueue(std::any(std::move(closure)), ltre);
}

// ================================================================
// print_rule
// ================================================================

void print_rule(LRule* rule, std::ostream& stream) {
    stream << "\n Rule #" << rule->counter
           << " [dbclass: " << (rule->dbclass ? rule->dbclass->name : "?")
           << "]";
}

// ================================================================
// rules_waiting
// ================================================================

bool rules_waiting(LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    return !ltre->queue.empty();
}

// ================================================================
// get_rule
// ================================================================

LRule* get_rule(int num, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    LRule* found = nullptr;
    map_dbclass([&](DbClass* dbclass) {
        for (LRule* rule : dbclass->rules) {
            if (rule->counter == num) {
                found = rule;
                return;
            }
        }
    }, ltre);

    return found;
}
