/// LTMS Examples and Tests - Implementation
/// Converted from ltms-ex.lisp
/// Copyright (c) 1986-1990, Kenneth D. Forbus, University of Illinois,
/// Johan de Kleer and Xerox Corporation. All rights reserved.

#include "ltms_ex.h"
#include <iostream>
#include <stdexcept>
#include <vector>
#include <string>

// Helper to build formula from TmsNode* values
static std::any N(TmsNode* n) { return std::any(n); }
static std::any S(const std::string& s) { return std::any(s); }

static std::any make_or(std::initializer_list<std::any> args) {
    std::vector<std::any> v;
    v.push_back(S(":OR"));
    for (auto& a : args) v.push_back(a);
    return std::any(v);
}

static std::any make_not(const std::any& arg) {
    std::vector<std::any> v;
    v.push_back(S(":NOT"));
    v.push_back(arg);
    return std::any(v);
}

static std::any make_implies(const std::any& a, const std::any& b) {
    std::vector<std::any> v;
    v.push_back(S(":IMPLIES"));
    v.push_back(a);
    v.push_back(b);
    return std::any(v);
}

static std::any make_and(std::initializer_list<std::any> args) {
    std::vector<std::any> v;
    v.push_back(S(":AND"));
    for (auto& a : args) v.push_back(a);
    return std::any(v);
}

static std::any make_tax(std::initializer_list<std::any> args) {
    std::vector<std::any> v;
    v.push_back(S(":TAXONOMY"));
    for (auto& a : args) v.push_back(a);
    return std::any(v);
}

static std::any make_iff(const std::any& a, const std::any& b) {
    std::vector<std::any> v;
    v.push_back(S(":IFF"));
    v.push_back(a);
    v.push_back(b);
    return std::any(v);
}

// ================================================================
// test_explain
// ================================================================

void test_explain() {
    auto* ltms = create_ltms("Explain Example");
    auto* x = tms_create_node(ltms, "x", true);

    // (:OR "x" "y")
    add_formula(ltms, make_or({S("x"), S("y")}));

    // (:OR (:NOT "y") "z")
    add_formula(ltms, make_or({make_not(S("y")), S("z")}));

    // (:OR (:NOT "z") "r")
    add_formula(ltms, make_or({make_not(S("z")), S("r")}));

    enable_assumption(x, NodeLabel::FALSE);
    explain_node(find_node(ltms, S("r")));
}

// ================================================================
// test_formula
// ================================================================

void test_formula(bool complete) {
    std::any comp = complete ? std::any(true) : std::any();
    auto* ltms = create_ltms("Formula", nullptr, false, true,
                              nullptr, nullptr, true, comp);
    auto* r = tms_create_node(ltms, "r");
    auto* s = tms_create_node(ltms, "s");
    auto* tt = tms_create_node(ltms, "t");
    auto* u = tms_create_node(ltms, "u");

    // (:implies (:and r (:implies s t)) u)
    add_formula(ltms, make_implies(
        make_and({N(r), make_implies(N(s), N(tt))}),
        N(u)));
}

// ================================================================
// run_ltms_tests
// ================================================================

void run_ltms_tests() {
    std::cout << "Running LTMS tests..." << std::endl;

    test_ask();
    std::cout << "\ntest_ask passed." << std::endl;

    test_avoid_all();
    std::cout << "test_avoid_all passed." << std::endl;

    test_bug();
    std::cout << "test_bug passed." << std::endl;

    test_bug1();
    std::cout << "test_bug1 passed." << std::endl;

    test_tax(3);
    std::cout << "\ntest_tax passed." << std::endl;

    test_tax1(3);
    std::cout << "\ntest_tax1 passed." << std::endl;
}

// ================================================================
// test_ask
// ================================================================

void test_ask() {
    auto* ltms = create_ltms("Testing asking");
    auto* n1 = tms_create_node(ltms, "N1", true);
    auto* n2 = tms_create_node(ltms, "N2", true);
    enable_assumption(n1, NodeLabel::FALSE);
    enable_assumption(n2, NodeLabel::FALSE);

    add_formula(ltms, make_or({N(n1), N(n2)}));
    why_nodes(ltms);
}

// ================================================================
// test_avoid_all
// ================================================================

void test_avoid_all() {
    auto* ltms = create_ltms("Testing avoid all", nullptr, false, true,
                              avoid_all);
    auto* n1 = tms_create_node(ltms, "N1", true);
    auto* n2 = tms_create_node(ltms, "N2", true);
    enable_assumption(n1, NodeLabel::FALSE);
    enable_assumption(n2, NodeLabel::FALSE);

    add_formula(ltms, make_or({N(n1), N(n2)}));
    why_nodes(ltms);
}

// ================================================================
// test1
// ================================================================

void test1(bool complete) {
    std::any comp = complete ? std::any(true) : std::any();
    auto* ltms = create_ltms("TEST1", nullptr, false, true,
                              nullptr, nullptr, true, comp);
    auto* x = tms_create_node(ltms, "x");
    auto* y = tms_create_node(ltms, "y");

    add_formula(ltms, make_or({N(x), N(y)}));
    add_formula(ltms, make_or({N(x), make_not(N(y))}));
    complete_ltms(ltms);

    if (!true_node(x)) {
        throw std::runtime_error("TEST1 failed");
    }
}

// ================================================================
// test_bug
// ================================================================

void test_bug() {
    auto* ltms = create_ltms("BUG check");
    auto* x = tms_create_node(ltms, "x", true);
    auto* y = tms_create_node(ltms, "y", true);
    auto* z = tms_create_node(ltms, "z");

    add_formula(ltms, make_or({N(x), N(z)}));
    add_formula(ltms, make_or({N(y), N(z)}));

    enable_assumption(x, NodeLabel::FALSE);
    enable_assumption(y, NodeLabel::FALSE);
    why_nodes(ltms);

    retract_assumption(x);
    why_nodes(ltms);
}

// ================================================================
// test_bug1
// ================================================================

void test_bug1(bool complete) {
    std::any comp = complete ? std::any(true) : std::any();
    auto* ltms = create_ltms("BUG check", nullptr, false, true,
                              nullptr, nullptr, true, comp);
    auto* x = tms_create_node(ltms, "x", true);
    auto* y = tms_create_node(ltms, "y", true);
    auto* z = tms_create_node(ltms, "z");

    add_formula(ltms, make_or({N(x), N(z)}));
    add_formula(ltms, make_or({N(y), N(z)}));

    enable_assumption(x, NodeLabel::FALSE);
    enable_assumption(y, NodeLabel::FALSE);
    why_nodes(ltms);

    retract_assumption(x);
    why_nodes(ltms);
}

// ================================================================
// test_tax
// ================================================================

void test_tax(int n, bool complete) {
    std::any comp = complete ? std::any(true) : std::any();
    auto* ltms = create_ltms("taxing", nullptr, false, true,
                              nullptr, nullptr, true, comp);

    std::vector<std::any> tax_args;
    tax_args.push_back(S(":TAXONOMY"));
    for (int i = 0; i < n; i++) {
        tax_args.push_back(N(tms_create_node(ltms, std::to_string(i))));
    }
    add_formula(ltms, std::any(tax_args));
    std::cout << "\n " << ltms->clause_counter << " prime implicates";
}

void test_tax1(int n) {
    auto* ltms = create_ltms("taxing");

    std::vector<std::any> tax_args;
    tax_args.push_back(S(":TAXONOMY"));
    for (int i = 0; i < n; i++) {
        tax_args.push_back(N(tms_create_node(ltms, std::to_string(i))));
    }
    add_formula(ltms, std::any(tax_args));
    std::cout << "\n " << ltms->clause_counter << " prime implicates";
}

// ================================================================
// test_e
// ================================================================

void test_e(bool complete) {
    std::any comp = complete ? std::any(true) : std::any();
    auto* ltms = create_ltms("example", nullptr, true, true,
                              nullptr, nullptr, true, comp);
    auto* a = tms_create_node(ltms, "a", true);
    auto* b = tms_create_node(ltms, "b", true);
    auto* c = tms_create_node(ltms, "c", true);
    auto* d = tms_create_node(ltms, "d", true);
    auto* e = tms_create_node(ltms, "e", true);

    add_formula(ltms, make_or({make_not(N(a)), N(b)}));
    add_formula(ltms, make_or({make_not(N(c)), N(d)}));
    add_formula(ltms, make_or({make_not(N(c)), N(e)}));
    add_formula(ltms, make_or({make_not(N(b)), make_not(N(d)), make_not(N(e))}));
}

// ================================================================
// test_remove
// ================================================================

void test_remove() {
    auto* ltms = create_ltms("Delay", nullptr, true, true,
                              nullptr, nullptr, true, std::any(true));
    auto* a = tms_create_node(ltms, "a", true);
    auto* b = tms_create_node(ltms, "b", true);
    auto* c = tms_create_node(ltms, "c", true);

    add_formula(ltms, make_or({N(a), N(b), N(c)}));
    enable_assumption(a, NodeLabel::FALSE);
    enable_assumption(b, NodeLabel::FALSE);
    why_nodes(ltms);

    add_formula(ltms, make_or({N(a), N(b)}));
    why_nodes(ltms);
}

// ================================================================
// test_delay
// ================================================================

void test_delay() {
    auto* ltms = create_ltms("Delay", nullptr, true, true,
                              nullptr, nullptr, true, std::any(true));
    auto* a = tms_create_node(ltms, "a", true);
    auto* b = tms_create_node(ltms, "b", true);
    auto* c = tms_create_node(ltms, "c", true);

    enable_assumption(a, NodeLabel::FALSE);
    enable_assumption(b, NodeLabel::FALSE);

    add_formula(ltms, make_or({N(a), make_not(N(b))}));
    add_formula(ltms, make_or({N(b), N(c)}));

    pretty_print_clauses(ltms);
    why_nodes(ltms);

    retract_assumption(b);
    pretty_print_clauses(ltms);
    why_nodes(ltms);
}

// ================================================================
// Utility
// ================================================================

void dirty_clauses_check(LTMS* ltms) {
    int count = 0;
    walk_clauses(ltms, [&](Clause* cl) {
        if (cl->status == ClauseStatus::DIRTY) count++;
    });
    std::cout << "\n There are now " << count << " dirty clauses." << std::endl;
}

void print_statistics(LTMS* ltms) {
    std::vector<int> lengths(100, 0);
    std::cout << "\n There are " << ltms->node_counter << " propositional symbols";
    walk_clauses(ltms, [&](Clause* cl) {
        if (cl->length < 100) lengths[cl->length]++;
    });
    for (int i = 0; i < 100; i++) {
        if (lengths[i] != 0) {
            std::cout << "\n There are " << lengths[i]
                      << " prime implicates of size " << i;
        }
    }
}
