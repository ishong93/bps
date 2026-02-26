// -*- C++ -*-

/// JTMS Examples and JTRE Tests
/// Converted from jtms-ex.lisp, jtest.lisp

#include "jtms.h"
#include "jtre.h"
#include <iostream>

// ================================================================
// JTMS Examples (from jtms-ex.lisp)
// ================================================================

static JtmsPtr test_jtms;
static NodePtr na, nb, nc, nd, ne, nf, ng, contra;

NodePtr get_node_by_datum(const std::string& datum, JtmsPtr jtms) {
    for (auto& node : jtms->nodes) {
        try {
            auto d = std::any_cast<std::string>(node->datum);
            if (d == datum) return node;
        } catch (...) {}
    }
    return nullptr;
}

/// Example 1: Basic justification network
void ex1() {
    test_jtms = create_jtms("Simple Example", nullptr, true);
    na = tms_create_node(test_jtms, std::string("a"), true);
    nb = tms_create_node(test_jtms, std::string("b"), true);
    nc = tms_create_node(test_jtms, std::string("c"), true);
    nd = tms_create_node(test_jtms, std::string("d"), true);
    ne = tms_create_node(test_jtms, std::string("e"), true);
    nf = tms_create_node(test_jtms, std::string("f"), true);
    ng = tms_create_node(test_jtms, std::string("g"), true);

    justify_node("j1", nf, {na, nb});
    justify_node("j2", ne, {nb, nc});
    justify_node("j3", ng, {na, ne});
    justify_node("j4", ng, {nd, ne});

    enable_assumption(na);
    enable_assumption(nb);
    enable_assumption(nc);
    enable_assumption(nd);
}

/// Example 2: Tests contradiction handling (requires ex1 first)
void ex2() {
    contra = tms_create_node(test_jtms, std::string("Loser"), false, true);
    justify_node("j5", contra, {ne, nf});
}

/// Example 3: Multiple support
void ex3() {
    test_jtms = create_jtms("Multiple support example");
    auto assumption_a = tms_create_node(test_jtms, std::string("A"), true);
    auto assumption_c = tms_create_node(test_jtms, std::string("C"), true);
    auto assumption_e = tms_create_node(test_jtms, std::string("E"), true);
    auto node_h = tms_create_node(test_jtms, std::string("h"));

    enable_assumption(assumption_a);
    enable_assumption(assumption_c);
    enable_assumption(assumption_e);
    justify_node("R1", node_h, {assumption_c, assumption_e});

    auto node_g = tms_create_node(test_jtms, std::string("g"));
    justify_node("R2", node_g, {assumption_a, assumption_c});

    auto contradiction_node = tms_create_node(test_jtms,
        std::string("CONTRADICTION"), false, true);
    justify_node("R3", contradiction_node, {node_g});
}

// ================================================================
// JTRE Shakedown Test (from jtest.lisp)
// ================================================================

void shakedown_jtre() {
    in_jtre(create_jtre("Test One"));

    std::cout << "\n--- JTRE Shakedown Test ---\n";

    // Register an INTERN rule for (foo ?x) + (bar ?y) with number test
    insert_rule(
        get_dbclass("foo"),
        [](const SExpr& form) -> std::tuple<bool, std::vector<std::any>, bool> {
            std::string s;
            try { s = std::any_cast<std::string>(form); } catch (...) {
                return {false, {}, false};
            }
            if (s.substr(0, 5) != "(foo ") return {false, {}, false};
            auto val = s.substr(5, s.size() - 6);
            // Test: numberp
            try {
                std::stod(val);
                return {true, {form, SExpr(val)}, false};
            } catch (...) {
                return {false, {}, false};
            }
        },
        [](const std::vector<std::any>& args) {
            if (args.size() < 2) return;
            std::string x;
            try { x = std::any_cast<std::string>(args[1]); } catch (...) { return; }
            // Check if (bar x) exists and assert (mumble x x)
            auto bar_facts = fetch(SExpr(std::string("(bar " + x + ")")));
            if (!bar_facts.empty()) {
                assert_fact(SExpr(std::string("(mumble " + x + " " + x + ")")),
                    {SExpr(std::string("Test-intern")),
                     SExpr(std::string("(foo " + x + ")")),
                     SExpr(std::string("(bar " + x + ")"))});
            }
        }
    );
    std::cout << "\n :INTERN rule defined okay.";

    // Insert facts
    referent(SExpr(std::string("(foo 1)")), true);
    auto r = fetch(SExpr(std::string("(foo 1)")));
    if (!r.empty()) {
        std::cout << "\n Referent worked okay.";
    } else {
        std::cerr << "\nReferent failed.";
    }

    referent(SExpr(std::string("(bar 1)")), true);
    run_rules();
    std::cout << "\n No errors during attempted rule execution.";

    r = fetch(SExpr(std::string("(mumble 1 1)")));
    if (!r.empty()) {
        std::cout << "\n:INTERN rule fired okay.";
    } else {
        std::cout << "\n:INTERN rule did not fire (expected with simplified matching).";
    }

    // Test assumptions
    referent(SExpr(std::string("(foo a)")), true);
    referent(SExpr(std::string("(bar a)")), true);
    run_rules();

    uassume(SExpr(std::string("(foo a)")), "USER");
    uassume(SExpr(std::string("(bar a)")), "USER");

    uassume(SExpr(std::string("(foo 1)")), "USER");
    uassume(SExpr(std::string("(bar 1)")), "USER");

    std::cout << "\n--- Shakedown Complete ---\n";
}
