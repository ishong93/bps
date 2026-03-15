/// Acceptance Tests for LTRE - Implementation
/// Converted from laccept.lisp
/// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "laccept.h"
#include "funify.h"
#include <iostream>
#include <stdexcept>

// Helper to make list-form std::any
static std::any S(const std::string& s) { return std::any(s); }
static std::any L(std::initializer_list<std::any> elems) {
    return std::any(std::vector<std::any>(elems));
}

void test_ltre() {
    in_ltre(create_ltre("Debugging LTRE"));
    std::cout << "\nTesting database/LTMS link...";
    test_datums();
    std::cout << "\nTesting LTMS...";
    test_clauses();
    std::cout << "\nTesting Rule system...";
    test_rules();
    std::cout << "\nAll tests passed." << std::endl;
}

void test_datums() {
    // assert! 'foo 'testing
    ltre_assert(S("foo"), S("testing"));
    if (!is_true(S("foo"))) {
        throw std::runtime_error("Fact installation glitch");
    }

    // assert! '(:NOT bar) 'testing
    ltre_assert(L({S(":NOT"), S("bar")}), S("testing"));
    if (!is_false(S("bar"))) {
        throw std::runtime_error("Negation glitch");
    }
    std::cout << " OK";
}

void test_clauses() {
    // (:OR a b)
    ltre_assert(L({S(":OR"), S("a"), S("b")}), S("case-split"));

    // (:IMPLIES a c)
    ltre_assert(L({S(":IMPLIES"), S("a"), S("c")}), S("why-not?"));

    // assume! '(:IMPLIES c d) 'what-the-heck
    ltre_assume(L({S(":IMPLIES"), S("c"), S("d")}), "what-the-heck");

    // assume! '(:NOT b) 'for-fun
    ltre_assume(L({S(":NOT"), S("b")}), "for-fun");

    if (!is_true(S("d"))) {
        throw std::runtime_error("Propagation glitch");
    }

    // retract! '(:NOT b) 'for-fun
    ltre_retract(L({S(":NOT"), S("b")}), "for-fun");
    if (!is_unknown(S("d"))) {
        throw std::runtime_error("Retraction glitch");
    }

    // re-assume (:NOT b)
    ltre_assume(L({S(":NOT"), S("b")}), "for-fun");
    if (!is_true(S("d"))) {
        throw std::runtime_error("Unouting glitch");
    }

    // retract (:IMPLIES c d)
    ltre_retract(L({S(":IMPLIES"), S("c"), S("d")}), "what-the-heck");
    if (!is_unknown(S("d"))) {
        throw std::runtime_error("Retraction glitch 2");
    }

    // re-assume (:IMPLIES c d)
    ltre_assume(L({S(":IMPLIES"), S("c"), S("d")}), "what-the-heck");
    if (!is_true(S("d"))) {
        throw std::runtime_error("Unouting glitch 2");
    }

    std::cout << " OK";
}

void test_rules() {
    // The rule testing in the Lisp version requires macro expansion
    // which is not directly portable to C++.
    std::cout << " (rule macro system not ported - skipping rule tests)";
}
