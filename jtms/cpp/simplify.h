// -*- C++ -*-

/// Algebraic Simplifier
/// Converted from simplify.lisp

#ifndef SIMPLIFY_H
#define SIMPLIFY_H

#include "match.h"
#include <unordered_map>

// ================================================================
// Simplification Rule
// ================================================================

struct SimplifyRule {
    SExprPtr pattern;
    SExprPtr predicate;  // nullptr if no predicate
    SExprPtr skeleton;

    std::string to_string() const {
        return "<SimplifyRule " + pattern->to_string() + ">";
    }
};

// ================================================================
// Simplifier API
// ================================================================

SExprPtr simplify(SExprPtr exp);
void clear_simplify_cache();

SExprPtr simplify_it(SExprPtr exp, const std::vector<SimplifyRule>& rules);
SExprPtr try_matcher_rules(SExprPtr exp, const std::vector<SimplifyRule>& rules);
bool check_predicate(SExprPtr proc, const Dict& bindings);

// ================================================================
// Algebra utilities
// ================================================================

bool alg_less(SExprPtr e1, SExprPtr e2);
bool alg_equal(SExprPtr e1, SExprPtr e2);
bool sorted_check(const std::vector<SExprPtr>& list);
bool is_plus_or_times(SExprPtr exp);
bool same_constant(SExprPtr exp, double constant);
bool is_zero(SExprPtr exp);
bool is_one(SExprPtr exp);
bool occurs_in(SExprPtr exp1, SExprPtr exp2);

// ================================================================
// Global algebra rules
// ================================================================

extern std::vector<SimplifyRule> algebra_rules;
void init_algebra_rules();

#endif // SIMPLIFY_H
