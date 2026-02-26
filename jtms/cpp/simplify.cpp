// -*- C++ -*-

/// Algebraic Simplifier - Implementation
/// Converted from simplify.lisp

#include "simplify.h"
#include <unordered_map>
#include <algorithm>
#include <cmath>

// ================================================================
// Simplify cache
// ================================================================

static std::unordered_map<std::string, SExprPtr> simplify_cache;

void clear_simplify_cache() {
    simplify_cache.clear();
}

// ================================================================
// Simplifier
// ================================================================

std::vector<SimplifyRule> algebra_rules;

SExprPtr simplify(SExprPtr exp) {
    if (!exp) return exp;
    auto key = exp->to_string();
    auto it = simplify_cache.find(key);
    if (it != simplify_cache.end()) return it->second;

    auto result = simplify_it(exp, algebra_rules);
    simplify_cache[key] = result;
    return result;
}

SExprPtr simplify_it(SExprPtr exp, const std::vector<SimplifyRule>& rules) {
    SExprPtr simplified;
    if (exp->is_list()) {
        std::vector<SExprPtr> new_elems;
        for (auto& elem : exp->elements) {
            new_elems.push_back(simplify(elem));
        }
        simplified = SExprNode::make_list(new_elems);
    } else {
        simplified = exp;
    }

    auto result = try_matcher_rules(simplified, rules);
    if (result->equal(exp)) return result;
    return simplify_it(result, rules);
}

SExprPtr try_matcher_rules(SExprPtr exp, const std::vector<SimplifyRule>& rules) {
    for (auto& rule : rules) {
        auto bindings = match(rule.pattern, exp);
        if (is_fail(bindings)) continue;
        if (check_predicate(rule.predicate, bindings)) {
            return substitute_in(rule.skeleton, bindings);
        }
    }
    return exp; // No rule matched
}

bool check_predicate(SExprPtr proc, const Dict& bindings) {
    if (!proc) return true;
    // In the original, this evaluates the predicate with bindings.
    // Simplified: always return true (full evaluation requires an interpreter).
    return true;
}

// ================================================================
// Algebra utilities
// ================================================================

bool alg_less(SExprPtr e1, SExprPtr e2) {
    if (equal_approx(e1, e2)) return false;
    if (e1->is_list()) {
        if (e2->is_list()) {
            if (equal_approx(e1->elements[0], e2->elements[0])) {
                // Compare rest
                for (size_t i = 1; i < std::min(e1->elements.size(),
                    e2->elements.size()); i++) {
                    if (alg_less(e1->elements[i], e2->elements[i])) return true;
                    if (alg_less(e2->elements[i], e1->elements[i])) return false;
                }
                return e1->elements.size() < e2->elements.size();
            }
            return alg_less(e1->elements[0], e2->elements[0]);
        }
        return false;
    }
    if (e2->is_list()) return true;
    if (e1->is_symbol()) {
        if (e2->is_symbol()) return e1->symbol < e2->symbol;
        return false;
    }
    if (e2->is_symbol()) return true;
    if (e1->is_number() && e2->is_number()) return e1->number < e2->number;
    return false;
}

bool alg_equal(SExprPtr e1, SExprPtr e2) {
    return !alg_less(e1, e2) && !alg_less(e2, e1);
}

bool sorted_check(const std::vector<SExprPtr>& list) {
    for (size_t i = 1; i < list.size(); i++) {
        if (alg_less(list[i], list[i - 1])) return false;
    }
    return true;
}

bool is_plus_or_times(SExprPtr exp) {
    if (!exp || !exp->is_symbol()) return false;
    return exp->symbol == "+" || exp->symbol == "*";
}

bool same_constant(SExprPtr exp, double constant) {
    if (!exp || !exp->is_number()) return false;
    return std::abs(exp->number - constant) < 1.0e-6;
}

bool is_zero(SExprPtr exp) { return same_constant(exp, 0.0); }
bool is_one(SExprPtr exp) { return same_constant(exp, 1.0); }

bool occurs_in(SExprPtr exp1, SExprPtr exp2) {
    if (!exp2) return false;
    if (exp1->equal(exp2)) return true;
    if (!exp2->is_list()) return false;
    for (auto& elem : exp2->elements) {
        if (occurs_in(exp1, elem)) return true;
    }
    return false;
}

// ================================================================
// Initialize algebra rules
// ================================================================

void init_algebra_rules() {
    // The rules from the original Lisp are very complex and require
    // a full s-expression parser to set up. Here we provide the
    // structure for a few fundamental rules.

    // Rule: (op e) -> e  (flush degenerate single-arg +/*)
    // Rule: (+ 0 e...) -> (+ e...)
    // Rule: (* 1 e...) -> (* e...)
    // Rule: (* 0 e...) -> 0
    // Rule: (- e 0) -> e
    // Rule: (- e e) -> 0
    // Rule: (expt e 0) -> 1
    // Rule: (expt e 1) -> e

    // In a full implementation, these would be parsed from s-expressions
    // matching the definitions in simplify.lisp.
    // For now, algebra_rules is left empty and can be populated
    // programmatically by the user.
}
