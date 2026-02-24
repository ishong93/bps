// -*- C++ -*-

/// Variables and Unification - Implementation
/// Converted from unify.lisp and funify.lisp

#include "unify.h"
#include <sstream>
#include <algorithm>

// ================================================================
// SExprNode methods
// ================================================================

bool SExprNode::equal(const SExprPtr& other) const {
    if (!other) return is_nil();
    if (type != other->type) return false;
    switch (type) {
        case SYMBOL: return symbol == other->symbol;
        case NUMBER: return number == other->number;
        case NIL_TYPE: return other->is_nil();
        case LIST:
            if (elements.size() != other->elements.size()) return false;
            for (size_t i = 0; i < elements.size(); i++) {
                if (!elements[i]->equal(other->elements[i])) return false;
            }
            return true;
    }
    return false;
}

std::string SExprNode::to_string() const {
    switch (type) {
        case SYMBOL: return symbol;
        case NUMBER: {
            std::ostringstream ss;
            ss << number;
            return ss.str();
        }
        case NIL_TYPE: return "NIL";
        case LIST: {
            std::ostringstream ss;
            ss << "(";
            for (size_t i = 0; i < elements.size(); i++) {
                if (i > 0) ss << " ";
                ss << elements[i]->to_string();
            }
            ss << ")";
            return ss.str();
        }
    }
    return "?";
}

// ================================================================
// Binding list
// ================================================================

const Bindings FAIL_BINDINGS = {{"__FAIL__", nullptr}};

bool is_fail(const Bindings& b) {
    return !b.empty() && b[0].first == "__FAIL__";
}

// ================================================================
// Unification (from unify.lisp)
// ================================================================

bool is_variable(const std::string& x) {
    return !x.empty() && x[0] == '?';
}

std::optional<SExprPtr> lookup_binding(const std::string& var,
                                       const Bindings& bindings) {
    for (auto& [k, v] : bindings) {
        if (k == var) return v;
    }
    return std::nullopt;
}

Bindings unify(SExprPtr a, SExprPtr b, Bindings bindings) {
    if (is_fail(bindings)) return FAIL_BINDINGS;

    if (a->equal(b)) return bindings;

    if (a->is_variable()) {
        return unify_variable(a->symbol, b, bindings);
    }
    if (b->is_variable()) {
        return unify_variable(b->symbol, a, bindings);
    }
    if (!a->is_list() || !b->is_list()) {
        return FAIL_BINDINGS;
    }
    if (a->elements.empty() && b->elements.empty()) {
        return bindings;
    }
    if (a->elements.empty() || b->elements.empty()) {
        return FAIL_BINDINGS;
    }

    // Unify car then cdr
    auto new_bindings = unify(a->elements[0], b->elements[0], bindings);
    if (is_fail(new_bindings)) return FAIL_BINDINGS;

    // Build cdr
    std::vector<SExprPtr> a_rest(a->elements.begin() + 1, a->elements.end());
    std::vector<SExprPtr> b_rest(b->elements.begin() + 1, b->elements.end());

    auto a_cdr = a_rest.empty() ? SExprNode::make_nil()
                                : SExprNode::make_list(a_rest);
    auto b_cdr = b_rest.empty() ? SExprNode::make_nil()
                                : SExprNode::make_list(b_rest);

    return unify(a_cdr, b_cdr, new_bindings);
}

Bindings unify_variable(const std::string& var, SExprPtr exp,
                        Bindings bindings) {
    auto val = lookup_binding(var, bindings);
    if (val.has_value()) {
        return unify(val.value(), exp, bindings);
    }
    if (free_in(var, exp, bindings)) {
        bindings.push_back({var, exp});
        return bindings;
    }
    return FAIL_BINDINGS;
}

bool free_in(const std::string& var, SExprPtr exp,
             const Bindings& bindings) {
    if (!exp || exp->is_nil()) return true;
    if (exp->is_variable()) {
        if (var == exp->symbol) return false; // occurs check
        auto val = lookup_binding(exp->symbol, bindings);
        if (val.has_value()) {
            return free_in(var, val.value(), bindings);
        }
        return true;
    }
    if (!exp->is_list()) return true;
    for (auto& elem : exp->elements) {
        if (!free_in(var, elem, bindings)) return false;
    }
    return true;
}

SExprPtr subst_bindings(const Bindings& bindings, SExprPtr pattern) {
    if (!pattern || pattern->is_nil()) return pattern;
    if (pattern->is_variable()) {
        auto val = lookup_binding(pattern->symbol, bindings);
        if (val.has_value()) {
            return subst_bindings(bindings, val.value());
        }
        return pattern;
    }
    if (!pattern->is_list()) return pattern;
    std::vector<SExprPtr> new_elems;
    for (auto& elem : pattern->elements) {
        new_elems.push_back(subst_bindings(bindings, elem));
    }
    return SExprNode::make_list(new_elems);
}

// ================================================================
// Pattern free variables (from funify.lisp)
// ================================================================

static void pattern_free_vars1(SExprPtr pattern,
    const std::vector<std::string>& bound_vars,
    std::vector<std::string>& vars) {
    if (!pattern || pattern->is_nil()) return;
    if (pattern->is_variable()) {
        auto& sym = pattern->symbol;
        if (std::find(vars.begin(), vars.end(), sym) == vars.end() &&
            std::find(bound_vars.begin(), bound_vars.end(), sym) == bound_vars.end()) {
            vars.push_back(sym);
        }
        return;
    }
    if (!pattern->is_list()) return;
    for (auto& elem : pattern->elements) {
        pattern_free_vars1(elem, bound_vars, vars);
    }
}

std::vector<std::string> pattern_free_variables(
    SExprPtr pattern, const std::vector<std::string>& bound_vars) {
    std::vector<std::string> vars;
    pattern_free_vars1(pattern, bound_vars, vars);
    return vars;
}
