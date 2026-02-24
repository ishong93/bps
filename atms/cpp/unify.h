// -*- C++ -*-

/// Variables and Unification
/// Converted from unify.lisp and funify.lisp

#ifndef UNIFY_H
#define UNIFY_H

#include <string>
#include <vector>
#include <map>
#include <optional>
#include <variant>
#include <any>
#include <functional>

// ================================================================
// S-Expression representation for pattern matching
// ================================================================

/// A simple symbolic expression type for pattern matching.
/// In the original Lisp, s-expressions are the native data type.
/// Here we represent them as a tree of strings and lists.
struct SExprNode;
using SExprPtr = std::shared_ptr<SExprNode>;

struct SExprNode {
    enum Type { SYMBOL, NUMBER, LIST, NIL_TYPE };
    Type type;
    std::string symbol;   // for SYMBOL type
    double number;        // for NUMBER type
    std::vector<SExprPtr> elements; // for LIST type

    static SExprPtr make_symbol(const std::string& s) {
        auto n = std::make_shared<SExprNode>();
        n->type = SYMBOL; n->symbol = s; return n;
    }
    static SExprPtr make_number(double d) {
        auto n = std::make_shared<SExprNode>();
        n->type = NUMBER; n->number = d; return n;
    }
    static SExprPtr make_list(const std::vector<SExprPtr>& elems) {
        auto n = std::make_shared<SExprNode>();
        n->type = LIST; n->elements = elems; return n;
    }
    static SExprPtr make_nil() {
        auto n = std::make_shared<SExprNode>();
        n->type = NIL_TYPE; return n;
    }

    bool is_variable() const {
        return type == SYMBOL && !symbol.empty() && symbol[0] == '?';
    }
    bool is_nil() const { return type == NIL_TYPE; }
    bool is_list() const { return type == LIST; }
    bool is_symbol() const { return type == SYMBOL; }
    bool is_number() const { return type == NUMBER; }

    bool equal(const SExprPtr& other) const;
    std::string to_string() const;
};

// ================================================================
// Binding list: variable -> value
// ================================================================

using Bindings = std::vector<std::pair<std::string, SExprPtr>>;

/// Special value indicating unification failure
extern const Bindings FAIL_BINDINGS;
bool is_fail(const Bindings& b);

// ================================================================
// Unification (from unify.lisp)
// ================================================================

/// Check if a symbol is a variable (starts with '?')
bool is_variable(const std::string& x);

/// Unify two s-expressions, returning updated bindings or FAIL
Bindings unify(SExprPtr a, SExprPtr b, Bindings bindings = {});

/// Unify a variable with an expression
Bindings unify_variable(const std::string& var, SExprPtr exp,
                        Bindings bindings);

/// Check if var occurs free in exp under given bindings
bool free_in(const std::string& var, SExprPtr exp,
             const Bindings& bindings);

/// Look up a variable in bindings
std::optional<SExprPtr> lookup_binding(const std::string& var,
                                       const Bindings& bindings);

/// Substitute bindings into a pattern
SExprPtr subst_bindings(const Bindings& bindings, SExprPtr pattern);

// ================================================================
// Pattern free variables (from funify.lisp)
// ================================================================

/// Find free variables in a pattern
std::vector<std::string> pattern_free_variables(
    SExprPtr pattern,
    const std::vector<std::string>& bound_vars = {});

#endif // UNIFY_H
