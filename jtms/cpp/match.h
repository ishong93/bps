// -*- C++ -*-

/// Pattern Matcher and Open-coding Unification
/// Converted from match.lisp, funify.lisp

#ifndef MATCH_H
#define MATCH_H

#include <string>
#include <vector>
#include <functional>
#include <optional>
#include <any>
#include <variant>
#include <memory>
#include <cmath>

// ================================================================
// S-Expression representation for pattern matching
// ================================================================

struct SExprNode;
using SExprPtr = std::shared_ptr<SExprNode>;

struct SExprNode {
    enum Type { SYMBOL, NUMBER, LIST, NIL_TYPE };
    Type type;
    std::string symbol;
    double number = 0.0;
    std::vector<SExprPtr> elements;

    static SExprPtr make_symbol(const std::string& s);
    static SExprPtr make_number(double d);
    static SExprPtr make_list(const std::vector<SExprPtr>& elems);
    static SExprPtr make_nil();

    bool is_variable() const;
    bool is_segment_var() const;
    bool is_nil() const { return type == NIL_TYPE; }
    bool is_list() const { return type == LIST; }
    bool is_symbol() const { return type == SYMBOL; }
    bool is_number() const { return type == NUMBER; }

    /// Name of a variable: (? name) -> name, (?? name) -> name
    std::string var_name() const;
    /// Restriction predicate of a variable: (? name pred) -> pred
    SExprPtr var_restriction() const;

    bool equal(const SExprPtr& other) const;
    std::string to_string() const;
};

// ================================================================
// Dictionary (binding list)
// ================================================================

struct DictEntry {
    std::string name;
    // For element variables: value is a single SExprPtr
    // For segment variables: value is (beg_ptr, end_ptr) pair
    bool is_segment = false;
    SExprPtr elem_value;         // element variable value
    SExprPtr seg_beg, seg_end;   // segment variable pointers (into original list)
};

using Dict = std::vector<DictEntry>;

extern const Dict FAIL_DICT;
bool is_fail(const Dict& d);

// ================================================================
// Pattern Matcher (from match.lisp)
// ================================================================

Dict match(SExprPtr pat, SExprPtr dat, Dict dict = {});
Dict match_element_var(SExprPtr pat, SExprPtr dat, Dict dict);
Dict match_segment_var(SExprPtr pat, SExprPtr dat, Dict dict);

bool equal_approx(SExprPtr a, SExprPtr b);

std::optional<DictEntry> lookup_var(SExprPtr var, const Dict& dict);
SExprPtr var_value(SExprPtr var, const Dict& dict);
SExprPtr segment_to_list(SExprPtr start, SExprPtr end_marker);

Dict bind_element_var(const std::string& name, SExprPtr dat, Dict dict);
Dict bind_segment_var(const std::string& name, SExprPtr beg,
                      SExprPtr end_marker, Dict dict);

SExprPtr substitute_in(SExprPtr exp, const Dict& dict);

// ================================================================
// Variable detection
// ================================================================

bool is_element_var(SExprPtr x);
bool is_segment_var(SExprPtr x);
bool is_pattern_variable(SExprPtr x);
bool is_variable_symbol(const std::string& s);

// ================================================================
// Open-coding unification helpers (from funify.lisp)
// ================================================================

SExprPtr quotize(SExprPtr pattern);
std::vector<std::string> pattern_free_variables(SExprPtr pattern,
    const std::vector<std::string>& bound_vars = {});

#endif // MATCH_H
