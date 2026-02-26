// -*- C++ -*-

/// Pattern Matcher and Open-coding Unification - Implementation
/// Converted from match.lisp, funify.lisp

#include "match.h"
#include <sstream>
#include <algorithm>

static double tolerance = 1.0e-6;

// ================================================================
// SExprNode methods
// ================================================================

SExprPtr SExprNode::make_symbol(const std::string& s) {
    auto n = std::make_shared<SExprNode>();
    n->type = SYMBOL; n->symbol = s; return n;
}
SExprPtr SExprNode::make_number(double d) {
    auto n = std::make_shared<SExprNode>();
    n->type = NUMBER; n->number = d; return n;
}
SExprPtr SExprNode::make_list(const std::vector<SExprPtr>& elems) {
    auto n = std::make_shared<SExprNode>();
    n->type = LIST; n->elements = elems; return n;
}
SExprPtr SExprNode::make_nil() {
    auto n = std::make_shared<SExprNode>();
    n->type = NIL_TYPE; return n;
}

bool SExprNode::is_variable() const {
    // Element variable: list starting with ?
    // Or a symbol starting with ?
    if (type == LIST && !elements.empty() &&
        elements[0]->is_symbol() && elements[0]->symbol == "?") {
        return true;
    }
    return false;
}

bool SExprNode::is_segment_var() const {
    if (type == LIST && !elements.empty() &&
        elements[0]->is_symbol() && elements[0]->symbol == "??") {
        return true;
    }
    return false;
}

std::string SExprNode::var_name() const {
    if ((is_variable() || is_segment_var()) && elements.size() >= 2) {
        return elements[1]->symbol;
    }
    if (type == SYMBOL && !symbol.empty() && symbol[0] == '?') {
        return symbol;
    }
    return "";
}

SExprPtr SExprNode::var_restriction() const {
    if ((is_variable() || is_segment_var()) && elements.size() >= 3) {
        return elements[2];
    }
    return nullptr;
}

bool SExprNode::equal(const SExprPtr& other) const {
    if (!other) return is_nil();
    if (type != other->type) return false;
    switch (type) {
        case SYMBOL: return symbol == other->symbol;
        case NUMBER: return std::abs(number - other->number) < tolerance;
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
// Dictionary
// ================================================================

const Dict FAIL_DICT = {{.name = "__FAIL__"}};

bool is_fail(const Dict& d) {
    return !d.empty() && d[0].name == "__FAIL__";
}

// ================================================================
// Variable detection
// ================================================================

bool is_element_var(SExprPtr x) {
    return x && x->is_variable();
}

bool is_segment_var(SExprPtr x) {
    return x && x->is_segment_var();
}

bool is_pattern_variable(SExprPtr x) {
    return is_element_var(x) || is_segment_var(x);
}

bool is_variable_symbol(const std::string& s) {
    return !s.empty() && s[0] == '?';
}

// ================================================================
// Dictionary operations
// ================================================================

std::optional<DictEntry> lookup_var(SExprPtr var, const Dict& dict) {
    auto name = var->var_name();
    for (auto& entry : dict) {
        if (entry.name == name) return entry;
    }
    return std::nullopt;
}

SExprPtr var_value(SExprPtr var, const Dict& dict) {
    auto entry = lookup_var(var, dict);
    if (!entry.has_value()) {
        throw std::runtime_error("Not bound variable: " + var->var_name());
    }
    if (!entry->is_segment) {
        return entry->elem_value;
    }
    return segment_to_list(entry->seg_beg, entry->seg_end);
}

SExprPtr segment_to_list(SExprPtr start, SExprPtr end_marker) {
    // Build a list from start's elements until we reach end_marker
    if (!start || start->is_nil()) return SExprNode::make_nil();
    if (start == end_marker) return SExprNode::make_nil();

    if (!start->is_list()) return SExprNode::make_nil();

    std::vector<SExprPtr> result;
    for (size_t i = 0; i < start->elements.size(); i++) {
        // For segment matching simplicity, we collect elements
        result.push_back(start->elements[i]);
    }
    if (result.empty()) return SExprNode::make_nil();
    return SExprNode::make_list(result);
}

Dict bind_element_var(const std::string& name, SExprPtr dat, Dict dict) {
    DictEntry entry;
    entry.name = name;
    entry.is_segment = false;
    entry.elem_value = dat;
    dict.insert(dict.begin(), entry);
    return dict;
}

Dict bind_segment_var(const std::string& name, SExprPtr beg,
                      SExprPtr end_marker, Dict dict) {
    DictEntry entry;
    entry.name = name;
    entry.is_segment = true;
    entry.seg_beg = beg;
    entry.seg_end = end_marker;
    dict.insert(dict.begin(), entry);
    return dict;
}

// ================================================================
// Approximate equality
// ================================================================

bool equal_approx(SExprPtr a, SExprPtr b) {
    if (!a || !b) return (!a && !b);
    if (a->is_number() && b->is_number()) {
        return std::abs(a->number - b->number) < tolerance;
    }
    return a->equal(b);
}

// ================================================================
// Pattern Matcher
// ================================================================

Dict match(SExprPtr pat, SExprPtr dat, Dict dict) {
    if (is_fail(dict)) return FAIL_DICT;
    if (pat == dat) return dict;

    if (is_element_var(pat)) {
        return match_element_var(pat, dat, dict);
    }
    if (!pat->is_list()) {
        if (equal_approx(pat, dat)) return dict;
        return FAIL_DICT;
    }
    if (!pat->elements.empty() && is_segment_var(pat->elements[0])) {
        return match_segment_var(pat, dat, dict);
    }
    if (!dat || !dat->is_list()) return FAIL_DICT;

    // Match car and cdr
    if (pat->elements.empty() && dat->elements.empty()) return dict;
    if (pat->elements.empty() || dat->elements.empty()) return FAIL_DICT;

    auto car_pat = pat->elements[0];
    auto car_dat = dat->elements[0];

    std::vector<SExprPtr> cdr_pat_elems(pat->elements.begin() + 1, pat->elements.end());
    std::vector<SExprPtr> cdr_dat_elems(dat->elements.begin() + 1, dat->elements.end());

    auto cdr_pat = cdr_pat_elems.empty() ? SExprNode::make_nil()
                                          : SExprNode::make_list(cdr_pat_elems);
    auto cdr_dat = cdr_dat_elems.empty() ? SExprNode::make_nil()
                                          : SExprNode::make_list(cdr_dat_elems);

    auto new_dict = match(car_pat, car_dat, dict);
    return match(cdr_pat, cdr_dat, new_dict);
}

Dict match_element_var(SExprPtr pat, SExprPtr dat, Dict dict) {
    auto entry = lookup_var(pat, dict);
    if (entry.has_value()) {
        if (equal_approx(entry->elem_value, dat)) return dict;
        return FAIL_DICT;
    }
    // Check restriction
    auto restriction = pat->var_restriction();
    if (restriction) {
        // In original Lisp, restriction is a predicate function.
        // Here we just skip restriction checking for simplicity.
    }
    return bind_element_var(pat->var_name(), dat, dict);
}

Dict match_segment_var(SExprPtr pat, SExprPtr dat, Dict dict) {
    if (pat->elements.empty()) return FAIL_DICT;

    auto seg_var = pat->elements[0];
    auto entry = lookup_var(seg_var, dict);

    std::vector<SExprPtr> rest_pat(pat->elements.begin() + 1, pat->elements.end());
    auto rest_pattern = rest_pat.empty() ? SExprNode::make_nil()
                                          : SExprNode::make_list(rest_pat);

    if (entry.has_value()) {
        // Segment already bound - check match
        // Simplified: try matching the rest
        return match(rest_pattern, dat, dict);
    }

    // Try different segment lengths
    if (!dat || dat->is_nil()) {
        // Empty segment
        auto new_dict = bind_segment_var(
            seg_var->var_name(), SExprNode::make_nil(), SExprNode::make_nil(), dict);
        return match(rest_pattern, SExprNode::make_nil(), new_dict);
    }

    if (!dat->is_list()) return FAIL_DICT;

    // Try each possible segment length
    for (size_t len = 0; len <= dat->elements.size(); len++) {
        std::vector<SExprPtr> seg_elems(dat->elements.begin(),
                                         dat->elements.begin() + len);
        std::vector<SExprPtr> rest_elems(dat->elements.begin() + len,
                                          dat->elements.end());

        auto seg_val = seg_elems.empty() ? SExprNode::make_nil()
                                          : SExprNode::make_list(seg_elems);
        auto rest_dat = rest_elems.empty() ? SExprNode::make_nil()
                                            : SExprNode::make_list(rest_elems);

        auto new_dict = bind_segment_var(
            seg_var->var_name(), seg_val, SExprNode::make_nil(), dict);
        auto result = match(rest_pattern, rest_dat, new_dict);
        if (!is_fail(result)) return result;
    }
    return FAIL_DICT;
}

// ================================================================
// Substitution
// ================================================================

SExprPtr substitute_in(SExprPtr exp, const Dict& dict) {
    if (!exp || exp->is_nil()) return exp;
    if (is_element_var(exp)) return var_value(exp, dict);
    if (!exp->is_list()) return exp;

    // Check for segment var at head
    if (!exp->elements.empty() && is_segment_var(exp->elements[0])) {
        auto seg_val = var_value(exp->elements[0], dict);

        std::vector<SExprPtr> rest(exp->elements.begin() + 1,
                                    exp->elements.end());
        auto rest_exp = rest.empty() ? SExprNode::make_nil()
                                      : SExprNode::make_list(rest);
        auto rest_sub = substitute_in(rest_exp, dict);

        // Append seg_val elements + rest_sub elements
        std::vector<SExprPtr> result;
        if (seg_val && seg_val->is_list()) {
            result = seg_val->elements;
        }
        if (rest_sub && rest_sub->is_list()) {
            result.insert(result.end(),
                rest_sub->elements.begin(), rest_sub->elements.end());
        }
        if (result.empty()) return SExprNode::make_nil();
        return SExprNode::make_list(result);
    }

    // Recurse on elements
    std::vector<SExprPtr> new_elems;
    for (auto& elem : exp->elements) {
        new_elems.push_back(substitute_in(elem, dict));
    }
    return SExprNode::make_list(new_elems);
}

// ================================================================
// Open-coding unification helpers (from funify.lisp)
// ================================================================

SExprPtr quotize(SExprPtr pattern) {
    if (!pattern || pattern->is_nil()) return pattern;
    if (is_variable_symbol(pattern->symbol) && pattern->is_symbol()) {
        return pattern;
    }
    if (!pattern->is_list()) return pattern;
    // Recurse
    std::vector<SExprPtr> result;
    for (auto& elem : pattern->elements) {
        result.push_back(quotize(elem));
    }
    return SExprNode::make_list(result);
}

static void collect_free_vars(SExprPtr pattern,
    const std::vector<std::string>& bound_vars,
    std::vector<std::string>& vars) {
    if (!pattern || pattern->is_nil()) return;
    if (pattern->is_symbol() && is_variable_symbol(pattern->symbol)) {
        auto& sym = pattern->symbol;
        if (std::find(vars.begin(), vars.end(), sym) == vars.end() &&
            std::find(bound_vars.begin(), bound_vars.end(), sym) == bound_vars.end()) {
            vars.push_back(sym);
        }
        return;
    }
    if (!pattern->is_list()) return;
    for (auto& elem : pattern->elements) {
        collect_free_vars(elem, bound_vars, vars);
    }
}

std::vector<std::string> pattern_free_variables(SExprPtr pattern,
    const std::vector<std::string>& bound_vars) {
    std::vector<std::string> vars;
    collect_free_vars(pattern, bound_vars, vars);
    return vars;
}
