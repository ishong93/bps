/// Open-coded Unification for LTRE -- Implementation
/// Converted from funify.lisp
/// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "funify.h"
#include <algorithm>
#include <sstream>
#include <cmath>

// ---------------------------------------------------------------------------
// is_variable
// ---------------------------------------------------------------------------

bool is_variable(const std::any& x) {
    if (auto sp = std::any_cast<std::string>(&x)) {
        return is_variable_str(*sp);
    }
    return false;
}

bool is_variable_str(const std::string& s) {
    return !s.empty() && s[0] == '?';
}

// ---------------------------------------------------------------------------
// any_equal -- structural equality for std::any Lisp forms
// ---------------------------------------------------------------------------

bool any_equal(const std::any& a, const std::any& b) {
    // Both empty => equal
    if (!a.has_value() && !b.has_value()) return true;
    if (!a.has_value() || !b.has_value()) return false;

    // Both strings
    if (auto sa = std::any_cast<std::string>(&a)) {
        if (auto sb = std::any_cast<std::string>(&b)) {
            return *sa == *sb;
        }
        return false;
    }

    // Both ints
    if (auto ia = std::any_cast<int>(&a)) {
        if (auto ib = std::any_cast<int>(&b)) {
            return *ia == *ib;
        }
        // int vs double
        if (auto db = std::any_cast<double>(&b)) {
            return static_cast<double>(*ia) == *db;
        }
        return false;
    }

    // Both doubles
    if (auto da = std::any_cast<double>(&a)) {
        if (auto db = std::any_cast<double>(&b)) {
            return *da == *db;
        }
        // double vs int
        if (auto ib = std::any_cast<int>(&b)) {
            return *da == static_cast<double>(*ib);
        }
        return false;
    }

    // Both vectors (lists)
    if (auto va = std::any_cast<std::vector<std::any>>(&a)) {
        if (auto vb = std::any_cast<std::vector<std::any>>(&b)) {
            if (va->size() != vb->size()) return false;
            for (size_t i = 0; i < va->size(); ++i) {
                if (!any_equal((*va)[i], (*vb)[i])) return false;
            }
            return true;
        }
        return false;
    }

    // Both bools
    if (auto ba = std::any_cast<bool>(&a)) {
        if (auto bb = std::any_cast<bool>(&b)) {
            return *ba == *bb;
        }
        return false;
    }

    // Unknown types -- not equal
    return false;
}

// ---------------------------------------------------------------------------
// lookup_binding
// ---------------------------------------------------------------------------

std::optional<std::any> lookup_binding(const std::string& var,
                                       const BindingMap& bindings) {
    auto it = bindings.find(var);
    if (it != bindings.end()) {
        return it->second;
    }
    return std::nullopt;
}

// ---------------------------------------------------------------------------
// occurs_in -- occurs check to prevent circular bindings
// ---------------------------------------------------------------------------

bool occurs_in(const std::string& var, const std::any& expr,
               const BindingMap& bindings) {
    if (!expr.has_value()) return false;

    // If expr is a variable
    if (auto sp = std::any_cast<std::string>(&expr)) {
        if (is_variable_str(*sp)) {
            if (*sp == var) return true;
            // Check if this variable is bound, and recurse
            auto bound = lookup_binding(*sp, bindings);
            if (bound.has_value()) {
                return occurs_in(var, *bound, bindings);
            }
            return false;
        }
        return false;
    }

    // If expr is a list, check each element
    if (auto vp = std::any_cast<std::vector<std::any>>(&expr)) {
        for (const auto& elem : *vp) {
            if (occurs_in(var, elem, bindings)) return true;
        }
        return false;
    }

    return false;
}

// ---------------------------------------------------------------------------
// unify
// ---------------------------------------------------------------------------

std::optional<BindingMap> unify(const std::any& pattern,
                                const std::any& datum,
                                BindingMap bindings) {
    // Both empty => success
    if (!pattern.has_value() && !datum.has_value()) {
        return bindings;
    }

    // If pattern is a variable
    if (auto pvar = std::any_cast<std::string>(&pattern)) {
        if (is_variable_str(*pvar)) {
            // Check if already bound
            auto bound = lookup_binding(*pvar, bindings);
            if (bound.has_value()) {
                return unify(*bound, datum, bindings);
            }
            // Occurs check
            if (occurs_in(*pvar, datum, bindings)) {
                return std::nullopt; // failure
            }
            bindings[*pvar] = datum;
            return bindings;
        }
    }

    // If datum is a variable
    if (auto dvar = std::any_cast<std::string>(&datum)) {
        if (is_variable_str(*dvar)) {
            auto bound = lookup_binding(*dvar, bindings);
            if (bound.has_value()) {
                return unify(pattern, *bound, bindings);
            }
            if (occurs_in(*dvar, pattern, bindings)) {
                return std::nullopt;
            }
            bindings[*dvar] = pattern;
            return bindings;
        }
    }

    // Both are lists => unify element-wise
    if (auto plist = std::any_cast<std::vector<std::any>>(&pattern)) {
        if (auto dlist = std::any_cast<std::vector<std::any>>(&datum)) {
            if (plist->size() != dlist->size()) {
                return std::nullopt; // different lengths => fail
            }
            for (size_t i = 0; i < plist->size(); ++i) {
                auto result = unify((*plist)[i], (*dlist)[i], bindings);
                if (!result.has_value()) {
                    return std::nullopt;
                }
                bindings = std::move(*result);
            }
            return bindings;
        }
        // pattern is list, datum is not => fail
        return std::nullopt;
    }

    // Both are atoms => check equality
    if (any_equal(pattern, datum)) {
        return bindings;
    }

    return std::nullopt; // fail
}

// ---------------------------------------------------------------------------
// sublis -- substitute bindings into a pattern
// ---------------------------------------------------------------------------

std::any sublis(const BindingMap& bindings, const std::any& pattern) {
    if (!pattern.has_value()) return pattern;

    // If pattern is a variable, substitute
    if (auto sp = std::any_cast<std::string>(&pattern)) {
        if (is_variable_str(*sp)) {
            auto bound = lookup_binding(*sp, bindings);
            if (bound.has_value()) {
                // Recursively substitute in case the value contains variables
                return sublis(bindings, *bound);
            }
            return pattern; // unbound variable, leave as-is
        }
        return pattern; // non-variable string
    }

    // If pattern is a list, recurse on each element
    if (auto vp = std::any_cast<std::vector<std::any>>(&pattern)) {
        std::vector<std::any> result;
        result.reserve(vp->size());
        for (const auto& elem : *vp) {
            result.push_back(sublis(bindings, elem));
        }
        return result;
    }

    // Atom (int, double, etc.) -- return as-is
    return pattern;
}

// ---------------------------------------------------------------------------
// pattern_free_variables
// ---------------------------------------------------------------------------

std::vector<std::string> pattern_free_variables(
    const std::any& pattern,
    const std::vector<std::string>& bound_vars) {
    return pattern_free_vars1(pattern, {}, bound_vars);
}

std::vector<std::string> pattern_free_vars1(
    const std::any& pattern,
    std::vector<std::string> vars,
    const std::vector<std::string>& bound_vars) {

    if (!pattern.has_value()) return vars;

    // If pattern is a string
    if (auto sp = std::any_cast<std::string>(&pattern)) {
        if (is_variable_str(*sp)) {
            // Check if already in vars or in bound_vars
            if (std::find(vars.begin(), vars.end(), *sp) != vars.end()) {
                return vars;
            }
            if (std::find(bound_vars.begin(), bound_vars.end(), *sp) != bound_vars.end()) {
                return vars;
            }
            vars.push_back(*sp);
            return vars;
        }
        return vars; // non-variable atom
    }

    // If pattern is a list, recurse
    if (auto vp = std::any_cast<std::vector<std::any>>(&pattern)) {
        for (const auto& elem : *vp) {
            vars = pattern_free_vars1(elem, std::move(vars), bound_vars);
        }
        return vars;
    }

    // Other atoms (int, double) have no variables
    return vars;
}

// ---------------------------------------------------------------------------
// quotize
// ---------------------------------------------------------------------------

std::any quotize(const std::any& pattern) {
    if (!pattern.has_value()) return pattern;

    // Variables pass through
    if (auto sp = std::any_cast<std::string>(&pattern)) {
        return pattern;
    }

    // Lists: recurse on each element
    if (auto vp = std::any_cast<std::vector<std::any>>(&pattern)) {
        // Check for (:EVAL expr) form
        if (vp->size() == 2) {
            if (auto tag = std::any_cast<std::string>(&(*vp)[0])) {
                if (*tag == ":EVAL") {
                    // Return the second element directly (it's an evaluated form)
                    return (*vp)[1];
                }
            }
        }
        std::vector<std::any> result;
        result.reserve(vp->size());
        for (const auto& elem : *vp) {
            result.push_back(quotize(elem));
        }
        return result;
    }

    // Atoms pass through
    return pattern;
}

// ---------------------------------------------------------------------------
// any_to_string -- debug/display helper
// ---------------------------------------------------------------------------

std::string any_to_string(const std::any& val) {
    if (!val.has_value()) return "NIL";

    if (auto sp = std::any_cast<std::string>(&val)) return *sp;

    if (auto ip = std::any_cast<int>(&val)) return std::to_string(*ip);

    if (auto dp = std::any_cast<double>(&val)) {
        std::ostringstream oss;
        oss << *dp;
        return oss.str();
    }

    if (auto bp = std::any_cast<bool>(&val)) return *bp ? "T" : "NIL";

    if (auto vp = std::any_cast<std::vector<std::any>>(&val)) {
        std::ostringstream oss;
        oss << "(";
        for (size_t i = 0; i < vp->size(); ++i) {
            if (i > 0) oss << " ";
            oss << any_to_string((*vp)[i]);
        }
        oss << ")";
        return oss.str();
    }

    return "<unknown>";
}
