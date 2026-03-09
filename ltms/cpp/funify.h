#pragma once

/// Open-coded Unification for LTRE
/// Converted from funify.lisp
/// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include <any>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

// ---------------------------------------------------------------------------
// Lisp-form representation
// ---------------------------------------------------------------------------
// In the LTMS/LTRE system, Lisp forms are represented using std::any:
//   - Atoms (symbols): std::string
//   - Numbers (int):   int
//   - Numbers (float): double
//   - Lists:           std::vector<std::any>
//   - Nil/empty:       empty std::any (no value)
//
// Pattern variables are strings starting with '?', e.g. "?x", "?foo".
// ---------------------------------------------------------------------------

// Binding map: variable name -> bound value
using BindingMap = std::unordered_map<std::string, std::any>;

// ---------------------------------------------------------------------------
// Core functions
// ---------------------------------------------------------------------------

/// Check if a value is a pattern variable (a string starting with '?').
bool is_variable(const std::any& x);

/// Check if a string is a variable name (starts with '?').
bool is_variable_str(const std::string& s);

/// Compare two std::any values for structural equality.
/// Handles strings, ints, doubles, vectors, and empty values.
bool any_equal(const std::any& a, const std::any& b);

/// Unify two expressions (represented as std::any).
/// Returns a BindingMap on success, or std::nullopt on failure.
/// The optional `bindings` parameter provides initial bindings.
std::optional<BindingMap> unify(const std::any& pattern,
                                const std::any& datum,
                                BindingMap bindings = {});

/// Substitute bindings into a pattern, replacing variables with
/// their bound values. Variables not in bindings are left as-is.
std::any sublis(const BindingMap& bindings, const std::any& pattern);

// ---------------------------------------------------------------------------
// Pattern variable utilities (from funify.lisp)
// ---------------------------------------------------------------------------

/// Find all free variables in a pattern that are not in `bound_vars`.
/// Returns a list of variable names (strings starting with '?').
std::vector<std::string> pattern_free_variables(
    const std::any& pattern,
    const std::vector<std::string>& bound_vars = {});

/// Recursive helper for pattern_free_variables.
std::vector<std::string> pattern_free_vars1(
    const std::any& pattern,
    std::vector<std::string> vars,
    const std::vector<std::string>& bound_vars);

/// Quotize a pattern: used by the rule system to construct
/// code that reconstructs pattern values at runtime.
/// For C++, this returns a deep copy of the pattern with
/// variables left intact and non-variable atoms preserved.
std::any quotize(const std::any& pattern);

// ---------------------------------------------------------------------------
// Lookup helper
// ---------------------------------------------------------------------------

/// Look up a variable in a binding map.
/// Returns the bound value if found, or std::nullopt.
std::optional<std::any> lookup_binding(const std::string& var,
                                       const BindingMap& bindings);

/// Check if a variable occurs in an expression (occurs check).
bool occurs_in(const std::string& var, const std::any& expr,
               const BindingMap& bindings);

// ---------------------------------------------------------------------------
// Conversion helpers
// ---------------------------------------------------------------------------

/// Convert a std::any to string representation (for debugging/display).
std::string any_to_string(const std::any& val);
