/// LTRE Database - Implementation
/// Converted from ldata.lisp
/// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ldata.h"
#include "linter.h"
#include "funify.h"
#include <iostream>
#include <sstream>
#include <algorithm>

// ================================================================
// Connective list
// ================================================================

static const std::vector<std::string> connective_list = {
    ":IMPLIES", ":AND", ":OR", ":IFF", ":NOT", ":TAXONOMY"
};

bool is_connective(const std::string& s) {
    return std::find(connective_list.begin(), connective_list.end(), s)
           != connective_list.end();
}

// ================================================================
// Proposition classification
// ================================================================

bool simple_proposition(const std::string& x) {
    // A simple proposition is an atom (non-list) or a list whose
    // head is not a connective. Since we represent "lists" as strings
    // like "(foo bar)", we check if the string starts with '(' and
    // then check if the first word after '(' is a connective.
    if (x.empty()) return true;
    if (x[0] != '(') return true;  // atom => simple
    // Extract the first token after '('
    size_t start = 1;
    while (start < x.size() && x[start] == ' ') start++;
    size_t end = start;
    while (end < x.size() && x[end] != ' ' && x[end] != ')') end++;
    std::string head = x.substr(start, end - start);
    return !is_connective(head);
}

bool negated_proposition(const std::string& form) {
    // (:NOT <simple-proposition>)
    if (form.size() < 6) return false;
    if (form[0] != '(') return false;
    // Check if starts with "(:NOT "
    if (form.substr(0, 6) != "(:NOT ") return false;
    // Extract the inner part
    std::string inner = form.substr(6, form.size() - 7);  // strip "(:NOT " and ")"
    return simple_proposition(inner);
}

std::string strip_negation(const std::string& form) {
    if (negated_proposition(form)) {
        return form.substr(6, form.size() - 7);
    }
    return form;
}

// ================================================================
// Helper: extract fact as string
// ================================================================

static std::string fact_to_string(const std::any& fact) {
    return any_to_string(fact);
}

// Helper: extract the "form" (stripping negation if needed)
static std::string get_form_string(const std::any& fact) {
    std::string s = fact_to_string(fact);
    if (negated_proposition(s)) {
        return strip_negation(s);
    }
    return s;
}

// Helper: extract first symbol from a fact for dbclass lookup
static std::string get_dbclass_name(const std::any& fact) {
    std::string s = fact_to_string(fact);
    if (negated_proposition(s)) {
        s = strip_negation(s);
    }
    // If it's a list like "(foo bar)", extract "foo"
    if (!s.empty() && s[0] == '(') {
        size_t start = 1;
        while (start < s.size() && s[start] == ' ') start++;
        size_t end = start;
        while (end < s.size() && end != ' ' && s[end] != ')') end++;
        return s.substr(start, end - start);
    }
    return s;  // atom
}

// ================================================================
// Assert / Assume
// ================================================================

void ltre_assert(const std::any& fact, const std::any& just, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    if (ltre->debugging) {
        std::cerr << "\n    Asserting " << fact_to_string(fact)
                  << " via " << fact_to_string(just) << ".";
    }
    std::any formula = build_tms_formula(fact, ltre);
    add_formula(ltre->ltms, formula, just);
}

Datum* ltre_assume(const std::any& fact, const std::string& reason, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    std::string fact_str = fact_to_string(fact);
    bool is_neg = negated_proposition(fact_str);
    std::string form_str = is_neg ? strip_negation(fact_str) : fact_str;

    Datum* datum = referent(fact, true, ltre);
    TmsNode* node = datum->tms_node;

    if (ltre->debugging) {
        std::cerr << "\n    Assuming " << fact_str << " via " << reason << ".";
    }

    // If the fact is a complex formula (not simple and not negated),
    // install clauses for it.
    if (!is_neg && !simple_proposition(fact_str)) {
        // Build (:IMPLIES node formula)
        std::vector<std::any> imp;
        imp.push_back(std::any(std::string(":IMPLIES")));
        imp.push_back(std::any(node));  // TmsNode*
        imp.push_back(build_tms_formula(fact, ltre));
        add_formula(ltre->ltms, std::any(imp), std::any(reason));
    }

    if (datum->assumption_reason.empty()) {
        datum->assumption_reason = reason;
        convert_to_assumption(node);
        enable_assumption(node, is_neg ? NodeLabel::FALSE : NodeLabel::TRUE);
    } else if (reason == datum->assumption_reason) {
        // Already assumed for same reason - do nothing
    } else {
        throw std::runtime_error(
            "Fact " + show_datum(datum) + " assumed because of " +
            datum->assumption_reason + " assumed again because of " + reason);
    }
    return datum;
}

bool already_assumed(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    Datum* d = referent(fact, true, ltre);
    return d && !d->assumption_reason.empty();
}

void quiet_assert(const std::any& fact, LTRE* ltre, const std::any& just) {
    if (!ltre) ltre = current_ltre;
    without_contradiction_check(ltre->ltms, [&]() {
        ltre_assert(fact, just, ltre);
    });
}

// ================================================================
// Formula building
// ================================================================

std::any build_tms_formula(const std::any& formula, LTRE* ltre) {
    // If formula is a vector (list), check if head is a connective
    if (auto vp = std::any_cast<std::vector<std::any>>(&formula)) {
        if (!vp->empty()) {
            if (auto sp = std::any_cast<std::string>(&(*vp)[0])) {
                if (is_connective(*sp)) {
                    // Recursively build sub-formulas
                    std::vector<std::any> result;
                    result.push_back((*vp)[0]);  // connective
                    for (size_t i = 1; i < vp->size(); i++) {
                        result.push_back(build_tms_formula((*vp)[i], ltre));
                    }
                    return std::any(result);
                }
            }
        }
    }

    // If formula is a TmsNode* already, return it
    if (auto np = std::any_cast<TmsNode*>(&formula)) {
        return formula;
    }

    // Otherwise, it's a datum reference - get its TMS node
    Datum* datum = referent(formula, true, ltre);
    return std::any(datum->tms_node);
}

// ================================================================
// Retraction
// ================================================================

TmsNode* ltre_retract(const std::any& fact, const std::string& just,
                      LTRE* ltre, bool quiet) {
    if (!ltre) ltre = current_ltre;
    Datum* datum = referent(fact, true, ltre);
    TmsNode* node = datum->tms_node;

    if (!node->is_assumption) {
        if (!quiet) {
            std::cout << "\n" << show_datum(datum) << " isn't an assumption.";
        }
    } else if (!known_node(node)) {
        if (!quiet) {
            std::cout << "\nThe assumption " << fact_to_string(fact)
                      << " is not currently in.";
        }
    } else if (just == datum->assumption_reason) {
        if (ltre->debugging) {
            std::cerr << "\n    Retracting " << fact_to_string(fact)
                      << " via " << just << ".";
        }
        datum->assumption_reason.clear();
        retract_assumption(node);
    } else if (!quiet) {
        std::cout << "\n" << just << " not source of assumption for "
                  << fact_to_string(fact);
    }
    return node;
}

// ================================================================
// Contradiction
// ================================================================

void ltre_contradiction(const std::vector<std::any>& losers, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    std::vector<TmsNode*> trues, falses;

    for (const auto& fact : losers) {
        Datum* datum = referent(fact, true, ltre);
        std::string fact_str = fact_to_string(fact);
        if (negated_proposition(fact_str)) {
            falses.push_back(datum->tms_node);
        } else {
            trues.push_back(datum->tms_node);
        }
    }

    add_clause(trues, falses, std::any(std::string("DECLARED-CONTRADICTION")));
}

// ================================================================
// DbClass management
// ================================================================

DbClass* get_dbclass(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    std::string name = get_dbclass_name(fact);
    if (name.empty()) {
        throw std::runtime_error("NIL can't be a dbclass.");
    }

    // Check if it's a variable
    if (is_variable_str(name)) {
        throw std::runtime_error("Dbclass unbound: " + name);
    }

    // Look up or create
    auto it = ltre->dbclass_table.find(name);
    if (it != ltre->dbclass_table.end()) {
        return it->second;
    }

    auto* dbclass = new DbClass();
    dbclass->name = name;
    dbclass->ltre = ltre;
    ltre->dbclass_table[name] = dbclass;
    return dbclass;
}

// ================================================================
// Referent / Insert / Fetch
// ================================================================

Datum* referent(const std::any& fact, bool virtual_mode, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    if (virtual_mode) {
        return insert(fact, ltre);
    }
    return referent1(fact, ltre);
}

Datum* referent1(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    std::string form = get_form_string(fact);
    DbClass* dbc = get_dbclass(fact, ltre);

    for (auto* candidate : dbc->facts) {
        if (candidate->lisp_form == form) {
            return candidate;
        }
    }
    return nullptr;
}

Datum* insert(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    Datum* existing = referent1(fact, ltre);
    if (existing) return existing;

    std::string form = get_form_string(fact);

    auto* datum = new Datum();
    datum->counter = ++(ltre->datum_counter);
    datum->ltre = ltre;
    datum->lisp_form = form;
    datum->dbclass = get_dbclass(std::any(form), ltre);

    // Create a TMS node with datum as its datum
    datum->tms_node = tms_create_node(ltre->ltms, form);
    datum->tms_node->datum = std::any(datum);

    // Add to dbclass facts
    datum->dbclass->facts.push_back(datum);

    // Try existing rules on this new datum
    try_rules(datum);

    return datum;
}

std::vector<std::string> fetch(const std::string& pattern, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    std::string form = pattern;
    if (negated_proposition(form)) {
        form = strip_negation(form);
    }

    std::vector<std::string> unifiers;
    auto candidates = get_candidates(form, ltre);
    for (auto* candidate : candidates) {
        if (unify_match(form, candidate->lisp_form)) {
            unifiers.push_back(candidate->lisp_form);
        }
    }
    return unifiers;
}

std::vector<Datum*> get_candidates(const std::string& pattern, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    DbClass* dbc = get_dbclass(std::any(pattern), ltre);
    return dbc->facts;
}

void map_dbclass(std::function<void(DbClass*)> proc, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    for (auto& [name, dbclass] : ltre->dbclass_table) {
        proc(dbclass);
    }
}

// ================================================================
// Truth queries
// ================================================================

bool is_true(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    Datum* r = referent(fact, false, ltre);
    if (!r) return false;
    std::string fact_str = fact_to_string(fact);
    if (negated_proposition(fact_str)) {
        return false_node(r->tms_node);
    }
    return true_node(r->tms_node);
}

bool is_false(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    Datum* r = referent(fact, false, ltre);
    if (!r) return false;
    std::string fact_str = fact_to_string(fact);
    if (negated_proposition(fact_str)) {
        return true_node(r->tms_node);
    }
    return false_node(r->tms_node);
}

bool is_known(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    Datum* r = referent(fact, false, ltre);
    if (!r) return false;
    return known_node(r->tms_node);
}

bool is_unknown(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    Datum* r = referent(fact, false, ltre);
    if (!r) return true;
    return unknown_node(r->tms_node);
}

std::string label_of(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    Datum* r = referent(fact, false, ltre);
    if (!r) return "UNKNOWN";
    return node_label_string(r->tms_node->label);
}

// ================================================================
// Why / TMS node access
// ================================================================

void why(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    Datum* r = referent(fact, false, ltre);
    if (r) {
        why_node(r->tms_node);
        if (known_node(r->tms_node) && !r->assumption_reason.empty()) {
            std::cout << " (" << r->assumption_reason << ")";
        }
    } else {
        std::cout << "\n" << fact_to_string(fact) << " not in database.";
    }
}

TmsNode* get_tms_node(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    Datum* d = referent(fact, true, ltre);
    return d ? d->tms_node : nullptr;
}

// ================================================================
// View / Display
// ================================================================

std::string view_node_form(TmsNode* node) {
    if (auto dp = std::any_cast<Datum*>(&node->datum)) {
        return (*dp)->lisp_form;
    }
    return default_node_string(node);
}

std::string signed_view_node(TmsNode* node) {
    if (false_node(node)) {
        return "(:NOT " + view_node_form(node) + ")";
    }
    if (true_node(node)) {
        return view_node_form(node);
    }
    throw std::runtime_error("SIGNED-VIEW-NODE requires knowing label");
}

std::string show_datum(Datum* datum) {
    return datum->lisp_form;
}

std::string make_node_string(TmsNode* node) {
    if (auto dp = std::any_cast<Datum*>(&node->datum)) {
        return show_datum(*dp);
    }
    return default_node_string(node);
}

// ================================================================
// Assumptions / Consequences
// ================================================================

std::vector<std::string> ltre_assumptions_of(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    Datum* d = referent(fact, true, ltre);
    auto nodes = assumptions_of_node(d->tms_node);
    std::vector<std::string> result;
    for (auto* n : nodes) {
        result.push_back(view_node_form(n));
    }
    return result;
}

void consequences(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    if (!is_known(fact, ltre)) return;
    TmsNode* node = get_tms_node(fact, ltre);
    if (node) show_node_consequences(node);
}

void explore(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    TmsNode* node = get_tms_node(fact, ltre);
    if (node) explore_network(node);
}

// ================================================================
// Data display / lookup
// ================================================================

int show_data(LTRE* ltre, std::ostream& stream) {
    if (!ltre) ltre = current_ltre;
    int counter = 0;
    stream << "\n" << ltre->datum_counter << " facts total.";

    for (auto& [key, dbclass] : ltre->dbclass_table) {
        for (auto* datum : dbclass->facts) {
            counter++;
            std::string status;
            if (true_node(datum->tms_node)) {
                status = "TRUE";
            } else if (false_node(datum->tms_node)) {
                status = "FALSE";
            } else {
                status = "UNKNOWN";
            }
            stream << "\n" << show_datum(datum) << ": " << status;
        }
    }
    return counter;
}

Datum* get_datum(int num, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    for (auto& [key, dbclass] : ltre->dbclass_table) {
        for (auto* datum : dbclass->facts) {
            if (datum->counter == num) {
                return datum;
            }
        }
    }
    return nullptr;
}

Clause* get_clause(int num, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    for (auto* clause : ltre->ltms->clauses) {
        if (clause->index == num) {
            return clause;
        }
    }
    return nullptr;
}

std::vector<std::string> fetch_global(const std::string& pattern,
                                       const std::string& status,
                                       LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    std::vector<std::string> results;

    map_dbclass([&](DbClass* dbclass) {
        for (auto* datum : dbclass->facts) {
            bool status_ok = true;
            if (status == "TRUE") {
                status_ok = true_node(datum->tms_node);
            } else if (status == "FALSE") {
                status_ok = false_node(datum->tms_node);
            } else if (status == "KNOWN") {
                status_ok = known_node(datum->tms_node);
            } else if (status == "UNKNOWN") {
                status_ok = !known_node(datum->tms_node);
            }

            if (status_ok && unify_match(pattern, datum->lisp_form)) {
                results.push_back(datum->lisp_form);
            }
        }
    }, ltre);

    return results;
}

// ================================================================
// Variable check and simple unify
// ================================================================

bool is_variable(const std::string& s) {
    return is_variable_str(s);
}

bool unify_match(const std::string& pattern, const std::string& candidate) {
    // Simple string-based pattern matching.
    // Variables (starting with '?') match any token.
    // For a more complete implementation, parse into std::any lists
    // and use the full unify() from funify.h.

    // Quick equality check
    if (pattern == candidate) return true;

    // If pattern has no variables, it must match exactly
    if (pattern.find('?') == std::string::npos) {
        return pattern == candidate;
    }

    // Tokenize both strings
    auto tokenize = [](const std::string& s) -> std::vector<std::string> {
        std::vector<std::string> tokens;
        std::string current;
        for (char c : s) {
            if (c == '(' || c == ')' || c == ' ') {
                if (!current.empty()) {
                    tokens.push_back(current);
                    current.clear();
                }
            } else {
                current += c;
            }
        }
        if (!current.empty()) tokens.push_back(current);
        return tokens;
    };

    auto pat_tokens = tokenize(pattern);
    auto cand_tokens = tokenize(candidate);

    if (pat_tokens.size() != cand_tokens.size()) return false;

    for (size_t i = 0; i < pat_tokens.size(); i++) {
        if (is_variable_str(pat_tokens[i])) continue;  // variable matches anything
        if (pat_tokens[i] != cand_tokens[i]) return false;
    }
    return true;
}
