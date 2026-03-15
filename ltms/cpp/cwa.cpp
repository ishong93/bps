/// Closed-World Assumptions - Implementation
/// Converted from cwa.lisp
/// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "cwa.h"
#include "funify.h"
#include <iostream>
#include <algorithm>
#include <sstream>

// ================================================================
// CWA form helpers
// ================================================================

bool is_cwa_form(const std::string& form) {
    // A CWA form looks like "(set-name CWA (members...))"
    return form.find(" CWA ") != std::string::npos;
}

std::string make_cwa_form(const std::string& set_name,
                          const std::vector<std::string>& members) {
    std::ostringstream oss;
    oss << "(" << set_name << " CWA (";
    for (size_t i = 0; i < members.size(); i++) {
        if (i > 0) oss << " ";
        oss << members[i];
    }
    oss << "))";
    return oss.str();
}

std::pair<std::string, std::vector<std::string>> parse_cwa_form(const std::string& cwa_form) {
    // Extract set name (first token) and members list
    // Format: "(set-name CWA (member1 member2 ...))"
    std::string inner = cwa_form;
    if (!inner.empty() && inner[0] == '(') inner = inner.substr(1);
    if (!inner.empty() && inner.back() == ')') inner.pop_back();

    size_t space1 = inner.find(' ');
    std::string set_name = inner.substr(0, space1);

    std::vector<std::string> members;
    // Find the inner list after "CWA "
    size_t paren_start = inner.find('(');
    if (paren_start != std::string::npos) {
        size_t paren_end = inner.rfind(')');
        std::string members_str = inner.substr(paren_start + 1, paren_end - paren_start - 1);
        std::istringstream iss(members_str);
        std::string member;
        while (iss >> member) {
            members.push_back(member);
        }
    }
    return {set_name, members};
}

// ================================================================
// Set information
// ================================================================

SetInfo get_set_information(const std::string& set_name, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    SetInfo info;
    std::string pattern = "(" + set_name + " HAS-MEMBER ?member)";
    auto possibles = fetch(pattern, ltre);

    for (const auto& possible : possibles) {
        if (is_true(std::any(possible), ltre)) {
            // Extract member name (third token)
            std::istringstream iss(possible);
            std::string tok;
            std::vector<std::string> tokens;
            while (iss >> tok) {
                // Remove parens
                tok.erase(std::remove(tok.begin(), tok.end(), '('), tok.end());
                tok.erase(std::remove(tok.begin(), tok.end(), ')'), tok.end());
                if (!tok.empty()) tokens.push_back(tok);
            }
            if (tokens.size() >= 3) {
                info.known_members.push_back(tokens[2]);
            }
        } else if (is_false(std::any(possible), ltre)) {
            std::istringstream iss(possible);
            std::string tok;
            std::vector<std::string> tokens;
            while (iss >> tok) {
                tok.erase(std::remove(tok.begin(), tok.end(), '('), tok.end());
                tok.erase(std::remove(tok.begin(), tok.end(), ')'), tok.end());
                if (!tok.empty()) tokens.push_back(tok);
            }
            if (tokens.size() >= 3) {
                info.known_not.push_back(tokens[2]);
            }
        }
    }
    return info;
}

// ================================================================
// Set members
// ================================================================

SetMembersResult set_members(const std::string& set_name, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    SetMembersResult result;
    std::string pattern = "(" + set_name + " MEMBERS ?elements)";
    auto matches = fetch(pattern, ltre);

    for (const auto& mform : matches) {
        if (is_true(std::any(mform), ltre)) {
            result.found = true;
            // Extract members from the form
            auto [name, members] = parse_cwa_form(mform);
            result.members = members;

            // Find associated CWA
            auto assumptions = ltre_assumptions_of(std::any(mform), ltre);
            for (const auto& asn : assumptions) {
                if (is_cwa_form(asn) && asn.find(set_name) == 1) {
                    result.cwa_form = asn;
                    break;
                }
            }
            break;
        }
    }
    return result;
}

// ================================================================
// CWA management
// ================================================================

void assume_cwa_if_needed(const std::string& cwa_form, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    if (is_false(std::any(cwa_form), ltre)) {
        TmsNode* node = get_tms_node(std::any(cwa_form), ltre);
        if (node) {
            auto unknown_nodes = propagate_unknownness(node);
            find_alternative_support(ltre->ltms, unknown_nodes);
        }
    }
    ltre_assume(std::any(cwa_form), "CWA", ltre);
}

void retract_cwa(const std::string& cwa, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    std::string reason;
    if (already_assumed(std::any(cwa), ltre)) {
        Datum* d = referent(std::any(cwa), true, ltre);
        reason = d->assumption_reason;
    }
    if (!reason.empty()) {
        ltre_retract(std::any(cwa), reason, ltre);
    }
}

void retract_cwas(const std::string& set_name, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    std::string pattern = "(" + set_name + " CWA ?members)";
    auto cwas = fetch(pattern, ltre);
    for (const auto& cwa : cwas) {
        if (is_known(std::any(cwa), ltre) && already_assumed(std::any(cwa), ltre)) {
            retract_cwa(cwa, ltre);
        }
    }
}

bool cwa_invalid(const std::string& cwa, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    auto [set_name, presumed_els] = parse_cwa_form(cwa);

    // Check that all presumed elements are still true
    for (const auto& el : presumed_els) {
        std::string hm = "(" + set_name + " HAS-MEMBER " + el + ")";
        if (!is_true(std::any(hm), ltre)) {
            return true;
        }
    }

    // Check no new elements have appeared
    std::string pattern = "(" + set_name + " HAS-MEMBER ?el)";
    auto hm_forms = fetch(pattern, ltre);
    for (const auto& hm_form : hm_forms) {
        if (is_true(std::any(hm_form), ltre)) {
            // Extract the element
            std::istringstream iss(hm_form);
            std::string tok;
            std::vector<std::string> tokens;
            while (iss >> tok) {
                tok.erase(std::remove(tok.begin(), tok.end(), '('), tok.end());
                tok.erase(std::remove(tok.begin(), tok.end(), ')'), tok.end());
                if (!tok.empty()) tokens.push_back(tok);
            }
            if (tokens.size() >= 3) {
                std::string member = tokens[2];
                if (std::find(presumed_els.begin(), presumed_els.end(), member)
                    == presumed_els.end()) {
                    return true;
                }
            }
        }
    }
    return false;
}

// ================================================================
// Close set
// ================================================================

CloseSetResult close_set_if_needed(const std::string& set_name, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    auto sm = set_members(set_name, ltre);
    if (sm.found && !sm.cwa_form.empty()) {
        return {sm.members, sm.cwa_form, false};
    }
    return close_set(set_name, ltre);
}

CloseSetResult close_set(const std::string& set_name, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    auto info = get_set_information(set_name, ltre);
    std::string cwa_form = make_cwa_form(set_name, info.known_members);

    std::ostringstream oss;
    oss << "(" << set_name << " MEMBERS (";
    for (size_t i = 0; i < info.known_members.size(); i++) {
        if (i > 0) oss << " ";
        oss << info.known_members[i];
    }
    oss << "))";
    std::string members_form = oss.str();

    retract_cwas(set_name, ltre);
    assume_cwa_if_needed(cwa_form, ltre);

    // Justify the CWA
    // Build the justification assertion
    std::vector<std::any> just_parts;
    just_parts.push_back(std::any(std::string("CWA-JUSTIFICATION")));

    // Build the antecedent: (:AND (SET name) (name HAS-MEMBER m1) ... (:NOT (name HAS-MEMBER nm1)) ... cwa-form)
    std::vector<std::any> ante;
    ante.push_back(std::any(std::string(":AND")));
    ante.push_back(std::any(std::string("(SET " + set_name + ")")));
    for (const auto& m : info.known_members) {
        ante.push_back(std::any(std::string("(" + set_name + " HAS-MEMBER " + m + ")")));
    }
    for (const auto& nm : info.known_not) {
        ante.push_back(std::any(std::string("(:NOT (" + set_name + " HAS-MEMBER " + nm + "))")));
    }
    ante.push_back(std::any(cwa_form));
    just_parts.push_back(std::any(ante));
    just_parts.push_back(std::any(members_form));

    ltre_assert(std::any(just_parts), std::any(std::string("SET-CWA-CLOSURE")), ltre);

    return {info.known_members, cwa_form, true};
}

// ================================================================
// Contradiction handling
// ================================================================

void set_cwa_handler(std::vector<Clause*>& clauses, LTMS* ltms,
                     const std::string& set_name, const std::string& cwa) {
    TmsNode* cwa_node = get_tms_node(std::any(cwa));

    for (auto* cl : clauses) {
        auto asns = assumptions_of_clause(cl);
        bool has_cwa = std::find(asns.begin(), asns.end(), cwa_node) != asns.end();
        if (has_cwa && cwa_invalid(cwa)) {
            retract_cwa(cwa);
            // In the Lisp version, this does a throw to a catch tag.
            // In C++, we'd use an exception or callback mechanism.
            return;
        }
    }
}

// ================================================================
// Test
// ================================================================

void cwa_shakedown() {
    in_ltre(create_ltre("CWA Test One", true));

    ltre_assert(std::any(std::string("(SET foo)")), std::any(std::string("user")));

    // Close and add members...
    auto r1 = close_set("foo");
    ltre_assume(std::any(std::string("(foo HAS-MEMBER a)")), "A-IN");
    auto r2 = close_set("foo");
    ltre_assume(std::any(std::string("(foo HAS-MEMBER b)")), "B-IN");
    auto r3 = close_set("foo");
    run_rules();

    std::cout << "\nCWA shakedown complete." << std::endl;
}
