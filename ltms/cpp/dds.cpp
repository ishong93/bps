/// Dependency-Directed Search - Implementation
/// Converted from dds.lisp
/// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "dds.h"
#include "funify.h"
#include <iostream>
#include <algorithm>

bool debug_dds = false;

// ================================================================
// DD-Search
// ================================================================
// Dependency-directed search iterates over choice sets.
// For each choice in the current set:
//   - If already false, skip it (known loser)
//   - If already true, recurse with remaining sets
//   - Otherwise, assume it, run rules, and recurse.
//     On contradiction, retract and record a nogood.

void dd_search(const std::vector<std::vector<std::any>>& choice_sets,
               std::function<void()> end_fn,
               LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    if (choice_sets.empty()) {
        if (debug_dds) std::cerr << "\n    DDS: Found solution.";
        end_fn();
        return;
    }

    const auto& choices = choice_sets[0];
    std::vector<std::vector<std::any>> remaining(
        choice_sets.begin() + 1, choice_sets.end());

    for (const auto& choice : choices) {
        std::string choice_str = any_to_string(choice);
        if (debug_dds) {
            std::cerr << "\n    DDS: Considering " << choice_str << "...";
        }

        if (is_false(choice, ltre)) {
            if (debug_dds) {
                std::cerr << "\n    DDS: " << choice_str << " already known nogood.";
            }
            continue;
        }

        if (is_true(choice, ltre)) {
            if (debug_dds) {
                std::cerr << "\n    DDS: " << choice_str << " true by implication.";
            }
            dd_search(remaining, end_fn, ltre);
            return;
        }

        if (debug_dds) {
            std::cerr << "\n    DDS: Assuming " << choice_str << ".";
        }

        bool contradiction_found = false;
        std::vector<std::string> losers;

        with_contradiction_handler(ltre->ltms,
            [&](std::vector<Clause*>& clauses, LTMS* ltms_ptr) -> bool {
                for (auto* cl : clauses) {
                    auto asns = assumptions_of_clause(cl);
                    for (auto* asn : asns) {
                        std::string asn_form = view_node_form(asn);
                        if (asn_form == choice_str ||
                            (negated_proposition(choice_str) &&
                             asn_form == strip_negation(choice_str))) {
                            contradiction_found = true;
                            for (auto* a : asns) {
                                if (a != asn) {
                                    losers.push_back(signed_view_node(a));
                                }
                            }
                            return true;
                        }
                    }
                }
                return false;
            },
            [&]() {
                // Assume the choice
                Datum* datum = referent(choice, true, ltre);
                TmsNode* node = datum->tms_node;
                convert_to_assumption(node);
                bool is_neg = negated_proposition(choice_str);
                enable_assumption(node, is_neg ? NodeLabel::FALSE : NodeLabel::TRUE);

                run_rules(ltre);

                if (!contradiction_found) {
                    dd_search(remaining, end_fn, ltre);
                }

                // Retract the assumption
                retract_assumption(node);
            }
        );

        if (contradiction_found && !losers.empty()) {
            if (debug_dds) {
                std::cerr << "\n    DDS: " << choice_str << " inconsistent.";
            }
            // Assert nogood: (:NOT (:AND choice loser1 loser2 ...))
            std::string nogood = "(:NOT (:AND " + choice_str;
            for (const auto& l : losers) {
                nogood += " " + l;
            }
            nogood += "))";
            ltre_assert(std::any(nogood),
                        std::any(std::string("DD-SEARCH-NOGOOD")), ltre);
        }
    }
}

// ================================================================
// Test
// ================================================================

void test_dd_search(bool debugging) {
    in_ltre(create_ltre("DDS Test", debugging));

    // In the Lisp version, rules are installed via (rule ...) macros:
    //   A ^ C => contradiction
    //   B ^ E => contradiction
    // Since the rule macro system is not ported, we just demonstrate
    // the search structure.
    std::cout << "DD-Search test framework ready." << std::endl;
    std::cout << "(Full test requires rule macro system)" << std::endl;
}

void show_dd_test_solution(LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    std::vector<std::string> vars = {"F", "E", "D", "C", "B", "A"};
    std::vector<std::string> result;
    for (const auto& var : vars) {
        if (is_true(std::any(var), ltre)) {
            result.push_back(var);
        }
    }
    std::cout << "\n Consistent solution: (";
    for (size_t i = 0; i < result.size(); i++) {
        if (i > 0) std::cout << " ";
        std::cout << result[i];
    }
    std::cout << ")." << std::endl;
}
