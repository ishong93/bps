/// Indirect Proof Mechanism - Implementation
/// Converted from indirect.lisp
/// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "indirect.h"
#include "funify.h"
#include <iostream>
#include <algorithm>

// ================================================================
// try_indirect_proof
// ================================================================
// Attempts to prove a fact by assuming its negation and deriving
// a contradiction. If the negation leads to contradiction, the
// fact must be true.

bool try_indirect_proof(const std::any& fact, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    if (is_known(fact, ltre)) return true;

    std::string fact_str = any_to_string(fact);

    with_contradiction_handler(ltre->ltms,
        [&](std::vector<Clause*>& contradictions, LTMS* ltms) -> bool {
            if (contradictions.empty()) return false;

            auto assumptions = assumptions_of_clause(contradictions[0]);
            Datum* datum = referent(fact, true, ltre);
            TmsNode* fact_node = datum->tms_node;

            auto it = std::find(assumptions.begin(), assumptions.end(), fact_node);
            if (it != assumptions.end()) {
                NodeLabel status = fact_node->label;
                retract_assumption(fact_node);

                // Build nogood from remaining assumptions
                std::vector<TmsNode*> remaining;
                for (auto* a : assumptions) {
                    remaining.push_back(a);
                }
                add_nogood(fact_node, status, remaining);
                return true;
            }
            return false;
        },
        [&]() {
            // Assume the negation of fact
            std::string neg_fact = "(:NOT " + fact_str + ")";

            Datum* datum = referent(std::any(neg_fact), true, ltre);
            TmsNode* node = datum->tms_node;
            convert_to_assumption(node);
            enable_assumption(node, NodeLabel::FALSE);

            run_rules(ltre);
        }
    );

    return is_known(fact, ltre);
}

// ================================================================
// Example
// ================================================================

void indirect_proof_example() {
    in_ltre(create_ltre("Indirect Proof Example"));

    // assert (:OR p q)
    ltre_assert(std::any(std::string("(:OR p q)")),
                std::any(std::string("user")));

    // assert (:IMPLIES p r)
    ltre_assert(std::any(std::string("(:IMPLIES p r)")),
                std::any(std::string("user")));

    // assert (:IMPLIES q r)
    ltre_assert(std::any(std::string("(:IMPLIES q r)")),
                std::any(std::string("user")));

    std::cout << "Before indirect proof, r known? "
              << (is_known(std::any(std::string("r"))) ? "yes" : "no")
              << std::endl;

    try_indirect_proof(std::any(std::string("r")));

    std::cout << "After indirect proof, r known? "
              << (is_known(std::any(std::string("r"))) ? "yes" : "no")
              << std::endl;
}
