// -*- C++ -*-

/// LTRE (LTMS-based Tiny Rule Engine) - Core implementation
/// Converted from linter.lisp
/// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "linter.h"
#include "ldata.h"

// ================================================================
// Global LTRE pointer
// ================================================================

LTRE* current_ltre = nullptr;

// ================================================================
// Print helper
// ================================================================

std::string print_ltre(LTRE* ltre) {
    return "<LTRE: " + ltre->title + ">";
}

// ================================================================
// LTRE management
// ================================================================

static std::string ltre_make_node_string(TmsNode* node);

LTRE* create_ltre(const std::string& title, bool debugging) {
    auto ltre = new LTRE();
    ltre->title = title;
    ltre->debugging = debugging;

    // Create the underlying LTMS.
    ltre->ltms = create_ltms(
        "LTMS-OF-" + title,
        [](TmsNode* n) -> std::string {
            return ltre_make_node_string(n);
        },
        false,   // debugging
        true,    // checking_contradictions
        nullptr, // contradiction_handler
        nullptr, // enqueue_procedure (set below)
        false,   // cache_datums = nil (nodes_enabled = false)
        {},      // complete
        true     // delay_sat
    );

    // Set the enqueue procedure to feed into the LTRE queue.
    LTRE* captured_ltre = ltre;
    auto enqueue_fn = [captured_ltre](std::any pair) {
        enqueue(pair, captured_ltre);
    };
    change_ltms(ltre->ltms,
                nullptr,   // node_string_fn
                nullptr,   // debugging
                nullptr,   // checking_contradictions
                nullptr,   // contradiction_handler
                enqueue_fn // enqueue_procedure
    );

    current_ltre = ltre;
    return ltre;
}

void change_ltre(LTRE* ltre, std::optional<bool> debugging) {
    if (debugging.has_value()) {
        ltre->debugging = debugging.value();
    }
}

void in_ltre(LTRE* ltre) {
    current_ltre = ltre;
}

// ================================================================
// Assert / Assume interface
// ================================================================

void uassert(const std::any& fact, const std::any& just, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    LTRE* saved = current_ltre;
    current_ltre = ltre;
    ltre_assert(fact, just, ltre);
    run_rules(ltre);
    current_ltre = saved;
}

void uassume(const std::any& fact, const std::string& reason, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    LTRE* saved = current_ltre;
    current_ltre = ltre;
    ltre_assume(fact, reason, ltre);
    run_rules(ltre);
    current_ltre = saved;
}

// ================================================================
// Execution
// ================================================================

void run_forms(const std::vector<std::function<void()>>& forms, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    LTRE* saved = current_ltre;
    current_ltre = ltre;
    for (auto& form : forms) {
        form();
        run_rules(ltre);
    }
    current_ltre = saved;
}

void run(LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    // Simplified version -- no interactive REPL in C++.
    // In the Lisp version this runs a read-eval-print loop.
    // For C++ we just run any queued rules.
    run_rules(ltre);
}

// ================================================================
// Display
// ================================================================

void show(LTRE* ltre, std::ostream& stream) {
    if (!ltre) ltre = current_ltre;
    stream << "For LTRE " << ltre->title << ":";
    show_data(ltre, stream);
    show_rules(ltre, stream);
}

int show_by_informant(const std::any& informant, LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    int count = 0;

    // Try to extract informant as string for comparison.
    std::string informant_str;
    bool informant_is_string = false;
    if (auto p = std::any_cast<std::string>(&informant)) {
        informant_str = *p;
        informant_is_string = true;
    }

    for (auto* clause : ltre->ltms->clauses) {
        bool match = false;

        // Check if clause informant matches.
        // The informant can be a string or a more complex structure.
        if (auto p = std::any_cast<std::string>(&clause->informant)) {
            if (informant_is_string && *p == informant_str) {
                match = true;
            }
        }

        if (match) {
            count++;
            std::cout << "\n" << view_clause(clause);
        }
    }
    return count;
}

std::string view_clause(Clause* cl) {
    std::string result = "(OR";
    for (auto* lit : cl->literals) {
        if (lit->sign == NodeLabel::FALSE) {
            result += " (NOT " + view_node_form(lit->node) + ")";
        } else {
            result += " " + view_node_form(lit->node);
        }
    }
    result += ")";
    return result;
}

// ================================================================
// Queue / rule running
// ================================================================

void enqueue(const std::any& pair, LTRE* ltre) {
    ltre->queue.push_back(pair);
}

int run_rules(LTRE* ltre) {
    if (!ltre) ltre = current_ltre;

    int counter = 0;
    while (!ltre->queue.empty()) {
        auto form = ltre->queue.front();
        ltre->queue.erase(ltre->queue.begin());

        // In the Lisp version, queued items are (body . bindings) pairs.
        // We represent them as std::function<void()> wrapped in std::any.
        if (auto fn = std::any_cast<std::function<void()>>(&form)) {
            (*fn)();
        }
        counter++;
    }

    if (ltre->debugging) {
        std::cerr << "\n    " << counter << " rules run.";
    }
    ltre->rules_run += counter;
    return counter;
}

// ================================================================
// Stub for show_rules (rules module not yet converted)
// ================================================================

void show_rules(LTRE* ltre, std::ostream& stream) {
    if (!ltre) ltre = current_ltre;
    stream << "\nThere are " << ltre->rule_counter << " rules.";
    stream << "\n " << (ltre->queue.empty() ? "None" :
        std::to_string(ltre->queue.size())) << " queued.";
}

// ================================================================
// Internal: make_node_string callback for LTMS
// ================================================================

static std::string ltre_make_node_string(TmsNode* node) {
    // Try to extract a Datum* from the node's datum field.
    if (auto dp = std::any_cast<Datum*>(&node->datum)) {
        return show_datum(*dp);
    }
    return default_node_string(node);
}
