#pragma once

/// Logic-based Truth Maintenance System, C++ translation.
/// Original: Common Lisp LTMS version 43, 7/5/93.
/// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include <any>
#include <algorithm>
#include <functional>
#include <iostream>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>
#include <stdexcept>
#include <variant>

// Forward declarations.
struct LTMS;
struct TmsNode;
struct Clause;
struct Literal;

// ---------------------------------------------------------------------------
// Enums
// ---------------------------------------------------------------------------

enum class NodeLabel { UNKNOWN, TRUE, FALSE };

inline std::string node_label_string(NodeLabel l) {
    switch (l) {
        case NodeLabel::TRUE:    return "TRUE";
        case NodeLabel::FALSE:   return "FALSE";
        default:                 return "UNKNOWN";
    }
}

inline NodeLabel node_label_opposite(NodeLabel l) {
    switch (l) {
        case NodeLabel::TRUE:  return NodeLabel::FALSE;
        case NodeLabel::FALSE: return NodeLabel::TRUE;
        default:               return NodeLabel::UNKNOWN;
    }
}

enum class ClauseStatus { NONE, SUBSUMED, QUEUED, DIRTY, NOT_INDEXED };

// ---------------------------------------------------------------------------
// Literal
// ---------------------------------------------------------------------------

struct Literal {
    TmsNode* node = nullptr;
    NodeLabel sign = NodeLabel::TRUE;
};

// ---------------------------------------------------------------------------
// AssumptionValue (for with_assumptions)
// ---------------------------------------------------------------------------

struct AssumptionValue {
    TmsNode* node = nullptr;
    NodeLabel label = NodeLabel::UNKNOWN;
};

// ---------------------------------------------------------------------------
// Support variant -- a node's support can be nullptr, a Clause*, or the
// sentinel string ":ENABLED-ASSUMPTION".
// ---------------------------------------------------------------------------

inline const std::string ENABLED_ASSUMPTION = ":ENABLED-ASSUMPTION";

// We use std::any for support to match the flexible Lisp semantics.
// Helpers to work with it:
inline bool support_is_null(const std::any& s) { return !s.has_value(); }
inline bool support_is_enabled_assumption(const std::any& s) {
    if (auto p = std::any_cast<std::string>(&s))
        return *p == ENABLED_ASSUMPTION;
    return false;
}
inline Clause* support_as_clause(const std::any& s) {
    if (auto p = std::any_cast<Clause*>(&s))
        return *p;
    return nullptr;
}

// ---------------------------------------------------------------------------
// TmsNode
// ---------------------------------------------------------------------------

struct TmsNode {
    int index = 0;
    std::any datum;               // flexible datum from the inference engine
    NodeLabel label = NodeLabel::UNKNOWN;
    std::any support;             // nullptr / Clause* / string(ENABLED_ASSUMPTION)
    std::vector<Clause*> true_clauses;
    std::vector<Clause*> false_clauses;
    std::any mark;
    bool is_assumption = false;
    std::vector<std::any> true_rules;
    std::vector<std::any> false_rules;
    LTMS* ltms = nullptr;
    Literal* true_literal = nullptr;
    Literal* false_literal = nullptr;
};

// ---------------------------------------------------------------------------
// Clause
// ---------------------------------------------------------------------------

struct Clause {
    int index = 0;
    std::any informant;
    std::vector<Literal*> literals;
    int pvs = 0;
    int length = 0;
    int sats = 0;
    ClauseStatus status = ClauseStatus::NONE;
};

// ---------------------------------------------------------------------------
// LTMS
// ---------------------------------------------------------------------------

struct LTMS {
    std::string title;
    int node_counter = 0;
    int clause_counter = 0;
    // When cache_datums is true we use this map; when false it stays empty
    // and nodes_enabled is false.
    bool nodes_enabled = true;
    std::unordered_map<std::string, TmsNode*> nodes;
    std::vector<Clause*> clauses;
    bool debugging = false;
    bool checking_contradictions = true;
    std::function<std::string(TmsNode*)> node_string_fn;
    std::vector<std::function<bool(std::vector<Clause*>&, LTMS*)>> contradiction_handlers;
    std::vector<Clause*> pending_contradictions;
    std::function<void(std::any)> enqueue_procedure;
    std::any complete;            // nil / bool(true) / trie-like
    std::vector<Clause*> violated_clauses;
    std::any queue;
    std::vector<Literal*> conses;
    bool delay_sat = true;
    int cons_size = 0;

    // Forward-reference function pointers for cltms integration.
    std::function<void(LTMS*)> ipia_fn;
    std::function<void(NodeLabel, TmsNode*, LTMS*)> propagate_more_unknownness_fn;
    std::function<void(LTMS*, std::vector<Literal*>&, std::any)> full_add_clause_fn;
    std::function<void(std::function<void(Clause*)>, std::any)> walk_trie_fn;
};

// ---------------------------------------------------------------------------
// Function declarations
// ---------------------------------------------------------------------------

// Print / string helpers
std::string print_ltms(LTMS* ltms);
std::string print_tms_node(TmsNode* node);
std::string print_clause(Clause* clause);
std::string node_string(TmsNode* node);
void debugging_ltms(LTMS* ltms, const std::string& msg, TmsNode* node = nullptr);
[[noreturn]] void ltms_error(const std::string& msg, const std::any& thing = {});
std::string default_node_string(TmsNode* n);

// Clause predicates
bool satisfied_clause(Clause* clause);
bool violated_clause(Clause* clause);

// Walk
void walk_clauses(LTMS* ltms, std::function<void(Clause*)> f);

// Creation / modification
LTMS* create_ltms(const std::string& title,
                  std::function<std::string(TmsNode*)> node_string_fn = nullptr,
                  bool debugging = false,
                  bool checking_contradictions = true,
                  std::function<bool(std::vector<Clause*>&, LTMS*)> contradiction_handler = nullptr,
                  std::function<void(std::any)> enqueue_procedure = nullptr,
                  bool cache_datums = true,
                  std::any complete = {},
                  bool delay_sat = true);

void change_ltms(LTMS* ltms,
                 std::function<std::string(TmsNode*)> node_string_fn = nullptr,
                 const bool* debugging = nullptr,
                 const bool* checking_contradictions = nullptr,
                 std::function<bool(std::vector<Clause*>&, LTMS*)> contradiction_handler = nullptr,
                 std::function<void(std::any)> enqueue_procedure = nullptr,
                 const std::any* complete = nullptr,
                 const bool* delay_sat = nullptr);

// Node queries
bool unknown_node(TmsNode* node);
bool known_node(TmsNode* node);
bool true_node(TmsNode* node);
bool false_node(TmsNode* node);

// Node creation
TmsNode* tms_create_node(LTMS* ltms, const std::string& datum, bool assumptionp = false);

// Assumption management
void enable_assumption(TmsNode* node, NodeLabel label);
void convert_to_assumption(TmsNode* node);
void retract_assumption(TmsNode* node);

// Formula handling
void add_formula(LTMS* ltms, std::any formula, std::any informant = {});

using LiteralList = std::vector<Literal*>;
using ClauseList = std::vector<LiteralList>;

LiteralList simplify_clause(LiteralList literals);
LiteralList sort_clause(LiteralList literals);

ClauseList normalize(LTMS* ltms, const std::any& exp);
ClauseList normalize_1(LTMS* ltms, const std::any& exp, bool negate);
ClauseList normalize_tax(LTMS* ltms, const std::vector<std::any>& exp, bool negate);
ClauseList normalize_conjunction(LTMS* ltms, const std::vector<std::any>& exp, bool negate);
ClauseList normalize_iff(LTMS* ltms, const std::vector<std::any>& exp, bool negate);
ClauseList normalize_disjunction(LTMS* ltms, const std::vector<std::any>& exp, bool negate);
ClauseList disjoin(const ClauseList& conj1, const ClauseList& conj2);

TmsNode* find_node(LTMS* ltms, const std::any& name);

// Clause management
void add_clause(std::vector<TmsNode*>& true_nodes, std::vector<TmsNode*>& false_nodes,
                std::any informant = {});
void add_clause_internal(std::vector<Literal*>& literals, std::any informant, bool internal_flag);
Clause* bcp_add_clause(LTMS* ltms, std::vector<Literal*>& literals, std::any informant,
                       bool index = true);

void insert_true_clause(Clause* cl, TmsNode* node);
void insert_false_clause(Clause* cl, TmsNode* node);

void add_nogood(TmsNode* culprit, NodeLabel sign, std::vector<TmsNode*>& assumptions);

// BCP
void check_clauses(LTMS* ltms, std::vector<Clause*> clauses_to_check);
void check_clause(LTMS* ltms, Clause* clause, std::vector<Clause*>& clauses_to_check);
Literal* find_unknown_pair(Clause* clause);

// Truth setting
void top_set_truth(TmsNode* node, NodeLabel value, std::any reason);
void set_truth(TmsNode* node, NodeLabel value, std::any reason,
               std::vector<Clause*>& clauses_to_check);

// Retraction
std::vector<TmsNode*> propagate_unknownness(TmsNode* in_node);
TmsNode* clause_consequent(Clause* clause);
void find_alternative_support(LTMS* ltms, std::vector<TmsNode*>& nodes);

// Contradiction handling
void check_for_contradictions(LTMS* ltms);
void contradiction_handler(LTMS* ltms, std::vector<Clause*>& violated_clauses);
void without_contradiction_check(LTMS* ltms, std::function<void()> body);
void with_contradiction_check(LTMS* ltms, std::function<void()> body);
void with_contradiction_handler(LTMS* ltms,
                                std::function<bool(std::vector<Clause*>&, LTMS*)> handler,
                                std::function<void()> body);
void with_assumptions(std::vector<AssumptionValue>& assumption_values, std::function<void()> body);

// Support queries
std::vector<TmsNode*> support_for_node(TmsNode* node, std::any* informant_out = nullptr);
std::vector<TmsNode*> assumptions_of_node(TmsNode* node);
std::vector<TmsNode*> assumptions_of_clause(Clause* in_clause);

// User interface / debugging
bool ask_user_handler(std::vector<Clause*>& contradictions, LTMS* ltms);
void handle_one_contradiction(Clause* violated_clause);
void print_contra_list(std::vector<TmsNode*>& nodes);
TmsNode* tms_answer(int num, std::vector<TmsNode*>& nodes);

bool avoid_all(std::vector<Clause*>& contradictions, LTMS* ltms);

// Display helpers
std::vector<TmsNode*> clause_antecedents(Clause* clause);
std::string signed_node_string(TmsNode* node);
std::vector<TmsNode*> node_consequences(TmsNode* node);

void why_node(TmsNode* node);
void why_nodes(LTMS* ltms);
void explain_node(TmsNode* node);
int explain_1(TmsNode* node, int line_count);

void pretty_print_clauses(LTMS* ltms);
void pretty_print_clause(Clause* clause);

void show_node_consequences(TmsNode* node);
void node_show_clauses(TmsNode* node);
void explore_network(TmsNode* node);
