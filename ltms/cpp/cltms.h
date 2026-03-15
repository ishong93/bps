#pragma once

/// Complete Logic-based Truth Maintenance System, C++ translation.
/// Original: Common Lisp CLTMS version 16 of 4/26/92.
/// Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ltms.h"
#include <functional>
#include <vector>
#include <variant>

// ---------------------------------------------------------------------------
// TrieEntry -- node in the trie used for clause storage.
// The trie maps sorted literal sequences to clauses.
// Each entry associates a Literal* key with either a sub-trie
// (vector of TrieEntry) or a leaf Clause*.
// ---------------------------------------------------------------------------

struct TrieEntry {
    Literal* literal = nullptr;
    // Children: either a vector of sub-entries, or a leaf clause.
    std::variant<std::vector<TrieEntry>, Clause*> sub_trie;

    TrieEntry() : sub_trie(static_cast<Clause*>(nullptr)) {}
    TrieEntry(Literal* lit, Clause* cl) : literal(lit), sub_trie(cl) {}
    TrieEntry(Literal* lit, std::vector<TrieEntry> children)
        : literal(lit), sub_trie(std::move(children)) {}
};

// Trie is a vector of TrieEntry.
using Trie = std::vector<TrieEntry>;

// ---------------------------------------------------------------------------
// QueueSlot -- bucket in the IPIA priority queue.
// Clauses are grouped by length for processing shortest-first.
// ---------------------------------------------------------------------------

struct QueueSlot {
    int length = 0;
    std::vector<Clause*> clauses;
};

// Queue is a vector of QueueSlot.
using Queue = std::vector<QueueSlot>;

// ---------------------------------------------------------------------------
// CompleteMode -- the "complete" field of LTMS can be:
//   COMPLETE_NONE   -- not a complete LTMS (nullptr in Lisp)
//   COMPLETE_TRUE   -- fully complete (T in Lisp)
//   COMPLETE_DELAY  -- delay mode (:DELAY in Lisp)
// ---------------------------------------------------------------------------

enum class CompleteMode { NONE, TRUE, DELAY };

// ---------------------------------------------------------------------------
// Function declarations -- complete LTMS operations
// ---------------------------------------------------------------------------

// Trie operations
void walk_trie(std::function<void(Clause*)> fn, Trie& trie);
void walk_trie_any(std::function<void(Clause*)> fn, std::any& trie);
std::vector<Clause*> collect(LTMS* ltms);
Trie build_trie(std::vector<Literal*>& lits, int start, Clause* cl);
Clause* subsumed(std::vector<Literal*>& lits, int start, Trie& trie);
Clause* subsumed_any(std::vector<Literal*>& lits, std::any& trie);
void add_to_trie(Clause* cl, LTMS* ltms);
void remove_subsumed(std::function<void(Clause*)> fn,
                     std::vector<Literal*>& lits, LTMS* ltms);
bool remove_subsumed_1(std::function<void(Clause*)> fn,
                       std::vector<Literal*>& lits, int start, Trie& trie);

// Queue operations
Queue& ltms_queue(LTMS* ltms);
void set_ltms_queue(LTMS* ltms, Queue queue);
void insert_list_2(Clause* cl, Queue& queue);
Queue insert_list(Clause* cl, Queue queue);
void insert_queue(Clause* cl, LTMS* ltms);

// Clause insertion (sorted by length)
void c_insert_true_clause(Clause* cl, TmsNode* node);
void c_insert_false_clause(Clause* cl, TmsNode* node);
void index_clause(Clause* cl, LTMS* ltms);
std::vector<Clause*> literal_connections(Literal* literal);

// Clause merging
struct DisjoinResult {
    std::vector<Literal*> literals;
    bool ok = false; // false means :FAIL
};
DisjoinResult disjoin_clauses(std::vector<Literal*>& terms1,
                              std::vector<Literal*>& terms2);

// Consensus
std::vector<Literal*> simplify_consensus(Clause* cl1, Clause* cl2,
                                          Literal* term1);
Clause* simplify_subsume_consensus(LTMS* ltms, Clause* cl1, Clause* cl2,
                                    Literal* p);

// Delay-sat check
bool delay_sat(Clause* clause, LTMS* ltms);

// Clause processing
void remove_clause(Clause* old_clause, Clause* new_clause);
Clause* install_clause(LTMS* ltms, std::vector<Literal*>& literals,
                       std::any informant);
Clause* process_clause(LTMS* ltms, std::vector<Literal*> literals,
                       std::any informant, bool internal_flag);

// Propagation
void c_propagate_more_unknownness(NodeLabel old_value, TmsNode* node, LTMS* ltms);

// Full add clause
void full_add_clause(LTMS* ltms, std::vector<Literal*>& literals,
                     std::any informant);

// IPIA
void ipia(LTMS* ltms);
Queue ipia_inner(LTMS* ltms, Queue sigma, std::vector<Clause*>& px,
                 Clause* s, Literal* lit);

// Complete LTMS control
void complete_ltms(LTMS* ltms);

// Complete normalization (overrides basic versions)
ClauseList c_normalize_disjunction(LTMS* ltms, const std::vector<std::any>& exp,
                                    bool negate);
ClauseList c_normalize_conjunction(LTMS* ltms, const std::vector<std::any>& exp,
                                    bool negate);

// Complete add-formula (overrides basic version)
void c_add_formula(LTMS* ltms, std::any formula, std::any informant = {});

// Add clause internal for cltms
void add_clause_internal_cltms(LTMS* ltms, std::vector<Literal*>& literals,
                               std::any informant, bool internal_flag);

// TMS environment query
std::vector<std::vector<Literal*>> tms_env(TmsNode* node, NodeLabel sign);

// PI (Prime Implicates)
LTMS* pi(std::any formula);

// Registration
void register_complete_ltms_functions(LTMS* ltms);

// Helper: get CompleteMode from ltms->complete
CompleteMode get_complete_mode(LTMS* ltms);
void set_complete_mode(LTMS* ltms, CompleteMode mode);
