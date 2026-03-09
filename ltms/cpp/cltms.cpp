/// Complete Logic-based Truth Maintenance System, C++ translation.
/// Original: Common Lisp CLTMS version 16 of 4/26/92.
/// Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "cltms.h"
#include <algorithm>
#include <cassert>
#include <iostream>

// ===========================================================================
// Helper: CompleteMode <-> std::any conversion
// ===========================================================================

CompleteMode get_complete_mode(LTMS* ltms) {
    if (!ltms->complete.has_value()) {
        return CompleteMode::NONE;
    }
    if (auto p = std::any_cast<CompleteMode>(&ltms->complete)) {
        return *p;
    }
    // Legacy: check for bool or string representations.
    if (auto p = std::any_cast<bool>(&ltms->complete)) {
        return *p ? CompleteMode::TRUE : CompleteMode::NONE;
    }
    if (auto p = std::any_cast<std::string>(&ltms->complete)) {
        if (*p == "DELAY" || *p == ":DELAY") return CompleteMode::DELAY;
        if (*p == "COMPLETE" || *p == ":COMPLETE") return CompleteMode::TRUE;
    }
    return CompleteMode::NONE;
}

void set_complete_mode(LTMS* ltms, CompleteMode mode) {
    ltms->complete = mode;
}

// ===========================================================================
// Trie operations
// ===========================================================================

// walk_trie applies fn to every clause stored in the trie.
void walk_trie(std::function<void(Clause*)> fn, Trie& trie) {
    for (auto& entry : trie) {
        if (auto* cl = std::get_if<Clause*>(&entry.sub_trie)) {
            if (*cl) fn(*cl);
        } else if (auto* children = std::get_if<Trie>(&entry.sub_trie)) {
            walk_trie(fn, *children);
        }
    }
}

// walk_trie_any walks the trie stored in a std::any (which holds a Trie).
void walk_trie_any(std::function<void(Clause*)> fn, std::any& trie_any) {
    if (!trie_any.has_value()) return;
    if (auto* t = std::any_cast<Trie>(&trie_any)) {
        walk_trie(fn, *t);
    }
}

// collect returns all clauses stored in the LTMS's trie (via clauses field used as trie).
std::vector<Clause*> collect(LTMS* ltms) {
    std::vector<Clause*> result;
    // The trie is stored in ltms->complete when acting as clause storage,
    // but for temporary LTMSes used in normalization we store in clauses_trie.
    // We check the queue/complete field. For temporary LTMSes the clauses
    // trie is stored in the 'complete' any field as a Trie.
    // Actually per the Lisp code, the trie IS ltms->clauses -- but in our
    // C++ ltms.h, clauses is vector<Clause*>. So for the complete LTMS we
    // store the trie in ltms->complete as a Trie.
    // However, for normalization temps, we use a separate approach.
    // Following the Go code: the trie is stored in ltms->complete.
    walk_trie_any([&](Clause* cl) { result.push_back(cl); }, ltms->complete);
    return result;
}

// build_trie builds a chain of trie entries from lits[start..] terminating at cl.
Trie build_trie(std::vector<Literal*>& lits, int start, Clause* cl) {
    if (start >= static_cast<int>(lits.size())) {
        return {};
    }
    TrieEntry entry;
    entry.literal = lits[start];
    if (start == static_cast<int>(lits.size()) - 1) {
        entry.sub_trie = cl;
    } else {
        entry.sub_trie = build_trie(lits, start + 1, cl);
    }
    return { entry };
}

// Find a literal pointer in a sorted literal list starting at 'start'.
// Returns the index if found, -1 otherwise.
static int find_literal_in_sorted(Literal* lit, std::vector<Literal*>& lits, int start) {
    for (int i = start; i < static_cast<int>(lits.size()); i++) {
        if (lits[i] == lit) return i;
    }
    return -1;
}

// subsumed checks whether lits[start..] is subsumed by any clause in the trie.
// Returns the subsuming Clause* or nullptr.
Clause* subsumed(std::vector<Literal*>& lits, int start, Trie& trie) {
    for (auto& entry : trie) {
        if (start >= static_cast<int>(lits.size())) return nullptr;
        int slot = find_literal_in_sorted(entry.literal, lits, start);
        if (slot >= 0) {
            if (auto* cl = std::get_if<Clause*>(&entry.sub_trie)) {
                if (*cl) return *cl;
            } else if (auto* children = std::get_if<Trie>(&entry.sub_trie)) {
                Clause* result = subsumed(lits, slot + 1, *children);
                if (result) return result;
            }
        }
    }
    return nullptr;
}

// subsumed_any checks subsumption against the trie stored in a std::any.
Clause* subsumed_any(std::vector<Literal*>& lits, std::any& trie_any) {
    if (!trie_any.has_value()) return nullptr;
    if (auto* t = std::any_cast<Trie>(&trie_any)) {
        return subsumed(lits, 0, *t);
    }
    return nullptr;
}

// add_to_trie_helper inserts lits[litIdx..] / clause into the given trie.
static void add_to_trie_helper(std::vector<Literal*>& lits, int litIdx,
                                Clause* clause, Trie& trie) {
    for (int li = litIdx; li < static_cast<int>(lits.size()); li++) {
        Literal* lit = lits[li];
        int index = lit->node->index;

        // Search for a matching entry.
        TrieEntry* slot = nullptr;
        int insert_pos = -1;
        for (int i = 0; i < static_cast<int>(trie.size()); i++) {
            if (trie[i].literal == lit) {
                slot = &trie[i];
                break;
            }
            if (trie[i].literal->node->index > index) {
                insert_pos = i;
                break;
            }
        }

        if (!slot) {
            // No matching entry; build the rest of the trie and insert.
            Trie new_entries = build_trie(lits, li, clause);
            if (insert_pos >= 0) {
                trie.insert(trie.begin() + insert_pos,
                            new_entries.begin(), new_entries.end());
            } else {
                trie.insert(trie.end(),
                            new_entries.begin(), new_entries.end());
            }
            return;
        }

        // Found a matching entry; descend.
        if (li == static_cast<int>(lits.size()) - 1) {
            // Last literal: store the clause as leaf.
            slot->sub_trie = clause;
            return;
        }

        if (auto* children = std::get_if<Trie>(&slot->sub_trie)) {
            // Recurse into the sub-trie.
            add_to_trie_helper(lits, li + 1, clause, *children);
            return;
        } else {
            // Leaf (or null): replace with a new sub-trie.
            Trie new_sub = build_trie(lits, li + 1, clause);
            slot->sub_trie = std::move(new_sub);
            return;
        }
    }
}

// add_to_trie adds a clause to the LTMS's trie (stored in ltms->complete).
void add_to_trie(Clause* cl, LTMS* ltms) {
    std::vector<Literal*>& lits = cl->literals;
    if (lits.empty()) return;

    if (!ltms->complete.has_value() ||
        !std::any_cast<Trie>(&ltms->complete)) {
        // No trie yet; build one from scratch.
        Trie t = build_trie(lits, 0, cl);
        // Preserve the CompleteMode if present.
        ltms->complete = std::move(t);
        return;
    }

    Trie& trie = *std::any_cast<Trie>(&ltms->complete);
    add_to_trie_helper(lits, 0, cl, trie);
}

// walkTrieEntries helper
static void walk_trie_entries(std::function<void(Clause*)> fn, Trie& entries) {
    walk_trie(fn, entries);
}

// remove_subsumed_1 recursively removes subsumed clauses from the trie.
// Returns true if the entire sub-trie was removed.
bool remove_subsumed_1(std::function<void(Clause*)> fn,
                       std::vector<Literal*>& lits, int start, Trie& trie) {
    if (start >= static_cast<int>(lits.size())) {
        // All literals matched; everything below is subsumed.
        walk_trie(fn, trie);
        return true;
    }
    if (trie.empty()) return false;

    int au = lits[start]->node->index;
    size_t i = 0;
    while (i < trie.size()) {
        auto& entry = trie[i];
        int entry_idx = entry.literal->node->index;
        bool removed = false;

        if (entry_idx >= au) {
            if (entry.literal == lits[start]) {
                // Same literal: recurse with remaining lits.
                if (auto* cl = std::get_if<Clause*>(&entry.sub_trie)) {
                    if (start + 1 >= static_cast<int>(lits.size())) {
                        // Leaf and all lits matched.
                        if (*cl) fn(*cl);
                        removed = true;
                    }
                } else if (auto* children = std::get_if<Trie>(&entry.sub_trie)) {
                    if (remove_subsumed_1(fn, lits, start + 1, *children)) {
                        removed = true;
                    }
                }
            } else if (entry_idx > au) {
                // Past the point where our literal could appear.
                break;
            }
        } else {
            // Entry index < au: check if lits is subsumed deeper.
            if (auto* children = std::get_if<Trie>(&entry.sub_trie)) {
                if (remove_subsumed_1(fn, lits, start, *children)) {
                    removed = true;
                }
            } else if (auto* cl = std::get_if<Clause*>(&entry.sub_trie)) {
                // Leaf at a smaller index: this can't subsume.
                // (no removal)
            }
        }

        if (removed) {
            trie.erase(trie.begin() + i);
            if (trie.empty()) return true;
            // Don't increment i; vector shifted.
        } else {
            i++;
        }
    }
    return false;
}

// remove_subsumed removes from the trie all clauses subsumed by lits.
void remove_subsumed(std::function<void(Clause*)> fn,
                     std::vector<Literal*>& lits, LTMS* ltms) {
    if (!ltms->complete.has_value()) return;
    Trie* trie = std::any_cast<Trie>(&ltms->complete);
    if (!trie) return;
    if (remove_subsumed_1(fn, lits, 0, *trie)) {
        ltms->complete.reset();
    }
}

// ===========================================================================
// Queue operations
// ===========================================================================

// Get the queue from ltms->queue. If not yet initialized, creates one.
static Queue s_empty_queue;

Queue& ltms_queue(LTMS* ltms) {
    if (!ltms->queue.has_value()) {
        ltms->queue = Queue{};
    }
    Queue* q = std::any_cast<Queue>(&ltms->queue);
    if (!q) {
        ltms->queue = Queue{};
        q = std::any_cast<Queue>(&ltms->queue);
    }
    return *q;
}

void set_ltms_queue(LTMS* ltms, Queue queue) {
    if (queue.empty()) {
        ltms->queue = Queue{};
    } else {
        ltms->queue = std::move(queue);
    }
}

// insert_list_2 inserts clause cl into a sorted queue of (length, clauses) slots.
void insert_list_2(Clause* cl, Queue& queue) {
    int cl_count = cl->length;
    for (size_t i = 0; i < queue.size(); i++) {
        if (cl_count == queue[i].length) {
            queue[i].clauses.push_back(cl);
            return;
        }
        if (cl_count < queue[i].length) {
            QueueSlot slot;
            slot.length = cl_count;
            slot.clauses.push_back(cl);
            queue.insert(queue.begin() + i, std::move(slot));
            return;
        }
    }
    QueueSlot slot;
    slot.length = cl_count;
    slot.clauses.push_back(cl);
    queue.push_back(std::move(slot));
}

// insert_list inserts a clause and returns the updated queue (value semantics).
Queue insert_list(Clause* cl, Queue queue) {
    insert_list_2(cl, queue);
    return queue;
}

// insert_queue adds a clause to the LTMS's priority queue.
void insert_queue(Clause* cl, LTMS* ltms) {
    cl->status = ClauseStatus::QUEUED;
    Queue& q = ltms_queue(ltms);
    insert_list_2(cl, q);
}

// ===========================================================================
// Clause insertion (sorted by length for connection graph)
// ===========================================================================

static std::vector<Clause*> insert_clause_sorted(Clause* cl,
                                                  std::vector<Clause*> list) {
    int cl_count = cl->length;
    for (size_t i = 0; i < list.size(); i++) {
        if (cl_count >= list[i]->length) {
            continue;
        }
        list.insert(list.begin() + i, cl);
        return list;
    }
    list.push_back(cl);
    return list;
}

void c_insert_true_clause(Clause* cl, TmsNode* node) {
    node->true_clauses = insert_clause_sorted(cl, node->true_clauses);
}

void c_insert_false_clause(Clause* cl, TmsNode* node) {
    node->false_clauses = insert_clause_sorted(cl, node->false_clauses);
}

// ===========================================================================
// Index clause
// ===========================================================================

void index_clause(Clause* cl, LTMS* ltms) {
    for (auto* term : cl->literals) {
        if (term->sign == NodeLabel::TRUE) {
            c_insert_true_clause(cl, term->node);
        } else {
            c_insert_false_clause(cl, term->node);
        }
    }
    check_clauses(ltms, { cl });
}

// ===========================================================================
// Literal connections
// ===========================================================================

std::vector<Clause*> literal_connections(Literal* literal) {
    if (literal->sign == NodeLabel::TRUE) {
        return literal->node->false_clauses;
    }
    return literal->node->true_clauses;
}

// ===========================================================================
// Disjoin clauses
// ===========================================================================

DisjoinResult disjoin_clauses(std::vector<Literal*>& terms1,
                              std::vector<Literal*>& terms2) {
    DisjoinResult res;
    std::vector<Literal*> result;
    size_t i = 0, j = 0;

    while (true) {
        if (i >= terms1.size()) {
            // Append remaining terms2.
            for (size_t k = j; k < terms2.size(); k++) {
                result.push_back(terms2[k]);
            }
            // Reverse result (nreconc).
            std::reverse(result.begin(), result.end());
            res.literals = std::move(result);
            res.ok = true;
            return res;
        }
        if (j >= terms2.size()) {
            for (size_t k = i; k < terms1.size(); k++) {
                result.push_back(terms1[k]);
            }
            std::reverse(result.begin(), result.end());
            res.literals = std::move(result);
            res.ok = true;
            return res;
        }

        Literal* t1 = terms1[i];
        Literal* t2 = terms2[j];

        if (t1 == t2) {
            result.push_back(t1);
            i++;
            j++;
        } else if (t1->node->index == t2->node->index) {
            // Same node, different sign: tautology -> FAIL.
            res.ok = false;
            return res;
        } else if (t1->node->index < t2->node->index) {
            result.push_back(t1);
            i++;
        } else {
            result.push_back(t2);
            j++;
        }
    }
}

// ===========================================================================
// Consensus algorithm
// ===========================================================================

std::vector<Literal*> simplify_consensus(Clause* cl1, Clause* cl2,
                                          Literal* term1) {
    // Don't resolve two clauses with the same non-null informant.
    if (cl1->informant.has_value() && cl2->informant.has_value()) {
        // Compare informant pointers (same std::any content).
        // In the Lisp, this checks (eq informant1 informant2).
        // We use a pointer-based check: if both are Clause* and equal, skip.
        auto* p1 = std::any_cast<Clause*>(&cl1->informant);
        auto* p2 = std::any_cast<Clause*>(&cl2->informant);
        if (p1 && p2 && *p1 == *p2) return {};
        // Also check string informants.
        auto* s1 = std::any_cast<std::string>(&cl1->informant);
        auto* s2 = std::any_cast<std::string>(&cl2->informant);
        if (s1 && s2 && *s1 == *s2) return {};
    }

    std::vector<Literal*> result;
    size_t i = 0, j = 0;
    auto& terms1 = cl1->literals;
    auto& terms2 = cl2->literals;

    while (true) {
        if (i >= terms1.size()) {
            for (size_t k = j; k < terms2.size(); k++) {
                result.push_back(terms2[k]);
            }
            std::reverse(result.begin(), result.end());
            return result;
        }
        if (j >= terms2.size()) {
            for (size_t k = i; k < terms1.size(); k++) {
                result.push_back(terms1[k]);
            }
            std::reverse(result.begin(), result.end());
            return result;
        }

        Literal* t1 = terms1[i];
        Literal* t2 = terms2[j];

        if (t1 == t2) {
            result.push_back(t1);
            i++;
            j++;
        } else if (t1->node->index == t2->node->index) {
            // Same node, different sign: complementary pair.
            if (t1 != term1) {
                // A second complementary pair means no consensus.
                return {};
            }
            i++;
            j++;
        } else if (t1->node->index < t2->node->index) {
            result.push_back(t1);
            i++;
        } else {
            result.push_back(t2);
            j++;
        }
    }
}

Clause* simplify_subsume_consensus(LTMS* ltms, Clause* cl1, Clause* cl2,
                                    Literal* p) {
    // Check informant mismatch (either can be null, or they differ).
    if (cl1->informant.has_value() && cl2->informant.has_value()) {
        auto* p1 = std::any_cast<Clause*>(&cl1->informant);
        auto* p2 = std::any_cast<Clause*>(&cl2->informant);
        if (p1 && p2 && *p1 == *p2) return nullptr;
        auto* s1 = std::any_cast<std::string>(&cl1->informant);
        auto* s2 = std::any_cast<std::string>(&cl2->informant);
        if (s1 && s2 && *s1 == *s2) return nullptr;
    }

    std::vector<Literal*> literals = simplify_consensus(cl1, cl2, p);
    if (literals.empty()) return nullptr;
    if (subsumed_any(literals, ltms->complete)) return nullptr;

    // Build informant: (RESOLVE cl1 cl2 p)
    std::vector<std::any> inf = {
        std::string("RESOLVE"),
        std::any(cl1),
        std::any(cl2),
        std::any(p)
    };
    std::vector<Literal*> lits_copy = literals;
    return process_clause(ltms, std::move(lits_copy), std::any(inf), true);
}

// ===========================================================================
// Delay-sat check
// ===========================================================================

bool delay_sat(Clause* clause, LTMS* ltms) {
    if (ltms->delay_sat && satisfied_clause(clause)) {
        clause->status = ClauseStatus::DIRTY;
        return true;
    }
    return false;
}

// ===========================================================================
// Clause removal / processing
// ===========================================================================

void remove_clause(Clause* old_clause, Clause* new_clause) {
    if (old_clause->status != ClauseStatus::NOT_INDEXED) {
        for (auto* term : old_clause->literals) {
            if (term->sign == NodeLabel::TRUE) {
                auto& tc = term->node->true_clauses;
                tc.erase(std::remove(tc.begin(), tc.end(), old_clause), tc.end());
            } else {
                auto& fc = term->node->false_clauses;
                fc.erase(std::remove(fc.begin(), fc.end(), old_clause), fc.end());
            }
        }
    }
    old_clause->status = ClauseStatus::SUBSUMED;

    TmsNode* node = clause_consequent(old_clause);
    if (!node) return;

    // Check if node appears in new_clause's literals.
    bool found = false;
    for (auto* lit : new_clause->literals) {
        if (lit->node == node) {
            found = true;
            break;
        }
    }
    if (found) {
        node->support = new_clause;
    } else {
        // A contradiction is being introduced.
        auto unknowns = propagate_unknownness(node);
        find_alternative_support(node->ltms, unknowns);
    }
}

Clause* install_clause(LTMS* ltms, std::vector<Literal*>& literals,
                       std::any informant) {
    if (subsumed_any(literals, ltms->complete)) return nullptr;
    return process_clause(ltms, literals, std::move(informant), false);
}

Clause* process_clause(LTMS* ltms, std::vector<Literal*> literals,
                       std::any informant, bool internal_flag) {
    Clause* cl = bcp_add_clause(ltms, literals, std::move(informant), false);

    remove_subsumed([cl](Clause* old_clause) {
        remove_clause(old_clause, cl);
    }, literals, ltms);

    add_to_trie(cl, ltms);

    if (internal_flag) {
        // Internal clause: don't index or queue yet.
        return cl;
    }

    if (delay_sat(cl, ltms)) {
        index_clause(cl, ltms);
    } else {
        index_clause(cl, ltms);
        insert_queue(cl, ltms);
    }
    return cl;
}

// ===========================================================================
// Propagation
// ===========================================================================

void c_propagate_more_unknownness(NodeLabel old_value, TmsNode* node, LTMS* ltms) {
    std::vector<Clause*>* clauses = nullptr;
    if (old_value == NodeLabel::TRUE) {
        clauses = &node->true_clauses;
    } else if (old_value == NodeLabel::FALSE) {
        clauses = &node->false_clauses;
    } else {
        return;
    }

    for (auto* clause : *clauses) {
        clause->sats--;
        if (clause->sats == 0 && clause->status == ClauseStatus::DIRTY) {
            insert_queue(clause, ltms);
        }
    }
}

// ===========================================================================
// Full add clause
// ===========================================================================

void full_add_clause(LTMS* ltms, std::vector<Literal*>& literals,
                     std::any informant) {
    Clause* cl = install_clause(ltms, literals, std::move(informant));
    if (cl) {
        CompleteMode mode = get_complete_mode(ltms);
        if (mode != CompleteMode::DELAY) {
            check_for_contradictions(ltms);
            ipia(ltms);
        }
    }
}

// ===========================================================================
// Complete LTMS control
// ===========================================================================

void complete_ltms(LTMS* ltms) {
    std::any old = ltms->complete;
    try {
        ltms->complete = CompleteMode::TRUE;
        ipia(ltms);
    } catch (...) {
        ltms->complete = old;
        check_for_contradictions(ltms);
        throw;
    }
    ltms->complete = old;
    check_for_contradictions(ltms);
}

// ===========================================================================
// IPIA: Iterative Prime Implicate Algorithm (Tison's method)
// ===========================================================================

static bool literal_in_clause(Literal* lit, Clause* clause) {
    for (auto* l : clause->literals) {
        if (l == lit) return true;
    }
    return false;
}

void ipia(LTMS* ltms) {
    while (true) {
        Queue& queue = ltms_queue(ltms);
        if (queue.empty()) break;

        auto& slot = queue[0];
        if (slot.clauses.empty()) {
            queue.erase(queue.begin());
            continue;
        }

        // Pop the first clause from the first slot.
        Clause* c = slot.clauses[0];
        if (slot.clauses.size() > 1) {
            slot.clauses.erase(slot.clauses.begin());
        } else {
            queue.erase(queue.begin());
        }

        if (c->status != ClauseStatus::QUEUED) continue;
        if (delay_sat(c, ltms)) continue;

        // Build sigma and pxs.
        Queue sigma = insert_list(c, Queue{});

        std::vector<std::vector<Clause*>> pxs;
        for (auto* lit : c->literals) {
            std::vector<Clause*> px;
            auto connections = literal_connections(lit);
            for (auto* p : connections) {
                if (p->status == ClauseStatus::QUEUED) continue;
                if (p->status == ClauseStatus::SUBSUMED) continue;
                if (p->status == ClauseStatus::DIRTY && delay_sat(p, ltms)) continue;
                auto consensus = simplify_consensus(c, p, lit);
                if (consensus.empty()) continue;
                if (delay_sat(p, ltms)) continue;
                px.push_back(p);
            }
            pxs.push_back(std::move(px));
        }

        // Resolve sigma with pxs.
        if (!pxs.empty()) {
            size_t px_idx = 0;
            for (auto* lit : c->literals) {
                if (px_idx >= pxs.size()) break;
                auto& px = pxs[px_idx];
                px_idx++;
                if (px.empty()) continue;

                // Iterate over all clauses in sigma.
                for (auto& l : sigma) {
                    // Copy the clauses vector since sigma may be modified.
                    auto s_clauses = l.clauses;
                    for (auto* s : s_clauses) {
                        if (s->status == ClauseStatus::SUBSUMED) continue;
                        if (!literal_in_clause(lit, s)) continue;
                        if (delay_sat(s, ltms)) continue;
                        sigma = ipia_inner(ltms, std::move(sigma), px, s, lit);
                    }
                }
            }
        }

        if (c->status == ClauseStatus::QUEUED) {
            c->status = ClauseStatus::NONE;
        }

        // Index all NOT-INDEXED clauses in sigma.
        for (auto& l : sigma) {
            for (auto* s : l.clauses) {
                if (s->status == ClauseStatus::NOT_INDEXED) {
                    index_clause(s, ltms);
                    s->status = ClauseStatus::NONE;
                }
            }
        }

        check_for_contradictions(ltms);
    }
}

Queue ipia_inner(LTMS* ltms, Queue sigma, std::vector<Clause*>& px,
                 Clause* s, Literal* lit) {
    std::vector<Clause*> s_children;

    for (auto* p : px) {
        if (delay_sat(p, ltms)) continue;
        if (p->status == ClauseStatus::SUBSUMED) continue;
        Clause* consensus = simplify_subsume_consensus(ltms, s, p, lit);
        if (!consensus) continue;
        consensus->status = ClauseStatus::NOT_INDEXED;
        if (s->status == ClauseStatus::SUBSUMED) {
            sigma = insert_list(consensus, std::move(sigma));
            s_children.clear();
            break;
        }
        s_children.push_back(consensus);
    }

    for (auto* child : s_children) {
        if (child->status != ClauseStatus::SUBSUMED) {
            sigma = insert_list(child, std::move(sigma));
        }
    }

    return sigma;
}

// ===========================================================================
// Normalization (complete versions with trie-based subsumption)
// ===========================================================================

ClauseList c_normalize_disjunction(LTMS* ltms, const std::vector<std::any>& exp,
                                    bool negate) {
    if (exp.size() < 2) {
        return { LiteralList{} };
    }

    ClauseList result = normalize_1(ltms, exp[1], negate);

    // Create a temporary LTMS for trie storage.
    LTMS* temp_ltms = create_ltms("Normalize disjunction");
    // Initialize trie as empty (will be set by add_to_trie).
    for (auto& c : result) {
        Clause* cl = new Clause();
        cl->literals = c;
        cl->length = static_cast<int>(c.size());
        add_to_trie(cl, temp_ltms);
    }

    for (size_t k = 2; k < exp.size(); k++) {
        LTMS* nltms = create_ltms("Normalize disjunction");
        ClauseList sub_norm = normalize_1(ltms, exp[k], negate);

        for (auto& disj1 : sub_norm) {
            walk_trie_any([&](Clause* disj2) {
                auto dr = disjoin_clauses(disj1, disj2->literals);
                if (!dr.ok) return;
                if (subsumed_any(dr.literals, nltms->complete)) return;
                auto null_fn = [](Clause*) {};
                remove_subsumed(null_fn, dr.literals, nltms);
                Clause* cl = new Clause();
                cl->literals = dr.literals;
                cl->length = static_cast<int>(dr.literals.size());
                add_to_trie(cl, nltms);
            }, temp_ltms->complete);
        }
        delete temp_ltms;
        temp_ltms = nltms;
    }

    auto collected = collect(temp_ltms);
    ClauseList clauses;
    for (auto* cl : collected) {
        clauses.push_back(cl->literals);
    }
    delete temp_ltms;
    return clauses;
}

ClauseList c_normalize_conjunction(LTMS* ltms, const std::vector<std::any>& exp,
                                    bool negate) {
    LTMS* temp_ltms = create_ltms("Normalize conjunction");

    for (size_t k = 1; k < exp.size(); k++) {
        ClauseList sub_norm = normalize_1(ltms, exp[k], negate);
        for (auto& disjunct : sub_norm) {
            if (subsumed_any(disjunct, temp_ltms->complete)) continue;
            auto null_fn = [](Clause*) {};
            remove_subsumed(null_fn, disjunct, temp_ltms);
            Clause* cl = new Clause();
            cl->literals = disjunct;
            cl->length = static_cast<int>(disjunct.size());
            add_to_trie(cl, temp_ltms);
        }
    }

    auto collected = collect(temp_ltms);
    ClauseList clauses;
    for (auto* cl : collected) {
        clauses.push_back(cl->literals);
    }
    delete temp_ltms;
    return clauses;
}

// ===========================================================================
// Add clause internal for complete LTMS
// ===========================================================================

void add_clause_internal_cltms(LTMS* ltms, std::vector<Literal*>& literals,
                               std::any informant, bool internal_flag) {
    if (literals.empty()) {
        ltms_error("Total contradiction: Null clause", informant);
        return;
    }

    CompleteMode mode = get_complete_mode(ltms);
    if (mode != CompleteMode::NONE && ltms->full_add_clause_fn) {
        ltms->full_add_clause_fn(ltms, literals, std::move(informant));
    } else {
        Clause* cl = bcp_add_clause(ltms, literals, std::move(informant), true);
        ltms->clauses.push_back(cl);
    }

    if (!internal_flag) {
        check_for_contradictions(ltms);
    }
}

// ===========================================================================
// Map-over helper (maps clause literals through a node transform function)
// ===========================================================================

static std::vector<Literal*> map_over_literals(
    std::vector<Literal*>& lits,
    std::function<TmsNode*(TmsNode*)> mapf) {
    std::vector<Literal*> result;
    result.reserve(lits.size());
    for (auto* lit : lits) {
        TmsNode* mapped_node = mapf(lit->node);
        if (lit->sign == NodeLabel::TRUE) {
            result.push_back(mapped_node->true_literal);
        } else {
            result.push_back(mapped_node->false_literal);
        }
    }
    return result;
}

static void sort_nodes_by_mark(std::vector<TmsNode*>& nodes) {
    std::sort(nodes.begin(), nodes.end(), [](TmsNode* a, TmsNode* b) {
        int ma = 0, mb = 0;
        if (auto* p = std::any_cast<int>(&a->mark)) ma = *p;
        if (auto* p = std::any_cast<int>(&b->mark)) mb = *p;
        return ma < mb;
    });
}

// ===========================================================================
// Add formula (complete version)
// ===========================================================================

void c_add_formula(LTMS* ltms, std::any formula, std::any informant) {
    // Create a temporary complete LTMS for normalization.
    LTMS* tltms = create_ltms("Temporary for add-formula",
                              nullptr,  // node_string_fn
                              false,    // debugging
                              true,     // checking_contradictions
                              nullptr,  // contradiction_handler
                              nullptr,  // enqueue_procedure
                              true,     // cache_datums
                              CompleteMode::DELAY,  // complete
                              false);   // delay_sat
    register_complete_ltms_functions(tltms);

    ClauseList clauses = normalize(ltms, formula);

    if (!informant.has_value()) {
        std::vector<std::any> inf = { std::string(":IMPLIED-BY"), formula };
        informant = std::any(inf);
    }

    // Clear marks on all nodes.
    for (auto& [name, node] : ltms->nodes) {
        node->mark = 0;
    }

    // Collect literals used in clauses and count occurrences.
    std::vector<TmsNode*> literals;
    for (auto& clause : clauses) {
        for (auto* literal : clause) {
            int mark_val = 0;
            if (auto* p = std::any_cast<int>(&literal->node->mark)) {
                mark_val = *p;
            }
            if (mark_val == 0) {
                literals.push_back(literal->node);
            }
            literal->node->mark = mark_val + 1;
        }
    }

    // Sort literals by occurrence count (ascending) and create temp nodes.
    sort_nodes_by_mark(literals);
    for (auto* literal : literals) {
        // Create a node in the temp LTMS mapped from the original node.
        std::string datum_str = default_node_string(literal);
        TmsNode* temp_node = tms_create_node(tltms, datum_str, false);
        // Store the original node as the temp node's datum for back-mapping.
        temp_node->datum = literal;
        literal->mark = std::any(temp_node);
    }

    // Map clauses to the temp LTMS and add them.
    for (auto& clause : clauses) {
        auto mapped = map_over_literals(clause, [](TmsNode* node) -> TmsNode* {
            return std::any_cast<TmsNode*>(node->mark);
        });
        auto sorted = sort_clause(std::move(mapped));
        add_clause_internal_cltms(tltms, sorted, std::any{}, true);
    }

    // Run IPIA on the temp LTMS.
    complete_ltms(tltms);

    // Walk the resulting trie and install clauses in the main LTMS.
    walk_trie_any([&](Clause* clause) {
        auto mapped = map_over_literals(clause->literals, [](TmsNode* node) -> TmsNode* {
            // The temp node's datum holds the original TmsNode*.
            return std::any_cast<TmsNode*>(node->datum);
        });
        auto sorted = sort_clause(std::move(mapped));
        add_clause_internal_cltms(ltms, sorted, informant, true);
    }, tltms->complete);

    check_for_contradictions(ltms);
    if (get_complete_mode(ltms) == CompleteMode::TRUE) {
        ipia(ltms);
    }

    delete tltms;
}

// ===========================================================================
// TMS environment query
// ===========================================================================

std::vector<std::vector<Literal*>> tms_env(TmsNode* node, NodeLabel sign) {
    std::vector<std::vector<Literal*>> label;

    if (node->is_assumption) {
        Literal* lit = (sign == NodeLabel::TRUE)
                           ? node->true_literal
                           : node->false_literal;
        label.push_back({ lit });
    }

    std::vector<Clause*>& clauses = (sign == NodeLabel::TRUE)
                                        ? node->true_clauses
                                        : node->false_clauses;

    for (auto* p : clauses) {
        // Check if all other literals' nodes are assumptions.
        bool has_non_assumption = false;
        for (auto* lit : p->literals) {
            if (lit->node != node && !lit->node->is_assumption) {
                has_non_assumption = true;
                break;
            }
        }
        if (has_non_assumption) continue;

        std::vector<Literal*> env;
        for (auto* lit : p->literals) {
            if (lit->node != node) {
                // Negate: TRUE -> false_literal, FALSE -> true_literal.
                if (lit->sign == NodeLabel::TRUE) {
                    env.push_back(lit->node->false_literal);
                } else {
                    env.push_back(lit->node->true_literal);
                }
            }
        }
        label.push_back(std::move(env));
    }

    return label;
}

// ===========================================================================
// PI (Prime Implicates)
// ===========================================================================

LTMS* pi(std::any formula) {
    LTMS* ltms = create_ltms("Prime Implicates");
    LTMS* tltms = create_ltms("Prime Implicates",
                              nullptr, false, true, nullptr, nullptr, true,
                              CompleteMode::DELAY, false);
    register_complete_ltms_functions(tltms);

    ClauseList clauses = normalize(ltms, formula);

    // Collect all nodes and count occurrences.
    std::vector<TmsNode*> literals;
    for (auto& [name, node] : ltms->nodes) {
        node->mark = 0;
        literals.push_back(node);
    }

    for (auto& clause : clauses) {
        for (auto* literal : clause) {
            int mark_val = 0;
            if (auto* p = std::any_cast<int>(&literal->node->mark)) {
                mark_val = *p;
            }
            literal->node->mark = mark_val + 1;
        }
    }

    sort_nodes_by_mark(literals);
    for (auto* literal : literals) {
        // Get the datum from the original node.
        std::string datum_str = default_node_string(literal);
        TmsNode* temp_node = tms_create_node(tltms, datum_str, false);
        temp_node->datum = literal;
        literal->mark = std::any(temp_node);
    }

    for (auto& clause : clauses) {
        auto mapped = map_over_literals(clause, [](TmsNode* node) -> TmsNode* {
            return std::any_cast<TmsNode*>(node->mark);
        });
        auto sorted = sort_clause(std::move(mapped));
        add_clause_internal_cltms(tltms, sorted, std::any{}, true);
    }

    complete_ltms(tltms);
    auto collected = collect(tltms);
    std::cout << "\n There now are " << collected.size() << " clauses";

    delete ltms;
    return tltms;
}

// ===========================================================================
// Registration
// ===========================================================================

void register_complete_ltms_functions(LTMS* ltms) {
    ltms->full_add_clause_fn = [](LTMS* l, std::vector<Literal*>& lits, std::any inf) {
        full_add_clause(l, lits, std::move(inf));
    };
    ltms->propagate_more_unknownness_fn = c_propagate_more_unknownness;
    ltms->ipia_fn = ipia;
    ltms->walk_trie_fn = [](std::function<void(Clause*)> fn, std::any trie) {
        walk_trie_any(fn, trie);
    };
    // Initialize the queue.
    ltms->queue = Queue{};
}
