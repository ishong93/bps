/// Logic-based Truth Maintenance System, C++ implementation.
/// Translated from Common Lisp LTMS version 43, 7/5/93.
/// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ltms.h"

// ---------------------------------------------------------------------------
// Helpers (internal to this translation unit)
// ---------------------------------------------------------------------------

/// Convert an std::any to a printable string (best-effort).
static std::string any_to_string(const std::any& a) {
    if (!a.has_value()) return "<nil>";
    if (auto p = std::any_cast<std::string>(&a)) return *p;
    if (auto p = std::any_cast<const char*>(&a)) return std::string(*p);
    if (auto p = std::any_cast<int>(&a)) return std::to_string(*p);
    if (auto p = std::any_cast<TmsNode*>(&a)) return print_tms_node(*p);
    if (auto p = std::any_cast<Clause*>(&a)) return print_clause(*p);
    return "<any>";
}

/// Check whether an std::any holds a "complete == true" boolean.
static bool complete_is_true(const std::any& c) {
    if (auto p = std::any_cast<bool>(&c)) return *p;
    return false;
}

static bool complete_is_set(const std::any& c) {
    return c.has_value();
}

// ---------------------------------------------------------------------------
// Print / string helpers
// ---------------------------------------------------------------------------

std::string print_ltms(LTMS* ltms) {
    return "#<LTMS: " + ltms->title + ">";
}

std::string print_tms_node(TmsNode* node) {
    return "#<NODE: " + node_string(node) + ">";
}

std::string print_clause(Clause* clause) {
    return "#<Clause " + std::to_string(clause->index) + ">";
}

std::string node_string(TmsNode* node) {
    if (node->ltms && node->ltms->node_string_fn) {
        return node->ltms->node_string_fn(node);
    }
    return default_node_string(node);
}

void debugging_ltms(LTMS* ltms, const std::string& msg, TmsNode* node) {
    if (ltms->debugging) {
        if (node) {
            std::cerr << msg << " " << node_string(node) << std::endl;
        } else {
            std::cerr << msg << std::endl;
        }
    }
}

[[noreturn]] void ltms_error(const std::string& msg, const std::any& thing) {
    std::string full = msg;
    if (thing.has_value()) {
        full += " " + any_to_string(thing);
    }
    throw std::runtime_error(full);
}

std::string default_node_string(TmsNode* n) {
    return any_to_string(n->datum);
}

// ---------------------------------------------------------------------------
// Clause predicates
// ---------------------------------------------------------------------------

bool satisfied_clause(Clause* clause) { return clause->sats > 0; }
bool violated_clause(Clause* clause)  { return clause->pvs == 0; }

// ---------------------------------------------------------------------------
// Walk
// ---------------------------------------------------------------------------

void walk_clauses(LTMS* ltms, std::function<void(Clause*)> f) {
    if (complete_is_set(ltms->complete) && ltms->walk_trie_fn) {
        ltms->walk_trie_fn(f, ltms->complete);
    } else {
        for (auto* c : ltms->clauses) {
            f(c);
        }
    }
}

// ---------------------------------------------------------------------------
// Creation / modification
// ---------------------------------------------------------------------------

LTMS* create_ltms(const std::string& title,
                  std::function<std::string(TmsNode*)> node_string_fn,
                  bool debugging,
                  bool checking_contradictions,
                  std::function<bool(std::vector<Clause*>&, LTMS*)> contradiction_handler_fn,
                  std::function<void(std::any)> enqueue_procedure,
                  bool cache_datums,
                  std::any complete,
                  bool delay_sat) {
    auto* ltms = new LTMS();
    ltms->title = title;
    ltms->node_string_fn = node_string_fn ? node_string_fn : default_node_string;
    ltms->debugging = debugging;
    ltms->checking_contradictions = checking_contradictions;
    ltms->enqueue_procedure = enqueue_procedure;
    ltms->delay_sat = delay_sat;
    ltms->complete = complete;
    ltms->nodes_enabled = cache_datums;

    if (contradiction_handler_fn) {
        ltms->contradiction_handlers.push_back(contradiction_handler_fn);
    } else {
        ltms->contradiction_handlers.push_back(ask_user_handler);
    }

    return ltms;
}

void change_ltms(LTMS* ltms,
                 std::function<std::string(TmsNode*)> node_string_fn,
                 const bool* debugging,
                 const bool* checking_contradictions,
                 std::function<bool(std::vector<Clause*>&, LTMS*)> contradiction_handler_fn,
                 std::function<void(std::any)> enqueue_procedure,
                 const std::any* complete,
                 const bool* delay_sat) {
    if (node_string_fn) ltms->node_string_fn = node_string_fn;
    if (debugging) ltms->debugging = *debugging;
    if (checking_contradictions) ltms->checking_contradictions = *checking_contradictions;
    if (contradiction_handler_fn) {
        ltms->contradiction_handlers.clear();
        ltms->contradiction_handlers.push_back(contradiction_handler_fn);
    }
    if (enqueue_procedure) ltms->enqueue_procedure = enqueue_procedure;
    if (complete) ltms->complete = *complete;
    if (delay_sat) ltms->delay_sat = *delay_sat;
}

// ---------------------------------------------------------------------------
// Node queries
// ---------------------------------------------------------------------------

bool unknown_node(TmsNode* node) { return node->label == NodeLabel::UNKNOWN; }
bool known_node(TmsNode* node)   { return node->label != NodeLabel::UNKNOWN; }
bool true_node(TmsNode* node)    { return node->label == NodeLabel::TRUE; }
bool false_node(TmsNode* node)   { return node->label == NodeLabel::FALSE; }

// ---------------------------------------------------------------------------
// Node creation
// ---------------------------------------------------------------------------

TmsNode* tms_create_node(LTMS* ltms, const std::string& datum, bool assumptionp) {
    if (ltms->nodes_enabled) {
        auto it = ltms->nodes.find(datum);
        if (it != ltms->nodes.end()) {
            ltms_error("Two nodes with same datum:", std::any(datum));
        }
    }

    ltms->node_counter++;
    auto* node = new TmsNode();
    node->index = ltms->node_counter;
    node->datum = datum;
    node->is_assumption = assumptionp;
    node->ltms = ltms;

    node->true_literal = new Literal{node, NodeLabel::TRUE};
    node->false_literal = new Literal{node, NodeLabel::FALSE};

    if (ltms->nodes_enabled) {
        ltms->nodes[datum] = node;
    }

    if (complete_is_set(ltms->complete) && ltms->node_counter > ltms->cons_size) {
        ltms->conses.clear();
        ltms->cons_size += 50;
        for (int i = 0; i < ltms->cons_size; i++) {
            ltms->conses.push_back(new Literal());
        }
    }

    return node;
}

// ---------------------------------------------------------------------------
// Assumption management
// ---------------------------------------------------------------------------

void enable_assumption(TmsNode* node, NodeLabel label) {
    if (!node->is_assumption) {
        ltms_error("Can't enable the non-assumption", std::any(node_string(node)));
    }
    if (node->label == label) {
        node->support = std::any(ENABLED_ASSUMPTION);
    } else if (node->label == NodeLabel::UNKNOWN) {
        top_set_truth(node, label, std::any(ENABLED_ASSUMPTION));
    } else {
        ltms_error("Can't set an already set node", std::any(node_string(node)));
    }
}

void convert_to_assumption(TmsNode* node) {
    if (!node->is_assumption) {
        debugging_ltms(node->ltms, "Converting into an assumption", node);
        node->is_assumption = true;
    }
}

void retract_assumption(TmsNode* node) {
    if (known_node(node) && support_is_enabled_assumption(node->support)) {
        auto unknown_nodes = propagate_unknownness(node);
        find_alternative_support(node->ltms, unknown_nodes);
    }
}

// ---------------------------------------------------------------------------
// Adding formulas
// ---------------------------------------------------------------------------

void add_formula(LTMS* ltms, std::any formula, std::any informant) {
    // Build informant: (:IMPLIED-BY formula informant)
    std::vector<std::any> inf;
    inf.push_back(std::any(std::string(":IMPLIED-BY")));
    inf.push_back(formula);
    inf.push_back(informant);
    std::any inf_any = inf;

    ClauseList clauses = normalize(ltms, formula);
    for (auto& clause_lits : clauses) {
        auto simplified = simplify_clause(clause_lits);
        // If simplify_clause returns empty with a special flag, it means tautology.
        // We use a convention: if the original had literals but simplified is empty,
        // that signals a tautology (:TRUE). But we need a better way --
        // We'll use a sentinel: simplify_clause returns a vector with a single nullptr
        // literal to indicate tautology. Actually, let's match the Go pattern:
        // return empty vector to signal tautology when complementary literals found.
        // A truly empty clause (contradiction) would come from empty input.
        if (clause_lits.empty()) continue;  // skip empty
        if (simplified.empty()) continue;    // tautology
        add_clause_internal(simplified, inf_any, true);
    }
    check_for_contradictions(ltms);
}

// Sentinel value: simplify_clause returns empty vector for tautology.
// An actual empty clause (total contradiction) is handled elsewhere.

LiteralList simplify_clause(LiteralList literals) {
    if (literals.empty()) return literals;
    literals = sort_clause(literals);

    size_t i = 0;
    for (size_t j = 1; j < literals.size(); j++) {
        if (literals[i]->node != literals[j]->node) {
            i++;
            literals[i] = literals[j];
        } else if (literals[i]->sign != literals[j]->sign) {
            // Complementary literals: tautology
            return {};
        }
        // else duplicate, skip
    }
    literals.resize(i + 1);
    return literals;
}

LiteralList sort_clause(LiteralList literals) {
    // Copy to avoid shared structure bugs.
    LiteralList result(literals);
    std::sort(result.begin(), result.end(), [](Literal* a, Literal* b) {
        return a->node->index < b->node->index;
    });
    return result;
}

// ---------------------------------------------------------------------------
// Normalize
// ---------------------------------------------------------------------------

ClauseList normalize(LTMS* ltms, const std::any& exp) {
    return normalize_1(ltms, exp, false);
}

ClauseList normalize_1(LTMS* ltms, const std::any& exp, bool negate) {
    // Check if exp is a vector<any> (compound formula)
    if (auto vec_ptr = std::any_cast<std::vector<std::any>>(&exp)) {
        if (!vec_ptr->empty()) {
            if (auto op_ptr = std::any_cast<std::string>(&(*vec_ptr)[0])) {
                const std::string& op = *op_ptr;
                if (op == ":IMPLIES") {
                    if (negate) {
                        auto r1 = normalize_1(ltms, (*vec_ptr)[1], false);
                        auto r2 = normalize_1(ltms, (*vec_ptr)[2], true);
                        r1.insert(r1.end(), r2.begin(), r2.end());
                        return r1;
                    }
                    return disjoin(
                        normalize_1(ltms, (*vec_ptr)[1], true),
                        normalize_1(ltms, (*vec_ptr)[2], false));
                }
                if (op == ":IFF") {
                    return normalize_iff(ltms, *vec_ptr, negate);
                }
                if (op == ":OR") {
                    if (negate) return normalize_conjunction(ltms, *vec_ptr, true);
                    return normalize_disjunction(ltms, *vec_ptr, false);
                }
                if (op == ":AND") {
                    if (negate) return normalize_disjunction(ltms, *vec_ptr, true);
                    return normalize_conjunction(ltms, *vec_ptr, false);
                }
                if (op == ":NOT") {
                    return normalize_1(ltms, (*vec_ptr)[1], !negate);
                }
                if (op == ":TAXONOMY") {
                    return normalize_tax(ltms, *vec_ptr, negate);
                }
            }
        }
    }

    // Leaf node
    TmsNode* node = find_node(ltms, exp);
    if (negate) {
        return {{node->false_literal}};
    }
    return {{node->true_literal}};
}

ClauseList normalize_tax(LTMS* ltms, const std::vector<std::any>& exp, bool negate) {
    // items = exp[1:]
    // Build: (:AND (:OR items...) (:NOT (:AND a b)) for each pair)
    std::vector<std::any> items(exp.begin() + 1, exp.end());

    std::vector<std::any> and_parts;
    and_parts.push_back(std::any(std::string(":AND")));

    // (:OR items...)
    std::vector<std::any> or_part;
    or_part.push_back(std::any(std::string(":OR")));
    for (auto& item : items) or_part.push_back(item);
    and_parts.push_back(std::any(or_part));

    // Pairwise exclusion: (:NOT (:AND a b))
    for (size_t i = 0; i < items.size(); i++) {
        for (size_t j = i + 1; j < items.size(); j++) {
            std::vector<std::any> and_pair;
            and_pair.push_back(std::any(std::string(":AND")));
            and_pair.push_back(items[i]);
            and_pair.push_back(items[j]);

            std::vector<std::any> not_expr;
            not_expr.push_back(std::any(std::string(":NOT")));
            not_expr.push_back(std::any(and_pair));

            and_parts.push_back(std::any(not_expr));
        }
    }

    return normalize_1(ltms, std::any(and_parts), negate);
}

ClauseList normalize_conjunction(LTMS* ltms, const std::vector<std::any>& exp, bool negate) {
    ClauseList result;
    for (size_t i = 1; i < exp.size(); i++) {
        auto sub = normalize_1(ltms, exp[i], negate);
        result.insert(result.end(), sub.begin(), sub.end());
    }
    return result;
}

ClauseList normalize_iff(LTMS* ltms, const std::vector<std::any>& exp, bool negate) {
    // (:IMPLIES a b) and (:IMPLIES b a)
    std::vector<std::any> imp1 = {
        std::any(std::string(":IMPLIES")), exp[1], exp[2]
    };
    std::vector<std::any> imp2 = {
        std::any(std::string(":IMPLIES")), exp[2], exp[1]
    };
    auto r1 = normalize_1(ltms, std::any(imp1), negate);
    auto r2 = normalize_1(ltms, std::any(imp2), negate);
    r1.insert(r1.end(), r2.begin(), r2.end());
    return r1;
}

ClauseList normalize_disjunction(LTMS* ltms, const std::vector<std::any>& exp, bool negate) {
    if (exp.size() < 2) {
        // Return a single empty clause (nil).
        return {{}};
    }
    ClauseList result = normalize_1(ltms, exp[1], negate);
    for (size_t i = 2; i < exp.size(); i++) {
        result = disjoin(normalize_1(ltms, exp[i], negate), result);
    }
    return result;
}

ClauseList disjoin(const ClauseList& conj1, const ClauseList& conj2) {
    if (conj1.empty() || conj2.empty()) return {};
    ClauseList result;
    for (auto& disj1 : conj1) {
        for (auto& disj2 : conj2) {
            LiteralList combined;
            combined.insert(combined.end(), disj1.begin(), disj1.end());
            combined.insert(combined.end(), disj2.begin(), disj2.end());
            result.push_back(combined);
        }
    }
    return result;
}

// ---------------------------------------------------------------------------
// find_node
// ---------------------------------------------------------------------------

TmsNode* find_node(LTMS* ltms, const std::any& name) {
    // If it's already a TmsNode*, return it.
    if (auto p = std::any_cast<TmsNode*>(&name)) {
        return *p;
    }
    // Try string lookup.
    std::string key;
    if (auto p = std::any_cast<std::string>(&name)) {
        key = *p;
    } else if (auto p = std::any_cast<const char*>(&name)) {
        key = *p;
    } else {
        key = any_to_string(name);
    }
    if (ltms->nodes_enabled) {
        auto it = ltms->nodes.find(key);
        if (it != ltms->nodes.end()) {
            return it->second;
        }
    }
    return tms_create_node(ltms, key);
}

// ---------------------------------------------------------------------------
// Adding clauses
// ---------------------------------------------------------------------------

void add_clause(std::vector<TmsNode*>& true_nodes, std::vector<TmsNode*>& false_nodes,
                std::any informant) {
    std::vector<Literal*> literals;
    for (auto* n : true_nodes) {
        literals.push_back(n->true_literal);
    }
    for (auto* n : false_nodes) {
        literals.push_back(n->false_literal);
    }
    add_clause_internal(literals, informant, false);
}

void add_clause_internal(std::vector<Literal*>& literals, std::any informant, bool internal_flag) {
    if (literals.empty() || literals[0]->node == nullptr) {
        ltms_error("Total contradiction: Null clause", informant);
    }
    LTMS* ltms = literals[0]->node->ltms;
    if (complete_is_set(ltms->complete) && ltms->full_add_clause_fn) {
        ltms->full_add_clause_fn(ltms, literals, informant);
    } else {
        auto* cl = bcp_add_clause(ltms, literals, informant, true);
        ltms->clauses.push_back(cl);
    }
    if (!internal_flag) {
        check_for_contradictions(ltms);
    }
}

Clause* bcp_add_clause(LTMS* ltms, std::vector<Literal*>& literals, std::any informant,
                       bool index) {
    ltms->clause_counter++;
    auto* cl = new Clause();
    cl->index = ltms->clause_counter;
    cl->literals = literals;
    cl->informant = informant;
    cl->length = static_cast<int>(literals.size());

    for (auto* term : literals) {
        NodeLabel label = term->node->label;
        if (label == NodeLabel::UNKNOWN) {
            cl->pvs++;
        }
        switch (term->sign) {
            case NodeLabel::TRUE:
                if (index) insert_true_clause(cl, term->node);
                if (label == NodeLabel::TRUE) {
                    cl->sats++;
                    cl->pvs++;
                }
                break;
            case NodeLabel::FALSE:
                if (index) insert_false_clause(cl, term->node);
                if (label == NodeLabel::FALSE) {
                    cl->sats++;
                    cl->pvs++;
                }
                break;
            default:
                break;
        }
    }

    if (index) {
        check_clauses(ltms, {cl});
    }
    return cl;
}

void insert_true_clause(Clause* cl, TmsNode* node) {
    node->true_clauses.push_back(cl);
}

void insert_false_clause(Clause* cl, TmsNode* node) {
    node->false_clauses.push_back(cl);
}

void add_nogood(TmsNode* culprit, NodeLabel sign, std::vector<TmsNode*>& assumptions) {
    std::vector<TmsNode*> trues, falses;
    for (auto* a : assumptions) {
        NodeLabel lbl = (a == culprit) ? sign : a->label;
        switch (lbl) {
            case NodeLabel::TRUE:
                falses.push_back(a);
                break;
            case NodeLabel::FALSE:
                trues.push_back(a);
                break;
            default:
                break;
        }
    }
    add_clause(trues, falses, std::any(std::string("NOGOOD")));
}

// ---------------------------------------------------------------------------
// Boolean Constraint Propagation
// ---------------------------------------------------------------------------

void check_clauses(LTMS* ltms, std::vector<Clause*> clauses_to_check) {
    debugging_ltms(ltms, "Beginning propagation...");
    while (!clauses_to_check.empty()) {
        Clause* cl = clauses_to_check.front();
        clauses_to_check.erase(clauses_to_check.begin());
        check_clause(ltms, cl, clauses_to_check);
    }
}

void check_clause(LTMS* ltms, Clause* clause, std::vector<Clause*>& clauses_to_check) {
    if (violated_clause(clause)) {
        // pushnew
        bool found = false;
        for (auto* vc : ltms->violated_clauses) {
            if (vc == clause) { found = true; break; }
        }
        if (!found) {
            ltms->violated_clauses.push_back(clause);
        }
    } else if (clause->pvs == 1) {
        Literal* unknown_pair = find_unknown_pair(clause);
        if (unknown_pair) {
            set_truth(unknown_pair->node, unknown_pair->sign,
                      std::any(clause), clauses_to_check);
        }
    }
}

Literal* find_unknown_pair(Clause* clause) {
    for (auto* term : clause->literals) {
        if (unknown_node(term->node)) return term;
    }
    return nullptr;
}

// ---------------------------------------------------------------------------
// Truth setting
// ---------------------------------------------------------------------------

void top_set_truth(TmsNode* node, NodeLabel value, std::any reason) {
    std::vector<Clause*> clauses_to_check;
    set_truth(node, value, reason, clauses_to_check);
    check_clauses(node->ltms, clauses_to_check);
    check_for_contradictions(node->ltms);
}

void set_truth(TmsNode* node, NodeLabel value, std::any reason,
               std::vector<Clause*>& clauses_to_check) {
    LTMS* ltms = node->ltms;
    debugging_ltms(ltms, "Setting " + node_string(node) + " to " +
                   node_label_string(value) + " via " + any_to_string(reason));

    node->support = reason;
    node->label = value;

    switch (value) {
        case NodeLabel::TRUE:
            if (ltms->enqueue_procedure) {
                for (auto& rule : node->true_rules) {
                    ltms->enqueue_procedure(rule);
                }
                node->true_rules.clear();
            }
            for (auto* clause : node->true_clauses) {
                clause->sats++;
            }
            for (auto* clause : node->false_clauses) {
                clause->pvs--;
                if (clause->pvs < 2) {
                    clauses_to_check.push_back(clause);
                }
            }
            break;

        case NodeLabel::FALSE:
            if (ltms->enqueue_procedure) {
                for (auto& rule : node->false_rules) {
                    ltms->enqueue_procedure(rule);
                }
                node->false_rules.clear();
            }
            for (auto* clause : node->false_clauses) {
                clause->sats++;
            }
            for (auto* clause : node->true_clauses) {
                clause->pvs--;
                if (clause->pvs < 2) {
                    clauses_to_check.push_back(clause);
                }
            }
            break;

        default:
            break;
    }
}

// ---------------------------------------------------------------------------
// Retracting an assumption
// ---------------------------------------------------------------------------

std::vector<TmsNode*> propagate_unknownness(TmsNode* in_node) {
    LTMS* ltms = in_node->ltms;
    std::vector<TmsNode*> forget_queue = {in_node};
    std::vector<TmsNode*> unknown_queue;

    while (!forget_queue.empty()) {
        TmsNode* node = forget_queue.front();
        forget_queue.erase(forget_queue.begin());
        unknown_queue.push_back(node);

        debugging_ltms(ltms, "Retracting", node);
        NodeLabel old_value = node->label;
        node->label = NodeLabel::UNKNOWN;
        node->support = std::any();

        std::vector<Clause*>* clauses_ptr = nullptr;
        switch (old_value) {
            case NodeLabel::TRUE:
                clauses_ptr = &node->false_clauses;
                break;
            case NodeLabel::FALSE:
                clauses_ptr = &node->true_clauses;
                break;
            default:
                break;
        }

        if (clauses_ptr) {
            for (auto* clause : *clauses_ptr) {
                clause->pvs++;
                if (clause->pvs == 2) {
                    TmsNode* node2 = clause_consequent(clause);
                    if (node2) {
                        forget_queue.push_back(node2);
                    }
                }
            }
        }

        if (complete_is_set(ltms->complete) && ltms->propagate_more_unknownness_fn) {
            ltms->propagate_more_unknownness_fn(old_value, node, ltms);
        }
    }
    return unknown_queue;
}

TmsNode* clause_consequent(Clause* clause) {
    for (auto* term : clause->literals) {
        if (term->node->label == term->sign) {
            Clause* sup = support_as_clause(term->node->support);
            if (sup == clause) {
                return term->node;
            }
            return nullptr;
        }
    }
    return nullptr;
}

void find_alternative_support(LTMS* ltms, std::vector<TmsNode*>& nodes) {
    for (auto* node : nodes) {
        if (unknown_node(node)) {
            // Copy clause lists to avoid mutation issues during iteration.
            auto tc = node->true_clauses;
            auto fc = node->false_clauses;
            check_clauses(ltms, tc);
            check_clauses(ltms, fc);
        }
    }
    if (complete_is_true(ltms->complete) && ltms->ipia_fn) {
        ltms->ipia_fn(ltms);
    }
}

// ---------------------------------------------------------------------------
// Contradiction handling
// ---------------------------------------------------------------------------

void check_for_contradictions(LTMS* ltms) {
    std::vector<Clause*> still_violated;
    for (auto* c : ltms->violated_clauses) {
        if (violated_clause(c)) {
            still_violated.push_back(c);
        }
    }
    ltms->violated_clauses = still_violated;
    if (!still_violated.empty()) {
        contradiction_handler(ltms, still_violated);
    }
}

void contradiction_handler(LTMS* ltms, std::vector<Clause*>& violated_clauses) {
    if (!ltms->checking_contradictions) {
        // Update pending contradictions cache.
        std::vector<Clause*> pending;
        for (auto* c : ltms->pending_contradictions) {
            if (violated_clause(c)) {
                pending.push_back(c);
            }
        }
        for (auto* vc : violated_clauses) {
            if (violated_clause(vc)) {
                bool found = false;
                for (auto* p : pending) {
                    if (p == vc) { found = true; break; }
                }
                if (!found) {
                    pending.push_back(vc);
                }
            }
        }
        ltms->pending_contradictions = pending;
    } else {
        for (auto& handler : ltms->contradiction_handlers) {
            if (handler(violated_clauses, ltms)) return;
        }
    }
}

void without_contradiction_check(LTMS* ltms, std::function<void()> body) {
    bool old_val = ltms->checking_contradictions;
    ltms->checking_contradictions = false;
    try {
        body();
    } catch (...) {
        ltms->checking_contradictions = old_val;
        throw;
    }
    ltms->checking_contradictions = old_val;
}

void with_contradiction_check(LTMS* ltms, std::function<void()> body) {
    bool old_val = ltms->checking_contradictions;
    ltms->checking_contradictions = true;
    try {
        body();
    } catch (...) {
        ltms->checking_contradictions = old_val;
        throw;
    }
    ltms->checking_contradictions = old_val;
}

void with_contradiction_handler(LTMS* ltms,
                                std::function<bool(std::vector<Clause*>&, LTMS*)> handler,
                                std::function<void()> body) {
    auto old_handlers = ltms->contradiction_handlers;
    ltms->contradiction_handlers.insert(ltms->contradiction_handlers.begin(), handler);
    try {
        body();
    } catch (...) {
        ltms->contradiction_handlers = old_handlers;
        throw;
    }
    ltms->contradiction_handlers = old_handlers;
}

void with_assumptions(std::vector<AssumptionValue>& assumption_values, std::function<void()> body) {
    for (auto& av : assumption_values) {
        enable_assumption(av.node, av.label);
    }
    try {
        body();
    } catch (...) {
        for (auto& av : assumption_values) {
            retract_assumption(av.node);
        }
        throw;
    }
    for (auto& av : assumption_values) {
        retract_assumption(av.node);
    }
}

// ---------------------------------------------------------------------------
// Support queries
// ---------------------------------------------------------------------------

std::vector<TmsNode*> support_for_node(TmsNode* node, std::any* informant_out) {
    if (support_is_null(node->support)) {
        if (informant_out) *informant_out = std::any();
        return {};
    }
    if (support_is_enabled_assumption(node->support)) {
        if (informant_out) *informant_out = std::any(ENABLED_ASSUMPTION);
        return {};
    }
    Clause* clause = support_as_clause(node->support);
    if (!clause) {
        if (informant_out) *informant_out = std::any();
        return {};
    }
    std::vector<TmsNode*> result;
    for (auto* pair : clause->literals) {
        if (pair->node != node) {
            result.push_back(pair->node);
        }
    }
    if (informant_out) *informant_out = clause->informant;
    return result;
}

std::vector<TmsNode*> assumptions_of_node(TmsNode* node) {
    if (support_is_enabled_assumption(node->support)) {
        return {node};
    }
    if (known_node(node)) {
        Clause* clause = support_as_clause(node->support);
        if (clause) {
            return assumptions_of_clause(clause);
        }
    }
    return {};
}

std::vector<TmsNode*> assumptions_of_clause(Clause* in_clause) {
    std::vector<Clause*> clause_queue = {in_clause};
    // Use a unique marker (pointer to a local) for visited check.
    int mark_val = 0;
    std::any mark = std::any(&mark_val);
    std::vector<TmsNode*> assumptions;

    while (!clause_queue.empty()) {
        Clause* clause = clause_queue.front();
        clause_queue.erase(clause_queue.begin());

        for (auto* term_pair : clause->literals) {
            TmsNode* node = term_pair->node;
            // Check if already marked.
            if (auto p = std::any_cast<int*>(&node->mark)) {
                if (*p == &mark_val) continue;
            }
            if (node->label != term_pair->sign) {
                if (support_is_enabled_assumption(node->support)) {
                    assumptions.push_back(node);
                } else if (support_is_null(node->support)) {
                    ltms_error("Node is unknown", std::any(node_string(node)));
                } else {
                    Clause* sup = support_as_clause(node->support);
                    if (sup) {
                        clause_queue.push_back(sup);
                    }
                }
            }
            node->mark = mark;
        }
    }
    return assumptions;
}

// ---------------------------------------------------------------------------
// Simple user interface
// ---------------------------------------------------------------------------

bool ask_user_handler(std::vector<Clause*>& contradictions, LTMS* /*ltms*/) {
    for (auto* contradiction : contradictions) {
        if (violated_clause(contradiction)) {
            handle_one_contradiction(contradiction);
        }
    }
    return true;
}

void handle_one_contradiction(Clause* violated_cl) {
    auto contra_assumptions = assumptions_of_clause(violated_cl);
    if (contra_assumptions.empty()) {
        ltms_error("Global contradiction", std::any(print_clause(violated_cl)));
    }
    std::cout << "\nContradiction found:" << std::endl;
    print_contra_list(contra_assumptions);
    std::cout << "Call tms_answer(<number>) to retract assumption." << std::endl;
    // In the Lisp version this enters an interactive break.
    // In C++ we just print and auto-retract the first assumption.
    if (!contra_assumptions.empty()) {
        std::cout << "Auto-retracting assumption 1: "
                  << node_string(contra_assumptions[0]) << std::endl;
        retract_assumption(contra_assumptions[0]);
    }
}

void print_contra_list(std::vector<TmsNode*>& nodes) {
    int counter = 1;
    for (auto* n : nodes) {
        std::cout << counter << " " << node_string(n) << std::endl;
        counter++;
    }
}

TmsNode* tms_answer(int num, std::vector<TmsNode*>& nodes) {
    if (num < 1) {
        std::cout << "Ignoring answer, too small" << std::endl;
        return nullptr;
    }
    if (num > static_cast<int>(nodes.size())) {
        std::cout << "Ignoring answer, too big." << std::endl;
        return nullptr;
    }
    return nodes[num - 1];
}

bool avoid_all(std::vector<Clause*>& contradictions, LTMS* /*ltms*/) {
    for (auto* contradiction : contradictions) {
        if (violated_clause(contradiction)) {
            auto culprits = assumptions_of_clause(contradiction);
            if (culprits.empty()) {
                ltms_error("Total contradiction", std::any(print_clause(contradiction)));
            }
            TmsNode* culprit = culprits[0];
            NodeLabel sign = culprit->label;
            retract_assumption(culprit);
            add_nogood(culprit, sign, culprits);
        }
    }
    return true;
}

// ---------------------------------------------------------------------------
// Display helpers
// ---------------------------------------------------------------------------

std::vector<TmsNode*> clause_antecedents(Clause* clause) {
    std::vector<TmsNode*> result;
    for (auto* pair : clause->literals) {
        Clause* sup = support_as_clause(pair->node->support);
        if (sup != clause) {
            result.push_back(pair->node);
        }
    }
    return result;
}

std::string signed_node_string(TmsNode* node) {
    if (true_node(node)) return node_string(node);
    if (false_node(node)) return "Not[" + node_string(node) + "]";
    return "Unknown[" + node_string(node) + "]";
}

std::vector<TmsNode*> node_consequences(TmsNode* node) {
    std::vector<TmsNode*> conseqs;
    std::vector<Clause*>* clauses_ptr = nullptr;
    switch (node->label) {
        case NodeLabel::TRUE:
            clauses_ptr = &node->false_clauses;
            break;
        case NodeLabel::FALSE:
            clauses_ptr = &node->true_clauses;
            break;
        default:
            return conseqs;
    }
    for (auto* cl : *clauses_ptr) {
        Clause* sup = support_as_clause(node->support);
        if (cl != sup) {
            TmsNode* conseq = clause_consequent(cl);
            if (conseq) {
                conseqs.push_back(conseq);
            }
        }
    }
    return conseqs;
}

void why_node(TmsNode* node) {
    if (unknown_node(node)) {
        std::cout << node_string(node) << " is unknown." << std::endl;
        return;
    }
    if (support_is_enabled_assumption(node->support)) {
        std::cout << node_string(node) << " is " << node_label_string(node->label)
                  << " <" << ENABLED_ASSUMPTION << ">" << std::endl;
        return;
    }
    Clause* sup = support_as_clause(node->support);
    if (sup) {
        std::string inf_str;
        if (sup->informant.has_value()) {
            inf_str = any_to_string(sup->informant);
        } else {
            inf_str = print_clause(sup);
        }
        std::cout << node_string(node) << " is " << node_label_string(node->label)
                  << " via " << inf_str << " on" << std::endl;
        for (auto* term_pair : sup->literals) {
            if (term_pair->node->label != term_pair->sign) {
                std::cout << "   " << node_string(term_pair->node) << " is "
                          << node_label_string(term_pair->node->label) << std::endl;
            }
        }
    }
}

void why_nodes(LTMS* ltms) {
    if (ltms->nodes_enabled) {
        for (auto& [key, node] : ltms->nodes) {
            why_node(node);
        }
    }
}

void explain_node(TmsNode* node) {
    if (node->label == NodeLabel::UNKNOWN) return;
    // Clear marks on all nodes.
    if (node->ltms->nodes_enabled) {
        for (auto& [key, n] : node->ltms->nodes) {
            n->mark = std::any();
        }
    }
    explain_1(node, 0);
}

int explain_1(TmsNode* node, int line_count) {
    // If already marked (has an int value), skip.
    if (auto p = std::any_cast<int>(&node->mark)) {
        return *p;
    }

    if (support_is_enabled_assumption(node->support)) {
        line_count++;
        std::string lit_str = true_node(node) ? node_string(node)
                                               : ("(:NOT " + node_string(node) + ")");
        std::cout << line_count << " " << lit_str << "  ()  Assumption" << std::endl;
        node->mark = std::any(line_count);
        return line_count;
    }

    Clause* sup = support_as_clause(node->support);
    if (!sup) return line_count;

    auto antecedents = clause_antecedents(sup);
    std::vector<int> ante_lines;
    for (auto* ante : antecedents) {
        line_count = explain_1(ante, line_count);
        if (auto p = std::any_cast<int>(&ante->mark)) {
            ante_lines.push_back(*p);
        }
    }

    line_count++;
    std::string lit_str = true_node(node) ? node_string(node)
                                           : ("(:NOT " + node_string(node) + ")");
    std::ostringstream ante_ss;
    ante_ss << "(";
    for (size_t i = 0; i < ante_lines.size(); i++) {
        if (i > 0) ante_ss << " ";
        ante_ss << ante_lines[i];
    }
    ante_ss << ")";

    std::cout << line_count << " " << lit_str << " " << ante_ss.str() << "  ";
    pretty_print_clause(sup);
    node->mark = std::any(line_count);
    return line_count;
}

void pretty_print_clauses(LTMS* ltms) {
    walk_clauses(ltms, [](Clause* cl) {
        std::cout << " ";
        pretty_print_clause(cl);
    });
}

void pretty_print_clause(Clause* clause) {
    std::cout << "(:OR";
    for (auto* literal : clause->literals) {
        if (literal->sign == NodeLabel::TRUE) {
            std::cout << " " << node_string(literal->node);
        } else {
            std::cout << " (:NOT " << node_string(literal->node) << ")";
        }
    }
    std::cout << ")" << std::endl;
}

void show_node_consequences(TmsNode* node) {
    auto conseqs = node_consequences(node);
    if (!conseqs.empty()) {
        std::cout << " Consequences of " << signed_node_string(node) << ":" << std::endl;
        for (auto* conseq : conseqs) {
            std::cout << "  " << signed_node_string(conseq) << std::endl;
        }
    } else {
        std::cout << " " << node_string(node) << " has no consequences." << std::endl;
    }
}

void node_show_clauses(TmsNode* node) {
    std::cout << "For " << node_string(node) << ":" << std::endl;
    for (auto* cl : node->true_clauses) {
        pretty_print_clause(cl);
    }
    for (auto* cl : node->false_clauses) {
        pretty_print_clause(cl);
    }
}

void explore_network(TmsNode* node) {
    if (!known_node(node)) {
        std::cout << " Sorry, " << node_string(node) << " not believed." << std::endl;
        return;
    }
    // Simplified non-interactive version: just print antecedents and consequences.
    std::cout << "\nExploring network from " << node_string(node) << "..." << std::endl;
    why_node(node);
    auto conseqs = node_consequences(node);
    if (!conseqs.empty()) {
        std::cout << "Consequences:" << std::endl;
        for (auto* c : conseqs) {
            std::cout << "  " << signed_node_string(c) << std::endl;
        }
    }
}
