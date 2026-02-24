// -*- C++ -*-

/// ATRE (ATMS-based Tiny Rule Engine) - Implementation
/// Converted from ainter.lisp, adata.lisp, arules.lisp

#include "atre.h"

// ================================================================
// Global ATRE pointer
// ================================================================
AtrePtr global_atre = nullptr;

// ================================================================
// ATRE interface (from ainter.lisp)
// ================================================================

AtrePtr create_atre(const std::string& title, bool debugging) {
    auto atre = std::make_shared<Atre>();
    atre->title = title;
    atre->debugging = debugging;

    // Create the underlying ATMS
    atre->atms = create_atms(
        std::string("ATMS-OF ") + title,
        [](NodePtr n) -> std::string { return stringify_node(n); },
        false,
        [atre](std::any pair) { enqueue(pair, atre); }
    );

    in_atre(atre);

    // Create a default contradiction datum
    auto false_datum = std::make_shared<Datum>();
    false_datum->counter = ++atre->datum_counter;
    false_datum->atre = atre;
    false_datum->lisp_form = std::string("FALSE");
    false_datum->dbclass = get_dbclass("FALSE", atre);
    false_datum->tms_node = atre->atms->contra_node;
    atre->atms->contra_node->datum = std::static_pointer_cast<void>(false_datum);
    false_datum->dbclass->facts.push_back(false_datum);

    return atre;
}

void change_atre(AtrePtr atre, std::optional<bool> debugging) {
    if (debugging.has_value()) atre->debugging = debugging.value();
}

void in_atre(AtrePtr atre) {
    global_atre = atre;
}

void change_focus(EnvPtr env, AtrePtr atre) {
    if (!atre) atre = global_atre;
    if (!atre) return;

    if (env && !env->is_nogood()) {
        atre->focus = env;
        // Re-queue implied-by rules
        for (auto& r : atre->imp_rules) {
            atre->queue.push_back(r);
        }
        atre->imp_rules.clear();
    }
}

bool focus_okay(AtrePtr atre) {
    return atre->focus && !atre->focus->is_nogood();
}

void contradiction_rule(EnvPtr env,
    std::function<void(EnvPtr)> proc, AtrePtr atre) {
    if (!atre) atre = global_atre;
    if (env->is_nogood()) {
        // Immediately enqueue
        std::vector<std::any> args = {std::any(proc),
            std::any(std::vector<EnvPtr>{env}), std::any()};
        enqueue(std::any(args), atre);
    } else {
        std::vector<std::any> entry = {std::any(proc),
            std::any(std::vector<EnvPtr>{env}), std::any()};
        env->rules.push_back(std::any(entry));
    }
}

void show(AtrePtr atre, std::ostream& stream) {
    if (!atre) atre = global_atre;
    stream << "For ATRE " << atre->title << ":\n Focus = ";
    if (atre->focus) stream << atre->focus->to_string();
    else stream << "empty";
    stream << ".";
    show_data(atre, stream);
    show_rules(atre, stream);
}

std::vector<EnvPtr> solutions(AtrePtr atre,
    const std::vector<std::vector<SExpr>>& choice_sets) {
    std::vector<std::vector<NodePtr>> node_choice_sets;
    for (auto& cs : choice_sets) {
        std::vector<NodePtr> nodes;
        for (auto& f : cs) {
            nodes.push_back(get_tms_node(f, atre));
        }
        node_choice_sets.push_back(nodes);
    }
    return interpretations(atre->atms, node_choice_sets);
}

// ================================================================
// ATRE database (from adata.lisp)
// ================================================================

DatumPtr assert_fact(const SExpr& fact, const std::vector<SExpr>& just,
                     AtrePtr atre) {
    if (!atre) atre = global_atre;

    auto datum = referent(fact, true, atre);
    auto node = datum->tms_node;

    if (atre->debugging) {
        std::cerr << "\n    Asserting " << show_datum(datum) << " via [";
        for (size_t i = 0; i < just.size(); i++) {
            if (i > 0) std::cerr << ", ";
            try { std::cerr << std::any_cast<std::string>(just[i]); }
            catch (...) { std::cerr << "?"; }
        }
        std::cerr << "].";
    }

    // First element is informant, rest are antecedent facts
    std::string informant;
    try { informant = std::any_cast<std::string>(just[0]); }
    catch (...) { informant = "?"; }

    std::vector<NodePtr> antecedents;
    for (size_t i = 1; i < just.size(); i++) {
        auto ref = referent(just[i], true, atre);
        antecedents.push_back(ref->tms_node);
    }

    justify_node(informant, node, antecedents);
    return datum;
}

DatumPtr assume_fact(const SExpr& fact, const std::string& reason,
                     AtrePtr atre) {
    if (!atre) atre = global_atre;

    auto datum = referent(fact, true, atre);
    auto node = datum->tms_node;

    if (datum->assumption_reason.empty()) {
        datum->assumption_reason = reason;
        if (atre->debugging) {
            std::cerr << "\n    Assuming " << show_datum(datum)
                      << " via " << reason << ".";
        }
        assume_node(node);
    } else if (datum->assumption_reason != reason) {
        throw std::runtime_error(
            "Fact assumed again with different reason: " +
            show_datum(datum) + " was " + datum->assumption_reason +
            " now " + reason);
    }
    return datum;
}

bool already_assumed(const SExpr& fact, AtrePtr atre) {
    if (!atre) atre = global_atre;
    auto node = get_tms_node(fact, atre);
    return node->assumption;
}

void assume_if_needed(const SExpr& fact, const std::string& reason,
                      AtrePtr atre) {
    if (!already_assumed(fact, atre)) {
        assume_fact(fact, reason, atre);
    }
}

void contradiction(const SExpr& fact, AtrePtr atre) {
    if (!atre) atre = global_atre;
    auto datum = referent(fact, true, atre);
    make_contradiction(datum->tms_node);
}

DbclassPtr get_dbclass(const std::string& fact, AtrePtr atre) {
    if (!atre) atre = global_atre;
    if (fact.empty()) {
        throw std::runtime_error("Empty string can't be a dbclass.");
    }

    auto it = atre->dbclass_table.find(fact);
    if (it != atre->dbclass_table.end()) {
        return it->second;
    }

    auto dbclass = std::make_shared<Dbclass>();
    dbclass->name = fact;
    dbclass->atre = atre;
    atre->dbclass_table[fact] = dbclass;
    atre->dbclasses.push_back(dbclass);
    return dbclass;
}

DatumPtr referent(const SExpr& fact, bool virtual_mode, AtrePtr atre) {
    if (!atre) atre = global_atre;
    if (virtual_mode) return insert_datum(fact, atre);
    return referent1(fact, atre);
}

DatumPtr referent1(const SExpr& fact, AtrePtr atre) {
    if (!atre) atre = global_atre;
    std::string fact_str;
    try { fact_str = std::any_cast<std::string>(fact); }
    catch (...) { return nullptr; }

    // Extract dbclass name (first word or the whole thing)
    std::string dbname = fact_str;
    if (fact_str.front() == '(') {
        // Extract first symbol from s-expression
        auto pos = fact_str.find_first_of(" )");
        dbname = fact_str.substr(1, pos - 1);
    }

    auto it = atre->dbclass_table.find(dbname);
    if (it == atre->dbclass_table.end()) return nullptr;

    for (auto& datum : it->second->facts) {
        std::string df;
        try { df = std::any_cast<std::string>(datum->lisp_form); }
        catch (...) { continue; }
        if (df == fact_str) return datum;
    }
    return nullptr;
}

DatumPtr insert_datum(const SExpr& fact, AtrePtr atre) {
    if (!atre) atre = global_atre;

    auto existing = referent1(fact, atre);
    if (existing) return existing;

    std::string fact_str;
    try { fact_str = std::any_cast<std::string>(fact); }
    catch (...) { fact_str = "?"; }

    // Extract dbclass name
    std::string dbname = fact_str;
    if (!fact_str.empty() && fact_str.front() == '(') {
        auto pos = fact_str.find_first_of(" )");
        dbname = fact_str.substr(1, pos - 1);
    }

    auto datum = std::make_shared<Datum>();
    datum->counter = ++atre->datum_counter;
    datum->atre = atre;
    datum->lisp_form = fact;
    datum->dbclass = get_dbclass(dbname, atre);
    datum->tms_node = tms_create_node(atre->atms, std::any(datum));
    datum->dbclass->facts.push_back(datum);

    try_rules(datum);
    return datum;
}

std::vector<SExpr> fetch(const SExpr& pattern, AtrePtr atre) {
    if (!atre) atre = global_atre;
    // Simplified: return all facts matching the pattern's dbclass
    std::string pat_str;
    try { pat_str = std::any_cast<std::string>(pattern); }
    catch (...) { return {}; }

    std::string dbname = pat_str;
    if (!pat_str.empty() && pat_str.front() == '(') {
        auto pos = pat_str.find_first_of(" )");
        dbname = pat_str.substr(1, pos - 1);
    }

    auto it = atre->dbclass_table.find(dbname);
    if (it == atre->dbclass_table.end()) return {};

    std::vector<SExpr> result;
    for (auto& datum : it->second->facts) {
        result.push_back(datum->lisp_form);
    }
    return result;
}

bool true_fact(const SExpr& fact, AtrePtr atre) {
    if (!atre) atre = global_atre;
    auto r = referent(fact, false, atre);
    if (!r) return false;
    return true_node(r->tms_node);
}

bool in_fact(const SExpr& fact, EnvPtr env, AtrePtr atre) {
    if (!atre) atre = global_atre;
    auto r = referent(fact, false, atre);
    if (!r) return false;
    return in_node(r->tms_node, env);
}

bool out_fact(const SExpr& fact, EnvPtr env, AtrePtr atre) {
    if (!atre) atre = global_atre;
    auto r = referent(fact, false, atre);
    if (!r) return false;
    return out_node(r->tms_node, env);
}

bool consistent_with_fact(const SExpr& fact, EnvPtr env, AtrePtr atre) {
    if (!atre) atre = global_atre;
    auto r = referent(fact, false, atre);
    if (!r) return false;
    return node_consistent_with(r->tms_node, env);
}

NodePtr get_tms_node(const SExpr& fact, AtrePtr atre) {
    if (!atre) atre = global_atre;
    auto datum = referent(fact, true, atre);
    return datum->tms_node;
}

SExpr view_node(NodePtr node) {
    try {
        auto datum = std::any_cast<std::shared_ptr<Datum>>(node->datum);
        return datum->lisp_form;
    } catch (...) {
        return node->datum;
    }
}

std::string stringify_node(NodePtr node) {
    auto form = view_node(node);
    try { return std::any_cast<std::string>(form); }
    catch (...) { return "?"; }
}

std::vector<EnvPtr> assumptions_of(const SExpr& fact, AtrePtr atre) {
    if (!atre) atre = global_atre;
    auto datum = referent(fact, true, atre);
    return datum->tms_node->label;
}

EnvPtr environment_of(const std::vector<SExpr>& facts, AtrePtr atre) {
    if (!atre) atre = global_atre;
    auto env = atre->atms->empty_env;
    for (auto& fact : facts) {
        auto node = get_tms_node(fact, atre);
        if (!node->assumption) {
            throw std::runtime_error("Non-assumption in environment_of");
        }
        env = cons_env(node, env);
        if (env->is_nogood()) return nullptr;
    }
    return env;
}

EnvPtr environment_cons(const SExpr& fact, EnvPtr env, AtrePtr atre) {
    if (!atre) atre = global_atre;
    return cons_env(get_tms_node(fact, atre), env);
}

std::string show_datum(DatumPtr datum) {
    try { return std::any_cast<std::string>(datum->lisp_form); }
    catch (...) { return "?"; }
}

int show_data(AtrePtr atre, std::ostream& stream) {
    if (!atre) atre = global_atre;
    int counter = 0;
    stream << "\n" << atre->datum_counter << " facts total.";
    for (auto& dbclass : atre->dbclasses) {
        for (auto& datum : dbclass->facts) {
            counter++;
            stream << "\n" << show_datum(datum) << ": ";
            auto label = datum->tms_node->label;
            stream << "(";
            for (size_t i = 0; i < label.size(); i++) {
                if (i > 0) stream << " ";
                stream << label[i]->to_string();
            }
            stream << ")";
        }
    }
    return counter;
}

int show_context(EnvPtr env, AtrePtr atre, std::ostream& stream) {
    if (!atre) atre = global_atre;
    int counter = 0;
    for (auto& dbclass : atre->dbclasses) {
        for (auto& datum : dbclass->facts) {
            if (in_node(datum->tms_node, env)) {
                counter++;
                stream << "\n" << show_datum(datum);
            }
        }
    }
    stream << "\n" << counter << " facts total.";
    return counter;
}

int show_dbclasses(AtrePtr atre, std::ostream& stream) {
    if (!atre) atre = global_atre;
    int counter = 0;
    for (auto& dbclass : atre->dbclasses) {
        counter++;
        stream << "\n " << dbclass->name
               << ": " << dbclass->facts.size() << " facts, "
               << dbclass->rules.size() << " rules";
    }
    return counter;
}

DatumPtr get_datum(int num, AtrePtr atre) {
    if (!atre) atre = global_atre;
    for (auto& [key, dbclass] : atre->dbclass_table) {
        for (auto& datum : dbclass->facts) {
            if (datum->counter == num) return datum;
        }
    }
    return nullptr;
}

JustPtr get_just(int num, AtrePtr atre) {
    if (!atre) atre = global_atre;
    for (auto& just : atre->atms->justs) {
        if (just->index == num) return just;
    }
    return nullptr;
}

// ================================================================
// Rules system (from arules.lisp)
// ================================================================

void insert_rule(DbclassPtr dbclass,
    std::function<std::tuple<bool, std::vector<std::any>, std::string>(const SExpr&)> matcher,
    std::function<void(const std::vector<std::any>&)> body,
    const std::vector<NodePtr>& in_nodes,
    const std::vector<NodePtr>& imp_nodes) {

    auto atre = dbclass->atre.lock();
    if (!atre) return;

    auto rule = std::make_shared<Rule>();
    rule->matcher = matcher;
    rule->atre = atre;
    rule->body = body;
    rule->dbclass = dbclass;
    rule->counter = ++atre->rule_counter;
    rule->in_nodes = in_nodes;
    rule->imp_nodes = imp_nodes;

    atre->rules.push_back(rule);
    dbclass->rules.push_back(rule);

    // Try rule on existing facts
    for (auto& candidate : dbclass->facts) {
        try_rule_on(rule, candidate);
    }
}

void try_rules(DatumPtr datum) {
    for (auto& rule : datum->dbclass->rules) {
        try_rule_on(rule, datum);
    }
}

void try_rule_on(RulePtr rule, DatumPtr datum) {
    auto [okay, bindings, condition] = rule->matcher(datum->lisp_form);
    if (!okay) return;

    if (condition == "IN" || condition == "IMPLIED-BY") {
        bindings.insert(bindings.begin(), std::any(datum->tms_node));
    }

    // Build node-condition pair
    std::vector<NodePtr> in_nodes_combined;
    std::vector<NodePtr> imp_nodes_combined;

    if (condition == "IN") {
        in_nodes_combined.push_back(datum->tms_node);
        in_nodes_combined.insert(in_nodes_combined.end(),
            rule->in_nodes.begin(), rule->in_nodes.end());
        imp_nodes_combined = rule->imp_nodes;
    } else if (condition == "IMPLIED-BY") {
        in_nodes_combined = rule->in_nodes;
        imp_nodes_combined.push_back(datum->tms_node);
        imp_nodes_combined.insert(imp_nodes_combined.end(),
            rule->imp_nodes.begin(), rule->imp_nodes.end());
    } else { // INTERN
        in_nodes_combined = rule->in_nodes;
        imp_nodes_combined = rule->imp_nodes;
    }

    auto atre = datum->atre.lock();
    if (!atre) return;

    // Queued rule: (body, bindings, (in_nodes, imp_nodes))
    struct QueuedRule {
        std::function<void(const std::vector<std::any>&)> body;
        std::vector<std::any> bindings;
        std::vector<NodePtr> in_nodes;
        std::vector<NodePtr> imp_nodes;
    };
    auto qr = std::make_shared<QueuedRule>();
    qr->body = rule->body;
    qr->bindings = bindings;
    qr->in_nodes = in_nodes_combined;
    qr->imp_nodes = imp_nodes_combined;

    enqueue(std::any(qr), atre);
}

int run_rules(AtrePtr atre) {
    if (!atre) atre = global_atre;

    // Move in-rules to queue
    for (auto& r : atre->in_rules) {
        atre->queue.push_back(r);
    }
    atre->in_rules.clear();

    int counter = 0;
    while (!atre->queue.empty()) {
        auto form = atre->queue.front();
        atre->queue.erase(atre->queue.begin());
        execute_rule(form, atre);
        counter++;
    }

    if (atre->debugging) {
        std::cerr << "\n    " << counter << " rules run.";
    }
    atre->rules_run += counter;
    return counter;
}

void execute_rule(const std::any& queued_rule, AtrePtr atre) {
    struct QueuedRule {
        std::function<void(const std::vector<std::any>&)> body;
        std::vector<std::any> bindings;
        std::vector<NodePtr> in_nodes;
        std::vector<NodePtr> imp_nodes;
    };

    std::shared_ptr<QueuedRule> qr;
    try {
        qr = std::any_cast<std::shared_ptr<QueuedRule>>(queued_rule);
    } catch (...) {
        return;
    }

    if (!in_triggers_ready(qr->in_nodes, atre)) {
        atre->in_rules.push_back(queued_rule);
        return;
    }
    if (!implied_by_triggers_ready(qr->imp_nodes, atre)) {
        atre->imp_rules.push_back(queued_rule);
        return;
    }

    qr->body(qr->bindings);
}

bool in_triggers_ready(const std::vector<NodePtr>& nodes, AtrePtr atre,
                       EnvPtr env) {
    if (!env) env = atre->atms->empty_env;
    if (env->is_nogood()) return false;
    if (nodes.empty()) return true;

    for (auto& new_env : nodes[0]->label) {
        auto u = union_env(new_env, env);
        if (u && !u->is_nogood()) {
            std::vector<NodePtr> rest(nodes.begin() + 1, nodes.end());
            if (in_triggers_ready(rest, atre, u)) return true;
        }
    }
    return false;
}

bool implied_by_triggers_ready(const std::vector<NodePtr>& nodes, AtrePtr atre) {
    if (nodes.empty()) return true;
    if (!focus_okay(atre)) return false;
    for (auto& n : nodes) {
        if (!in_node(n, atre->focus)) return false;
    }
    return true;
}

void enqueue(std::any item, AtrePtr atre) {
    atre->queue.push_back(item);
}

std::any dequeue(AtrePtr atre) {
    if (atre->queue.empty()) return {};
    auto front = atre->queue.front();
    atre->queue.erase(atre->queue.begin());
    return front;
}

void show_rules(AtrePtr atre, std::ostream& stream) {
    if (!atre) atre = global_atre;
    int counter = 0;
    std::vector<std::pair<std::string, int>> dist;
    for (auto& dbclass : atre->dbclasses) {
        int inc = static_cast<int>(dbclass->rules.size());
        if (inc > 0) {
            dist.push_back({dbclass->name, inc});
            counter += inc;
        }
    }

    int in = static_cast<int>(atre->in_rules.size());
    int imp = static_cast<int>(atre->imp_rules.size());
    int queued = static_cast<int>(atre->queue.size());
    counter += in + imp;

    stream << "\n " << atre->title << " has " << counter << " rules in all.";
    stream << "\n  " << (queued > 0 ? std::to_string(queued) : "None")
           << " queued.";
    if (in + imp > 0) {
        stream << "  Pending: "
               << (in > 0 ? std::to_string(in) : "No") << " in, "
               << (imp > 0 ? std::to_string(imp) : "No") << " implied-by.";
    } else {
        stream << "  None pending.";
    }
    if (!dist.empty()) {
        stream << "\n Cached under dbclasses:";
        for (auto& [name, cnt] : dist) {
            stream << "\n    " << name << ": " << cnt;
        }
    }
}
