// -*- C++ -*-

/// JTRE (JTMS-based Tiny Rule Engine) - Implementation
/// Converted from jinter.lisp, jdata.lisp, jrules.lisp

#include "jtre.h"

// ================================================================
// Global JTRE pointer
// ================================================================
JtrePtr global_jtre = nullptr;

// ================================================================
// Utility
// ================================================================

std::string extract_dbclass_name(const SExpr& fact) {
    std::string fact_str;
    try { fact_str = std::any_cast<std::string>(fact); }
    catch (...) { return "?"; }

    if (!fact_str.empty() && fact_str.front() == '(') {
        auto pos = fact_str.find_first_of(" )");
        if (pos != std::string::npos) {
            return fact_str.substr(1, pos - 1);
        }
    }
    return fact_str;
}

// ================================================================
// JTRE interface (from jinter.lisp)
// ================================================================

static std::string jtre_view_node(NodePtr node) {
    try {
        auto datum = std::any_cast<DatumPtr>(node->datum);
        return show_datum(datum);
    } catch (...) {
        return default_node_string(node);
    }
}

JtrePtr create_jtre(const std::string& title, bool debugging) {
    auto jtre = std::make_shared<Jtre>();
    jtre->title = title;
    jtre->debugging = debugging;

    jtre->jtms = create_jtms(
        "JTMS-OF " + title,
        [](NodePtr n) -> std::string { return jtre_view_node(n); },
        false, true, nullptr, nullptr
    );

    // Set enqueue procedure
    change_jtms(jtre->jtms, nullptr,
        [jtre](std::any rule) { enqueue(rule, jtre); });

    in_jtre(jtre);
    return jtre;
}

void change_jtre(JtrePtr jtre, std::optional<bool> debugging) {
    if (debugging.has_value()) jtre->debugging = debugging.value();
}

void in_jtre(JtrePtr jtre) {
    global_jtre = jtre;
}

void uassert(const SExpr& fact, const SExpr& just) {
    std::string just_str;
    try { just_str = std::any_cast<std::string>(just); } catch (...) {}
    assert_fact(fact, {just}, global_jtre);
    run_rules(global_jtre);
}

void uassume(const SExpr& fact, const std::string& reason) {
    assume_fact(fact, reason, global_jtre);
    run_rules(global_jtre);
}

void show(JtrePtr jtre, std::ostream& stream) {
    if (!jtre) jtre = global_jtre;
    show_data(jtre, stream);
    show_rules(jtre, stream);
}

// ================================================================
// Database (from jdata.lisp)
// ================================================================

DatumPtr assert_fact(const SExpr& fact, const std::vector<SExpr>& just,
    JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;

    auto datum = referent(fact, true, jtre);
    auto node = datum->tms_node;

    if (jtre->debugging) {
        std::string fact_str;
        try { fact_str = std::any_cast<std::string>(fact); } catch (...) {}
        std::cerr << "\n    Asserting " << fact_str << " via [";
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
    catch (...) { informant = "user"; }

    std::vector<NodePtr> antecedents;
    for (size_t i = 1; i < just.size(); i++) {
        auto r = referent(just[i], true, jtre);
        antecedents.push_back(r->tms_node);
    }

    justify_node(informant, node, antecedents);
    return datum;
}

DatumPtr quiet_assert(const SExpr& fact, const std::vector<SExpr>& just,
    JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    auto old_checking = jtre->jtms->checking_contradictions;
    jtre->jtms->checking_contradictions = false;
    auto result = assert_fact(fact, just, jtre);
    jtre->jtms->checking_contradictions = old_checking;
    return result;
}

DatumPtr assume_fact(const SExpr& fact, const std::string& reason,
    JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;

    auto datum = referent(fact, true, jtre);
    auto node = datum->tms_node;

    if (datum->assumption_reason.empty()) {
        datum->assumption_reason = reason;
        if (jtre->debugging) {
            std::cerr << "\n    Assuming " << show_datum(datum)
                      << " via " << reason << ".";
        }
        assume_node(node);
        enable_assumption(node);
    } else if (datum->assumption_reason != reason) {
        throw std::runtime_error(
            "Fact " + show_datum(datum) + " assumed because of " +
            datum->assumption_reason + " assumed again because of " + reason);
    }
    return datum;
}

bool already_assumed(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    auto datum = referent(fact, true, jtre);
    return !datum->assumption_reason.empty();
}

NodePtr retract_fact(const SExpr& fact, const std::string& just,
    bool quiet, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;

    auto datum = referent(fact, true, jtre);
    auto node = datum->tms_node;

    if (!node->assumption) {
        if (!quiet) {
            std::cout << "\n" << show_datum(datum) << " isn't an assumption.";
        }
    } else if (!in_node(node)) {
        if (!quiet) {
            std::string fact_str;
            try { fact_str = std::any_cast<std::string>(fact); } catch (...) {}
            std::cout << "\nThe assumption " << fact_str
                      << " is not currently in.";
        }
    } else if (just == datum->assumption_reason) {
        if (jtre->debugging) {
            std::string fact_str;
            try { fact_str = std::any_cast<std::string>(fact); } catch (...) {}
            std::cerr << "\n    Retracting " << fact_str << " via " << just << ".";
        }
        datum->assumption_reason.clear();
        retract_assumption(node);
    } else if (!quiet) {
        std::string fact_str;
        try { fact_str = std::any_cast<std::string>(fact); } catch (...) {}
        std::cout << "\n" << just << " not source of assumption for "
                  << fact_str;
    }
    return node;
}

void contradiction(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    auto datum = referent(fact, true, jtre);
    make_contradiction(datum->tms_node);
}

bool in_fact(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    auto r = referent(fact, false, jtre);
    if (!r) return false;
    return in_node(r->tms_node);
}

bool out_fact(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    auto r = referent(fact, false, jtre);
    if (!r) return false;
    return out_node(r->tms_node);
}

void why_fact(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    auto r = referent(fact, false, jtre);
    if (r) why_node(r->tms_node);
}

std::vector<SExpr> assumptions_of(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    auto datum = referent(fact, true, jtre);
    auto nodes = assumptions_of_node(datum->tms_node);
    std::vector<SExpr> result;
    for (auto& n : nodes) {
        result.push_back(view_node(n));
    }
    return result;
}

std::vector<SExpr> fetch(const SExpr& pattern, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;

    std::string dbname = extract_dbclass_name(pattern);
    auto it = jtre->dbclass_table.find(dbname);
    if (it == jtre->dbclass_table.end()) return {};

    std::vector<SExpr> result;
    for (auto& datum : it->second->facts) {
        result.push_back(datum->lisp_form);
    }
    return result;
}

DbclassPtr get_dbclass(const std::string& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    if (fact.empty()) {
        throw std::runtime_error("Empty string can't be a dbclass.");
    }

    auto it = jtre->dbclass_table.find(fact);
    if (it != jtre->dbclass_table.end()) return it->second;

    auto dbclass = std::make_shared<Dbclass>();
    dbclass->name = fact;
    dbclass->jtre = jtre;
    jtre->dbclass_table[fact] = dbclass;
    return dbclass;
}

DatumPtr referent(const SExpr& fact, bool virtual_mode, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    if (virtual_mode) return insert_datum(fact, jtre);
    return referent1(fact, jtre);
}

DatumPtr referent1(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    std::string fact_str;
    try { fact_str = std::any_cast<std::string>(fact); }
    catch (...) { return nullptr; }

    std::string dbname = extract_dbclass_name(fact);
    auto it = jtre->dbclass_table.find(dbname);
    if (it == jtre->dbclass_table.end()) return nullptr;

    for (auto& datum : it->second->facts) {
        std::string df;
        try { df = std::any_cast<std::string>(datum->lisp_form); }
        catch (...) { continue; }
        if (df == fact_str) return datum;
    }
    return nullptr;
}

DatumPtr insert_datum(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;

    auto existing = referent1(fact, jtre);
    if (existing) return existing;

    std::string dbname = extract_dbclass_name(fact);
    auto datum = std::make_shared<Datum>();
    datum->id = ++jtre->datum_counter;
    datum->lisp_form = fact;
    datum->dbclass = get_dbclass(dbname, jtre);
    datum->tms_node = tms_create_node(jtre->jtms, SExpr(datum));
    datum->dbclass->facts.push_back(datum);

    try_rules(datum);
    return datum;
}

NodePtr get_tms_node(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    auto datum = referent(fact, true, jtre);
    return datum->tms_node;
}

SExpr view_node(NodePtr node) {
    try {
        auto datum = std::any_cast<DatumPtr>(node->datum);
        return datum->lisp_form;
    } catch (...) {
        return node->datum;
    }
}

std::string show_datum(DatumPtr datum) {
    try { return std::any_cast<std::string>(datum->lisp_form); }
    catch (...) { return "?"; }
}

int show_data(JtrePtr jtre, std::ostream& stream) {
    if (!jtre) jtre = global_jtre;
    int counter = 0;
    stream << "\n" << jtre->datum_counter << " facts total.";
    for (auto& [name, dbclass] : jtre->dbclass_table) {
        for (auto& datum : dbclass->facts) {
            counter++;
            stream << "\n" << show_datum(datum) << ": "
                   << (in_node(datum->tms_node) ? "IN" : "OUT");
        }
    }
    return counter;
}

DatumPtr get_datum(int num, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    for (auto& [name, dbclass] : jtre->dbclass_table) {
        for (auto& datum : dbclass->facts) {
            if (datum->id == num) return datum;
        }
    }
    return nullptr;
}

JustPtr get_just(int num, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    for (auto& just : jtre->jtms->justs) {
        if (just->index == num) return just;
    }
    return nullptr;
}

void wfs(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    if (out_fact(fact, jtre)) {
        std::string fact_str;
        try { fact_str = std::any_cast<std::string>(fact); } catch (...) {}
        std::cout << "\n " << fact_str << " is OUT.";
        return;
    }

    auto node = get_tms_node(fact, jtre);
    std::vector<NodePtr> queue = {node};
    std::vector<NodePtr> so_far = {node};

    while (!queue.empty()) {
        auto current = queue.front();
        queue.erase(queue.begin());
        why_node(current);
        if (!out_node(current) && !current->assumption &&
            current->support_just) {
            for (auto& ante : current->support_just->antecedents) {
                if (std::find(so_far.begin(), so_far.end(), ante) == so_far.end()) {
                    so_far.push_back(ante);
                    queue.push_back(ante);
                }
            }
        }
    }
    std::cout << "\n--------";
}

void show_justifications(const SExpr& fact, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    std::string fact_str;
    try { fact_str = std::any_cast<std::string>(fact); } catch (...) {}
    std::cout << "\n " << fact_str << "::";

    auto node = get_tms_node(fact, jtre);
    if (node->justs.empty()) {
        std::cout << " No justifications.";
        return;
    }
    for (auto& j : node->justs) {
        std::cout << "\n " << j->informant;
        if (!j->antecedents.empty()) {
            std::cout << ", on:";
            for (auto& ante : j->antecedents) {
                auto v = view_node(ante);
                std::string vs;
                try { vs = std::any_cast<std::string>(v); } catch (...) { vs = "?"; }
                std::cout << "\n  " << vs << ": "
                          << (in_node(ante) ? "IN" : "OUT");
            }
        }
        std::cout << ".";
    }
}

// ================================================================
// Rules system (from jrules.lisp)
// ================================================================

void insert_rule(DbclassPtr dbclass,
    std::function<std::tuple<bool, std::vector<std::any>, bool>(const SExpr&)> matcher,
    std::function<void(const std::vector<std::any>&)> body) {

    auto jtre = dbclass->jtre.lock();
    if (!jtre) return;

    auto rule = std::make_shared<Rule>();
    rule->matcher = matcher;
    rule->body = body;
    rule->dbclass = dbclass;
    rule->id = ++jtre->rule_counter;
    rule->jtre = jtre;

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
    auto jtre = datum->dbclass->jtre.lock();
    if (!jtre) return;

    auto [okay, bindings, needs_node] = rule->matcher(datum->lisp_form);
    if (!okay) return;

    if (needs_node) {
        bindings.insert(bindings.begin(), SExpr(datum->tms_node));
    }

    // Queue: pair of (body, bindings)
    struct QueuedRule {
        std::function<void(const std::vector<std::any>&)> body;
        std::vector<std::any> bindings;
    };
    auto qr = std::make_shared<QueuedRule>();
    qr->body = rule->body;
    qr->bindings = bindings;
    enqueue(std::any(qr), jtre);
}

int run_rules(JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;

    int counter = 0;
    while (!jtre->queue.empty()) {
        auto form = jtre->queue.front();
        jtre->queue.erase(jtre->queue.begin());

        struct QueuedRule {
            std::function<void(const std::vector<std::any>&)> body;
            std::vector<std::any> bindings;
        };
        try {
            auto qr = std::any_cast<std::shared_ptr<QueuedRule>>(form);
            qr->body(qr->bindings);
        } catch (...) {}
        counter++;
    }

    if (jtre->debugging) {
        std::cerr << "\n    " << counter << " rules run.";
    }
    jtre->rules_run += counter;
    return counter;
}

bool rules_waiting(JtrePtr jtre) {
    return !jtre->queue.empty();
}

void enqueue(std::any item, JtrePtr jtre) {
    jtre->queue.push_back(item);
}

std::any dequeue(JtrePtr jtre) {
    if (jtre->queue.empty()) return {};
    auto front = jtre->queue.front();
    jtre->queue.erase(jtre->queue.begin());
    return front;
}

void show_rules(JtrePtr jtre, std::ostream& stream) {
    if (!jtre) jtre = global_jtre;
    stream << "\nThere are " << jtre->rule_counter
           << " rules in " << jtre->title << ":";
    stream << "\n " << (jtre->queue.empty() ? "None" :
        std::to_string(jtre->queue.size())) << " queued.";
    for (auto& [name, dbclass] : jtre->dbclass_table) {
        for (auto& rule : dbclass->rules) {
            stream << "\n <Rule " << rule->id << "> for " << name;
        }
    }
}

RulePtr get_rule(int num, JtrePtr jtre) {
    if (!jtre) jtre = global_jtre;
    for (auto& [name, dbclass] : jtre->dbclass_table) {
        for (auto& rule : dbclass->rules) {
            if (rule->id == num) return rule;
        }
    }
    return nullptr;
}
