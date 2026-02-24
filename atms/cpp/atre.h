// -*- C++ -*-

/// ATRE (ATMS-based Tiny Rule Engine) - Definitions and Interface
/// Converted from ainter.lisp, adata.lisp, arules.lisp

#ifndef ATRE_H
#define ATRE_H

#include "atms.h"
#include <unordered_map>
#include <tuple>

// Forward declarations
struct Atre;
struct Dbclass;
struct Datum;
struct Rule;

using AtrePtr = std::shared_ptr<Atre>;
using DbclassPtr = std::shared_ptr<Dbclass>;
using DatumPtr = std::shared_ptr<Datum>;
using RulePtr = std::shared_ptr<Rule>;

/// A Lisp-like S-expression represented as a variant
using SExpr = std::any;

/// ATRE: ATMS-based Tiny Rule Engine
struct Atre : std::enable_shared_from_this<Atre> {
    std::string title;
    AtmsPtr atms;
    std::vector<DbclassPtr> dbclasses;
    std::unordered_map<std::string, DbclassPtr> dbclass_table;
    int datum_counter = 0;
    std::vector<RulePtr> rules;
    int rule_counter = 0;
    bool debugging = false;
    std::vector<std::any> queue;
    int rules_run = 0;
    std::vector<std::any> in_rules;
    EnvPtr focus;
    std::vector<std::any> contradiction_rules;
    std::vector<std::any> imp_rules;

    std::string to_string() const {
        return "<ATRE: " + title + ">";
    }
};

/// Dbclass: Groups facts and rules under a common symbol.
struct Dbclass {
    std::string name;
    std::weak_ptr<Atre> atre;
    std::vector<DatumPtr> facts;
    std::vector<RulePtr> rules;

    std::string to_string() const {
        return "<Dbclass " + name + ">";
    }
};

/// Datum: A single fact/assertion in the database.
struct Datum {
    int counter = 0;
    std::weak_ptr<Atre> atre;
    SExpr lisp_form;
    NodePtr tms_node;
    DbclassPtr dbclass;
    std::string assumption_reason; // empty if not assumed
    std::unordered_map<std::string, std::any> plist;

    std::string to_string() const {
        return "<Datum " + std::to_string(counter) + ">";
    }
};

/// Rule: A pattern-matching rule in the ATRE system.
struct Rule {
    int counter = 0;
    std::weak_ptr<Atre> atre;
    DbclassPtr dbclass;
    // matcher: takes a lisp_form, returns (ok, bindings, condition)
    std::function<std::tuple<bool, std::vector<std::any>, std::string>(const SExpr&)> matcher;
    // body: takes arguments list
    std::function<void(const std::vector<std::any>&)> body;
    std::vector<NodePtr> in_nodes;
    std::vector<NodePtr> imp_nodes;

    std::string to_string() const {
        return "<Rule " + std::to_string(counter) + ">";
    }
};

// ================================================================
// Global ATRE pointer
// ================================================================
extern AtrePtr global_atre;

// ================================================================
// ATRE interface (from ainter.lisp)
// ================================================================

AtrePtr create_atre(const std::string& title, bool debugging = false);
void change_atre(AtrePtr atre, std::optional<bool> debugging = std::nullopt);
void in_atre(AtrePtr atre);

void change_focus(EnvPtr env, AtrePtr atre = nullptr);
bool focus_okay(AtrePtr atre);

void contradiction_rule(EnvPtr env,
    std::function<void(EnvPtr)> proc, AtrePtr atre);

void show(AtrePtr atre = nullptr, std::ostream& stream = std::cout);
std::vector<EnvPtr> solutions(AtrePtr atre,
    const std::vector<std::vector<SExpr>>& choice_sets);

int run_rules(AtrePtr atre = nullptr);

// ================================================================
// ATRE database (from adata.lisp)
// ================================================================

DatumPtr assert_fact(const SExpr& fact, const std::vector<SExpr>& just,
                     AtrePtr atre = nullptr);
DatumPtr assume_fact(const SExpr& fact, const std::string& reason,
                     AtrePtr atre = nullptr);
bool already_assumed(const SExpr& fact, AtrePtr atre = nullptr);
void assume_if_needed(const SExpr& fact, const std::string& reason,
                      AtrePtr atre = nullptr);
void contradiction(const SExpr& fact, AtrePtr atre = nullptr);

DbclassPtr get_dbclass(const std::string& fact, AtrePtr atre = nullptr);
DatumPtr referent(const SExpr& fact, bool virtual_mode = false,
                  AtrePtr atre = nullptr);
DatumPtr referent1(const SExpr& fact, AtrePtr atre = nullptr);
DatumPtr insert_datum(const SExpr& fact, AtrePtr atre = nullptr);
std::vector<SExpr> fetch(const SExpr& pattern, AtrePtr atre = nullptr);

bool true_fact(const SExpr& fact, AtrePtr atre = nullptr);
bool in_fact(const SExpr& fact, EnvPtr env, AtrePtr atre = nullptr);
bool out_fact(const SExpr& fact, EnvPtr env, AtrePtr atre = nullptr);
bool consistent_with_fact(const SExpr& fact, EnvPtr env,
                          AtrePtr atre = nullptr);

NodePtr get_tms_node(const SExpr& fact, AtrePtr atre = nullptr);
SExpr view_node(NodePtr node);
std::string stringify_node(NodePtr node);
std::vector<EnvPtr> assumptions_of(const SExpr& fact, AtrePtr atre = nullptr);
EnvPtr environment_of(const std::vector<SExpr>& facts,
                      AtrePtr atre = nullptr);
EnvPtr environment_cons(const SExpr& fact, EnvPtr env,
                        AtrePtr atre = nullptr);

std::string show_datum(DatumPtr datum);
int show_data(AtrePtr atre = nullptr, std::ostream& stream = std::cout);
int show_context(EnvPtr env, AtrePtr atre = nullptr,
                 std::ostream& stream = std::cout);
int show_dbclasses(AtrePtr atre = nullptr,
                   std::ostream& stream = std::cout);

DatumPtr get_datum(int num, AtrePtr atre = nullptr);
JustPtr get_just(int num, AtrePtr atre = nullptr);

// ================================================================
// Rules system (from arules.lisp)
// ================================================================

void insert_rule(DbclassPtr dbclass,
    std::function<std::tuple<bool, std::vector<std::any>, std::string>(const SExpr&)> matcher,
    std::function<void(const std::vector<std::any>&)> body,
    const std::vector<NodePtr>& in_nodes = {},
    const std::vector<NodePtr>& imp_nodes = {});

void try_rules(DatumPtr datum);
void try_rule_on(RulePtr rule, DatumPtr datum);
void execute_rule(const std::any& queued_rule, AtrePtr atre);

bool in_triggers_ready(const std::vector<NodePtr>& nodes, AtrePtr atre,
                       EnvPtr env = nullptr);
bool implied_by_triggers_ready(const std::vector<NodePtr>& nodes, AtrePtr atre);

void enqueue(std::any item, AtrePtr atre);
std::any dequeue(AtrePtr atre);

void show_rules(AtrePtr atre = nullptr,
                std::ostream& stream = std::cout);

#endif // ATRE_H
