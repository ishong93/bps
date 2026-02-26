// -*- C++ -*-

/// JTRE (JTMS-based Tiny Rule Engine) - Definitions
/// Converted from jinter.lisp, jdata.lisp, jrules.lisp

#ifndef JTRE_H
#define JTRE_H

#include "jtms.h"
#include <unordered_map>
#include <tuple>

// Forward declarations
struct Jtre;
struct Dbclass;
struct Datum;
struct Rule;

using JtrePtr = std::shared_ptr<Jtre>;
using DbclassPtr = std::shared_ptr<Dbclass>;
using DatumPtr = std::shared_ptr<Datum>;
using RulePtr = std::shared_ptr<Rule>;

using SExpr = std::any;

/// JTRE: JTMS-based Tiny Rule Engine
struct Jtre : std::enable_shared_from_this<Jtre> {
    std::string title;
    JtmsPtr jtms;
    std::unordered_map<std::string, DbclassPtr> dbclass_table;
    int datum_counter = 0;
    int rule_counter = 0;
    bool debugging = false;
    std::vector<std::any> queue;
    int rules_run = 0;

    std::string to_string() const {
        return "<JTRE: " + title + ">";
    }
};

/// Dbclass: Groups facts and rules under a common symbol.
struct Dbclass {
    std::string name;
    std::weak_ptr<Jtre> jtre;
    std::vector<DatumPtr> facts;
    std::vector<RulePtr> rules;
};

/// Datum: A single fact/assertion in the database.
struct Datum {
    int id = 0;
    SExpr lisp_form;
    NodePtr tms_node;
    DbclassPtr dbclass;
    std::string assumption_reason; // empty if not assumed
    std::unordered_map<std::string, std::any> plist;
};

/// Rule: A pattern-matching rule in the JTRE system.
struct Rule {
    int id = 0;
    std::weak_ptr<Jtre> jtre;
    DbclassPtr dbclass;
    std::function<std::tuple<bool, std::vector<std::any>, bool>(const SExpr&)> matcher;
    std::function<void(const std::vector<std::any>&)> body;
};

// ================================================================
// Global JTRE pointer
// ================================================================
extern JtrePtr global_jtre;

// ================================================================
// JTRE interface (from jinter.lisp)
// ================================================================

JtrePtr create_jtre(const std::string& title, bool debugging = false);
void change_jtre(JtrePtr jtre, std::optional<bool> debugging = std::nullopt);
void in_jtre(JtrePtr jtre);

void uassert(const SExpr& fact, const SExpr& just = SExpr(std::string("user")));
void uassume(const SExpr& fact, const std::string& reason);
void show(JtrePtr jtre = nullptr, std::ostream& stream = std::cout);

// ================================================================
// Database (from jdata.lisp)
// ================================================================

DatumPtr assert_fact(const SExpr& fact, const std::vector<SExpr>& just,
    JtrePtr jtre = nullptr);
DatumPtr quiet_assert(const SExpr& fact, const std::vector<SExpr>& just,
    JtrePtr jtre = nullptr);
DatumPtr assume_fact(const SExpr& fact, const std::string& reason,
    JtrePtr jtre = nullptr);
bool already_assumed(const SExpr& fact, JtrePtr jtre = nullptr);
NodePtr retract_fact(const SExpr& fact, const std::string& just = "user",
    bool quiet = false, JtrePtr jtre = nullptr);
void contradiction(const SExpr& fact, JtrePtr jtre = nullptr);

bool in_fact(const SExpr& fact, JtrePtr jtre = nullptr);
bool out_fact(const SExpr& fact, JtrePtr jtre = nullptr);
void why_fact(const SExpr& fact, JtrePtr jtre = nullptr);
std::vector<SExpr> assumptions_of(const SExpr& fact, JtrePtr jtre = nullptr);

std::vector<SExpr> fetch(const SExpr& pattern, JtrePtr jtre = nullptr);
DbclassPtr get_dbclass(const std::string& fact, JtrePtr jtre = nullptr);
DatumPtr referent(const SExpr& fact, bool virtual_mode = false,
    JtrePtr jtre = nullptr);
DatumPtr referent1(const SExpr& fact, JtrePtr jtre = nullptr);
DatumPtr insert_datum(const SExpr& fact, JtrePtr jtre = nullptr);

NodePtr get_tms_node(const SExpr& fact, JtrePtr jtre = nullptr);
SExpr view_node(NodePtr node);
std::string show_datum(DatumPtr datum);

int show_data(JtrePtr jtre = nullptr, std::ostream& stream = std::cout);
DatumPtr get_datum(int num, JtrePtr jtre = nullptr);
JustPtr get_just(int num, JtrePtr jtre = nullptr);

void wfs(const SExpr& fact, JtrePtr jtre = nullptr);
void show_justifications(const SExpr& fact, JtrePtr jtre = nullptr);

// ================================================================
// Rules system (from jrules.lisp)
// ================================================================

void insert_rule(DbclassPtr dbclass,
    std::function<std::tuple<bool, std::vector<std::any>, bool>(const SExpr&)> matcher,
    std::function<void(const std::vector<std::any>&)> body);

void try_rules(DatumPtr datum);
void try_rule_on(RulePtr rule, DatumPtr datum);

int run_rules(JtrePtr jtre = nullptr);
bool rules_waiting(JtrePtr jtre);

void enqueue(std::any item, JtrePtr jtre);
std::any dequeue(JtrePtr jtre);

void show_rules(JtrePtr jtre = nullptr, std::ostream& stream = std::cout);
RulePtr get_rule(int num, JtrePtr jtre = nullptr);

// ================================================================
// Utility for extracting dbclass name from a fact
// ================================================================
std::string extract_dbclass_name(const SExpr& fact);

#endif // JTRE_H
