#pragma once

/// LTRE Database - Definitions
/// Converted from ldata.lisp
/// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ltms.h"
#include <unordered_map>
#include <optional>

// Forward declarations
struct LTRE;
struct DbClass;
struct Datum;
struct LRule;

// ---------------------------------------------------------------------------
// DbClass
// ---------------------------------------------------------------------------

struct DbClass {
    std::string name;
    LTRE* ltre = nullptr;
    std::vector<Datum*> facts;
    std::vector<LRule*> rules;
};

// ---------------------------------------------------------------------------
// Datum
// ---------------------------------------------------------------------------

struct Datum {
    int counter = 0;
    LTRE* ltre = nullptr;
    std::string lisp_form;          // The textual representation of the fact
    TmsNode* tms_node = nullptr;
    DbClass* dbclass = nullptr;
    std::string assumption_reason;  // Empty if not assumed; otherwise the reason string
    std::unordered_map<std::string, std::any> plist;
};

// ---------------------------------------------------------------------------
// Connective list
// ---------------------------------------------------------------------------

bool is_connective(const std::string& s);

// ---------------------------------------------------------------------------
// Proposition classification
// ---------------------------------------------------------------------------

bool simple_proposition(const std::string& x);
bool negated_proposition(const std::string& form);
std::string strip_negation(const std::string& form);

// ---------------------------------------------------------------------------
// Assert / Assume
// ---------------------------------------------------------------------------

void ltre_assert(const std::any& fact, const std::any& just,
                 LTRE* ltre = nullptr);
Datum* ltre_assume(const std::any& fact, const std::string& reason,
                   LTRE* ltre = nullptr);
bool already_assumed(const std::any& fact, LTRE* ltre = nullptr);
void quiet_assert(const std::any& fact, LTRE* ltre = nullptr,
                  const std::any& just = std::any(std::string("user")));

// ---------------------------------------------------------------------------
// Formula building
// ---------------------------------------------------------------------------

std::any build_tms_formula(const std::any& formula, LTRE* ltre);

// ---------------------------------------------------------------------------
// Retraction
// ---------------------------------------------------------------------------

TmsNode* ltre_retract(const std::any& fact, const std::string& just,
                      LTRE* ltre = nullptr, bool quiet = true);

// ---------------------------------------------------------------------------
// Contradiction
// ---------------------------------------------------------------------------

void ltre_contradiction(const std::vector<std::any>& losers, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// DbClass management
// ---------------------------------------------------------------------------

DbClass* get_dbclass(const std::any& fact, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// Referent / Insert / Fetch
// ---------------------------------------------------------------------------

Datum* referent(const std::any& fact, bool virtual_mode = false,
                LTRE* ltre = nullptr);
Datum* referent1(const std::any& fact, LTRE* ltre = nullptr);
Datum* insert(const std::any& fact, LTRE* ltre = nullptr);

std::vector<std::string> fetch(const std::string& pattern, LTRE* ltre = nullptr);
std::vector<Datum*> get_candidates(const std::string& pattern, LTRE* ltre = nullptr);

void map_dbclass(std::function<void(DbClass*)> proc, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// Truth queries
// ---------------------------------------------------------------------------

bool is_true(const std::any& fact, LTRE* ltre = nullptr);
bool is_false(const std::any& fact, LTRE* ltre = nullptr);
bool is_known(const std::any& fact, LTRE* ltre = nullptr);
bool is_unknown(const std::any& fact, LTRE* ltre = nullptr);
std::string label_of(const std::any& fact, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// Why / TMS node access
// ---------------------------------------------------------------------------

void why(const std::any& fact, LTRE* ltre = nullptr);
TmsNode* get_tms_node(const std::any& fact, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// View / Display
// ---------------------------------------------------------------------------

std::string view_node_form(TmsNode* node);
std::string signed_view_node(TmsNode* node);
std::string show_datum(Datum* datum);
std::string make_node_string(TmsNode* node);

// ---------------------------------------------------------------------------
// Assumptions / Consequences
// ---------------------------------------------------------------------------

std::vector<std::string> ltre_assumptions_of(const std::any& fact,
                                              LTRE* ltre = nullptr);
void consequences(const std::any& fact, LTRE* ltre = nullptr);
void explore(const std::any& fact, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// Data display / lookup
// ---------------------------------------------------------------------------

int show_data(LTRE* ltre = nullptr, std::ostream& stream = std::cout);
Datum* get_datum(int num, LTRE* ltre = nullptr);
Clause* get_clause(int num, LTRE* ltre = nullptr);

std::vector<std::string> fetch_global(const std::string& pattern,
                                       const std::string& status = "",
                                       LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// Utility: variable? check and simple unify (forward declared from funify)
// ---------------------------------------------------------------------------

bool is_variable(const std::string& s);
bool unify_match(const std::string& pattern, const std::string& candidate);
