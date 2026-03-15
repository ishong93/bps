#pragma once

/// LTRE (LTMS-based Tiny Rule Engine) - Core definitions
/// Converted from linter.lisp
/// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ltms.h"
#include <unordered_map>
#include <functional>
#include <optional>

// Forward declarations
struct LTRE;
struct DbClass;
struct Datum;
struct LRule;

// ---------------------------------------------------------------------------
// LTRE struct
// ---------------------------------------------------------------------------

struct LTRE {
    std::string title;
    LTMS* ltms = nullptr;
    std::unordered_map<std::string, DbClass*> dbclass_table;
    int datum_counter = 0;
    int rule_counter = 0;
    bool debugging = false;
    std::vector<std::any> queue;
    int rules_run = 0;
};

// ---------------------------------------------------------------------------
// Global LTRE pointer
// ---------------------------------------------------------------------------

extern LTRE* current_ltre;

// ---------------------------------------------------------------------------
// Print helpers
// ---------------------------------------------------------------------------

std::string print_ltre(LTRE* ltre);

// ---------------------------------------------------------------------------
// LTRE management
// ---------------------------------------------------------------------------

LTRE* create_ltre(const std::string& title, bool debugging = false);
void change_ltre(LTRE* ltre, std::optional<bool> debugging = std::nullopt);
void in_ltre(LTRE* ltre);

// ---------------------------------------------------------------------------
// Assert / Assume interface
// ---------------------------------------------------------------------------

void uassert(const std::any& fact, const std::any& just = std::any(std::string("user")),
             LTRE* ltre = nullptr);
void uassume(const std::any& fact, const std::string& reason, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// Execution
// ---------------------------------------------------------------------------

void run_forms(const std::vector<std::function<void()>>& forms, LTRE* ltre = nullptr);
void run(LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// Display
// ---------------------------------------------------------------------------

void show(LTRE* ltre = nullptr, std::ostream& stream = std::cout);
int show_by_informant(const std::any& informant, LTRE* ltre = nullptr);
std::string view_clause(Clause* cl);

// ---------------------------------------------------------------------------
// Queue / rule running (forward declared, defined in ldata or rules)
// ---------------------------------------------------------------------------

void enqueue(const std::any& pair, LTRE* ltre);
int run_rules(LTRE* ltre = nullptr);
void try_rules(Datum* datum);

// ---------------------------------------------------------------------------
// Data display (forward declared, defined in ldata)
// ---------------------------------------------------------------------------

int show_data(LTRE* ltre = nullptr, std::ostream& stream = std::cout);
void show_rules(LTRE* ltre = nullptr, std::ostream& stream = std::cout);
