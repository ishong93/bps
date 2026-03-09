#pragma once

/// Closed-World Assumptions
/// Converted from cwa.lisp
/// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ldata.h"
#include "linter.h"
#include <string>
#include <vector>
#include <tuple>

// ---------------------------------------------------------------------------
// Set member queries
// ---------------------------------------------------------------------------

struct SetMembersResult {
    std::vector<std::string> members;
    std::string cwa_form;
    bool found = false;
};

SetMembersResult set_members(const std::string& set_name, LTRE* ltre = nullptr);

struct CloseSetResult {
    std::vector<std::string> members;
    std::string cwa_form;
    bool newly_closed = false;
};

CloseSetResult close_set_if_needed(const std::string& set_name, LTRE* ltre = nullptr);
CloseSetResult close_set(const std::string& set_name, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// CWA form helpers
// ---------------------------------------------------------------------------

bool is_cwa_form(const std::string& form);
std::string make_cwa_form(const std::string& set_name,
                          const std::vector<std::string>& members);
std::pair<std::string, std::vector<std::string>> parse_cwa_form(const std::string& cwa_form);

// ---------------------------------------------------------------------------
// Set information
// ---------------------------------------------------------------------------

struct SetInfo {
    std::vector<std::string> known_members;
    std::vector<std::string> known_not;
};

SetInfo get_set_information(const std::string& set_name, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// CWA management
// ---------------------------------------------------------------------------

void assume_cwa_if_needed(const std::string& cwa_form, LTRE* ltre = nullptr);
void retract_cwa(const std::string& cwa, LTRE* ltre = nullptr);
void retract_cwas(const std::string& set_name, LTRE* ltre = nullptr);
bool cwa_invalid(const std::string& cwa, LTRE* ltre = nullptr);

// ---------------------------------------------------------------------------
// Contradiction handling for CWA
// ---------------------------------------------------------------------------

void set_cwa_handler(std::vector<Clause*>& clauses, LTMS* ltms,
                     const std::string& set_name, const std::string& cwa);

// ---------------------------------------------------------------------------
// Test functions
// ---------------------------------------------------------------------------

void cwa_shakedown();
