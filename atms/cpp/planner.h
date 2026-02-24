// -*- C++ -*-

/// ATMS-based Planner
/// Converted from aplanr.lisp, plan-a.lisp, plan-e.lisp, bcode.lisp, blocks.lisp

#ifndef PLANNER_H
#define PLANNER_H

#include "atre.h"
#include <deque>

// Forward declarations
struct Plnpr;
struct Operator;

using PlnprPtr = std::shared_ptr<Plnpr>;
using OperatorPtr = std::shared_ptr<Operator>;

/// PlnPr: Planning Problem structure
struct Plnpr : std::enable_shared_from_this<Plnpr> {
    std::string title;
    AtrePtr atre;
    std::vector<std::vector<SExpr>> basis_set; // choice sets defining states
    std::map<std::string, OperatorPtr> operators;
    std::map<std::string, std::any> plist; // cache for intermediate results
    bool debugging = false;

    std::string to_string() const {
        return "<PlnPr: " + title + ">";
    }
};

/// Operator: A planning operator (Pickup, Putdown, Stack, Unstack)
struct Operator {
    std::vector<std::string> form;       // e.g., {"Pickup", "?x"}
    std::vector<SExpr> preconditions;
    std::vector<SExpr> add_list;
    std::vector<SExpr> delete_list;
    std::function<bool(const std::map<std::string, std::string>&)> test;

    std::string to_string() const {
        std::string s = "<Operator";
        for (auto& f : form) s += " " + f;
        s += ">";
        return s;
    }
};

// ================================================================
// Global planner pointer
// ================================================================
extern PlnprPtr global_plnpr;

// ================================================================
// Planner utilities (from aplanr.lisp)
// ================================================================

PlnprPtr create_planning_problem(const std::string& title,
    const std::vector<std::vector<SExpr>>& basis_set);

void in_plnpr(PlnprPtr plnpr);
void setup_choice_sets(PlnprPtr plnpr = nullptr);
void set_debug_plnpr(bool state, PlnprPtr plnpr = nullptr);

void register_operator(const std::string& name,
    const std::vector<std::string>& form,
    const std::vector<SExpr>& preconditions,
    const std::vector<SExpr>& add_list,
    const std::vector<SExpr>& delete_list,
    std::function<bool(const std::map<std::string, std::string>&)> test = nullptr,
    PlnprPtr plnpr = nullptr);

OperatorPtr fetch_operator(const std::string& op_name,
    PlnprPtr plnpr = nullptr);

std::vector<std::vector<std::string>> find_applicable_operators(
    EnvPtr state, PlnprPtr plnpr = nullptr);

EnvPtr apply_operator(EnvPtr state,
    const std::vector<std::string>& op_inst,
    PlnprPtr plnpr = nullptr);

std::vector<EnvPtr> fetch_states(const std::vector<SExpr>& facts,
    PlnprPtr plnpr = nullptr);

struct GoalResult {
    bool success;
    std::map<std::string, std::string> bindings;
};

GoalResult satisfies_goal(EnvPtr state, const std::vector<SExpr>& goals,
    PlnprPtr plnpr = nullptr);

void show_plan(const std::vector<std::any>& plan);

// ================================================================
// Envisioner (from plan-e.lisp)
// ================================================================

struct EnvisionmentResult {
    std::vector<EnvPtr> states;
    // transitions: for each state, list of (operator-inst, resulting-state) pairs
    std::vector<std::pair<EnvPtr,
        std::vector<std::pair<std::vector<std::string>, EnvPtr>>>> transitions;
};

EnvisionmentResult envision(PlnprPtr plnpr = nullptr);
void show_envisionment(PlnprPtr plnpr = nullptr,
                       std::ostream& stream = std::cout);

/// Find a plan by searching the envisionment (BFS)
struct PlanResult {
    bool found;
    std::vector<std::any> path; // alternating: state, operator, state, operator, ...
};

PlanResult find_plan(const std::vector<SExpr>& start,
    const std::vector<SExpr>& goals,
    PlnprPtr plnpr = nullptr);

// ================================================================
// Antecedent Planner (from plan-a.lisp)
// ================================================================

struct PlanAResult {
    bool found;
    std::vector<std::any> path;
    int number_examined;
};

PlanAResult plan_a(EnvPtr start, const std::vector<SExpr>& goal,
    PlnprPtr plnpr = nullptr);

// ================================================================
// Blocks World (from bcode.lisp, blocks.lisp)
// ================================================================

PlnprPtr build_blocks_problem(const std::string& title,
    const std::vector<std::string>& blocks_list,
    bool debugging = false);

std::vector<std::vector<SExpr>> make_blocks_basis_set(
    const std::vector<std::string>& blocks);

void register_blocks_rules(AtrePtr atre, PlnprPtr plnpr);
void register_blocks_operators(PlnprPtr plnpr);

#endif // PLANNER_H
