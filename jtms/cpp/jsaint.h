// -*- C++ -*-

/// JSAINT: A rational reconstruction of Slagel's SAINT program
/// Converted from jsaint.lisp, jsrules.lisp, jsops.lisp

#ifndef JSAINT_H
#define JSAINT_H

#include "jtre.h"
#include "simplify.h"
#include <deque>

// Forward declarations
struct Jsaint;
struct IntegrationOp;

using JsaintPtr = std::shared_ptr<Jsaint>;
using IntegrationOpPtr = std::shared_ptr<IntegrationOp>;

/// JSAINT: Symbolic Integration Agenda-based problem solver
struct Jsaint : std::enable_shared_from_this<Jsaint> {
    std::string title;
    JtrePtr jtre;
    // Agenda entries: (difficulty, subproblem-string)
    std::deque<std::pair<int, std::string>> agenda;
    std::string problem;
    std::string solution;       // empty = unsolved
    int n_subproblems = 0;
    int max_tasks = 20;
    bool debugging = false;

    std::string to_string() const {
        return "<Agenda " + title + ">";
    }
};

/// Integration Operator definition
struct IntegrationOp {
    std::string name;
    std::string trigger_pattern;
    std::string test;           // condition expression (empty = always)
    std::vector<std::pair<std::string, std::string>> subproblems; // (var, form)
    std::string result;
};

// ================================================================
// Global JSAINT
// ================================================================
extern JsaintPtr global_jsaint;

// ================================================================
// JSAINT interface
// ================================================================

JsaintPtr create_jsaint(const std::string& title, const std::string& problem,
    bool debugging = false, int max_tasks = 20);
void use_jsaint(JsaintPtr js);
void change_jsaint(JsaintPtr js,
    std::optional<bool> debugging = std::nullopt,
    std::optional<int> max_tasks = std::nullopt);

// ================================================================
// Solving
// ================================================================

enum class SolveResult { SOLVED, FAILED_PROBLEM, FAILED_EMPTY, TIME_OUT };

std::pair<SolveResult, std::string> run_jsaint(JsaintPtr js);
void process_subproblem(const std::string& item, JsaintPtr js);
void open_subproblem(const std::string& item, JsaintPtr js);
void queue_problem(const std::string& problem, const std::string& parent,
    JsaintPtr js);

std::string fetch_solution(const std::string& problem, JsaintPtr js);
void explain_result(JsaintPtr js = nullptr);

// ================================================================
// Difficulty estimation
// ================================================================

int estimate_difficulty(const std::string& problem);
int count_symbols(const std::string& pr);
int max_depth(const std::string& pr);

// ================================================================
// Integration operators (from jsops.lisp)
// ================================================================

void register_integration_operator(IntegrationOpPtr op, JsaintPtr js = nullptr);
void load_jsaint_rules(JsaintPtr js);
void load_jsaint_operators(JsaintPtr js);

// ================================================================
// Display
// ================================================================

void show_problem(const std::string& pr, JsaintPtr js = nullptr);
void show_ao_graph(JsaintPtr js = nullptr);

// ================================================================
// Convenience
// ================================================================

std::pair<SolveResult, std::string> solve_integral(const std::string& integral,
    const std::string& title = "JSAINT",
    bool debugging = false, int max_tasks = 20);

void try_jsaint(const std::string& problem,
    const std::string& title = "JSAINT Test");

#endif // JSAINT_H
