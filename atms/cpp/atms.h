// -*- C++ -*-

/// Assumption-based Truth Maintenance System (ATMS)
/// Converted from atms.lisp

/// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation.
/// All rights reserved.

#ifndef ATMS_H
#define ATMS_H

#include <string>
#include <vector>
#include <list>
#include <map>
#include <functional>
#include <memory>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <any>
#include <variant>
#include <optional>
#include <cassert>

// Forward declarations
struct Atms;
struct TmsNode;
struct Just;
struct Env;

using NodePtr = std::shared_ptr<TmsNode>;
using JustPtr = std::shared_ptr<Just>;
using EnvPtr = std::shared_ptr<Env>;
using AtmsPtr = std::shared_ptr<Atms>;

/// Comparison result for environments
enum class EnvCompare { EQ, S12, S21, DISJOINT };

/// ATMS: Top-level container managing all nodes, justifications, environments, and nogoods.
struct Atms : std::enable_shared_from_this<Atms> {
    std::string title;
    int node_counter = 0;
    int just_counter = 0;
    int env_counter = 0;
    std::vector<NodePtr> nodes;
    std::vector<JustPtr> justs;
    std::vector<NodePtr> contradictions;
    std::vector<NodePtr> assumptions;
    bool debugging = false;
    // Tables: map from count -> list of Env
    std::map<int, std::vector<EnvPtr>> nogood_table;
    NodePtr contra_node;
    std::map<int, std::vector<EnvPtr>> env_table;
    EnvPtr empty_env;
    std::function<std::string(NodePtr)> node_string;
    std::function<void(std::any)> enqueue_procedure;

    std::string to_string() const {
        return "#<ATMS: " + title + ">";
    }
};

/// TMS-NODE: A proposition in the ATMS.
/// The label is the list of minimal environments under which this node is true.
struct TmsNode : std::enable_shared_from_this<TmsNode> {
    int index = 0;
    std::any datum;
    std::vector<EnvPtr> label;
    std::vector<JustPtr> justs;
    std::vector<JustPtr> consequences;
    bool contradictory = false;
    bool assumption = false;
    std::vector<std::any> rules;
    std::weak_ptr<Atms> atms;

    std::string to_string() const;
};

/// JUST: A justification (inference rule).
/// "If all antecedents are true, then consequence is true."
struct Just {
    int index = 0;
    std::string informant;
    NodePtr consequence;
    std::vector<NodePtr> antecedents;

    std::string to_string() const {
        return "<" + informant + " " + std::to_string(index) + ">";
    }
};

/// ENV: An environment = a set of assumptions = one "worldview".
struct Env : std::enable_shared_from_this<Env> {
    int index = 0;
    int count = 0;
    std::vector<NodePtr> assumptions;
    std::vector<NodePtr> nodes;
    // nullptr => not nogood, non-null => nogood (holds the justification or parent env)
    std::any nogood;
    std::vector<std::any> rules;

    bool is_nogood() const { return nogood.has_value(); }
    void set_nogood(std::any val) { nogood = val; }

    std::string to_string() const {
        return "E-" + std::to_string(index);
    }
};

// ================================================================
// Utility functions
// ================================================================

std::string node_string(NodePtr node);
std::string default_node_string(NodePtr n);

/// Insert item into a sorted list maintaining order.
template<typename T, typename Comp>
void ordered_insert(std::vector<T>& list, const T& item, Comp comp) {
    for (auto it = list.begin(); it != list.end(); ++it) {
        if (*it == item) return; // already exists
        if (comp(item, *it)) {
            list.insert(it, item);
            return;
        }
    }
    list.push_back(item);
}

inline bool assumption_order(const NodePtr& a1, const NodePtr& a2) {
    return a1->index < a2->index;
}

inline bool env_order(const EnvPtr& e1, const EnvPtr& e2) {
    return e1->index < e2->index;
}

// ================================================================
// Inference Engine Interface
// ================================================================

AtmsPtr create_atms(const std::string& title,
                    std::function<std::string(NodePtr)> node_string_fn = nullptr,
                    bool debugging = false,
                    std::function<void(std::any)> enqueue_procedure = nullptr);

void change_atms(AtmsPtr atms,
                 std::function<std::string(NodePtr)> node_string_fn = nullptr,
                 std::function<void(std::any)> enqueue_procedure = nullptr,
                 std::optional<bool> debugging = std::nullopt);

bool true_node(NodePtr node);
bool in_node(NodePtr n, EnvPtr env = nullptr);
bool out_node(NodePtr n, EnvPtr env);
bool node_consistent_with(NodePtr n, EnvPtr env);

NodePtr tms_create_node(AtmsPtr atms, std::any datum,
                        bool assumptionp = false,
                        bool contradictoryp = false);

void assume_node(NodePtr node);
void make_contradiction(NodePtr node);
JustPtr justify_node(const std::string& informant, NodePtr consequence,
                     const std::vector<NodePtr>& antecedents);
void nogood_nodes(const std::string& informant, const std::vector<NodePtr>& nodes);

// ================================================================
// Label Updating
// ================================================================

void propagate(JustPtr just, NodePtr antecedent, std::vector<EnvPtr> envs);
void update(std::vector<EnvPtr> new_envs, NodePtr consequence, JustPtr just);
std::vector<EnvPtr> update_label(NodePtr node, std::vector<EnvPtr> new_envs);
std::vector<EnvPtr> weave(NodePtr antecedent, std::vector<EnvPtr> envs,
                          const std::vector<NodePtr>& antecedents);
bool in_antecedent(const std::vector<NodePtr>& nodes);
bool weave_check(EnvPtr env, const std::vector<NodePtr>& nodes);
bool supporting_antecedent(const std::vector<NodePtr>& nodes, EnvPtr env);

void remove_node(NodePtr node);

// ================================================================
// Environment Creation and Extension
// ================================================================

EnvPtr create_env(AtmsPtr atms, const std::vector<NodePtr>& assumptions);
EnvPtr union_env(EnvPtr e1, EnvPtr e2);
EnvPtr cons_env(NodePtr assumption, EnvPtr env);
EnvPtr find_or_make_env(const std::vector<NodePtr>& assumptions, AtmsPtr atms);
EnvPtr lookup_env(const std::vector<NodePtr>& assumes);

// ================================================================
// Env Tables
// ================================================================

void insert_in_table(std::map<int, std::vector<EnvPtr>>& table, EnvPtr env);
bool subset_env(EnvPtr e1, EnvPtr e2);
EnvCompare compare_env(EnvPtr e1, EnvPtr e2);

// ================================================================
// Nogood Processing
// ================================================================

void new_nogood(AtmsPtr atms, EnvPtr cenv, std::any just);
void set_env_contradictory(AtmsPtr atms, EnvPtr env);
void remove_env_from_labels(EnvPtr env, AtmsPtr atms);

// ================================================================
// Interpretation Construction
// ================================================================

std::vector<EnvPtr> interpretations(AtmsPtr atms,
    const std::vector<std::vector<NodePtr>>& choice_sets,
    const std::vector<NodePtr>& defaults = {});

void get_depth_solutions1(EnvPtr solution,
    const std::vector<std::vector<EnvPtr>>& choice_sets,
    size_t index,
    std::vector<EnvPtr>& solutions);

void extend_via_defaults(EnvPtr solution,
    const std::vector<NodePtr>& remaining, size_t idx,
    const std::vector<NodePtr>& original,
    std::vector<EnvPtr>& solutions);

// ================================================================
// Explanation Generation
// ================================================================

struct Explanation {
    std::string type; // "ASSUME" or informant
    NodePtr node;     // for ASSUME type
    JustPtr just;     // for justification type
};

std::vector<Explanation> explain_node(NodePtr node, EnvPtr env);

// ================================================================
// Printing
// ================================================================

void why_node(NodePtr node, std::ostream& stream = std::cout,
              const std::string& prefix = "");
void why_nodes(AtmsPtr atms, std::ostream& stream = std::cout);
void node_justifications(NodePtr node, std::ostream& stream = std::cout);
void print_justification(JustPtr j, std::ostream& stream = std::cout);
EnvPtr e(AtmsPtr atms, int n);
void print_env(EnvPtr e, std::ostream& stream = std::cout);
std::string env_string(EnvPtr e);
void print_nogoods(AtmsPtr atms, std::ostream& stream = std::cout);
void print_envs(AtmsPtr atms, std::ostream& stream = std::cout);
void print_env_table(const std::map<int, std::vector<EnvPtr>>& table,
                     std::ostream& stream = std::cout);
void print_atms_statistics(AtmsPtr atms);

#endif // ATMS_H
