// -*- C++ -*-

/// Justification-based Truth Maintenance System (JTMS)
/// Converted from jtms.lisp

/// Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation.
/// All rights reserved.

#ifndef JTMS_H
#define JTMS_H

#include <string>
#include <vector>
#include <list>
#include <functional>
#include <memory>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <any>
#include <optional>
#include <cassert>
#include <stdexcept>

// Forward declarations
struct Jtms;
struct TmsNode;
struct Just;

using JtmsPtr = std::shared_ptr<Jtms>;
using NodePtr = std::shared_ptr<TmsNode>;
using JustPtr = std::shared_ptr<Just>;

/// Node label: IN (believed) or OUT (disbelieved)
enum class NodeLabel { IN, OUT };

/// Support type for a node
enum class SupportType { NONE, ENABLED_ASSUMPTION, JUSTIFICATION };

/// JTMS: Top-level TMS container
struct Jtms : std::enable_shared_from_this<Jtms> {
    std::string title;
    int node_counter = 0;
    int just_counter = 0;
    std::vector<NodePtr> nodes;
    std::vector<JustPtr> justs;
    bool debugging = false;
    std::vector<NodePtr> contradictions;
    std::vector<NodePtr> assumptions;
    bool checking_contradictions = true;
    std::function<std::string(NodePtr)> node_string;
    std::function<void(JtmsPtr, std::vector<NodePtr>)> contradiction_handler;
    std::function<void(std::any)> enqueue_procedure;

    std::string to_string() const {
        return "#<JTMS: " + title + ">";
    }
};

/// TMS-NODE: A proposition node in the JTMS.
struct TmsNode : std::enable_shared_from_this<TmsNode> {
    int index = 0;
    std::any datum;                // External problem solver data
    NodeLabel label = NodeLabel::OUT;
    SupportType support_type = SupportType::NONE;
    JustPtr support_just;          // Valid when support_type == JUSTIFICATION
    std::vector<JustPtr> justs;    // Possible justifications for this node
    std::vector<JustPtr> consequences; // Justifications where this is antecedent
    int mark = 0;                  // Marker for sweep algorithms
    bool contradictory = false;
    bool assumption = false;
    std::vector<std::any> in_rules;
    std::vector<std::any> out_rules;
    std::weak_ptr<Jtms> jtms;

    bool is_in() const { return label == NodeLabel::IN; }
    bool is_out() const { return label == NodeLabel::OUT; }

    std::string to_string() const;
};

/// JUST: A justification (inference rule).
struct Just {
    int index = 0;
    std::string informant;
    NodePtr consequence;
    std::vector<NodePtr> antecedents;

    std::string to_string() const {
        return "#<Just " + std::to_string(index) + ">";
    }
};

// ================================================================
// Utility functions
// ================================================================

std::string node_string(NodePtr node);
std::string default_node_string(NodePtr n);

// ================================================================
// JTMS creation and configuration
// ================================================================

JtmsPtr create_jtms(const std::string& title,
    std::function<std::string(NodePtr)> node_string_fn = nullptr,
    bool debugging = false,
    bool checking_contradictions = true,
    std::function<void(JtmsPtr, std::vector<NodePtr>)> contradiction_handler = nullptr,
    std::function<void(std::any)> enqueue_procedure = nullptr);

void change_jtms(JtmsPtr jtms,
    std::function<std::string(NodePtr)> node_string_fn = nullptr,
    std::function<void(std::any)> enqueue_procedure = nullptr,
    std::optional<bool> debugging = std::nullopt,
    std::optional<bool> checking_contradictions = std::nullopt,
    std::function<void(JtmsPtr, std::vector<NodePtr>)> contradiction_handler = nullptr);

// ================================================================
// Node operations
// ================================================================

bool in_node(NodePtr node);
bool out_node(NodePtr node);
bool tms_node_premise(NodePtr node);

NodePtr tms_create_node(JtmsPtr jtms, std::any datum,
    bool assumptionp = false, bool contradictoryp = false);

void assume_node(NodePtr node);
void make_contradiction(NodePtr node);
JustPtr justify_node(const std::string& informant, NodePtr consequence,
    const std::vector<NodePtr>& antecedents);

// ================================================================
// Support and propagation
// ================================================================

bool check_justification(JustPtr just);
bool justification_satisfied(JustPtr just);
void install_support(NodePtr conseq, JustPtr just);
void propagate_inness(NodePtr node);
void make_node_in(NodePtr conseq, JustPtr reason);
void make_node_in_assumption(NodePtr conseq);

// ================================================================
// Assumption manipulation
// ================================================================

void retract_assumption(NodePtr node);
void enable_assumption(NodePtr node);
void make_node_out(NodePtr node);
std::vector<NodePtr> propagate_outness(NodePtr node, JtmsPtr jtms);
void find_alternative_support(JtmsPtr jtms, std::vector<NodePtr> out_queue);

// ================================================================
// Contradiction handling
// ================================================================

void check_for_contradictions(JtmsPtr jtms);
void ask_user_handler(JtmsPtr jtms, std::vector<NodePtr> contradictions);
void handle_one_contradiction(NodePtr contra_node);

// ================================================================
// Well-founded support queries
// ================================================================

JustPtr supporting_justification_for_node(NodePtr node);
std::vector<NodePtr> assumptions_of_node(NodePtr node);
std::vector<NodePtr> enabled_assumptions(JtmsPtr jtms);

// ================================================================
// Display
// ================================================================

void why_node(NodePtr node, std::ostream& stream = std::cout);
void why_nodes(JtmsPtr jtms, std::ostream& stream = std::cout);
void explore_network(NodePtr node, std::ostream& stream = std::cout);

#endif // JTMS_H
