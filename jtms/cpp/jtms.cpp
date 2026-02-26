// -*- C++ -*-

/// Justification-based Truth Maintenance System (JTMS) - Implementation
/// Converted from jtms.lisp

#include "jtms.h"

// ================================================================
// TmsNode methods
// ================================================================

std::string TmsNode::to_string() const {
    return "#<Node: " + node_string(
        const_cast<TmsNode*>(this)->shared_from_this()) + ">";
}

// ================================================================
// Utility functions
// ================================================================

std::string node_string(NodePtr node) {
    auto jtms = node->jtms.lock();
    if (jtms && jtms->node_string) {
        return jtms->node_string(node);
    }
    return default_node_string(node);
}

std::string default_node_string(NodePtr n) {
    try {
        return std::any_cast<std::string>(n->datum);
    } catch (...) {
        return "?";
    }
}

// ================================================================
// JTMS creation and configuration
// ================================================================

JtmsPtr create_jtms(const std::string& title,
    std::function<std::string(NodePtr)> node_string_fn,
    bool debugging,
    bool checking_contradictions,
    std::function<void(JtmsPtr, std::vector<NodePtr>)> contradiction_handler,
    std::function<void(std::any)> enqueue_procedure) {

    auto jtms = std::make_shared<Jtms>();
    jtms->title = title;
    jtms->node_string = node_string_fn ? node_string_fn : default_node_string;
    jtms->debugging = debugging;
    jtms->checking_contradictions = checking_contradictions;
    jtms->contradiction_handler = contradiction_handler
        ? contradiction_handler : ask_user_handler;
    jtms->enqueue_procedure = enqueue_procedure;
    return jtms;
}

void change_jtms(JtmsPtr jtms,
    std::function<std::string(NodePtr)> node_string_fn,
    std::function<void(std::any)> enqueue_procedure,
    std::optional<bool> debugging,
    std::optional<bool> checking_contradictions,
    std::function<void(JtmsPtr, std::vector<NodePtr>)> contradiction_handler) {

    if (node_string_fn) jtms->node_string = node_string_fn;
    if (enqueue_procedure) jtms->enqueue_procedure = enqueue_procedure;
    if (debugging.has_value()) jtms->debugging = debugging.value();
    if (checking_contradictions.has_value())
        jtms->checking_contradictions = checking_contradictions.value();
    if (contradiction_handler) jtms->contradiction_handler = contradiction_handler;
}

// ================================================================
// Node operations
// ================================================================

bool in_node(NodePtr node) { return node->is_in(); }
bool out_node(NodePtr node) { return node->is_out(); }

bool tms_node_premise(NodePtr node) {
    if (node->support_type == SupportType::JUSTIFICATION && node->support_just) {
        return node->support_just->antecedents.empty();
    }
    return false;
}

NodePtr tms_create_node(JtmsPtr jtms, std::any datum,
    bool assumptionp, bool contradictoryp) {

    auto node = std::make_shared<TmsNode>();
    node->index = ++jtms->node_counter;
    node->datum = datum;
    node->assumption = assumptionp;
    node->contradictory = contradictoryp;
    node->jtms = jtms;

    if (assumptionp) jtms->assumptions.push_back(node);
    if (contradictoryp) jtms->contradictions.push_back(node);
    jtms->nodes.push_back(node);
    return node;
}

void assume_node(NodePtr node) {
    auto jtms = node->jtms.lock();
    if (!node->assumption) {
        if (jtms && jtms->debugging) {
            std::cerr << "\nConverting " << node_string(node)
                      << " into an assumption";
        }
        node->assumption = true;
    }
    enable_assumption(node);
}

void make_contradiction(NodePtr node) {
    auto jtms = node->jtms.lock();
    if (!node->contradictory) {
        node->contradictory = true;
        if (jtms) {
            jtms->contradictions.push_back(node);
            check_for_contradictions(jtms);
        }
    }
}

JustPtr justify_node(const std::string& informant, NodePtr consequence,
    const std::vector<NodePtr>& antecedents) {

    auto jtms = consequence->jtms.lock();
    if (!jtms) return nullptr;

    auto just = std::make_shared<Just>();
    just->index = ++jtms->just_counter;
    just->informant = informant;
    just->consequence = consequence;
    just->antecedents = antecedents;

    consequence->justs.push_back(just);
    for (auto& node : antecedents) {
        node->consequences.push_back(just);
    }
    jtms->justs.push_back(just);

    if (jtms->debugging) {
        std::cerr << "\nJustifying " << node_string(consequence)
                  << " by " << informant << " using [";
        for (size_t i = 0; i < antecedents.size(); i++) {
            if (i > 0) std::cerr << ", ";
            std::cerr << node_string(antecedents[i]);
        }
        std::cerr << "].";
    }

    if (!antecedents.empty() || out_node(consequence)) {
        if (check_justification(just)) {
            install_support(consequence, just);
        }
    } else {
        consequence->support_type = SupportType::JUSTIFICATION;
        consequence->support_just = just;
    }

    check_for_contradictions(jtms);
    return just;
}

// ================================================================
// Support and propagation
// ================================================================

bool check_justification(JustPtr just) {
    return out_node(just->consequence) && justification_satisfied(just);
}

bool justification_satisfied(JustPtr just) {
    for (auto& ant : just->antecedents) {
        if (!in_node(ant)) return false;
    }
    return true;
}

void install_support(NodePtr conseq, JustPtr just) {
    make_node_in(conseq, just);
    propagate_inness(conseq);
}

void propagate_inness(NodePtr node) {
    auto jtms = node->jtms.lock();
    if (!jtms) return;

    std::vector<NodePtr> q = {node};
    while (!q.empty()) {
        auto current = q.front();
        q.erase(q.begin());

        if (jtms->debugging) {
            std::cerr << "\n   Propagating belief in "
                      << node_string(current) << ".";
        }

        for (auto& justification : current->consequences) {
            if (check_justification(justification)) {
                make_node_in(justification->consequence, justification);
                q.push_back(justification->consequence);
            }
        }
    }
}

void make_node_in(NodePtr conseq, JustPtr reason) {
    auto jtms = conseq->jtms.lock();
    if (!jtms) return;

    if (jtms->debugging) {
        std::cerr << "\n     Making " << node_string(conseq)
                  << " in via " << reason->informant << ".";
    }

    conseq->label = NodeLabel::IN;
    conseq->support_type = SupportType::JUSTIFICATION;
    conseq->support_just = reason;

    if (jtms->enqueue_procedure) {
        for (auto& rule : conseq->in_rules) {
            jtms->enqueue_procedure(rule);
        }
        conseq->in_rules.clear();
    }
}

void make_node_in_assumption(NodePtr conseq) {
    auto jtms = conseq->jtms.lock();
    if (!jtms) return;

    if (jtms->debugging) {
        std::cerr << "\n     Making " << node_string(conseq)
                  << " in via ENABLED-ASSUMPTION.";
    }

    conseq->label = NodeLabel::IN;
    conseq->support_type = SupportType::ENABLED_ASSUMPTION;
    conseq->support_just = nullptr;

    if (jtms->enqueue_procedure) {
        for (auto& rule : conseq->in_rules) {
            jtms->enqueue_procedure(rule);
        }
        conseq->in_rules.clear();
    }
}

// ================================================================
// Assumption manipulation
// ================================================================

void retract_assumption(NodePtr node) {
    if (node->support_type != SupportType::ENABLED_ASSUMPTION) return;
    auto jtms = node->jtms.lock();
    if (!jtms) return;

    if (jtms->debugging) {
        std::cerr << "\n  Retracting assumption "
                  << node_string(node) << ".";
    }

    make_node_out(node);
    auto out_queue = propagate_outness(node, jtms);
    out_queue.insert(out_queue.begin(), node);
    find_alternative_support(jtms, out_queue);
}

void enable_assumption(NodePtr node) {
    auto jtms = node->jtms.lock();
    if (!jtms) return;

    if (!node->assumption) {
        throw std::runtime_error("Can't enable the non-assumption "
            + node_string(node));
    }

    if (jtms->debugging) {
        std::cerr << "\n  Enabling assumption "
                  << node_string(node) << ".";
    }

    if (out_node(node)) {
        make_node_in_assumption(node);
        propagate_inness(node);
    } else if (node->support_type == SupportType::ENABLED_ASSUMPTION) {
        // Already enabled, do nothing
    } else if (node->support_type == SupportType::JUSTIFICATION &&
               node->support_just &&
               node->support_just->antecedents.empty()) {
        // Supported by a premise, do nothing
    } else {
        node->support_type = SupportType::ENABLED_ASSUMPTION;
        node->support_just = nullptr;
    }

    check_for_contradictions(jtms);
}

void make_node_out(NodePtr node) {
    auto jtms = node->jtms.lock();
    if (!jtms) return;

    if (jtms->debugging) {
        std::cerr << "\n     Retracting belief in "
                  << node_string(node) << ".";
    }

    node->support_type = SupportType::NONE;
    node->support_just = nullptr;
    node->label = NodeLabel::OUT;

    if (jtms->enqueue_procedure) {
        for (auto& rule : node->out_rules) {
            jtms->enqueue_procedure(rule);
        }
    }
    node->out_rules.clear();
}

std::vector<NodePtr> propagate_outness(NodePtr node, JtmsPtr jtms) {
    if (jtms->debugging) {
        std::cerr << "\n   Propagating disbelief in "
                  << node_string(node) << ".";
    }

    std::vector<NodePtr> out_queue;
    std::vector<JustPtr> js = node->consequences;

    size_t i = 0;
    while (i < js.size()) {
        auto& j = js[i];
        auto conseq = j->consequence;
        if (conseq->support_type == SupportType::JUSTIFICATION &&
            conseq->support_just == j) {
            make_node_out(conseq);
            out_queue.push_back(conseq);
            // Add this node's consequences to process
            for (auto& cj : conseq->consequences) {
                js.push_back(cj);
            }
        }
        i++;
    }
    return out_queue;
}

void find_alternative_support(JtmsPtr jtms, std::vector<NodePtr> out_queue) {
    if (jtms->debugging) {
        std::cerr << "\n   Looking for alternative supports.";
    }

    for (auto& node : out_queue) {
        if (in_node(node)) continue;
        for (auto& just : node->justs) {
            if (check_justification(just)) {
                install_support(just->consequence, just);
                break;
            }
        }
    }
}

// ================================================================
// Contradiction handling
// ================================================================

void check_for_contradictions(JtmsPtr jtms) {
    if (!jtms->checking_contradictions) return;

    std::vector<NodePtr> contradictions;
    for (auto& cnode : jtms->contradictions) {
        if (in_node(cnode)) contradictions.push_back(cnode);
    }
    if (!contradictions.empty() && jtms->contradiction_handler) {
        jtms->contradiction_handler(jtms, contradictions);
    }
}

void ask_user_handler(JtmsPtr jtms, std::vector<NodePtr> contradictions) {
    handle_one_contradiction(contradictions[0]);
    check_for_contradictions(jtms);
}

void handle_one_contradiction(NodePtr contra_node) {
    auto contra_assumptions = assumptions_of_node(contra_node);
    if (contra_assumptions.empty()) {
        throw std::runtime_error("There is a flaw in the universe... "
            + node_string(contra_node));
    }

    std::cout << "\nContradiction found: " << node_string(contra_node);
    int counter = 1;
    for (auto& a : contra_assumptions) {
        std::cout << "\n" << counter++ << " " << node_string(a);
    }
    std::cout << "\nCall retract_assumption on the desired node to resolve.";

    // In the original, this breaks to the debugger for interactive input.
    // In C++, we just retract the first assumption automatically.
    if (!contra_assumptions.empty()) {
        retract_assumption(contra_assumptions[0]);
    }
}

// ================================================================
// Well-founded support queries
// ================================================================

JustPtr supporting_justification_for_node(NodePtr node) {
    return node->support_just;
}

std::vector<NodePtr> assumptions_of_node(NodePtr node) {
    static int marker_counter = 0;
    int marker = ++marker_counter;

    std::vector<NodePtr> assumptions;
    std::vector<NodePtr> nodes_to_process = {node};

    while (!nodes_to_process.empty()) {
        auto current = nodes_to_process.front();
        nodes_to_process.erase(nodes_to_process.begin());

        if (current->mark == marker) continue;
        current->mark = marker;

        if (current->support_type == SupportType::ENABLED_ASSUMPTION) {
            assumptions.push_back(current);
        } else if (in_node(current) &&
                   current->support_type == SupportType::JUSTIFICATION &&
                   current->support_just) {
            for (auto& ant : current->support_just->antecedents) {
                nodes_to_process.push_back(ant);
            }
        }
    }
    return assumptions;
}

std::vector<NodePtr> enabled_assumptions(JtmsPtr jtms) {
    std::vector<NodePtr> result;
    for (auto& a : jtms->assumptions) {
        if (a->support_type == SupportType::ENABLED_ASSUMPTION) {
            result.push_back(a);
        }
    }
    return result;
}

// ================================================================
// Display
// ================================================================

void why_node(NodePtr node, std::ostream& stream) {
    if (node->support_type == SupportType::ENABLED_ASSUMPTION) {
        stream << "\n" << node_string(node) << " is an enabled assumption";
    } else if (node->support_type == SupportType::JUSTIFICATION &&
               node->support_just) {
        stream << "\n" << node_string(node) << " is IN via "
               << node->support_just->informant << " on";
        for (auto& anode : node->support_just->antecedents) {
            stream << "\n  " << node_string(anode);
        }
    } else {
        stream << "\n" << node_string(node) << " is OUT.";
    }
}

void why_nodes(JtmsPtr jtms, std::ostream& stream) {
    for (auto& node : jtms->nodes) {
        why_node(node, stream);
    }
}

void explore_network(NodePtr node, std::ostream& stream) {
    if (!in_node(node)) {
        stream << "\n Sorry, " << node_string(node) << " not believed.";
        return;
    }

    std::vector<NodePtr> stack;
    auto current = node;

    while (true) {
        why_node(current, stream);
        std::vector<NodePtr> options;
        if (current->support_type == SupportType::JUSTIFICATION &&
            current->support_just) {
            options = current->support_just->antecedents;
        }

        stream << "\n>>> (enter 0 to go back, q to quit, or 1-"
               << options.size() << "): ";
        std::string input;
        std::getline(std::cin, input);

        if (input == "q" || input == "Q") return;

        int choice = 0;
        try { choice = std::stoi(input); } catch (...) { continue; }

        if (choice == 0) {
            if (stack.empty()) return;
            current = stack.back();
            stack.pop_back();
        } else if (choice >= 1 &&
                   choice <= static_cast<int>(options.size())) {
            stack.push_back(current);
            current = options[choice - 1];
        }
    }
}
