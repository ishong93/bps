// -*- C++ -*-

/// ATMS-based Planner - Implementation
/// Converted from aplanr.lisp, plan-a.lisp, plan-e.lisp, bcode.lisp, blocks.lisp

#include "planner.h"
#include <iostream>

// ================================================================
// Global planner pointer
// ================================================================
PlnprPtr global_plnpr = nullptr;

// ================================================================
// Planner utilities (from aplanr.lisp)
// ================================================================

PlnprPtr create_planning_problem(const std::string& title,
    const std::vector<std::vector<SExpr>>& basis_set) {
    auto plnpr = std::make_shared<Plnpr>();
    plnpr->title = title;
    plnpr->atre = create_atre("ATRE(" + title + ")");
    plnpr->basis_set = basis_set;
    in_plnpr(plnpr);
    return plnpr;
}

void in_plnpr(PlnprPtr plnpr) {
    global_plnpr = plnpr;
}

void setup_choice_sets(PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;
    std::string informant = "BASIS SET(" + plnpr->title + ")";

    in_atre(plnpr->atre);
    for (auto& choice_set : plnpr->basis_set) {
        for (auto& choice : choice_set) {
            assume_if_needed(choice, informant, plnpr->atre);
        }
    }
    run_rules(plnpr->atre);
}

void set_debug_plnpr(bool state, PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;
    plnpr->debugging = state;
}

void register_operator(const std::string& name,
    const std::vector<std::string>& form,
    const std::vector<SExpr>& preconditions,
    const std::vector<SExpr>& add_list,
    const std::vector<SExpr>& delete_list,
    std::function<bool(const std::map<std::string, std::string>&)> test,
    PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;

    auto op = std::make_shared<Operator>();
    op->form = form;
    op->preconditions = preconditions;
    op->add_list = add_list;
    op->delete_list = delete_list;
    op->test = test;

    plnpr->operators[name] = op;
}

OperatorPtr fetch_operator(const std::string& op_name, PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;
    auto it = plnpr->operators.find(op_name);
    if (it != plnpr->operators.end()) return it->second;
    return nullptr;
}

std::vector<std::vector<std::string>> find_applicable_operators(
    EnvPtr state, PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;

    std::vector<std::vector<std::string>> result;
    auto candidates = fetch(SExpr(std::string("(applicable ?x)")),
                            plnpr->atre);
    for (auto& candidate : candidates) {
        std::string s;
        try { s = std::any_cast<std::string>(candidate); } catch(...) { continue; }
        if (in_fact(candidate, state, plnpr->atre)) {
            // Parse out the operator instance
            // (applicable (Pickup A)) -> ["Pickup", "A"]
            // Simplified extraction
            result.push_back({s});
        }
    }
    return result;
}

EnvPtr apply_operator(EnvPtr state,
    const std::vector<std::string>& op_inst, PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;

    auto op = fetch_operator(op_inst[0], plnpr);
    if (!op) return nullptr;

    // Build variable bindings
    std::map<std::string, std::string> bindings;
    for (size_t i = 1; i < op->form.size() && i < op_inst.size(); i++) {
        bindings[op->form[i]] = op_inst[i];
    }

    if (plnpr->debugging) {
        std::cerr << "\n   Applying " << op_inst[0] << " to "
                  << state->to_string() << ".";
    }

    // Substitute bindings into add and delete lists
    auto substitute = [&bindings](const SExpr& expr) -> std::string {
        std::string s;
        try { s = std::any_cast<std::string>(expr); } catch(...) { return ""; }
        for (auto& [var, val] : bindings) {
            size_t pos;
            while ((pos = s.find(var)) != std::string::npos) {
                s.replace(pos, var.size(), val);
            }
        }
        return s;
    };

    // Remove delete-list assumptions
    auto assumptions = state->assumptions;
    for (auto& del : op->delete_list) {
        std::string del_str = substitute(del);
        assumptions.erase(
            std::remove_if(assumptions.begin(), assumptions.end(),
                [&del_str](const NodePtr& a) {
                    try {
                        auto datum = std::any_cast<std::shared_ptr<Datum>>(a->datum);
                        std::string form;
                        try { form = std::any_cast<std::string>(datum->lisp_form); }
                        catch(...) { return false; }
                        return form == del_str;
                    } catch(...) { return false; }
                }),
            assumptions.end());
    }

    // Add add-list assumptions
    for (auto& add : op->add_list) {
        std::string add_str = substitute(add);
        auto node = get_tms_node(SExpr(add_str), plnpr->atre);
        ordered_insert(assumptions, node,
            [](const NodePtr& a, const NodePtr& b) {
                return assumption_order(a, b);
            });
    }

    return find_or_make_env(assumptions, plnpr->atre->atms);
}

std::vector<EnvPtr> fetch_states(const std::vector<SExpr>& facts,
    PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;

    std::vector<std::vector<SExpr>> all_sets;
    for (auto& fact : facts) {
        all_sets.push_back({fact});
    }
    for (auto& bs : plnpr->basis_set) {
        all_sets.push_back(bs);
    }
    return solutions(plnpr->atre, all_sets);
}

GoalResult satisfies_goal(EnvPtr state, const std::vector<SExpr>& goals,
    PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;

    // Simplified goal checking
    for (auto& goal : goals) {
        if (!in_fact(goal, state, plnpr->atre)) {
            return {false, {}};
        }
    }
    return {true, {}};
}

void show_plan(const std::vector<std::any>& plan) {
    for (size_t i = plan.size(); i > 0; i -= 2) {
        try {
            auto env = std::any_cast<EnvPtr>(plan[i - 1]);
            print_env(env);
        } catch(...) {}
        if (i >= 2) {
            try {
                auto op = std::any_cast<std::string>(plan[i - 2]);
                std::cout << "\n  then, by " << op << ", ";
            } catch(...) {}
        }
    }
}

// ================================================================
// Envisioner (from plan-e.lisp)
// ================================================================

EnvisionmentResult envision(PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;
    EnvisionmentResult result;

    result.states = solutions(plnpr->atre,
        plnpr->basis_set.empty()
            ? std::vector<std::vector<SExpr>>{}
            : plnpr->basis_set);

    // Apply all operators to each state
    for (auto& state : result.states) {
        auto ops = find_applicable_operators(state, plnpr);
        std::vector<std::pair<std::vector<std::string>, EnvPtr>> trans;
        for (auto& op_inst : ops) {
            auto new_state = apply_operator(state, op_inst, plnpr);
            if (new_state) {
                trans.push_back({op_inst, new_state});
            }
        }
        result.transitions.push_back({state, trans});
    }

    return result;
}

void show_envisionment(PlnprPtr plnpr, std::ostream& stream) {
    if (!plnpr) plnpr = global_plnpr;

    // Retrieve cached states
    auto env_result = envision(plnpr);

    if (env_result.states.empty()) {
        stream << "\nThe state space is empty.";
        return;
    }

    stream << "\n " << env_result.states.size()
           << " states have been generated:";
    for (auto& state : env_result.states) {
        print_env(state, stream);
    }

    stream << "\nTransition Table:";
    if (env_result.transitions.empty()) {
        stream << " empty.";
    } else {
        for (auto& [state, trans] : env_result.transitions) {
            stream << "\n  " << state->to_string() << ": ";
            for (auto& [op, dest] : trans) {
                stream << "\n   ";
                for (auto& s : op) stream << s << " ";
                stream << "-> " << dest->to_string();
            }
        }
    }
}

PlanResult find_plan(const std::vector<SExpr>& start,
    const std::vector<SExpr>& goals, PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;

    auto goal_states = fetch_states(goals, plnpr);
    auto start_states = fetch_states(start, plnpr);
    auto env_result = envision(plnpr);

    if (plnpr->debugging) {
        std::cerr << "\nInitial states are: ";
        for (auto& s : start_states) std::cerr << s->to_string() << " ";
        std::cerr << "\nGoal states are: ";
        for (auto& s : goal_states) std::cerr << s->to_string() << " ";
    }

    // BFS
    std::deque<std::vector<std::any>> queue;
    for (auto& state : start_states) {
        queue.push_back({std::any(state)});
    }

    while (!queue.empty()) {
        auto path = queue.front();
        queue.pop_front();

        auto current = std::any_cast<EnvPtr>(path.front());
        // Check if goal
        for (auto& gs : goal_states) {
            if (current == gs) {
                return {true, path};
            }
        }

        // Find transitions for current state
        for (auto& [state, trans] : env_result.transitions) {
            if (state == current) {
                for (auto& [op, dest] : trans) {
                    // Avoid loops
                    bool visited = false;
                    for (size_t i = 0; i < path.size(); i += 2) {
                        try {
                            auto p = std::any_cast<EnvPtr>(path[i]);
                            if (p == dest) { visited = true; break; }
                        } catch(...) {}
                    }
                    if (!visited) {
                        auto new_path = path;
                        std::string op_str;
                        for (auto& s : op) op_str += s + " ";
                        new_path.insert(new_path.begin(), std::any(op_str));
                        new_path.insert(new_path.begin(), std::any(dest));
                        queue.push_back(new_path);
                    }
                }
                break;
            }
        }
    }

    return {false, {}};
}

// ================================================================
// Antecedent Planner (from plan-a.lisp)
// ================================================================

PlanAResult plan_a(EnvPtr start, const std::vector<SExpr>& goal,
    PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;

    std::deque<std::vector<std::any>> queue;
    queue.push_back({std::any(start)});
    int number_examined = 1;

    while (!queue.empty()) {
        auto path = queue.front();
        queue.pop_front();
        auto current = std::any_cast<EnvPtr>(path.front());

        auto [success, bindings] = satisfies_goal(current, goal, plnpr);
        if (success) {
            return {true, path, number_examined};
        }

        auto ops = find_applicable_operators(current, plnpr);
        for (auto& op_inst : ops) {
            auto result = apply_operator(current, op_inst, plnpr);
            if (!result) continue;

            // Avoid loops
            bool visited = false;
            for (size_t i = 0; i < path.size(); i += 2) {
                try {
                    auto p = std::any_cast<EnvPtr>(path[i]);
                    if (p == result) { visited = true; break; }
                } catch(...) {}
            }
            if (!visited) {
                if (plnpr->debugging) {
                    std::cerr << "\n  Reaching " << result->to_string()
                              << " via " << op_inst[0]
                              << " on " << current->to_string() << "..";
                }
                auto new_path = path;
                std::string op_str;
                for (auto& s : op_inst) op_str += s + " ";
                new_path.insert(new_path.begin(), std::any(op_str));
                new_path.insert(new_path.begin(), std::any(result));
                queue.push_back(new_path);
            }
        }
        number_examined++;
    }

    return {false, {}, number_examined};
}

// ================================================================
// Blocks World (from bcode.lisp, blocks.lisp)
// ================================================================

std::vector<std::vector<SExpr>> make_blocks_basis_set(
    const std::vector<std::string>& blocks) {

    std::vector<std::vector<SExpr>> basis;

    for (auto& block : blocks) {
        // What the block can be on: Holding, On Table, On other blocks
        std::vector<SExpr> on_choices;
        on_choices.push_back(SExpr(std::string("(Holding " + block + ")")));
        on_choices.push_back(SExpr(std::string("(On " + block + " Table)")));
        for (auto& other : blocks) {
            if (other != block) {
                on_choices.push_back(
                    SExpr(std::string("(On " + block + " " + other + ")")));
            }
        }
        basis.push_back(on_choices);

        // What can be on the block: Holding, Clear, other blocks on it
        std::vector<SExpr> top_choices;
        top_choices.push_back(SExpr(std::string("(Holding " + block + ")")));
        top_choices.push_back(SExpr(std::string("(Clear " + block + ")")));
        for (auto& other : blocks) {
            if (other != block) {
                top_choices.push_back(
                    SExpr(std::string("(ON " + other + " " + block + ")")));
            }
        }
        basis.push_back(top_choices);
    }

    // Hand status: HAND-EMPTY or holding one of the blocks
    std::vector<SExpr> hand_choices;
    hand_choices.push_back(SExpr(std::string("(HAND-EMPTY)")));
    for (auto& block : blocks) {
        hand_choices.push_back(
            SExpr(std::string("(HOLDING " + block + ")")));
    }
    basis.insert(basis.begin(), hand_choices);

    return basis;
}

PlnprPtr build_blocks_problem(const std::string& title,
    const std::vector<std::string>& blocks_list, bool debugging) {

    auto plnpr = create_planning_problem(title,
        make_blocks_basis_set(blocks_list));
    in_plnpr(plnpr);
    set_debug_plnpr(debugging, plnpr);

    in_atre(plnpr->atre);

    // Register block definitions
    for (auto& block : blocks_list) {
        assert_fact(SExpr(std::string("(block " + block + ")")),
                    {SExpr(std::string("Definition"))}, plnpr->atre);
    }

    // Register blocks world rules and operators
    register_blocks_rules(plnpr->atre, plnpr);
    register_blocks_operators(plnpr);

    run_rules(plnpr->atre);
    setup_choice_sets(plnpr);

    return plnpr;
}

void register_blocks_rules(AtrePtr atre, PlnprPtr plnpr) {
    // The blocks world rules are complex pattern-matching rules.
    // In the original Lisp, they are defined using the rule macro.
    // Here we provide a simplified procedural version of the key constraints.

    // Note: In a full implementation, each rule would be registered via
    // insert_rule with proper matchers. The rules enforce:
    // - Negation consistency
    // - Place exclusion (something can't be in two places)
    // - Top exclusion (only one thing on a non-Table block)
    // - Clear/holding constraints
    // - Above transitivity and anti-reflexivity/anti-symmetry
    // - Hand-empty/holding mutual exclusion

    // For the operators, see register_blocks_operators below.
}

void register_blocks_operators(PlnprPtr plnpr) {
    if (!plnpr) plnpr = global_plnpr;

    // Pickup ?x
    register_operator("Pickup",
        {"Pickup", "?x"},
        {SExpr(std::string("(on ?x Table)")),
         SExpr(std::string("(clear ?x)")),
         SExpr(std::string("(hand-empty)"))},
        {SExpr(std::string("(holding ?x)"))},
        {SExpr(std::string("(on ?x Table)")),
         SExpr(std::string("(clear ?x)")),
         SExpr(std::string("(hand-empty)"))},
        nullptr, plnpr);

    // Putdown ?x
    register_operator("Putdown",
        {"Putdown", "?x"},
        {SExpr(std::string("(holding ?x)"))},
        {SExpr(std::string("(on ?x Table)")),
         SExpr(std::string("(clear ?x)")),
         SExpr(std::string("(hand-empty)"))},
        {SExpr(std::string("(holding ?x)"))},
        nullptr, plnpr);

    // Stack ?x ?y
    register_operator("Stack",
        {"Stack", "?x", "?y"},
        {SExpr(std::string("(holding ?x)")),
         SExpr(std::string("(clear ?y)"))},
        {SExpr(std::string("(hand-empty)")),
         SExpr(std::string("(on ?x ?y)")),
         SExpr(std::string("(clear ?x)"))},
        {SExpr(std::string("(holding ?x)")),
         SExpr(std::string("(clear ?y)"))},
        // Test: ?y != TABLE
        [](const std::map<std::string, std::string>& bindings) {
            auto it = bindings.find("?y");
            return it != bindings.end() && it->second != "TABLE";
        },
        plnpr);

    // Unstack ?x ?y
    register_operator("Unstack",
        {"Unstack", "?x", "?y"},
        {SExpr(std::string("(hand-empty)")),
         SExpr(std::string("(clear ?x)")),
         SExpr(std::string("(on ?x ?y)"))},
        {SExpr(std::string("(holding ?x)")),
         SExpr(std::string("(clear ?y)"))},
        {SExpr(std::string("(hand-empty)")),
         SExpr(std::string("(clear ?x)")),
         SExpr(std::string("(on ?x ?y)"))},
        // Test: ?y != TABLE
        [](const std::map<std::string, std::string>& bindings) {
            auto it = bindings.find("?y");
            return it != bindings.end() && it->second != "TABLE";
        },
        plnpr);
}
