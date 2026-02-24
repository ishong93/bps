// -*- C++ -*-

/// Assumption-based Truth Maintenance System (ATMS) - Implementation
/// Converted from atms.lisp

#include "atms.h"

// ================================================================
// TmsNode methods
// ================================================================

std::string TmsNode::to_string() const {
    auto a = atms.lock();
    if (assumption && a) {
        return "A-" + std::to_string(index);
    }
    if (a) {
        return "#<NODE: " + node_string(
            const_cast<TmsNode*>(this)->shared_from_this()) + ">";
    }
    return "#<NODE: ?>";
}

// ================================================================
// Utility functions
// ================================================================

std::string node_string(NodePtr node) {
    auto atms = node->atms.lock();
    if (atms && atms->node_string) {
        return atms->node_string(node);
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
// Inference Engine Interface
// ================================================================

AtmsPtr create_atms(const std::string& title,
                    std::function<std::string(NodePtr)> node_string_fn,
                    bool debugging,
                    std::function<void(std::any)> enqueue_procedure) {
    auto atms = std::make_shared<Atms>();
    atms->title = title;
    atms->node_string = node_string_fn ? node_string_fn : default_node_string;
    atms->debugging = debugging;
    atms->enqueue_procedure = enqueue_procedure;

    atms->contra_node = tms_create_node(atms, std::string("The contradiction"),
                                        false, true);
    atms->empty_env = create_env(atms, {});
    return atms;
}

void change_atms(AtmsPtr atms,
                 std::function<std::string(NodePtr)> node_string_fn,
                 std::function<void(std::any)> enqueue_procedure,
                 std::optional<bool> debugging) {
    if (node_string_fn) atms->node_string = node_string_fn;
    if (debugging.has_value()) atms->debugging = debugging.value();
    if (enqueue_procedure) atms->enqueue_procedure = enqueue_procedure;
}

bool true_node(NodePtr node) {
    auto atms = node->atms.lock();
    if (!atms || node->label.empty()) return false;
    return node->label.front() == atms->empty_env;
}

bool in_node(NodePtr n, EnvPtr env) {
    if (env) {
        for (auto& le : n->label) {
            if (subset_env(le, env)) return true;
        }
        return false;
    }
    return !n->label.empty();
}

bool out_node(NodePtr n, EnvPtr env) {
    return !in_node(n, env);
}

bool node_consistent_with(NodePtr n, EnvPtr env) {
    for (auto& le : n->label) {
        auto u = union_env(le, env);
        if (u && !u->is_nogood()) return true;
    }
    return false;
}

NodePtr tms_create_node(AtmsPtr atms, std::any datum,
                        bool assumptionp, bool contradictoryp) {
    auto node = std::make_shared<TmsNode>();
    node->index = ++atms->node_counter;
    node->datum = datum;
    node->assumption = assumptionp;
    node->contradictory = contradictoryp;
    node->atms = atms;

    atms->nodes.push_back(node);
    if (contradictoryp) {
        atms->contradictions.push_back(node);
    }
    if (assumptionp) {
        atms->assumptions.push_back(node);
        auto env = create_env(atms, {node});
        node->label.push_back(env);
    }
    return node;
}

void assume_node(NodePtr node) {
    if (node->assumption) return;
    auto atms = node->atms.lock();
    if (!atms) return;

    if (atms->debugging) {
        std::cerr << "\nConverting " << node_string(node) << " into an assumption";
    }
    node->assumption = true;
    atms->assumptions.push_back(node);
    auto env = create_env(atms, {node});
    update({env}, node, nullptr);
}

void make_contradiction(NodePtr node) {
    auto atms = node->atms.lock();
    if (!atms) return;
    if (node->contradictory) return;

    node->contradictory = true;
    atms->contradictions.push_back(node);
    while (!node->label.empty()) {
        auto nogood = node->label.front();
        new_nogood(atms, nogood, std::any());
    }
}

JustPtr justify_node(const std::string& informant, NodePtr consequence,
                     const std::vector<NodePtr>& antecedents) {
    auto atms = consequence->atms.lock();
    if (!atms) return nullptr;

    auto just = std::make_shared<Just>();
    just->index = ++atms->just_counter;
    just->informant = informant;
    just->consequence = consequence;
    just->antecedents = antecedents;

    consequence->justs.push_back(just);
    for (auto& node : antecedents) {
        node->consequences.push_back(just);
    }
    atms->justs.push_back(just);

    if (atms->debugging) {
        std::cerr << "\nJustifying " << node_string(consequence)
                  << " in terms of " << informant << " on [";
        for (size_t i = 0; i < antecedents.size(); i++) {
            if (i > 0) std::cerr << ", ";
            std::cerr << node_string(antecedents[i]);
        }
        std::cerr << "]";
    }

    propagate(just, nullptr, {atms->empty_env});
    return just;
}

void nogood_nodes(const std::string& informant, const std::vector<NodePtr>& nodes) {
    if (nodes.empty()) return;
    auto atms = nodes[0]->atms.lock();
    if (!atms) return;
    justify_node(informant, atms->contra_node, nodes);
}

// ================================================================
// Label Updating
// ================================================================

void propagate(JustPtr just, NodePtr antecedent, std::vector<EnvPtr> envs) {
    auto new_envs = weave(antecedent, envs, just->antecedents);
    if (!new_envs.empty()) {
        update(new_envs, just->consequence, just);
    }
}

void update(std::vector<EnvPtr> new_envs, NodePtr consequence, JustPtr just) {
    auto atms = consequence->atms.lock();
    if (!atms) return;

    // If consequence is contradictory, register all new envs as nogood
    if (consequence->contradictory) {
        for (auto& env : new_envs) {
            new_nogood(atms, env, just ? std::any(just) : std::any());
        }
        return;
    }

    // Update label
    new_envs = update_label(consequence, new_envs);
    if (new_envs.empty()) return;

    // Enqueue rules
    if (atms->enqueue_procedure) {
        for (auto& rule : consequence->rules) {
            atms->enqueue_procedure(rule);
        }
        consequence->rules.clear();
    }

    // Propagate to consequences
    for (auto& supported_just : consequence->consequences) {
        propagate(supported_just, consequence, new_envs);

        // Filter out envs that were removed from label during propagation
        new_envs.erase(
            std::remove_if(new_envs.begin(), new_envs.end(),
                [&consequence](const EnvPtr& e) {
                    return std::find(consequence->label.begin(),
                                     consequence->label.end(), e)
                           == consequence->label.end();
                }),
            new_envs.end());
        if (new_envs.empty()) return;
    }
}

std::vector<EnvPtr> update_label(NodePtr node, std::vector<EnvPtr> new_envs) {
    auto& envs = node->label;

    for (size_t i = 0; i < new_envs.size(); i++) {
        if (!new_envs[i]) continue;
        bool added = true;
        for (size_t j = 0; j < envs.size(); j++) {
            if (!envs[j] || !new_envs[i]) continue;
            auto cmp = compare_env(new_envs[i], envs[j]);
            if (cmp == EnvCompare::EQ || cmp == EnvCompare::S21) {
                // New env is superset or equal -> discard new
                new_envs[i] = nullptr;
                added = false;
                break;
            } else if (cmp == EnvCompare::S12) {
                // New env is subset -> remove old
                auto& old_env = envs[j];
                auto it = std::find(old_env->nodes.begin(), old_env->nodes.end(), node);
                if (it != old_env->nodes.end()) old_env->nodes.erase(it);
                envs[j] = nullptr;
            }
        }
        if (added && new_envs[i]) {
            envs.push_back(new_envs[i]);
        }
    }

    // Clean up nulls from new_envs
    new_envs.erase(std::remove(new_envs.begin(), new_envs.end(), nullptr),
                   new_envs.end());
    // Register node in new envs
    for (auto& ne : new_envs) {
        ne->nodes.push_back(node);
    }
    // Clean up nulls from label
    envs.erase(std::remove(envs.begin(), envs.end(), nullptr), envs.end());
    return new_envs;
}

std::vector<EnvPtr> weave(NodePtr antecedent, std::vector<EnvPtr> envs,
                          const std::vector<NodePtr>& antecedents) {
    envs = std::vector<EnvPtr>(envs); // copy
    for (auto& node : antecedents) {
        if (node == antecedent) continue; // skip already-reflected antecedent

        std::vector<EnvPtr> new_envs;
        for (auto& env : envs) {
            if (!env) continue;
            for (auto& node_env : node->label) {
                auto new_env = union_env(env, node_env);
                if (!new_env || new_env->is_nogood()) continue;

                bool subsumed = false;
                for (size_t k = 0; k < new_envs.size(); k++) {
                    if (!new_envs[k]) continue;
                    auto cmp = compare_env(new_env, new_envs[k]);
                    if (cmp == EnvCompare::EQ || cmp == EnvCompare::S21) {
                        subsumed = true;
                        break;
                    } else if (cmp == EnvCompare::S12) {
                        new_envs[k] = nullptr;
                    }
                }
                if (!subsumed) {
                    new_envs.push_back(new_env);
                }
            }
        }
        new_envs.erase(std::remove(new_envs.begin(), new_envs.end(), nullptr),
                       new_envs.end());
        envs = new_envs;
        if (envs.empty()) return {};
    }
    return envs;
}

bool in_antecedent(const std::vector<NodePtr>& nodes) {
    if (nodes.empty()) return true;
    auto atms = nodes[0]->atms.lock();
    if (!atms) return false;
    return weave_check(atms->empty_env, nodes);
}

bool weave_check(EnvPtr env, const std::vector<NodePtr>& nodes) {
    if (nodes.empty()) return true;
    auto& first = nodes[0];
    std::vector<NodePtr> rest(nodes.begin() + 1, nodes.end());
    for (auto& e : first->label) {
        auto new_env = union_env(e, env);
        if (new_env && !new_env->is_nogood()) {
            if (weave_check(new_env, rest)) return true;
        }
    }
    return false;
}

bool supporting_antecedent(const std::vector<NodePtr>& nodes, EnvPtr env) {
    for (auto& node : nodes) {
        if (!in_node(node, env)) return false;
    }
    return true;
}

void remove_node(NodePtr node) {
    if (!node->consequences.empty()) {
        throw std::runtime_error("Can't remove node with consequences");
    }
    auto atms = node->atms.lock();
    if (!atms) return;

    // Remove from atms nodes
    atms->nodes.erase(
        std::find(atms->nodes.begin(), atms->nodes.end(), node));

    // Remove from antecedent consequences
    for (auto& just : node->justs) {
        for (auto& ant : just->antecedents) {
            auto& cons = ant->consequences;
            auto it = std::find(cons.begin(), cons.end(), just);
            if (it != cons.end()) cons.erase(it);
        }
    }

    // Remove from env nodes
    for (auto& env : node->label) {
        auto& en = env->nodes;
        auto it = std::find(en.begin(), en.end(), node);
        if (it != en.end()) en.erase(it);
    }
}

// ================================================================
// Environment Creation and Extension
// ================================================================

EnvPtr create_env(AtmsPtr atms, const std::vector<NodePtr>& assumptions) {
    auto e = std::make_shared<Env>();
    e->index = ++atms->env_counter;
    e->assumptions = assumptions;
    e->count = static_cast<int>(assumptions.size());

    insert_in_table(atms->env_table, e);
    set_env_contradictory(atms, e);
    return e;
}

EnvPtr union_env(EnvPtr e1, EnvPtr e2) {
    if (e1->count > e2->count) std::swap(e1, e2);
    for (auto& assume : e1->assumptions) {
        e2 = cons_env(assume, e2);
        if (e2->is_nogood()) return e2;
    }
    return e2;
}

EnvPtr cons_env(NodePtr assumption, EnvPtr env) {
    auto nassumes = env->assumptions;
    ordered_insert(nassumes, assumption,
        [](const NodePtr& a, const NodePtr& b) {
            return assumption_order(a, b);
        });

    auto existing = lookup_env(nassumes);
    if (existing) return existing;

    auto atms = assumption->atms.lock();
    if (!atms) return nullptr;
    return create_env(atms, nassumes);
}

EnvPtr find_or_make_env(const std::vector<NodePtr>& assumptions, AtmsPtr atms) {
    if (assumptions.empty()) return atms->empty_env;
    auto existing = lookup_env(assumptions);
    if (existing) return existing;
    return create_env(atms, assumptions);
}

EnvPtr lookup_env(const std::vector<NodePtr>& assumes) {
    if (assumes.empty()) return nullptr;
    auto atms = assumes[0]->atms.lock();
    if (!atms) return nullptr;

    int count = static_cast<int>(assumes.size());
    auto it = atms->env_table.find(count);
    if (it == atms->env_table.end()) return nullptr;

    for (auto& env : it->second) {
        if (env->assumptions == assumes) return env;
    }
    return nullptr;
}

// ================================================================
// Env Tables
// ================================================================

void insert_in_table(std::map<int, std::vector<EnvPtr>>& table, EnvPtr env) {
    table[env->count].push_back(env);
}

bool subset_env(EnvPtr e1, EnvPtr e2) {
    if (e1 == e2) return true;
    if (e1->count > e2->count) return false;
    for (auto& a : e1->assumptions) {
        if (std::find(e2->assumptions.begin(), e2->assumptions.end(), a)
            == e2->assumptions.end()) {
            return false;
        }
    }
    return true;
}

EnvCompare compare_env(EnvPtr e1, EnvPtr e2) {
    if (e1 == e2) return EnvCompare::EQ;
    if (e1->count < e2->count) {
        if (subset_env(e1, e2)) return EnvCompare::S12;
    } else if (e1->count > e2->count) {
        if (subset_env(e2, e1)) return EnvCompare::S21;
    } else {
        // Same count but different - check both directions (for equal-sized sets)
        // If same count and not eq, they can't be proper subsets
    }
    return EnvCompare::DISJOINT;
}

// ================================================================
// Nogood Processing
// ================================================================

void new_nogood(AtmsPtr atms, EnvPtr cenv, std::any just) {
    if (atms->debugging) {
        std::cerr << "\n  " << cenv->to_string() << " new minimal nogood.";
    }
    cenv->set_nogood(just);
    remove_env_from_labels(cenv, atms);
    insert_in_table(atms->nogood_table, cenv);

    int count = cenv->count;

    // Remove supersets from nogood_table
    for (auto& [sz, bucket] : atms->nogood_table) {
        if (sz > count) {
            bucket.erase(
                std::remove_if(bucket.begin(), bucket.end(),
                    [&cenv](const EnvPtr& old) {
                        return subset_env(cenv, old);
                    }),
                bucket.end());
        }
    }

    // Mark supersets in env_table as nogood
    for (auto& [sz, bucket] : atms->env_table) {
        if (sz > count) {
            for (auto& old : bucket) {
                if (!old->is_nogood() && subset_env(cenv, old)) {
                    old->set_nogood(cenv);
                    remove_env_from_labels(old, atms);
                }
            }
        }
    }
}

void set_env_contradictory(AtmsPtr atms, EnvPtr env) {
    if (env->is_nogood()) return;
    int count = env->count;
    for (auto& [sz, bucket] : atms->nogood_table) {
        if (sz > count) return; // Larger nogoods can't be subsets
        for (auto& cenv : bucket) {
            if (subset_env(cenv, env)) {
                env->set_nogood(cenv);
                return;
            }
        }
    }
}

void remove_env_from_labels(EnvPtr env, AtmsPtr atms) {
    if (atms->enqueue_procedure) {
        for (auto& rule : env->rules) {
            atms->enqueue_procedure(rule);
        }
        env->rules.clear();
    }
    for (auto& node : env->nodes) {
        node->label.erase(
            std::find(node->label.begin(), node->label.end(), env));
    }
}

// ================================================================
// Interpretation Construction
// ================================================================

std::vector<EnvPtr> interpretations(AtmsPtr atms,
    const std::vector<std::vector<NodePtr>>& choice_sets,
    const std::vector<NodePtr>& defaults) {

    if (atms->debugging) {
        std::cerr << "\n Constructing interpretations depth-first...";
    }

    std::vector<EnvPtr> solutions;

    // Convert choice_sets to lists of envs
    std::vector<std::vector<EnvPtr>> env_choice_sets;
    for (auto& alt_set : choice_sets) {
        std::vector<EnvPtr> envs;
        for (auto& alt : alt_set) {
            for (auto& e : alt->label) {
                envs.push_back(e);
            }
        }
        env_choice_sets.push_back(envs);
    }

    // DFS from first choice set
    if (!env_choice_sets.empty()) {
        for (auto& choice : env_choice_sets[0]) {
            get_depth_solutions1(choice, env_choice_sets, 1, solutions);
        }
    }

    solutions.erase(std::remove(solutions.begin(), solutions.end(), nullptr),
                    solutions.end());

    if (solutions.empty()) {
        if (!choice_sets.empty()) return {};
        solutions.push_back(atms->empty_env);
    }

    // Extend via defaults
    if (!defaults.empty()) {
        auto old_solutions = solutions;
        solutions.clear();
        for (auto& sol : old_solutions) {
            extend_via_defaults(sol, defaults, 0, defaults, solutions);
        }
    }

    solutions.erase(std::remove(solutions.begin(), solutions.end(), nullptr),
                    solutions.end());
    return solutions;
}

void get_depth_solutions1(EnvPtr solution,
    const std::vector<std::vector<EnvPtr>>& choice_sets,
    size_t index,
    std::vector<EnvPtr>& solutions) {

    if (index >= choice_sets.size()) {
        // Check minimality
        for (size_t i = 0; i < solutions.size(); i++) {
            if (!solutions[i]) continue;
            auto cmp = compare_env(solutions[i], solution);
            if (cmp == EnvCompare::EQ || cmp == EnvCompare::S12) return;
            if (cmp == EnvCompare::S21) solutions[i] = nullptr;
        }
        solutions.push_back(solution);
        return;
    }

    if (solution->is_nogood()) return;

    for (auto& choice : choice_sets[index]) {
        auto new_solution = union_env(solution, choice);
        if (new_solution && !new_solution->is_nogood()) {
            get_depth_solutions1(new_solution, choice_sets, index + 1, solutions);
        }
    }
}

void extend_via_defaults(EnvPtr solution,
    const std::vector<NodePtr>& remaining, size_t idx,
    const std::vector<NodePtr>& original,
    std::vector<EnvPtr>& solutions) {

    if (idx >= remaining.size()) {
        // Check if already in solutions
        for (auto& s : solutions) {
            if (s == solution) return;
        }
        // Check if any unadded default could still be added
        for (auto& def : original) {
            if (std::find(solution->assumptions.begin(),
                          solution->assumptions.end(), def)
                != solution->assumptions.end()) continue;
            auto test = cons_env(def, solution);
            if (!test->is_nogood()) return; // Could still be added
        }
        solutions.push_back(solution);
        return;
    }

    auto new_solution = cons_env(remaining[idx], solution);
    if (!new_solution->is_nogood()) {
        extend_via_defaults(new_solution, remaining, idx + 1, original, solutions);
    }
    extend_via_defaults(solution, remaining, idx + 1, original, solutions);
}

// ================================================================
// Explanation Generation
// ================================================================

static bool explain_node_1(EnvPtr env, NodePtr node,
    std::vector<NodePtr>& queued_nodes,
    std::vector<Explanation>& explanation,
    std::vector<Explanation>& result) {

    // Cycle prevention
    if (std::find(queued_nodes.begin(), queued_nodes.end(), node)
        != queued_nodes.end()) return false;

    // Assumption in env
    if (node->assumption &&
        std::find(env->assumptions.begin(), env->assumptions.end(), node)
        != env->assumptions.end()) {
        result = explanation;
        result.push_back({"ASSUME", node, nullptr});
        return true;
    }

    // Already explained
    for (auto& expl : explanation) {
        if (expl.just && expl.just->consequence == node) {
            result = explanation;
            return true;
        }
    }

    queued_nodes.push_back(node);
    for (auto& just : node->justs) {
        // Check all antecedents are in env
        bool all_in = true;
        for (auto& a : just->antecedents) {
            if (!in_node(a, env)) { all_in = false; break; }
        }
        if (!all_in) continue;

        auto new_explanation = explanation;
        bool ok = true;
        for (auto& a : just->antecedents) {
            std::vector<Explanation> sub_result;
            if (!explain_node_1(env, a, queued_nodes, new_explanation, sub_result)) {
                ok = false; break;
            }
            new_explanation = sub_result;
        }
        if (ok) {
            result = new_explanation;
            result.push_back({"", nullptr, just});
            queued_nodes.pop_back();
            return true;
        }
    }
    queued_nodes.pop_back();
    return false;
}

std::vector<Explanation> explain_node(NodePtr node, EnvPtr env) {
    std::vector<NodePtr> queued;
    std::vector<Explanation> explanation, result;
    explain_node_1(env, node, queued, explanation, result);
    return result;
}

// ================================================================
// Printing
// ================================================================

void why_node(NodePtr node, std::ostream& stream, const std::string& prefix) {
    std::string datum_str;
    try { datum_str = std::any_cast<std::string>(node->datum); }
    catch (...) { datum_str = "?"; }

    stream << "\n<" << prefix << datum_str << ",{";
    for (size_t i = 0; i < node->label.size(); i++) {
        if (i > 0) stream << ",";
        stream << env_string(node->label[i]);
    }
    stream << "}>";
}

void why_nodes(AtmsPtr atms, std::ostream& stream) {
    for (auto it = atms->nodes.rbegin(); it != atms->nodes.rend(); ++it) {
        why_node(*it, stream);
    }
}

void node_justifications(NodePtr node, std::ostream& stream) {
    stream << "\n For " << node_string(node) << ":";
    for (auto& j : node->justs) {
        print_justification(j, stream);
    }
}

void print_justification(JustPtr j, std::ostream& stream) {
    stream << "\n  " << j->informant << ", ";
    for (auto& a : j->antecedents) {
        why_node(a, stream, "     ");
    }
}

EnvPtr e(AtmsPtr atms, int n) {
    for (auto& [sz, bucket] : atms->env_table) {
        for (auto& env : bucket) {
            if (env->index == n) return env;
        }
    }
    return nullptr;
}

void print_env(EnvPtr e, std::ostream& stream) {
    stream << "\n" << e->to_string() << ":"
           << (e->is_nogood() ? "* " : " ")
           << env_string(e);
}

std::string env_string(EnvPtr e) {
    if (!e) return "{}";
    std::vector<std::string> strings;
    for (auto& a : e->assumptions) {
        strings.push_back(node_string(a));
    }
    std::sort(strings.begin(), strings.end());

    std::ostringstream ss;
    ss << "{";
    for (size_t i = 0; i < strings.size(); i++) {
        if (i > 0) ss << ",";
        ss << strings[i];
    }
    ss << "}";
    return ss.str();
}

void print_nogoods(AtmsPtr atms, std::ostream& stream) {
    print_env_table(atms->nogood_table, stream);
}

void print_envs(AtmsPtr atms, std::ostream& stream) {
    print_env_table(atms->env_table, stream);
}

void print_env_table(const std::map<int, std::vector<EnvPtr>>& table,
                     std::ostream& stream) {
    for (auto& [sz, bucket] : table) {
        for (auto& env : bucket) {
            print_env(env, stream);
        }
    }
}

void print_atms_statistics(AtmsPtr atms) {
    std::cout << "\n For env table:";
    for (auto& [sz, bucket] : atms->env_table) {
        std::cout << "\n   Length " << sz << ", " << bucket.size();
    }
    std::cout << "\n For nogood table:";
    for (auto& [sz, bucket] : atms->nogood_table) {
        std::cout << "\n   Length " << sz << ", " << bucket.size();
    }
}
