// -*- C++ -*-

/// JSAINT - Implementation
/// Converted from jsaint.lisp, jsrules.lisp, jsops.lisp

#include "jsaint.h"
#include <iostream>
#include <algorithm>

// ================================================================
// Global JSAINT
// ================================================================
JsaintPtr global_jsaint = nullptr;

// ================================================================
// JSAINT interface
// ================================================================

JsaintPtr create_jsaint(const std::string& title, const std::string& problem,
    bool debugging, int max_tasks) {

    auto js = std::make_shared<Jsaint>();
    js->title = title;
    js->problem = problem;
    js->debugging = debugging;
    js->max_tasks = max_tasks;
    js->jtre = create_jtre("JTRE of " + title);

    in_jtre(js->jtre);
    // Set contradiction handler
    change_jtms(js->jtre->jtms, nullptr, nullptr, std::nullopt, std::nullopt,
        [](JtmsPtr jtms, std::vector<NodePtr> contras) {
            ask_user_handler(jtms, contras);
        });

    use_jsaint(js);
    return js;
}

void use_jsaint(JsaintPtr js) {
    global_jsaint = js;
}

void change_jsaint(JsaintPtr js,
    std::optional<bool> debugging,
    std::optional<int> max_tasks) {
    if (debugging.has_value()) js->debugging = debugging.value();
    if (max_tasks.has_value()) js->max_tasks = max_tasks.value();
}

// ================================================================
// Solving
// ================================================================

std::pair<SolveResult, std::string> run_jsaint(JsaintPtr js) {
    if (!js->solution.empty()) {
        if (js->solution == ":FAILED-PROBLEM")
            return {SolveResult::FAILED_PROBLEM, ""};
        if (js->solution == ":FAILED-EMPTY")
            return {SolveResult::FAILED_EMPTY, ""};
        return {SolveResult::SOLVED, js->solution};
    }

    if (js->n_subproblems > js->max_tasks) {
        return {SolveResult::TIME_OUT, ""};
    }

    while (true) {
        // Check for solution
        auto sol = fetch_solution(js->problem, js);
        if (!sol.empty()) {
            js->solution = sol;
            if (js->debugging) {
                std::cout << "\n " << js->title
                          << ": Solved original problem.";
            }
            return {SolveResult::SOLVED, sol};
        }

        // Check for failure
        std::string failure_sig = "(Failed (Integrate " + js->problem + "))";
        if (in_fact(SExpr(failure_sig), js->jtre)) {
            if (js->debugging) {
                std::cout << "\n " << js->title
                          << ": Failed on original problem.";
            }
            js->solution = ":FAILED-PROBLEM";
            return {SolveResult::FAILED_PROBLEM, ""};
        }

        // Check empty agenda
        if (js->agenda.empty()) {
            if (js->debugging) {
                std::cout << "\n " << js->title << ": Agenda empty.";
            }
            js->solution = ":FAILED-EMPTY";
            return {SolveResult::FAILED_EMPTY, ""};
        }

        // Process next subproblem
        auto item = js->agenda.front().second;
        js->agenda.pop_front();
        process_subproblem(item, js);
    }
}

void process_subproblem(const std::string& item, JsaintPtr js) {
    if (js->debugging) {
        std::cout << "\n  Trying to solve " << item << ".";
    }

    open_subproblem(item, js);

    auto sol = fetch_solution(item, js);
    if (!sol.empty()) {
        if (js->debugging) {
            std::cout << "\n    ..already solved.";
        }
        return;
    }

    // Check if already expanded
    auto expanded = fetch(SExpr(std::string("(AND-SUBGOALS " + item + " ?x)")),
                          js->jtre);
    if (!expanded.empty()) {
        for (auto& e : expanded) {
            if (in_fact(e, js->jtre)) {
                if (js->debugging) {
                    std::cout << "\n   ..already expanded.";
                }
                return;
            }
        }
    }

    // Get suggestions
    auto suggestions = fetch(
        SExpr(std::string("(SUGGEST-FOR " + item + " ?operator)")),
        js->jtre);
    std::vector<std::string> suggestion_list;
    for (auto& s : suggestions) {
        if (in_fact(s, js->jtre)) {
            std::string s_str;
            try { s_str = std::any_cast<std::string>(s); } catch (...) { continue; }
            // Extract operator from (SUGGEST-FOR item operator)
            // Simplified: queue it
            queue_problem("(try " + s_str + ")", item, js);
            suggestion_list.push_back(s_str);
        }
    }

    // Assert OR-SUBGOALS
    assert_fact(
        SExpr(std::string("(OR-SUBGOALS " + item + " (" +
            [&]() {
                std::string r;
                for (size_t i = 0; i < suggestion_list.size(); i++) {
                    if (i > 0) r += " ";
                    r += "(try " + suggestion_list[i] + ")";
                }
                return r;
            }() + "))")),
        {SExpr(std::string("OR-SUBGOALS"))},
        js->jtre);

    run_rules(js->jtre);
}

void open_subproblem(const std::string& item, JsaintPtr js) {
    assert_fact(SExpr(std::string("(expanded " + item + ")")),
        {SExpr(std::string("EXPAND-AGENDA-ITEM"))}, js->jtre);
    assume_fact(SExpr(std::string("(open " + item + ")")),
        "EXPAND-AGENDA-ITEM", js->jtre);
    run_rules(js->jtre);
}

void queue_problem(const std::string& problem, const std::string& parent,
    JsaintPtr js) {
    int difficulty = estimate_difficulty(problem);
    if (js->debugging) {
        std::cout << "\n   Queueing " << problem
                  << ", difficulty = " << difficulty;
    }

    auto entry = std::make_pair(difficulty, problem);
    // Insert in sorted order (by difficulty)
    auto it = std::lower_bound(js->agenda.begin(), js->agenda.end(), entry,
        [](const auto& a, const auto& b) { return a.first < b.first; });
    js->agenda.insert(it, entry);
}

std::string fetch_solution(const std::string& problem, JsaintPtr js) {
    auto solutions = fetch(
        SExpr(std::string("(SOLUTION-OF " + problem + " ?answer)")),
        js->jtre);
    for (auto& sol : solutions) {
        if (in_fact(sol, js->jtre)) {
            std::string s;
            try { s = std::any_cast<std::string>(sol); } catch (...) { continue; }
            // Extract answer from (SOLUTION-OF problem answer)
            // Simplified extraction
            return s;
        }
    }
    return "";
}

void explain_result(JsaintPtr js) {
    if (!js) js = global_jsaint;

    if (js->solution.empty()) {
        std::cout << "\n Problem not solved yet.";
    } else if (js->solution == ":FAILED-PROBLEM") {
        std::cout << "\n Failed to find a solution.";
    } else if (js->solution == ":FAILED-EMPTY") {
        std::cout << "\n Ran out of things to do.";
    } else {
        std::cout << "\n Solved the problem: " << js->solution;
    }
}

// ================================================================
// Difficulty estimation
// ================================================================

int estimate_difficulty(const std::string& problem) {
    return max_depth(problem) + count_symbols(problem);
}

int count_symbols(const std::string& pr) {
    int count = 0;
    bool in_symbol = false;
    for (char c : pr) {
        if (c == ' ' || c == '(' || c == ')') {
            in_symbol = false;
        } else if (!in_symbol) {
            count++;
            in_symbol = true;
        }
    }
    return count;
}

int max_depth(const std::string& pr) {
    int max_d = 0, current = 0;
    for (char c : pr) {
        if (c == '(') {
            current++;
            if (current > max_d) max_d = current;
        } else if (c == ')') {
            current--;
        }
    }
    return max_d;
}

// ================================================================
// Integration operators (from jsops.lisp)
// ================================================================

void load_jsaint_rules(JsaintPtr js) {
    // The JSAINT rules from jsrules.lisp handle:
    // - AND-SUBGOALS expansion
    // - OR-SUBGOALS expansion
    // - PARENT-OF tracking
    // - SOLUTION-OF detection
    // - failure propagation
    //
    // In a full implementation, these would be registered via insert_rule.
    // Here we provide the structural framework.
}

void load_jsaint_operators(JsaintPtr js) {
    // Register standard integration operators from jsops.lisp

    // Integral-of-Constant: int(c, x) = c*x when c doesn't contain x
    auto op1 = std::make_shared<IntegrationOp>();
    op1->name = "Integral-of-Constant";
    op1->trigger_pattern = "(Integral ?t ?var)";
    op1->test = "(not (occurs-in? ?var ?t))";
    op1->result = "(* ?t ?var)";

    // Integral-of-Self: int(x, x) = x^2/2
    auto op2 = std::make_shared<IntegrationOp>();
    op2->name = "Integral-of-Self";
    op2->trigger_pattern = "(Integral ?exp ?exp)";
    op2->result = "(/ (expt ?exp 2) 2)";

    // Integral-of-Sum: int(a+b, x) = int(a,x) + int(b,x)
    auto op3 = std::make_shared<IntegrationOp>();
    op3->name = "Integral-of-Sum";
    op3->trigger_pattern = "(Integral (+ ?t1 ?t2) ?var)";
    op3->subproblems = {
        {"?int1", "(Integrate (Integral ?t1 ?var))"},
        {"?int2", "(Integrate (Integral ?t2 ?var))"}
    };
    op3->result = "(+ ?int1 ?int2)";

    // Move-Constant-outside: int(c*f, x) = c * int(f, x)
    auto op4 = std::make_shared<IntegrationOp>();
    op4->name = "Move-Constant-outside";
    op4->trigger_pattern = "(Integral (* ?const ?nonconst) ?var)";
    op4->test = "(and (not (occurs-in? ?var ?const)) (occurs-in? ?var ?nonconst))";
    op4->subproblems = {
        {"?int", "(Integrate (Integral ?nonconst ?var))"}
    };
    op4->result = "(* ?const ?int)";

    // Integral-of-SQR: int(x^2, x) = x^3/3
    auto op5 = std::make_shared<IntegrationOp>();
    op5->name = "Integral-of-SQR";
    op5->trigger_pattern = "(Integral (sqr ?var) ?var)";
    op5->result = "(/ (expt ?var 3) 3)";

    // Integral-of-polyterm: int(x^n, x) = x^(n+1)/(n+1) when n != -1
    auto op6 = std::make_shared<IntegrationOp>();
    op6->name = "Integral-of-polyterm";
    op6->trigger_pattern = "(Integral (expt ?var ?n) ?var)";
    op6->test = "(not (same-constant? ?n -1))";
    op6->result = "(/ (expt ?var (+ 1 ?n)) (+ 1 ?n))";

    // Simple-e-integral: int(e^x, x) = e^x
    auto op7 = std::make_shared<IntegrationOp>();
    op7->name = "Simple-e-integral";
    op7->trigger_pattern = "(Integral (expt %e ?var) ?var)";
    op7->result = "(expt %e ?var)";

    // e-integral: int(e^(ax), x) = e^(ax)/a
    auto op8 = std::make_shared<IntegrationOp>();
    op8->name = "e-integral";
    op8->trigger_pattern = "(Integral (expt %e (* ?a ?var)) ?var)";
    op8->test = "(not (occurs-in? ?var ?a))";
    op8->result = "(/ (expt %e (* ?a ?var)) ?a)";

    // sin-integral: int(sin(ax), x) = -cos(ax)/a
    auto op9 = std::make_shared<IntegrationOp>();
    op9->name = "sin-integral";
    op9->trigger_pattern = "(Integral (sin (* ?a ?var)) ?var)";
    op9->test = "(not (occurs-in? ?var ?a))";
    op9->result = "(- (/ (cos (* ?a ?var)) ?a))";

    // cos-integral: int(cos(ax), x) = sin(ax)/a
    auto op10 = std::make_shared<IntegrationOp>();
    op10->name = "cos-integral";
    op10->trigger_pattern = "(Integral (cos (* ?a ?var)) ?var)";
    op10->test = "(not (occurs-in? ?var ?a))";
    op10->result = "(/ (sin (* ?a ?var)) ?a)";

    // Log-Integral: int(ln(x), x) = x*ln(x) - x
    auto op11 = std::make_shared<IntegrationOp>();
    op11->name = "Log-Integral";
    op11->trigger_pattern = "(Integral (log ?var %e) ?var)";
    op11->result = "(- (* ?var (log ?var %e)) ?var)";

    // Store operators in js for reference
    // In a full implementation, each operator would generate
    // JTRE rules via defIntegration macro expansion.
}

void register_integration_operator(IntegrationOpPtr op, JsaintPtr js) {
    if (!js) js = global_jsaint;
    // In a full implementation, this would expand the operator
    // into JTRE rules similar to how defIntegration works.
}

// ================================================================
// Display
// ================================================================

void show_problem(const std::string& pr, JsaintPtr js) {
    if (!js) js = global_jsaint;
    std::cout << "\n" << pr << ":: (" << estimate_difficulty(pr) << ")";

    auto parents = fetch(SExpr(std::string("(parent-of " + pr + " ?x ?type)")),
                         js->jtre);
    if (!parents.empty()) {
        std::cout << "\n Parent(s): ";
        for (auto& p : parents) {
            std::string s;
            try { s = std::any_cast<std::string>(p); } catch (...) { continue; }
            std::cout << "\n   " << s;
        }
    } else {
        std::cout << "\n No parents found.";
    }

    auto sol = fetch_solution(pr, js);
    if (!sol.empty()) {
        std::cout << "\n Solved, solution = " << sol;
    }
}

void show_ao_graph(JsaintPtr js) {
    if (!js) js = global_jsaint;
    auto problems = fetch(SExpr(std::string("(expanded ?x)")), js->jtre);
    for (auto& p : problems) {
        std::string s;
        try { s = std::any_cast<std::string>(p); } catch (...) { continue; }
        show_problem(s, js);
    }
}

// ================================================================
// Convenience
// ================================================================

std::pair<SolveResult, std::string> solve_integral(const std::string& integral,
    const std::string& title, bool debugging, int max_tasks) {

    use_jsaint(create_jsaint(title, integral, debugging, max_tasks));
    queue_problem(global_jsaint->problem, "", global_jsaint);

    in_jtre(global_jsaint->jtre);
    load_jsaint_rules(global_jsaint);
    load_jsaint_operators(global_jsaint);

    return run_jsaint(global_jsaint);
}

void try_jsaint(const std::string& problem, const std::string& title) {
    auto [result, sol] = solve_integral(problem, title, true);
    switch (result) {
        case SolveResult::SOLVED:
            std::cout << "\nSolved: " << sol; break;
        case SolveResult::FAILED_PROBLEM:
            std::cout << "\nFailed on the problem."; break;
        case SolveResult::FAILED_EMPTY:
            std::cout << "\nRan out of things to do."; break;
        case SolveResult::TIME_OUT:
            std::cout << "\nTimed out."; break;
    }
}
