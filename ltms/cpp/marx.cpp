/// Marx Brothers Puzzle - Implementation
/// Converted from marx.lisp
/// Copyright (c) 1996, Kenneth D. Forbus. All rights reserved.

#include "marx.h"
#include "marxdata.h"
#include "funify.h"
#include <iostream>

const std::vector<std::string> marx_attributes = {
    "PLAYS-PIANO", "PLAYS-HARP",
    "SMOOTH-TALKER", "LIKES-GAMBLING", "LIKES-ANIMALS"
};

const std::vector<std::string> marx_objects = {
    "GROUCHO", "HARPO", "CHICO"
};

// ================================================================
// make_attribute_choice_sets
// ================================================================
// Each attribute can apply to exactly one object.
// Returns a vector of choice sets, where each choice set
// contains (attribute object) pairs.

std::vector<std::vector<std::any>> make_attribute_choice_sets(
    const std::vector<std::string>& attributes,
    const std::vector<std::string>& objects) {

    std::vector<std::vector<std::any>> choice_sets;
    for (const auto& attr : attributes) {
        std::vector<std::any> choices;
        for (const auto& obj : objects) {
            // Build the fact form "(ATTRIBUTE OBJECT)"
            std::string fact = "(" + attr + " " + obj + ")";
            choices.push_back(std::any(fact));
        }
        choice_sets.push_back(choices);
    }
    return choice_sets;
}

// ================================================================
// solve_attribution_problem
// ================================================================

void solve_attribution_problem(
    const std::vector<std::string>& attributes,
    const std::vector<std::string>& objects,
    std::function<void()> load_constraints) {

    in_ltre(create_ltre("Attribution Problem Scratchpad"));
    load_constraints();

    auto choice_sets = make_attribute_choice_sets(attributes, objects);
    dd_search(choice_sets, [&]() {
        show_attribute_solution(attributes);
    });
}

// ================================================================
// show_attribute_solution
// ================================================================

void show_attribute_solution(const std::vector<std::string>& attributes,
                             LTRE* ltre) {
    if (!ltre) ltre = current_ltre;
    std::cout << "\nSolution:";
    for (const auto& attribute : attributes) {
        std::string pattern = "(" + attribute + " ?object)";
        auto matches = fetch(pattern, ltre);
        for (const auto& match : matches) {
            if (is_true(std::any(match), ltre)) {
                std::cout << "\n  " << match;
            }
        }
    }
    std::cout << std::endl;
}

// ================================================================
// marx_brothers
// ================================================================

void marx_brothers() {
    solve_attribution_problem(marx_attributes, marx_objects, []() {
        load_marx_constraints();
    });
}
