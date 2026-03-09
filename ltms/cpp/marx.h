#pragma once

/// Marx Brothers Puzzle
/// Converted from marx.lisp
/// Copyright (c) 1996, Kenneth D. Forbus. All rights reserved.

#include "ldata.h"
#include "linter.h"
#include "dds.h"
#include <vector>
#include <string>
#include <functional>

extern const std::vector<std::string> marx_attributes;
extern const std::vector<std::string> marx_objects;

void marx_brothers();

std::vector<std::vector<std::any>> make_attribute_choice_sets(
    const std::vector<std::string>& attributes,
    const std::vector<std::string>& objects);

void solve_attribution_problem(
    const std::vector<std::string>& attributes,
    const std::vector<std::string>& objects,
    std::function<void()> load_constraints);

void show_attribute_solution(const std::vector<std::string>& attributes,
                             LTRE* ltre = nullptr);
