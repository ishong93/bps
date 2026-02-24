// -*- C++ -*-

/// Main entry point for ATMS system tests
/// Converted from atre.lisp (loader)

#include "atms.h"
#include "atre.h"
#include "planner.h"
#include <iostream>
#include <string>

// Test function declarations (from atest.cpp)
void atms_test1();
void atms_test2();
void atms_test3();
void step_1();

// Test function declarations (from atret.cpp)
void atre_test1(bool debugging = true);
void atre_test2(bool debugging = true);
void atre_test3(bool debugging = true);
void atre_test4(bool debugging = true);
void atre_test4a();
void atre_test5(bool debugging = true);

int main(int argc, char* argv[]) {
    std::string test = "all";
    if (argc > 1) test = argv[1];

    if (test == "atms1" || test == "all") {
        std::cout << "=== ATMS Test 1 ===" << std::endl;
        atms_test1();
        std::cout << std::endl;
    }

    if (test == "atms2" || test == "all") {
        std::cout << "=== ATMS Test 2 ===" << std::endl;
        atms_test1(); // must run test1 first
        atms_test2();
        std::cout << std::endl;
    }

    if (test == "atms3" || test == "all") {
        std::cout << "=== ATMS Test 3 ===" << std::endl;
        atms_test1();
        atms_test3();
        std::cout << std::endl;
    }

    if (test == "step1" || test == "all") {
        std::cout << "=== Step 1 (de Kleer example) ===" << std::endl;
        step_1();
        std::cout << std::endl;
    }

    if (test == "atre1" || test == "all") {
        std::cout << "\n=== ATRE Test 1 ===" << std::endl;
        atre_test1();
        std::cout << std::endl;
    }

    if (test == "atre2" || test == "all") {
        std::cout << "\n=== ATRE Test 2 ===" << std::endl;
        atre_test2();
        std::cout << std::endl;
    }

    if (test == "atre3" || test == "all") {
        std::cout << "\n=== ATRE Test 3 ===" << std::endl;
        atre_test3();
        std::cout << std::endl;
    }

    if (test == "atre4" || test == "all") {
        std::cout << "\n=== ATRE Test 4 ===" << std::endl;
        atre_test4();
        std::cout << std::endl;
    }

    if (test == "atre5" || test == "all") {
        std::cout << "\n=== ATRE Test 5 ===" << std::endl;
        atre_test5();
        std::cout << std::endl;
    }

    if (test == "blocks") {
        std::cout << "\n=== Blocks World ===" << std::endl;
        auto plnpr = build_blocks_problem("Test",
            {"A", "B", "C"}, true);
        show_envisionment(plnpr);
        std::cout << std::endl;
    }

    return 0;
}
