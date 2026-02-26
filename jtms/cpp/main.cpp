// -*- C++ -*-

/// JTMS C++ - Main entry point
/// Equivalent to jtre.lisp loader plus test dispatcher

#include "jtms.h"
#include "jtre.h"
#include "match.h"
#include "simplify.h"
#include "jsaint.h"
#include <iostream>
#include <string>
#include <cstring>

// Forward declarations from jtms_ex.cpp
void ex1();
void ex2();
void ex3();
void shakedown_jtre();

// Forward declarations from jqueens.cpp
void test_queens(int from, int to);

// Forward declarations from jsaint.cpp
void try_jsaint(const std::string& problem, const std::string& title);

void print_usage() {
    std::cout << "JTMS C++ - Justification-based Truth Maintenance System\n"
              << "Based on 'Building Problem Solvers' by Forbus & de Kleer\n\n"
              << "Usage: jtms_test <command> [args...]\n\n"
              << "Commands:\n"
              << "  ex1              JTMS example 1: basic justification network\n"
              << "  ex2              JTMS example 2: enable/retract assumptions\n"
              << "  ex3              JTMS example 3: contradiction handling\n"
              << "  jtre             JTRE shakedown test\n"
              << "  queens N         Solve N-queens puzzle\n"
              << "  queens N M       Solve queens puzzles from N to M\n"
              << "  jsaint EXPR      Solve an integral expression\n"
              << "  all              Run all tests\n";
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        print_usage();
        return 0;
    }

    std::string cmd = argv[1];

    if (cmd == "ex1") {
        std::cout << "=== JTMS Example 1 ===" << std::endl;
        ex1();
    }
    else if (cmd == "ex2") {
        std::cout << "=== JTMS Example 2 ===" << std::endl;
        ex2();
    }
    else if (cmd == "ex3") {
        std::cout << "=== JTMS Example 3 ===" << std::endl;
        ex3();
    }
    else if (cmd == "jtre") {
        std::cout << "=== JTRE Shakedown Test ===" << std::endl;
        shakedown_jtre();
    }
    else if (cmd == "queens") {
        int from = 4, to = 4;
        if (argc >= 3) from = to = std::stoi(argv[2]);
        if (argc >= 4) to = std::stoi(argv[3]);
        std::cout << "=== N-Queens (" << from << " to " << to << ") ===" << std::endl;
        test_queens(from, to);
    }
    else if (cmd == "jsaint") {
        if (argc < 3) {
            std::cout << "Usage: jtms_test jsaint \"(Integral ...)\"" << std::endl;
            return 1;
        }
        std::string problem = argv[2];
        std::cout << "=== JSAINT ===" << std::endl;
        try_jsaint(problem, "JSAINT Test");
    }
    else if (cmd == "all") {
        std::cout << "=== JTMS Example 1 ===" << std::endl;
        ex1();
        std::cout << "\n\n=== JTMS Example 2 ===" << std::endl;
        ex2();
        std::cout << "\n\n=== JTMS Example 3 ===" << std::endl;
        ex3();
        std::cout << "\n\n=== JTRE Shakedown Test ===" << std::endl;
        shakedown_jtre();
        std::cout << "\n\n=== N-Queens (4 to 8) ===" << std::endl;
        test_queens(4, 8);
    }
    else {
        std::cerr << "Unknown command: " << cmd << std::endl;
        print_usage();
        return 1;
    }

    std::cout << std::endl;
    return 0;
}
