// -*- C++ -*-

/// ATMS Test Code
/// Converted from atest.lisp

#include "atms.h"
#include <iostream>

// Global test variables
static AtmsPtr test_atms;
static NodePtr a, b, c, d, ef, f_node;

void atms_test1() {
    test_atms = create_atms("atms-test1", nullptr, true);
    a = tms_create_node(test_atms, std::string("A"));
    b = tms_create_node(test_atms, std::string("B"));
    c = tms_create_node(test_atms, std::string("C"));
    d = tms_create_node(test_atms, std::string("D"));
    ef = tms_create_node(test_atms, std::string("E"));
    f_node = tms_create_node(test_atms, std::string("F"));

    assume_node(a);
    assume_node(b);
    assume_node(c);
    justify_node("J1", d, {a, b});
    justify_node("J2", ef, {b, c});
    justify_node("J3", f_node, {d, ef});
}

void atms_test2() {
    justify_node("simpler", d, {a});
}

void atms_test3() {
    nogood_nodes("atms-test3", {a, b});
}

/// An example from de Kleer's paper by Gitchang
static NodePtr x_eq_1, y_eq_x, x_eq_z, y_eq_1, z_eq_1;

void step_1() {
    test_atms = create_atms("Step-1");
    a = tms_create_node(test_atms, std::string("A"));
    b = tms_create_node(test_atms, std::string("B"));
    c = tms_create_node(test_atms, std::string("C"));
    x_eq_1 = tms_create_node(test_atms, std::string("x=1"));
    y_eq_x = tms_create_node(test_atms, std::string("y=x"));
    x_eq_z = tms_create_node(test_atms, std::string("x=z"));
    y_eq_1 = tms_create_node(test_atms, std::string("y=1"));
    z_eq_1 = tms_create_node(test_atms, std::string("z=1"));

    assume_node(a);
    assume_node(b);
    assume_node(c);
    justify_node("j1", x_eq_1, {a});
    justify_node("j2", y_eq_x, {b});
    justify_node("j3", x_eq_z, {c});
    why_nodes(test_atms);
    print_envs(test_atms);

    std::cout << "\n\nNow register nogood{A,B}";
    nogood_nodes("NOGOOD", {a, b});
    why_nodes(test_atms);
    print_envs(test_atms);

    std::cout << "\n\nx=1, y=x => y=1";
    justify_node("j4", y_eq_1, {x_eq_1, y_eq_x});
    why_nodes(test_atms);
    print_envs(test_atms);

    std::cout << "\n\nWe have a premise z=1";
    justify_node("Premise", z_eq_1, {});
    why_nodes(test_atms);
    print_envs(test_atms);

    std::cout << "\n\nz=1, x=z => x=1";
    justify_node("j5", x_eq_1, {z_eq_1, x_eq_z});
    why_nodes(test_atms);
    print_envs(test_atms);
}

/*
Expected output matches the Lisp version:

The Contradiction is out.
A is in, under (E-2)
B is in, under (E-3)
C is in, under (E-4)
x=1 is in, under (E-2)
y=x is in, under (E-3)
x=z is in, under (E-4)
y=1 is out.
z=1 is out.
E-1: {}
E-2: {A}
E-3: {B}
E-4: {C}

Now register nogood{A,B}
...
z=1, x=z => x=1
...
y=1 is in, under (E-6)
...
E-6: {B, C}
*/
