// -*- C++ -*-

/// ATRE Test Code
/// Converted from atret.lisp

#include "atre.h"
#include <iostream>

/// Test 1: INTERN rules with assume!
/// Tests basic rule chaining: sentient-robot -> human -> mortal
void atre_test1(bool debugging = true) {
    auto atre = create_atre("Test ATRE", debugging);

    // In the original, rules are defined via macros.
    // Here we set up the same logic procedurally.

    // Rule: :INTERN ((implies ?ante ?conse) :VAR ?f1 ?ante)
    //   => (rassert! ?conse (:CE ?f1 ?ante))
    // This is registered via insert_rule for the "implies" dbclass.
    insert_rule(
        get_dbclass("implies", atre),
        // matcher
        [](const SExpr& form) -> std::tuple<bool, std::vector<std::any>, std::string> {
            // Match (implies ?ante ?conse)
            std::string s;
            try { s = std::any_cast<std::string>(form); } catch(...) {
                return {false, {}, ""};
            }
            // Simplified: check if starts with "(implies "
            if (s.substr(0, 9) != "(implies ") {
                return {false, {}, ""};
            }
            // Extract ante and conse from "(implies ANTE CONSE)"
            auto rest = s.substr(9, s.size() - 10); // remove "(implies " and ")"
            auto space = rest.find(") (");
            std::string ante, conse;
            if (space != std::string::npos) {
                ante = rest.substr(0, space + 1);
                conse = rest.substr(space + 2);
            } else {
                space = rest.find(' ');
                if (space == std::string::npos) return {false, {}, ""};
                ante = rest.substr(0, space);
                conse = rest.substr(space + 1);
            }
            return {true, {form, SExpr(ante), SExpr(conse)}, "INTERN"};
        },
        // body
        [atre](const std::vector<std::any>& args) {
            if (args.size() < 3) return;
            auto f1 = args[0]; // the implies fact
            std::string ante, conse;
            try { ante = std::any_cast<std::string>(args[1]); } catch(...) { return; }
            try { conse = std::any_cast<std::string>(args[2]); } catch(...) { return; }

            std::string f1_str;
            try { f1_str = std::any_cast<std::string>(f1); } catch(...) { f1_str = "?"; }

            // assert! ?conse (:CE ?f1 ?ante)
            std::vector<SExpr> just = {
                SExpr(std::string("CE")),
                f1,
                SExpr(ante)
            };
            assert_fact(SExpr(conse), just, atre);
        }
    );

    // assume! '(implies (sentient-robot Robbie) (Human Robbie)) :no-bias
    assume_fact(SExpr(std::string("(implies (sentient-robot Robbie) (Human Robbie))")),
                "no-bias", atre);
    run_rules(atre);

    // assume! '(implies (human Robbie) (mortal Robbie)) :sigh
    assume_fact(SExpr(std::string("(implies (human Robbie) (mortal Robbie))")),
                "sigh", atre);
    run_rules(atre);

    // assume! '(sentient-robot Robbie) :sort-of
    assume_fact(SExpr(std::string("(sentient-robot Robbie)")),
                "sort-of", atre);
    run_rules(atre);

    show_data(atre);
}

/// Test 2: INTERN with assert! (non-assumed implies)
void atre_test2(bool debugging = true) {
    auto atre = create_atre("Test ATRE", debugging);

    insert_rule(
        get_dbclass("implies", atre),
        [](const SExpr& form) -> std::tuple<bool, std::vector<std::any>, std::string> {
            std::string s;
            try { s = std::any_cast<std::string>(form); } catch(...) {
                return {false, {}, ""};
            }
            if (s.substr(0, 9) != "(implies ") return {false, {}, ""};
            auto rest = s.substr(9, s.size() - 10);
            auto space = rest.find(") (");
            std::string ante, conse;
            if (space != std::string::npos) {
                ante = rest.substr(0, space + 1);
                conse = rest.substr(space + 2);
            } else {
                space = rest.find(' ');
                if (space == std::string::npos) return {false, {}, ""};
                ante = rest.substr(0, space);
                conse = rest.substr(space + 1);
            }
            return {true, {form, SExpr(ante), SExpr(conse)}, "INTERN"};
        },
        [atre](const std::vector<std::any>& args) {
            if (args.size() < 3) return;
            auto f1 = args[0];
            std::string ante, conse;
            try { ante = std::any_cast<std::string>(args[1]); } catch(...) { return; }
            try { conse = std::any_cast<std::string>(args[2]); } catch(...) { return; }
            std::vector<SExpr> just = {SExpr(std::string("CE")), f1, SExpr(ante)};
            assert_fact(SExpr(conse), just, atre);
        }
    );

    // assert! (not assume!)
    assert_fact(SExpr(std::string("(implies (sentient-robot Robbie) (Human Robbie))")),
                {SExpr(std::string("no-bias"))}, atre);
    run_rules(atre);

    assert_fact(SExpr(std::string("(implies (human Robbie) (mortal Robbie))")),
                {SExpr(std::string("sigh"))}, atre);
    run_rules(atre);

    assume_fact(SExpr(std::string("(sentient-robot Robbie)")),
                "sort-of", atre);
    run_rules(atre);

    show_data(atre);
}

/// Test 3: IN rules
void atre_test3(bool debugging = true) {
    auto atre = create_atre("Test ATRE", debugging);

    insert_rule(
        get_dbclass("implies", atre),
        [](const SExpr& form) -> std::tuple<bool, std::vector<std::any>, std::string> {
            std::string s;
            try { s = std::any_cast<std::string>(form); } catch(...) {
                return {false, {}, ""};
            }
            if (s.substr(0, 9) != "(implies ") return {false, {}, ""};
            auto rest = s.substr(9, s.size() - 10);
            auto space = rest.find(") (");
            std::string ante, conse;
            if (space != std::string::npos) {
                ante = rest.substr(0, space + 1);
                conse = rest.substr(space + 2);
            } else {
                space = rest.find(' ');
                if (space == std::string::npos) return {false, {}, ""};
                ante = rest.substr(0, space);
                conse = rest.substr(space + 1);
            }
            return {true, {form, SExpr(ante), SExpr(conse)}, "IN"};
        },
        [atre](const std::vector<std::any>& args) {
            if (args.size() < 3) return;
            auto f1 = args[0];
            std::string ante, conse;
            try { ante = std::any_cast<std::string>(args[1]); } catch(...) { return; }
            try { conse = std::any_cast<std::string>(args[2]); } catch(...) { return; }
            std::vector<SExpr> just = {SExpr(std::string("CE")), f1, SExpr(ante)};
            assert_fact(SExpr(conse), just, atre);
        }
    );

    assert_fact(SExpr(std::string("(implies (sentient-robot Robbie) (Human Robbie))")),
                {SExpr(std::string("no-bias"))}, atre);
    run_rules(atre);

    assert_fact(SExpr(std::string("(implies (human Robbie) (mortal Robbie))")),
                {SExpr(std::string("sigh"))}, atre);
    run_rules(atre);

    assume_fact(SExpr(std::string("(sentient-robot Robbie)")),
                "sort-of", atre);
    run_rules(atre);

    show_data(atre);
}

/// Test 4: IMPLIED-BY rules
void atre_test4(bool debugging = true) {
    auto atre = create_atre("Test ATRE", debugging);

    insert_rule(
        get_dbclass("implies", atre),
        [](const SExpr& form) -> std::tuple<bool, std::vector<std::any>, std::string> {
            std::string s;
            try { s = std::any_cast<std::string>(form); } catch(...) {
                return {false, {}, ""};
            }
            if (s.substr(0, 9) != "(implies ") return {false, {}, ""};
            auto rest = s.substr(9, s.size() - 10);
            auto space = rest.find(") (");
            std::string ante, conse;
            if (space != std::string::npos) {
                ante = rest.substr(0, space + 1);
                conse = rest.substr(space + 2);
            } else {
                space = rest.find(' ');
                if (space == std::string::npos) return {false, {}, ""};
                ante = rest.substr(0, space);
                conse = rest.substr(space + 1);
            }
            return {true, {form, SExpr(ante), SExpr(conse)}, "IMPLIED-BY"};
        },
        [atre](const std::vector<std::any>& args) {
            if (args.size() < 3) return;
            auto f1 = args[0];
            std::string ante, conse;
            try { ante = std::any_cast<std::string>(args[1]); } catch(...) { return; }
            try { conse = std::any_cast<std::string>(args[2]); } catch(...) { return; }
            std::vector<SExpr> just = {SExpr(std::string("CE")), f1, SExpr(ante)};
            assert_fact(SExpr(conse), just, atre);
        }
    );

    assume_fact(SExpr(std::string("(implies (sentient-robot Robbie) (Human Robbie))")),
                "no-bias", atre);
    run_rules(atre);

    assert_fact(SExpr(std::string("(implies (human Robbie) (mortal Robbie))")),
                {SExpr(std::string("sigh"))}, atre);
    run_rules(atre);

    assume_fact(SExpr(std::string("(sentient-robot Robbie)")),
                "sort-of", atre);
    run_rules(atre);

    show_data(atre);
    print_envs(atre->atms);
}

/// Test 4a: Change focus after test 4
void atre_test4a() {
    auto atre = global_atre;
    auto env = environment_of({
        SExpr(std::string("(implies (sentient-robot Robbie) (Human Robbie))")),
        SExpr(std::string("(sentient-robot Robbie)"))
    }, atre);
    if (env) {
        change_focus(env, atre);
        run_rules(atre);
        show_data(atre);
        print_envs(atre->atms);
    }
}

/// Test 5: Contradiction rules
void atre_test5(bool debugging = true) {
    auto atre = create_atre("Test ATRE", debugging);

    // implies rule (INTERN)
    insert_rule(
        get_dbclass("implies", atre),
        [](const SExpr& form) -> std::tuple<bool, std::vector<std::any>, std::string> {
            std::string s;
            try { s = std::any_cast<std::string>(form); } catch(...) {
                return {false, {}, ""};
            }
            if (s.substr(0, 9) != "(implies ") return {false, {}, ""};
            auto rest = s.substr(9, s.size() - 10);
            auto space = rest.find(") (");
            std::string ante, conse;
            if (space != std::string::npos) {
                ante = rest.substr(0, space + 1);
                conse = rest.substr(space + 2);
            } else {
                space = rest.find(' ');
                if (space == std::string::npos) return {false, {}, ""};
                ante = rest.substr(0, space);
                conse = rest.substr(space + 1);
            }
            return {true, {form, SExpr(ante), SExpr(conse)}, "INTERN"};
        },
        [atre](const std::vector<std::any>& args) {
            if (args.size() < 3) return;
            auto f1 = args[0];
            std::string ante, conse;
            try { ante = std::any_cast<std::string>(args[1]); } catch(...) { return; }
            try { conse = std::any_cast<std::string>(args[2]); } catch(...) { return; }
            std::vector<SExpr> just = {SExpr(std::string("CE")), f1, SExpr(ante)};
            assert_fact(SExpr(conse), just, atre);
        }
    );

    assume_fact(SExpr(std::string("(sentient Robbie)")),
                "sort-of", atre);
    run_rules(atre);

    assume_fact(SExpr(std::string("(immortal Robbie)")),
                "why-not", atre);
    run_rules(atre);

    // mortal + immortal => nogood rule
    // In original: (rule :INTERN ((mortal ?x) :VAR ?f1 (immortal ?x) :VAR ?f2)
    //   (rnogood! :DEFINITION ?f1 ?f2))
    // Simplified: register a rule on "mortal" dbclass
    insert_rule(
        get_dbclass("mortal", atre),
        [](const SExpr& form) -> std::tuple<bool, std::vector<std::any>, std::string> {
            std::string s;
            try { s = std::any_cast<std::string>(form); } catch(...) {
                return {false, {}, ""};
            }
            if (s.substr(0, 8) != "(mortal ") return {false, {}, ""};
            auto name = s.substr(8, s.size() - 9);
            return {true, {form, SExpr(name)}, "INTERN"};
        },
        [atre](const std::vector<std::any>& args) {
            if (args.size() < 2) return;
            std::string name;
            try { name = std::any_cast<std::string>(args[1]); } catch(...) { return; }
            // Check if (immortal name) exists and assert nogood
            std::string immortal_fact = "(immortal " + name + ")";
            auto datum = referent1(SExpr(immortal_fact), atre);
            if (datum && in_node(datum->tms_node)) {
                // rnogood! :DEFINITION mortal-fact immortal-fact
                assert_fact(SExpr(std::string("FALSE")),
                    {SExpr(std::string("DEFINITION")), args[0],
                     SExpr(immortal_fact)}, atre);
            }
        }
    );

    // Contradiction rule on {sentient, immortal}
    auto env = environment_of({
        SExpr(std::string("(sentient Robbie)")),
        SExpr(std::string("(immortal Robbie)"))
    }, atre);
    if (env) {
        contradiction_rule(env,
            [](EnvPtr env) {
                std::cout << "\n Poor Robbie!  --> " << env->to_string() << ".";
            }, atre);
    }
    run_rules(atre);

    assert_fact(SExpr(std::string("(implies (sentient Robbie) (Human Robbie))")),
                {SExpr(std::string("no-bias"))}, atre);
    run_rules(atre);

    assert_fact(SExpr(std::string("(implies (human Robbie) (mortal Robbie))")),
                {SExpr(std::string("sigh"))}, atre);
    run_rules(atre);

    print_nogoods(atre->atms);
}
