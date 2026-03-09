/// Marx Brothers Puzzle Data - Implementation
/// Converted from marxdata.lisp
/// Copyright (c) 1996, Kenneth D. Forbus. All rights reserved.
///
/// Constraints from Building Problem Solvers, pages 648-649:
/// 1. The pianist, harpist, and talker are distinct brothers.
/// 2. Money, gambling, and animals are distinct interests.
/// 3. The one who likes to talk doesn't like gambling.
/// 4. The one who likes animals plays the harp.
/// 5. Groucho hates animals.
/// 6. Harpo is always silent.
/// 7. Chico plays the piano.

#include "marxdata.h"
#include "ldata.h"
#include "linter.h"
#include <string>

static std::any S(const std::string& s) { return std::any(s); }
static std::any L(std::initializer_list<std::any> elems) {
    return std::any(std::vector<std::any>(elems));
}

void load_marx_constraints() {
    // 1. The pianist, harpist, and talker are distinct brothers.
    ltre_assert(L({S("pairwise-nogood"), S("PLAYS-PIANO"), S("PLAYS-HARP")}), S("user"));
    ltre_assert(L({S("pairwise-nogood"), S("PLAYS-PIANO"), S("SMOOTH-TALKER")}), S("user"));
    ltre_assert(L({S("pairwise-nogood"), S("PLAYS-HARP"), S("SMOOTH-TALKER")}), S("user"));

    // 2. Money vs gambling vs animals
    ltre_assert(L({S("pairwise-nogood"), S("LIKES-MONEY"), S("LIKES-GAMBLING")}), S("user"));
    ltre_assert(L({S("pairwise-nogood"), S("LIKES-GAMBLING"), S("LIKES-ANIMALS")}), S("user"));

    // 3. Talker doesn't like gambling
    ltre_assert(L({S("pairwise-nogood"), S("SMOOTH-TALKER"), S("LIKES-GAMBLING")}), S("user"));

    // 4. Animal lover plays harp
    ltre_assert(L({S("same-entity"), S("LIKES-ANIMALS"), S("PLAYS-HARP")}), S("user"));

    // 5. Groucho hates animals: (:NOT (LIKES-ANIMALS GROUCHO))
    ltre_assert(L({S(":NOT"), L({S("LIKES-ANIMALS"), S("GROUCHO")})}), S("user"));

    // 6. Harpo is always silent: (:NOT (SMOOTH-TALKER HARPO))
    ltre_assert(L({S(":NOT"), L({S("SMOOTH-TALKER"), S("HARPO")})}), S("user"));

    // 7. Chico plays piano: (PLAYS-PIANO CHICO)
    ltre_assert(L({S("PLAYS-PIANO"), S("CHICO")}), S("user"));

    // Note: The pairwise-nogood and same-entity rules are higher-order
    // relations that require the rule macro system for full implementation.
    // In the Lisp version:
    //   (rule ((:true (pairwise-nogood ?a1 ?a2) :var ?hor)
    //          (:true (?a1 ?obj) :var ?f1)
    //          (:true (?a2 ?obj) :var ?f2))
    //     (rassert! (:not (:and ?hor ?f1 ?f2))))
    //
    //   (rule ((:true (same-entity ?a1 ?a2) :var ?hor)
    //          (:true (?a1 ?obj) :var ?f1))
    //     (rassert! (:implies (:and ?hor ?f1) (?a2 ?obj))))
}
