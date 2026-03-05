// LTRE: An LTMS-based Tiny Rule Engine.
// Marx Brothers constraint data and rules.
// Translated from marxdata.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1996, Kenneth D. Forbus, Northwestern University.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package ltms

// InstallMarxData installs the Marx Brothers constraints and rules.
// Corresponds to the data and rules in marxdata.lisp.
func InstallMarxData(ltre *LTRE) {
	// --- Constraints ---

	// 1. The pianist, harpist, and talker are distinct brothers.
	RAssert([]interface{}{"pairwise-nogood", "plays-piano", "plays-harp"}, "user", ltre)
	RAssert([]interface{}{"pairwise-nogood", "plays-piano", "smooth-talker"}, "user", ltre)
	RAssert([]interface{}{"pairwise-nogood", "plays-harp", "smooth-talker"}, "user", ltre)

	// 2. The brother who is fond of money is distinct from the one
	//    who is fond of gambling, who is also distinct from the one
	//    who is fond of animals.
	RAssert([]interface{}{"pairwise-nogood", "likes-money", "likes-gambling"}, "user", ltre)
	RAssert([]interface{}{"pairwise-nogood", "likes-gambling", "likes-animals"}, "user", ltre)

	// 3. The one who likes to talk doesn't like gambling.
	RAssert([]interface{}{"pairwise-nogood", "smooth-talker", "likes-gambling"}, "user", ltre)

	// 4. The one who likes animals plays the harp.
	RAssert([]interface{}{"same-entity", "likes-animals", "plays-harp"}, "user", ltre)

	// 5. Groucho hates animals.
	RAssert([]interface{}{":NOT", []interface{}{"likes-animals", "groucho"}}, "user", ltre)

	// 6. Harpo is always silent.
	RAssert([]interface{}{":NOT", []interface{}{"smooth-talker", "harpo"}}, "user", ltre)

	// 7. Chico plays the piano.
	RAssert([]interface{}{"plays-piano", "chico"}, "user", ltre)

	RunRules(ltre)

	// --- Rules implementing higher-order relations ---

	// Rule: pairwise-nogood
	// (rule ((:true (pairwise-nogood ?attribute1 ?attribute2) :var ?hor)
	//        (:true (?attribute1 ?obj) :var ?f1)
	//        (:true (?attribute2 ?obj) :var ?f2))
	//   (rassert! (:not (:and ?hor ?f1 ?f2))))
	InsertRule(
		GetDbclass("pairwise-nogood", ltre),
		func(p interface{}) (bool, []interface{}, bool) {
			list, ok := p.([]interface{})
			if !ok || len(list) != 3 {
				return false, nil, false
			}
			if s, ok := list[0].(string); !ok || s != "pairwise-nogood" {
				return false, nil, false
			}
			attr1 := list[1]
			attr2 := list[2]
			return true, []interface{}{attr1, attr2}, true
		},
		WrapTrueBody(func(args ...interface{}) {
			hor := args[0].(*TmsNode)
			attr1 := args[1]
			attr2 := args[2]
			// Inner rule: (:true (?attribute1 ?obj) :var ?f1)
			InsertRule(
				GetDbclass(attr1, ltre),
				func(p interface{}) (bool, []interface{}, bool) {
					list, ok := p.([]interface{})
					if !ok || len(list) != 2 {
						return false, nil, false
					}
					if !deepEqual(list[0], attr1) {
						return false, nil, false
					}
					return true, []interface{}{list[1]}, true
				},
				WrapTrueBody(func(args2 ...interface{}) {
					f1 := args2[0].(*TmsNode)
					obj := args2[1]
					// Inner rule: (:true (?attribute2 ?obj) :var ?f2)
					InsertRule(
						GetDbclass(attr2, ltre),
						func(p interface{}) (bool, []interface{}, bool) {
							list, ok := p.([]interface{})
							if !ok || len(list) != 2 {
								return false, nil, false
							}
							if !deepEqual(list[0], attr2) {
								return false, nil, false
							}
							if !deepEqual(list[1], obj) {
								return false, nil, false
							}
							return true, nil, true
						},
						WrapTrueBody(func(args3 ...interface{}) {
							f2 := args3[0].(*TmsNode)
							RAssert(
								[]interface{}{":NOT",
									[]interface{}{":AND", hor, f1, f2},
								},
								"pairwise-nogood-rule", ltre)
						}),
					)
				}),
			)
		}),
	)

	// Rule: same-entity
	// (rule ((:true (same-entity ?attribute1 ?attribute2) :var ?hor)
	//        (:true (?attribute1 ?obj) :var ?f1))
	//   (rassert! (:implies (:and ?hor ?f1) (?attribute2 ?obj))))
	InsertRule(
		GetDbclass("same-entity", ltre),
		func(p interface{}) (bool, []interface{}, bool) {
			list, ok := p.([]interface{})
			if !ok || len(list) != 3 {
				return false, nil, false
			}
			if s, ok := list[0].(string); !ok || s != "same-entity" {
				return false, nil, false
			}
			attr1 := list[1]
			attr2 := list[2]
			return true, []interface{}{attr1, attr2}, true
		},
		WrapTrueBody(func(args ...interface{}) {
			hor := args[0].(*TmsNode)
			attr1 := args[1]
			attr2 := args[2]
			// Inner rule: (:true (?attribute1 ?obj) :var ?f1)
			InsertRule(
				GetDbclass(attr1, ltre),
				func(p interface{}) (bool, []interface{}, bool) {
					list, ok := p.([]interface{})
					if !ok || len(list) != 2 {
						return false, nil, false
					}
					if !deepEqual(list[0], attr1) {
						return false, nil, false
					}
					return true, []interface{}{list[1]}, true
				},
				WrapTrueBody(func(args2 ...interface{}) {
					f1 := args2[0].(*TmsNode)
					obj := args2[1]
					RAssert(
						[]interface{}{":IMPLIES",
							[]interface{}{":AND", hor, f1},
							[]interface{}{attr2, obj},
						},
						"same-entity-rule", ltre)
				}),
			)
		}),
	)
}
