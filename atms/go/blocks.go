// -*- Mode: Go -*-

// Blocks world rules for ATRE
// Translated from blocks.lisp

// Copyright (c) 1988-1992 Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms

// RegisterBlocksWorldRules registers all blocks world constraint rules
// and operators into the given planning problem and ATRE.
// This replaces the Lisp file that was loaded dynamically.
func RegisterBlocksWorldRules(plnpr *Plnpr, atre *Atre) {
	// Constraint rules are registered as pattern-triggered rules in the ATRE.
	// In the Lisp version these were macros expanded at load time.
	// Here we register them programmatically.

	// Rule: (not ?fact) and ?fact => nogood
	RegisterConstraintRule(atre, "NEG-DEF",
		[]string{"not", "fact"},
		func(bindings map[string]interface{}) bool { return true },
	)

	// Rule: (on ?obj ?s1) and (on ?obj ?s2) where s1 != s2 => nogood
	RegisterConstraintRule(atre, "PLACE-EXCLUSION",
		[]string{"on-place"},
		func(bindings map[string]interface{}) bool {
			s1, s2 := bindings["s1"], bindings["s2"]
			return !SExprEqual(s1, s2)
		},
	)

	// Rule: (on ?obj1 ?s) and (on ?obj2 ?s) where obj1 != obj2 and s != TABLE => nogood
	RegisterConstraintRule(atre, "TOP-EXCLUSION",
		[]string{"on-top"},
		func(bindings map[string]interface{}) bool {
			obj1, obj2, s := bindings["obj1"], bindings["obj2"], bindings["s"]
			return !SExprEqual(obj1, obj2) && !SExprEqual(s, "Table")
		},
	)

	// Operators
	DefineOperator(plnpr,
		[]interface{}{"Pickup", "?x"},
		[][]interface{}{{"on", "?x", "Table"}, {"clear", "?x"}, {"hand-empty"}},
		[][]interface{}{{"holding", "?x"}},
		[][]interface{}{{"on", "?x", "Table"}, {"clear", "?x"}, {"hand-empty"}},
	)

	DefineOperator(plnpr,
		[]interface{}{"Putdown", "?x"},
		[][]interface{}{{"holding", "?x"}},
		[][]interface{}{{"on", "?x", "Table"}, {"clear", "?x"}, {"hand-empty"}},
		[][]interface{}{{"holding", "?x"}},
	)

	DefineOperator(plnpr,
		[]interface{}{"Stack", "?x", "?y"},
		[][]interface{}{{"holding", "?x"}, {"clear", "?y"}},
		[][]interface{}{{"hand-empty"}, {"on", "?x", "?y"}, {"clear", "?x"}},
		[][]interface{}{{"holding", "?x"}, {"clear", "?y"}},
	)

	DefineOperator(plnpr,
		[]interface{}{"Unstack", "?x", "?y"},
		[][]interface{}{{"hand-empty"}, {"clear", "?x"}, {"on", "?x", "?y"}},
		[][]interface{}{{"holding", "?x"}, {"clear", "?y"}},
		[][]interface{}{{"hand-empty"}, {"clear", "?x"}, {"on", "?x", "?y"}},
	)
}

// RegisterConstraintRule is a helper to register domain constraint rules.
// In the full system these would use the ATRE rule macro expansion.
func RegisterConstraintRule(atre *Atre, name string, tags []string, test func(map[string]interface{}) bool) {
	// Placeholder: In a complete implementation, this would register
	// rules that fire when matching patterns are asserted, checking
	// the test predicate, and creating nogoods when violations are found.
	_ = atre
	_ = name
	_ = tags
	_ = test
}
