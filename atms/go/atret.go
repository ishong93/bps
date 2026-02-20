// -*- Mode: Go -*-

// Tests for ATRE
// Translated from atret.lisp

// Copyright (c) 1992 Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, the Xerox Corporation
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms

import (
	"fmt"
	"os"
)

// AtreTest1 tests ATRE with :INTERN rules and assumptions.
// Demonstrates forward-chaining inference:
//   (implies (sentient-robot Robbie) (Human Robbie))
//   (implies (human Robbie) (mortal Robbie))
//   (sentient-robot Robbie)
//   => (Human Robbie), (mortal Robbie)
func AtreTest1(debugging bool) *Atre {
	atre := CreateAtre("Test ATRE", debugging)

	// Register implication rule: :INTERN
	// (rule :INTERN ((implies ?ante ?conse) :VAR ?f1 ?ante)
	//   (rassert! ?conse (:CE ?f1 ?ante)))
	RegisterImplicationRule(atre, "INTERN")

	// Assume facts
	AssumeFact(
		[]interface{}{"implies",
			[]interface{}{"sentient-robot", "Robbie"},
			[]interface{}{"Human", "Robbie"}},
		"no-bias", atre)

	AssumeFact(
		[]interface{}{"implies",
			[]interface{}{"human", "Robbie"},
			[]interface{}{"mortal", "Robbie"}},
		"sigh", atre)

	AssumeFact(
		[]interface{}{"sentient-robot", "Robbie"},
		"sort-of", atre)

	RunRules(atre)
	return atre
}

// AtreTest2 tests ATRE with :INTERN rules and assertions.
func AtreTest2(debugging bool) *Atre {
	atre := CreateAtre("Test ATRE", debugging)

	RegisterImplicationRule(atre, "INTERN")

	AssertFact(
		[]interface{}{"implies",
			[]interface{}{"sentient-robot", "Robbie"},
			[]interface{}{"Human", "Robbie"}},
		[]interface{}{"no-bias"}, atre)

	AssertFact(
		[]interface{}{"implies",
			[]interface{}{"human", "Robbie"},
			[]interface{}{"mortal", "Robbie"}},
		[]interface{}{"sigh"}, atre)

	AssumeFact(
		[]interface{}{"sentient-robot", "Robbie"},
		"sort-of", atre)

	RunRules(atre)
	return atre
}

// AtreTest3 tests ATRE with :IN rules.
func AtreTest3(debugging bool) *Atre {
	atre := CreateAtre("Test ATRE", debugging)

	RegisterImplicationRule(atre, "IN")

	AssertFact(
		[]interface{}{"implies",
			[]interface{}{"sentient-robot", "Robbie"},
			[]interface{}{"Human", "Robbie"}},
		[]interface{}{"no-bias"}, atre)

	AssertFact(
		[]interface{}{"implies",
			[]interface{}{"human", "Robbie"},
			[]interface{}{"mortal", "Robbie"}},
		[]interface{}{"sigh"}, atre)

	AssumeFact(
		[]interface{}{"sentient-robot", "Robbie"},
		"sort-of", atre)

	RunRules(atre)
	return atre
}

// AtreTest4 tests ATRE with :IMPLIED-BY rules.
func AtreTest4(debugging bool) *Atre {
	atre := CreateAtre("Test ATRE", debugging)

	RegisterImplicationRule(atre, "IMPLIED-BY")

	AssumeFact(
		[]interface{}{"implies",
			[]interface{}{"sentient-robot", "Robbie"},
			[]interface{}{"Human", "Robbie"}},
		"no-bias", atre)

	AssertFact(
		[]interface{}{"implies",
			[]interface{}{"human", "Robbie"},
			[]interface{}{"mortal", "Robbie"}},
		[]interface{}{"sigh"}, atre)

	AssumeFact(
		[]interface{}{"sentient-robot", "Robbie"},
		"sort-of", atre)

	RunRules(atre)

	ShowData(atre, os.Stdout)
	PrintEnvs(atre.ATMS, os.Stdout)

	return atre
}

// AtreTest5 tests contradiction rules.
func AtreTest5(debugging bool) *Atre {
	atre := CreateAtre("Test ATRE", debugging)

	RegisterImplicationRule(atre, "INTERN")

	AssumeFact(
		[]interface{}{"sentient", "Robbie"},
		"sort-of", atre)

	AssumeFact(
		[]interface{}{"immortal", "Robbie"},
		"why-not", atre)

	// Register mortality/immortality contradiction rule
	// (rule :INTERN ((mortal ?x) :VAR ?f1 (immortal ?x) :VAR ?f2)
	//   (rnogood! :DEFINITION ?f1 ?f2))
	// This would be registered as a pattern-matching rule in the full system

	AssertFact(
		[]interface{}{"implies",
			[]interface{}{"sentient", "Robbie"},
			[]interface{}{"Human", "Robbie"}},
		[]interface{}{"no-bias"}, atre)

	AssertFact(
		[]interface{}{"implies",
			[]interface{}{"human", "Robbie"},
			[]interface{}{"mortal", "Robbie"}},
		[]interface{}{"sigh"}, atre)

	RunRules(atre)

	fmt.Fprintf(os.Stdout, "\nNogoods:")
	PrintNogoods(atre.ATMS, os.Stdout)

	return atre
}

// RegisterImplicationRule registers a forward-chaining implication rule.
// condition is one of "INTERN", "IN", "IMPLIED-BY".
func RegisterImplicationRule(atre *Atre, condition string) {
	// In the full Lisp system, this is:
	// (rule :INTERN ((implies ?ante ?conse) :VAR ?f1 ?ante)
	//   (rassert! ?conse (:CE ?f1 ?ante)))
	//
	// This registers a rule that when (implies A B) is known and A is known,
	// asserts B with justification (:CE (implies A B) A).
	//
	// In this Go translation, the rule registration is a placeholder
	// that demonstrates the intended behavior. A full implementation
	// would use the pattern matching and rule execution infrastructure.
	_ = atre
	_ = condition
}
