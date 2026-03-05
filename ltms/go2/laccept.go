// LTRE: An LTMS-based Tiny Rule Engine.
// Acceptance tests module.
// Translated from laccept.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package ltms

import "fmt"

// TestLTRE runs the full LTRE acceptance test suite.
// Corresponds to the Lisp function test-ltre.
func TestLTRE() {
	InLTRE(CreateLTRE("Debugging LTRE", false))
	fmt.Println("\nTesting database/LTMS link...")
	TestDatums()
	fmt.Println("\nTesting LTMS...")
	TestClauses()
	fmt.Println("\nTesting Rule system...")
	TestRulesAcceptance()
}

// TestDatums tests basic datum assertion and negation.
// Corresponds to the Lisp function test-datums.
func TestDatums() {
	Assert("foo", "testing", CurrentLTRE)
	if !IsTrue("foo", CurrentLTRE) {
		panic("Fact installation glitch")
	}
	Assert([]interface{}{":NOT", "bar"}, "testing", CurrentLTRE)
	if !IsFalse("bar", CurrentLTRE) {
		panic("Negation glitch")
	}
	fmt.Println("  OKAY")
}

// TestClauses tests clause propagation and retraction.
// Corresponds to the Lisp function test-clauses.
func TestClauses() {
	ltre := CurrentLTRE
	Assert([]interface{}{":OR", "a", "b"}, "case-split", ltre)
	Assert([]interface{}{":IMPLIES", "a", "c"}, "why-not?", ltre)
	Assume([]interface{}{":IMPLIES", "c", "d"}, "what-the-heck", ltre)
	Assume([]interface{}{":NOT", "b"}, "for-fun", ltre)
	if !IsTrue("d", ltre) {
		panic("Propagation glitch")
	}
	Retract([]interface{}{":NOT", "b"}, "for-fun", ltre, false)
	if !IsUnknown("d", ltre) {
		panic("Retraction glitch")
	}
	Assume([]interface{}{":NOT", "b"}, "for-fun", ltre)
	if !IsTrue("d", ltre) {
		panic("Unouting glitch")
	}
	Retract([]interface{}{":IMPLIES", "c", "d"}, "what-the-heck", ltre, false)
	if !IsUnknown("d", ltre) {
		panic("Retraction glitch 2")
	}
	Assume([]interface{}{":IMPLIES", "c", "d"}, "what-the-heck", ltre)
	if !IsTrue("d", ltre) {
		panic("Unouting glitch 2")
	}
	fmt.Println("  OKAY")
}

// TestRulesAcceptance tests the rule system with :TRUE and :INTERN triggers.
// Corresponds to the Lisp function test-rules.
func TestRulesAcceptance() {
	ltre := CurrentLTRE

	// Rule 1: (:TRUE (foo ?x) :VAR ?f1) (:TRUE (bar ?y) :VAR ?f2)
	//   => (rassert! (:IMPLIES (:AND ?f1 ?f2) (mumble ?x ?y)) 'hack)
	InsertRule(
		GetDbclass("foo", ltre),
		func(p interface{}) (bool, []interface{}, bool) {
			list, ok := p.([]interface{})
			if !ok || len(list) != 2 {
				return false, nil, false
			}
			if s, ok := list[0].(string); !ok || s != "foo" {
				return false, nil, false
			}
			return true, []interface{}{list[1]}, true
		},
		WrapTrueBody(func(args ...interface{}) {
			f1 := args[0].(*TmsNode)
			x := args[1]
			// Inner rule on bar
			InsertRule(
				GetDbclass("bar", ltre),
				func(p interface{}) (bool, []interface{}, bool) {
					list, ok := p.([]interface{})
					if !ok || len(list) != 2 {
						return false, nil, false
					}
					if s, ok := list[0].(string); !ok || s != "bar" {
						return false, nil, false
					}
					return true, []interface{}{list[1]}, true
				},
				WrapTrueBody(func(innerArgs ...interface{}) {
					f2 := innerArgs[0].(*TmsNode)
					y := innerArgs[1]
					RAssert(
						[]interface{}{":IMPLIES",
							[]interface{}{":AND", f1, f2},
							[]interface{}{"mumble", x, y},
						},
						"hack", ltre)
				}),
			)
		}),
	)

	// Rule 2: (:INTERN (foo ?x) :VAR ?f1) (:INTERN (bar ?y) :VAR ?f2)
	//   => (rassert! (:IMPLIES (:AND ?f1 ?f2) (grumble ?x ?y)) 'hack)
	InsertRule(
		GetDbclass("foo", ltre),
		func(p interface{}) (bool, []interface{}, bool) {
			list, ok := p.([]interface{})
			if !ok || len(list) != 2 {
				return false, nil, false
			}
			if s, ok := list[0].(string); !ok || s != "foo" {
				return false, nil, false
			}
			return true, []interface{}{list[1]}, true
		},
		func(args ...interface{}) {
			// :INTERN trigger - fires on creation
			f1 := args[0].(*TmsNode)
			x := args[1]
			InsertRule(
				GetDbclass("bar", ltre),
				func(p interface{}) (bool, []interface{}, bool) {
					list, ok := p.([]interface{})
					if !ok || len(list) != 2 {
						return false, nil, false
					}
					if s, ok := list[0].(string); !ok || s != "bar" {
						return false, nil, false
					}
					return true, []interface{}{list[1]}, true
				},
				func(innerArgs ...interface{}) {
					f2 := innerArgs[0].(*TmsNode)
					y := innerArgs[1]
					RAssert(
						[]interface{}{":IMPLIES",
							[]interface{}{":AND", f1, f2},
							[]interface{}{"grumble", x, y},
						},
						"hack", ltre)
				},
			)
		},
	)

	Referent([]interface{}{"foo", 1}, true, ltre)
	Referent([]interface{}{"bar", 1}, true, ltre)
	RunRules(ltre)
	if Referent([]interface{}{"grumble", 1, 1}, false, ltre) == nil {
		panic("Intern triggering failure")
	}
	if Referent([]interface{}{"mumble", 1, 1}, false, ltre) != nil {
		panic("Premature triggering")
	}

	Assume([]interface{}{"foo", 1}, "why-not?", ltre)
	Assume([]interface{}{":NOT", []interface{}{"bar", 1}}, "monkeywrench", ltre)
	RunRules(ltre)
	if IsTrue([]interface{}{"mumble", 1, 1}, ltre) {
		panic("Badly conditioned triggering")
	}

	// retract! with wrong informant should fail silently
	Retract([]interface{}{":NOT", []interface{}{"bar", 1}}, "tweak", ltre, true)
	if !IsFalse([]interface{}{"bar", 1}, ltre) {
		panic("Retraction with wrong informant")
	}

	Retract([]interface{}{":NOT", []interface{}{"bar", 1}}, "monkeywrench", ltre, false)
	RunRules(ltre)
	if IsTrue([]interface{}{"mumble", 1, 1}, ltre) {
		panic("Badly conditioned triggering - 2")
	}

	Assume([]interface{}{"bar", 1}, "why", ltre)
	RunRules(ltre)
	if !IsTrue([]interface{}{"mumble", 1, 1}, ltre) {
		panic("Badly conditioned triggering - 3")
	}

	Assume([]interface{}{"foo", 2}, "go-for-it", ltre)
	RunRules(ltre)
	if !IsTrue([]interface{}{"mumble", 2, 1}, ltre) {
		panic("Rule chaining failure")
	}

	Assume([]interface{}{"bar", 2}, "alternate", ltre)
	RunRules(ltre)
	if !IsTrue([]interface{}{"mumble", 1, 2}, ltre) {
		panic("Subrule spawning failure")
	}
	if !IsTrue([]interface{}{"mumble", 2, 2}, ltre) {
		panic("Subrule spawning failure - 2")
	}
	fmt.Println("  OKAY")
}
