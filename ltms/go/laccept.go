// Acceptance tests for LTRE.
// Converted from laccept.lisp.
package ltms

import "fmt"

// TestLTRE runs all LTRE acceptance tests.
func TestLTRE() {
	InLTRE(CreateLTRE("Debugging LTRE", false))
	fmt.Println("\nTesting database/LTMS link...")
	TestDatums()
	fmt.Println("\nTesting LTMS...")
	TestClauses()
	fmt.Println("\nTesting Rule system...")
	TestRulesAccept()
}

// TestDatums tests basic datum operations.
func TestDatums() {
	Assert("foo", "testing", CurrentLTRE)
	if !IsTrue("foo", CurrentLTRE) {
		panic("Fact installation glitch")
	}
	Assert([]interface{}{":NOT", "bar"}, "testing", CurrentLTRE)
	if !IsFalse("bar", CurrentLTRE) {
		panic("Negation glitch")
	}
	fmt.Println("  Datums: OKAY")
}

// TestClauses tests clause propagation and retraction.
func TestClauses() {
	Assert([]interface{}{":OR", "a", "b"}, "case-split", CurrentLTRE)
	Assert([]interface{}{":IMPLIES", "a", "c"}, "why-not?", CurrentLTRE)
	Assume([]interface{}{":IMPLIES", "c", "d"}, "what-the-heck", CurrentLTRE)
	Assume([]interface{}{":NOT", "b"}, "for-fun", CurrentLTRE)
	RunRules(CurrentLTRE)
	if !IsTrue("d", CurrentLTRE) {
		panic("Propagation glitch")
	}
	Retract([]interface{}{":NOT", "b"}, "for-fun", CurrentLTRE, true)
	RunRules(CurrentLTRE)
	if !IsUnknown("d", CurrentLTRE) {
		panic("Retraction glitch")
	}
	Assume([]interface{}{":NOT", "b"}, "for-fun", CurrentLTRE)
	RunRules(CurrentLTRE)
	if !IsTrue("d", CurrentLTRE) {
		panic("Unouting glitch")
	}
	Retract([]interface{}{":IMPLIES", "c", "d"}, "what-the-heck", CurrentLTRE, true)
	RunRules(CurrentLTRE)
	if !IsUnknown("d", CurrentLTRE) {
		panic("Retraction glitch 2")
	}
	Assume([]interface{}{":IMPLIES", "c", "d"}, "what-the-heck", CurrentLTRE)
	RunRules(CurrentLTRE)
	if !IsTrue("d", CurrentLTRE) {
		panic("Unouting glitch 2")
	}
	fmt.Println("  Clauses: OKAY")
}

// TestRulesAccept tests the rule system.
func TestRulesAccept() {
	// Install a rule: when (foo ?x) is interned and (bar ?y) is interned,
	// assert (:IMPLIES (:AND (foo ?x) (bar ?y)) (grumble ?x ?y))
	MakeRule([]interface{}{"foo", "?x"}, "INTERN",
		func(bindings map[string]interface{}, node *TmsNode) {
			x := bindings["?x"]
			MakeRule([]interface{}{"bar", "?y"}, "INTERN",
				func(bindings2 map[string]interface{}, node2 *TmsNode) {
					y := bindings2["?y"]
					RAssert([]interface{}{":IMPLIES",
						[]interface{}{":AND",
							[]interface{}{"foo", x},
							[]interface{}{"bar", y}},
						[]interface{}{"grumble", x, y}}, "hack")
				})
		})

	Referent([]interface{}{"foo", 1}, true, CurrentLTRE)
	Referent([]interface{}{"bar", 1}, true, CurrentLTRE)
	RunRules(CurrentLTRE)
	if Referent([]interface{}{"grumble", 1, 1}, false, CurrentLTRE) == nil {
		panic("Intern triggering failure")
	}
	fmt.Println("  Rules: OKAY")
}
