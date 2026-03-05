// LTMS examples and tests.
// Translated from ltms-ex.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package ltms

import "fmt"

// TestExplain demonstrates the explain chain.
// Corresponds to the Lisp function test-explain.
func TestExplain() {
	ltms := CreateLTMS("Explain Example")
	TmsCreateNode(ltms, "x", true)
	AddFormula(ltms, []interface{}{":OR", "x", "y"}, "test")
	AddFormula(ltms, []interface{}{":OR", []interface{}{":NOT", "y"}, "z"}, "test")
	AddFormula(ltms, []interface{}{":OR", []interface{}{":NOT", "z"}, "r"}, "test")
	EnableAssumption(FindNode(ltms, "x"), LabelFalse)
	ExplainNode(FindNode(ltms, "r"))
}

// TestFormula tests formula normalization.
// Corresponds to the Lisp function test-formula.
func TestFormula(complete interface{}) {
	ltms := CreateLTMS("Formula", OptionComplete(complete))
	if complete != nil {
		RegisterCompleteLTMSFunctions(ltms)
	}
	r := TmsCreateNode(ltms, "r", false)
	s := TmsCreateNode(ltms, "s", false)
	tt := TmsCreateNode(ltms, "t", false)
	u := TmsCreateNode(ltms, "u", false)
	// (:implies (:and r (:implies s t)) u)
	AddFormula(ltms, []interface{}{":IMPLIES",
		[]interface{}{":AND", r, []interface{}{":IMPLIES", s, tt}},
		u,
	}, "test")
}

// RunExTests runs the main LTMS test suite.
// Corresponds to the Lisp function run-tests.
func RunExTests() {
	fmt.Println("=== TestAsk ===")
	TestAsk()
	fmt.Println("\n=== TestAvoidAll ===")
	TestAvoidAll()
	fmt.Println("\n=== TestBug ===")
	TestBug()
	fmt.Println("\n=== TestBug1 ===")
	TestBug1(true)
	fmt.Println("\n=== TestTax 3 ===")
	TestTax(3, true)
	fmt.Println("\n=== TestTax1 3 ===")
	TestTax1(3)
}

// TestAsk tests asking with assumptions.
// Corresponds to the Lisp function test-ask.
func TestAsk() {
	ltms := CreateLTMS("Testing asking")
	n1 := TmsCreateNode(ltms, "N1", true)
	n2 := TmsCreateNode(ltms, "N2", true)
	EnableAssumption(n1, LabelFalse)
	EnableAssumption(n2, LabelFalse)
	AddFormula(ltms, []interface{}{":OR", n1, n2}, "test")
	WhyNodes(ltms)
}

// TestAvoidAll tests the avoid-all contradiction handler.
// Corresponds to the Lisp function test-avoid-all.
func TestAvoidAll() {
	ltms := CreateLTMS("Testing avoid all",
		OptionContradictionHandler(AvoidAll))
	n1 := TmsCreateNode(ltms, "N1", true)
	n2 := TmsCreateNode(ltms, "N2", true)
	EnableAssumption(n1, LabelFalse)
	EnableAssumption(n2, LabelFalse)
	AddFormula(ltms, []interface{}{":OR", n1, n2}, "test")
	WhyNodes(ltms)
}

// Test1 tests prime implicate computation.
// Corresponds to the Lisp function test1.
func Test1(complete bool) {
	var opts []func(*LTMS)
	if complete {
		opts = append(opts, OptionComplete(true))
	}
	ltms := CreateLTMS("TEST1", opts...)
	if complete {
		RegisterCompleteLTMSFunctions(ltms)
	}
	x := TmsCreateNode(ltms, "x", false)
	y := TmsCreateNode(ltms, "y", false)
	AddFormula(ltms, []interface{}{":OR", x, y}, "test")
	AddFormula(ltms, []interface{}{":OR", x, []interface{}{":NOT", y}}, "test")
	CompleteLTMS(ltms)
	if !TrueNode(x) {
		panic("TEST1 failed")
	}
}

// TestBug tests retraction bug fix.
// Corresponds to the Lisp function test-bug.
func TestBug() {
	ltms := CreateLTMS("BUG check")
	x := TmsCreateNode(ltms, "x", true)
	y := TmsCreateNode(ltms, "y", true)
	z := TmsCreateNode(ltms, "z", false)
	AddFormula(ltms, []interface{}{":OR", x, z}, "test")
	AddFormula(ltms, []interface{}{":OR", y, z}, "test")
	EnableAssumption(x, LabelFalse)
	EnableAssumption(y, LabelFalse)
	WhyNodes(ltms)
	RetractAssumption(x)
	WhyNodes(ltms)
}

// TestBug1 is a variant of TestBug with optional complete LTMS.
// Corresponds to the Lisp function test-bug1.
func TestBug1(complete bool) {
	var opts []func(*LTMS)
	if complete {
		opts = append(opts, OptionComplete(true))
	}
	ltms := CreateLTMS("BUG check", opts...)
	if complete {
		RegisterCompleteLTMSFunctions(ltms)
	}
	x := TmsCreateNode(ltms, "x", true)
	y := TmsCreateNode(ltms, "y", true)
	z := TmsCreateNode(ltms, "z", false)
	AddFormula(ltms, []interface{}{":OR", x, z}, "test")
	AddFormula(ltms, []interface{}{":OR", y, z}, "test")
	EnableAssumption(x, LabelFalse)
	EnableAssumption(y, LabelFalse)
	WhyNodes(ltms)
	RetractAssumption(x)
	WhyNodes(ltms)
}

// TestTax tests taxonomy constraints with a complete LTMS.
// Corresponds to the Lisp function test-tax.
func TestTax(n int, complete bool) {
	var opts []func(*LTMS)
	if complete {
		opts = append(opts, OptionComplete(true))
	}
	ltms := CreateLTMS("taxing", opts...)
	if complete {
		RegisterCompleteLTMSFunctions(ltms)
	}
	var nodes []interface{}
	for i := 0; i < n; i++ {
		nodes = append(nodes, TmsCreateNode(ltms, i, false))
	}
	formula := append([]interface{}{":TAXONOMY"}, nodes...)
	AddFormula(ltms, formula, "test")
	fmt.Printf("\n %d prime implicates", ltms.ClauseCounter)
}

// TestTax1 tests taxonomy constraints without a complete LTMS.
// Corresponds to the Lisp function test-tax1.
func TestTax1(n int) {
	ltms := CreateLTMS("taxing")
	var nodes []interface{}
	for i := 0; i < n; i++ {
		nodes = append(nodes, TmsCreateNode(ltms, i, false))
	}
	formula := append([]interface{}{":TAXONOMY"}, nodes...)
	AddFormula(ltms, formula, "test")
	fmt.Printf("\n %d prime implicates", ltms.ClauseCounter)
}

// TestE tests an example with multiple assumptions.
// Corresponds to the Lisp function test-e.
func TestE(complete bool) {
	var opts []func(*LTMS)
	if complete {
		opts = append(opts, OptionComplete(true))
	}
	opts = append(opts, OptionDebugging(true))
	ltms := CreateLTMS("example", opts...)
	if complete {
		RegisterCompleteLTMSFunctions(ltms)
	}
	a := TmsCreateNode(ltms, "a", true)
	b := TmsCreateNode(ltms, "b", true)
	c := TmsCreateNode(ltms, "c", true)
	d := TmsCreateNode(ltms, "d", true)
	e := TmsCreateNode(ltms, "e", true)
	AddFormula(ltms, []interface{}{":OR", []interface{}{":NOT", a}, b}, "test")
	AddFormula(ltms, []interface{}{":OR", []interface{}{":NOT", c}, d}, "test")
	AddFormula(ltms, []interface{}{":OR", []interface{}{":NOT", c}, e}, "test")
	AddFormula(ltms, []interface{}{":OR",
		[]interface{}{":NOT", b},
		[]interface{}{":NOT", d},
		[]interface{}{":NOT", e},
	}, "test")
}

// TestRemove tests clause removal with the complete LTMS.
// Corresponds to the Lisp function test-remove.
func TestRemove() {
	ltms := CreateLTMS("Delay", OptionComplete(true), OptionDebugging(true))
	RegisterCompleteLTMSFunctions(ltms)
	a := TmsCreateNode(ltms, "a", true)
	b := TmsCreateNode(ltms, "b", true)
	c := TmsCreateNode(ltms, "c", true)
	AddFormula(ltms, []interface{}{":OR", a, b, c}, "test")
	EnableAssumption(a, LabelFalse)
	EnableAssumption(b, LabelFalse)
	WhyNodes(ltms)
	AddFormula(ltms, []interface{}{":OR", a, b}, "test")
	WhyNodes(ltms)
}

// TestDelay tests the delay-sat mechanism.
// Corresponds to the Lisp function test-delay.
func TestDelay() {
	ltms := CreateLTMS("Delay", OptionComplete(true), OptionDebugging(true))
	RegisterCompleteLTMSFunctions(ltms)
	a := TmsCreateNode(ltms, "a", true)
	b := TmsCreateNode(ltms, "b", true)
	c := TmsCreateNode(ltms, "c", true)
	EnableAssumption(a, LabelFalse)
	EnableAssumption(b, LabelFalse)

	AddFormula(ltms, []interface{}{":OR", a, []interface{}{":NOT", b}}, "test")
	AddFormula(ltms, []interface{}{":OR", b, c}, "test")
	PrettyPrintClauses(ltms)

	WhyNodes(ltms)
	RetractAssumption(b)
	PrettyPrintClauses(ltms)
	WhyNodes(ltms)
}

// DirtyClausesCheck counts and reports the number of dirty clauses.
// Corresponds to the Lisp function dirty-clauses?.
func DirtyClausesCheck(ltms *LTMS) int {
	count := 0
	WalkClauses(ltms, func(cl *Clause) {
		if cl.Status == StatusDirty {
			count++
		}
	})
	fmt.Printf("\n There are now %d dirty clauses.", count)
	return count
}

// TestAnd tests AND gate formula.
// Corresponds to the Lisp function test-and.
func TestAnd() {
	ltms := CreateLTMS("LTMS")
	ok := TmsCreateNode(ltms, "ok", true)
	z := TmsCreateNode(ltms, "z", true)
	x := TmsCreateNode(ltms, "x", true)
	y := TmsCreateNode(ltms, "y", true)
	AddFormula(ltms, []interface{}{":IMPLIES", ok,
		[]interface{}{":IFF", z, []interface{}{":AND", x, y}},
	}, "test")
	PrettyPrintClauses(ltms)
}

// JustifyNode is a helper that adds a clause with one positive consequent
// and negative antecedents.
// Corresponds to the Lisp function justify-node.
func JustifyNode(informant interface{}, consequent *TmsNode, antecedents []*TmsNode) {
	AddClause([]*TmsNode{consequent}, antecedents, informant)
}

// NogoodNodes adds a nogood clause (all negative literals).
// Corresponds to the Lisp function nogood-nodes.
func NogoodNodes(informant interface{}, nodes []*TmsNode) {
	AddClause(nil, nodes, informant)
}

// PrintStatistics prints clause length distribution.
// Corresponds to the Lisp function print-statistics.
func PrintStatistics(ltms *LTMS) {
	lengths := make(map[int]int)
	fmt.Printf("\n There are %d propositional symbols", ltms.NodeCounter)
	WalkClauses(ltms, func(cl *Clause) {
		lengths[cl.Length]++
	})
	for i := 0; i < 100; i++ {
		if count, ok := lengths[i]; ok && count > 0 {
			fmt.Printf("\n There are %d prime implicates of size %d", count, i)
		}
	}
}
