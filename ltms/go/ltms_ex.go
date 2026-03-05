// LTMS examples and tests.
// Converted from ltms-ex.lisp.
package ltms

import "fmt"

// TestExplain tests the explain functionality.
func TestExplain() {
	l := CreateLTMS("Explain Example", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		DelaySat:               true,
	})
	TmsCreateNode(l, "x", true)
	NormLTMS = l
	clauses := Normalize(l, []interface{}{":OR", "x", "y"})
	for _, clause := range clauses {
		s := SimplifyClause(clause)
		if s != nil {
			AddClauseInternal(s, nil, true)
		}
	}
	clauses = Normalize(l, []interface{}{":OR", []interface{}{":NOT", "y"}, "z"})
	for _, clause := range clauses {
		s := SimplifyClause(clause)
		if s != nil {
			AddClauseInternal(s, nil, true)
		}
	}
	clauses = Normalize(l, []interface{}{":OR", []interface{}{":NOT", "z"}, "r"})
	for _, clause := range clauses {
		s := SimplifyClause(clause)
		if s != nil {
			AddClauseInternal(s, nil, true)
		}
	}
	EnableAssumption(FindNode(l, "x"), LabelFalse)
	ExplainNode(FindNode(l, "r"))
}

// TestFormula tests formula normalization.
func TestFormula(complete bool) *LTMS {
	var cm CompleteMode
	if complete {
		cm = CompleteTrue
	}
	l := CreateLTMS("Formula", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		Complete:               cm,
		DelaySat:               true,
	})
	if complete {
		InitCompleteLTMS(l)
	}
	r := TmsCreateNode(l, "r", false)
	s := TmsCreateNode(l, "s", false)
	tt := TmsCreateNode(l, "t", false)
	u := TmsCreateNode(l, "u", false)
	_ = r
	_ = s
	_ = tt
	_ = u
	AddFormula(l, []interface{}{":IMPLIES",
		[]interface{}{":AND", r, []interface{}{":IMPLIES", s, tt}}, u}, nil)
	return l
}

// TestAsk tests the ask/user interface.
func TestAsk() {
	l := CreateLTMS("Testing asking", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		DelaySat:               true,
	})
	n1 := TmsCreateNode(l, "N1", true)
	n2 := TmsCreateNode(l, "N2", true)
	EnableAssumption(n1, LabelFalse)
	EnableAssumption(n2, LabelFalse)
	AddFormula(l, []interface{}{":OR", n1, n2}, nil)
	WhyNodes(l)
}

// TestAvoidAll tests the avoid-all contradiction handler.
func TestAvoidAll() {
	l := CreateLTMS("Testing avoid all", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AvoidAll,
		CacheDatums:            true,
		DelaySat:               true,
	})
	n1 := TmsCreateNode(l, "N1", true)
	n2 := TmsCreateNode(l, "N2", true)
	EnableAssumption(n1, LabelFalse)
	EnableAssumption(n2, LabelFalse)
	AddFormula(l, []interface{}{":OR", n1, n2}, nil)
	WhyNodes(l)
}

// Test1 tests basic resolution.
func Test1(complete bool) {
	var cm CompleteMode
	if complete {
		cm = CompleteTrue
	}
	l := CreateLTMS("TEST1", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		Complete:               cm,
		DelaySat:               true,
	})
	if complete {
		InitCompleteLTMS(l)
	}
	x := TmsCreateNode(l, "x", false)
	y := TmsCreateNode(l, "y", false)
	_ = y
	AddFormula(l, []interface{}{":OR", x, y}, nil)
	AddFormula(l, []interface{}{":OR", x, []interface{}{":NOT", y}}, nil)
	CompleteLTMS(l)
	if !TrueNode(x) {
		panic("TEST1 failed")
	}
	fmt.Println("TEST1 passed")
}

// TestBug tests a known LTMS bug scenario.
func TestBug() {
	l := CreateLTMS("BUG check", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		DelaySat:               true,
	})
	x := TmsCreateNode(l, "x", true)
	y := TmsCreateNode(l, "y", true)
	z := TmsCreateNode(l, "z", false)
	_ = z
	AddFormula(l, []interface{}{":OR", x, z}, nil)
	AddFormula(l, []interface{}{":OR", y, z}, nil)
	EnableAssumption(x, LabelFalse)
	EnableAssumption(y, LabelFalse)
	WhyNodes(l)
	RetractAssumption(x)
	WhyNodes(l)
}

// TestBug1 tests bug with complete LTMS.
func TestBug1(complete bool) {
	var cm CompleteMode
	if complete {
		cm = CompleteTrue
	}
	l := CreateLTMS("BUG check", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		Complete:               cm,
		DelaySat:               true,
	})
	if complete {
		InitCompleteLTMS(l)
	}
	x := TmsCreateNode(l, "x", true)
	y := TmsCreateNode(l, "y", true)
	z := TmsCreateNode(l, "z", false)
	_ = z
	AddFormula(l, []interface{}{":OR", x, z}, nil)
	AddFormula(l, []interface{}{":OR", y, z}, nil)
	EnableAssumption(x, LabelFalse)
	EnableAssumption(y, LabelFalse)
	WhyNodes(l)
	RetractAssumption(x)
	WhyNodes(l)
}

// TestTax tests taxonomy constraints.
func TestTax(n int, complete bool) {
	var cm CompleteMode
	if complete {
		cm = CompleteTrue
	}
	l := CreateLTMS("taxing", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		Complete:               cm,
		DelaySat:               true,
	})
	if complete {
		InitCompleteLTMS(l)
	}
	var nodes []*TmsNode
	for i := 0; i < n; i++ {
		nodes = append(nodes, TmsCreateNode(l, i, false))
	}
	taxForm := make([]interface{}, 0, n+1)
	taxForm = append(taxForm, ":TAXONOMY")
	for _, node := range nodes {
		taxForm = append(taxForm, node)
	}
	AddFormula(l, taxForm, nil)
	fmt.Printf("\n %d prime implicates", l.ClauseCounter)
}

// TestE tests with assumption nodes.
func TestE(complete bool) {
	var cm CompleteMode
	if complete {
		cm = CompleteTrue
	}
	l := CreateLTMS("example", LTMSOptions{
		NodeString:             DefaultNodeString,
		Debugging:              true,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		Complete:               cm,
		DelaySat:               true,
	})
	if complete {
		InitCompleteLTMS(l)
	}
	a := TmsCreateNode(l, "a", true)
	b := TmsCreateNode(l, "b", true)
	c := TmsCreateNode(l, "c", true)
	d := TmsCreateNode(l, "d", true)
	e := TmsCreateNode(l, "e", true)
	AddFormula(l, []interface{}{":OR", []interface{}{":NOT", a}, b}, nil)
	AddFormula(l, []interface{}{":OR", []interface{}{":NOT", c}, d}, nil)
	AddFormula(l, []interface{}{":OR", []interface{}{":NOT", c}, e}, nil)
	AddFormula(l, []interface{}{":OR", []interface{}{":NOT", b},
		[]interface{}{":NOT", d}, []interface{}{":NOT", e}}, nil)
}

// RunTests runs all basic LTMS tests.
func RunTests() {
	fmt.Println("Running TestAsk...")
	TestAsk()
	fmt.Println("\nRunning TestAvoidAll...")
	TestAvoidAll()
	fmt.Println("\nRunning TestBug...")
	TestBug()
	fmt.Println("\nRunning TestBug1...")
	TestBug1(true)
	fmt.Println("\nRunning TestTax(3)...")
	TestTax(3, true)
	fmt.Println("\nAll tests completed.")
}
