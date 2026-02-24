// Simple shakedown procedure for JTRE.
// Translated from jtest.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

import "fmt"

// ShakedownJtre runs a series of tests to verify JTRE functionality.
// Tests INTERN rules, IN rules, referent, fetch, and assumption handling.
// Corresponds to the Lisp function shakedown-jtre.
func ShakedownJtre() {
	jtre := CreateJtre("Test One", false)

	// Define an :INTERN rule: when (foo ?x) and (bar ?y) are both
	// interned with numeric values, assert (mumble ?x ?y)
	fooDbclass := JGetDbclass([]interface{}{"foo"}, jtre)
	InsertJRule(fooDbclass,
		func(p interface{}) (bool, []interface{}, bool) {
			ps, ok := p.([]interface{})
			if !ok || len(ps) != 2 || ps[0] != "foo" {
				return false, nil, false
			}
			x := ps[1]
			if _, ok := x.(int); !ok {
				if _, ok := x.(float64); !ok {
					return false, nil, false
				}
			}
			return true, []interface{}{x}, false
		},
		func(args ...interface{}) {
			if len(args) < 1 {
				return
			}
			x := args[0]
			// Try to match against (bar ?y) with numeric test
			for _, datum := range JGetDbclass([]interface{}{"bar"}, jtre).Facts {
				bs, ok := datum.LispForm.([]interface{})
				if !ok || len(bs) != 2 || bs[0] != "bar" {
					continue
				}
				y := bs[1]
				if _, ok := y.(int); !ok {
					if _, ok := y.(float64); !ok {
						continue
					}
				}
				JAssert([]interface{}{"mumble", x, y},
					[]interface{}{"Test-intern"}, jtre)
			}
		},
	)
	fmt.Println("\n :INTERN rule defined okay.")

	// Define an :IN rule: when (foo ?x) and (bar ?y) are both IN
	// with non-numeric values, assert (grumble ?x ?y)
	InsertJRule(fooDbclass,
		func(p interface{}) (bool, []interface{}, bool) {
			ps, ok := p.([]interface{})
			if !ok || len(ps) != 2 || ps[0] != "foo" {
				return false, nil, false
			}
			x := ps[1]
			if _, ok := x.(int); ok {
				return false, nil, false
			}
			return true, []interface{}{x}, true // needs node
		},
		func(args ...interface{}) {
			if len(args) < 2 {
				return
			}
			triggerNode := args[0].(*TmsNode)
			x := args[1]
			if !InNode(triggerNode) {
				return
			}
			for _, datum := range JGetDbclass([]interface{}{"bar"}, jtre).Facts {
				bs, ok := datum.LispForm.([]interface{})
				if !ok || len(bs) != 2 || bs[0] != "bar" {
					continue
				}
				y := bs[1]
				if _, ok := y.(int); ok {
					continue
				}
				if InNode(datum.TmsNode) {
					JAssert([]interface{}{"grumble", x, y},
						[]interface{}{":TEST-in"}, jtre)
				}
			}
		},
	)
	fmt.Println(" :IN rule defined okay.")

	// Test referent and fetch
	JReferent([]interface{}{"foo", 1}, true, jtre)
	results := JFetch([]interface{}{"foo", 1}, jtre)
	if len(results) > 0 {
		fmt.Println(" Referent worked okay.")
	} else {
		panic("Referent failed.")
	}

	JReferent([]interface{}{"bar", 1}, true, jtre)
	RunJRules(jtre)
	fmt.Println(" No errors during attempted rule execution.")

	results = JFetch([]interface{}{"mumble", 1, 1}, jtre)
	if len(results) > 0 {
		fmt.Println(" :INTERN rule fired okay.")
	} else {
		panic(":INTERN rule failed to fire.")
	}

	// Test IN rules with non-numeric values
	JReferent([]interface{}{"foo", "a"}, true, jtre)
	JReferent([]interface{}{"bar", "a"}, true, jtre)
	RunJRules(jtre)

	// Check that :IN rule hasn't fired yet (facts not assumed)
	if JIn([]interface{}{"grumble", "a", "a"}, jtre) {
		fmt.Println(" Premature triggering of :IN rule.")
	}

	// Now assume the facts
	Uassume([]interface{}{"foo", "a"}, "USER", jtre)
	Uassume([]interface{}{"bar", "a"}, "USER", jtre)

	if JIn([]interface{}{"grumble", "a", "a"}, jtre) {
		fmt.Println(" :IN rule worked okay.")
	} else {
		fmt.Println(" :IN rule failed to fire.")
	}

	// Test assumption with numeric values
	Uassume([]interface{}{"foo", 1}, "USER", jtre)
	Uassume([]interface{}{"bar", 1}, "USER", jtre)
	if !JIn([]interface{}{"mumble", 1, 1}, jtre) {
		fmt.Println(" Reference or JTMS failure.")
	}

	fmt.Println("\n JTRE shakedown complete.")
}
