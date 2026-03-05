// LTRE: An LTMS-based Tiny Rule Engine.
// Dependency-Directed Search.
// Translated from dds.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package ltms

import "fmt"

// DebugDDS controls whether DDS debugging output is printed.
var DebugDDS bool

// debugDDS prints a debug message if DebugDDS is enabled.
func debugDDS(format string, args ...interface{}) {
	if DebugDDS {
		fmt.Printf(format, args...)
	}
}

// ddsThrow is used as a panic value to simulate Lisp's catch/throw.
// The marker field identifies the catch tag, and losers carries the
// list of nogood facts discovered during the search.
type ddsThrow struct {
	marker interface{}
	losers []interface{}
}

// DDSearch performs dependency-directed search over the given choice sets.
// Each element of choiceSets is a slice of facts (interface{} values).
// When all choice sets are exhausted (a solution is found), end is called.
// Corresponds to the Lisp function DD-Search.
func DDSearch(choiceSets [][]interface{}, end func(), ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	ddSearch1(choiceSets, end, ltre)
}

// ddSearch1 is the internal recursive implementation of DDSearch.
func ddSearch1(choiceSets [][]interface{}, end func(), ltre *LTRE) {
	if len(choiceSets) == 0 {
		debugDDS("\n    DDS: Found solution.")
		end()
		return
	}

	choices := choiceSets[0]
	remaining := choiceSets[1:]
	// The marker is a unique value used to match the catch/throw tag.
	marker := &[2]interface{}{"DDS", choices}

	for _, choice := range choices {
		debugDDS("\n    DDS: Considering %v...", choice)

		if IsFalse(choice, ltre) {
			debugDDS("\n    DDS: %v already known nogood.", choice)
			continue
		}

		if IsTrue(choice, ltre) {
			debugDDS("\n    DDS: %v true by implication.", choice)
			ddSearch1(remaining, end, ltre)
			return
		}

		debugDDS("\n    DDS: Assuming %v.", choice)

		// Use panic/recover to simulate Lisp catch/throw.
		var answer *ddsThrow
		func() {
			defer func() {
				if r := recover(); r != nil {
					if dt, ok := r.(*ddsThrow); ok && dt.marker == marker {
						answer = dt
					} else {
						// Not our throw; re-panic.
						panic(r)
					}
				}
			}()

			WithContradictionHandler(ltre.LTMS,
				func(clauses []*Clause, ltms *LTMS) bool {
					for _, cl := range clauses {
						asns := AssumptionsOfClause(cl)
						for _, asn := range asns {
							viewedNode := LViewNode(asn)
							matched := false
							if deepEqual(choice, viewedNode) {
								matched = true
							} else if choiceList, ok := choice.([]interface{}); ok &&
								len(choiceList) >= 2 {
								if s, ok := choiceList[0].(string); ok && s == ":NOT" {
									if deepEqual(choiceList[1], viewedNode) {
										matched = true
									}
								}
							}
							if matched {
								// Build the losers list: signed views of all
								// assumptions except the matched one.
								var losers []interface{}
								for _, other := range asns {
									if other != asn {
										losers = append(losers, SignedViewNode(other))
									}
								}
								panic(&ddsThrow{
									marker: marker,
									losers: losers,
								})
							}
						}
					}
					return true
				},
				func() {
					Assuming([]interface{}{choice}, ltre, func() {
						RunRules(ltre)
						ddSearch1(remaining, end, ltre)
					})
				},
			)
		}()

		if answer != nil {
			// Assert a nogood: (:NOT (:AND choice losers...))
			andArgs := []interface{}{":AND", choice}
			andArgs = append(andArgs, answer.losers...)
			nogood := []interface{}{":NOT", andArgs}
			Assert(nogood, ":DD-SEARCH-NOGOOD", ltre)
		}
	}
}

// TestDDSearch demonstrates dependency-directed search.
// It sets up domain constraints and searches over choice sets.
// Corresponds to the Lisp function Test-DD-search.
func TestDDSearch(debugging bool) {
	ltre := CreateLTRE("DDS Test", debugging)
	InLTRE(ltre)

	// Rule: (:TRUE A) and (:TRUE C) => assert (:NOT (:AND A C))
	// Outer trigger on "A" with :TRUE
	InsertRule(
		GetDbclass("A", ltre),
		func(p interface{}) (bool, []interface{}, bool) {
			if s, ok := p.(string); ok && s == "A" {
				return true, nil, true
			}
			return false, nil, false
		},
		WrapTrueBody(func(args ...interface{}) {
			nodeA := args[0].(*TmsNode)
			// Inner trigger on "C" with :TRUE
			InsertRule(
				GetDbclass("C", ltre),
				func(p interface{}) (bool, []interface{}, bool) {
					if s, ok := p.(string); ok && s == "C" {
						return true, nil, true
					}
					return false, nil, false
				},
				WrapTrueBody(func(innerArgs ...interface{}) {
					nodeC := innerArgs[0].(*TmsNode)
					RAssert([]interface{}{":NOT", []interface{}{":AND", nodeA, nodeC}},
						"DOMAIN-NOGOOD", ltre)
				}),
			)
		}),
	)

	// Rule: (:TRUE B) and (:TRUE E) => assert (:NOT (:AND B E))
	InsertRule(
		GetDbclass("B", ltre),
		func(p interface{}) (bool, []interface{}, bool) {
			if s, ok := p.(string); ok && s == "B" {
				return true, nil, true
			}
			return false, nil, false
		},
		WrapTrueBody(func(args ...interface{}) {
			nodeB := args[0].(*TmsNode)
			InsertRule(
				GetDbclass("E", ltre),
				func(p interface{}) (bool, []interface{}, bool) {
					if s, ok := p.(string); ok && s == "E" {
						return true, nil, true
					}
					return false, nil, false
				},
				WrapTrueBody(func(innerArgs ...interface{}) {
					nodeE := innerArgs[0].(*TmsNode)
					RAssert([]interface{}{":NOT", []interface{}{":AND", nodeB, nodeE}},
						"DOMAIN-NOGOOD", ltre)
				}),
			)
		}),
	)

	DDSearch(
		[][]interface{}{
			{"A", "B"},
			{"C", "D"},
			{"E", "F"},
		},
		func() {
			fmt.Println("\n  Solution:")
			for _, fact := range []string{"A", "B", "C", "D", "E", "F"} {
				if IsTrue(fact, ltre) {
					fmt.Printf("    %s is TRUE\n", fact)
				} else if IsFalse(fact, ltre) {
					fmt.Printf("    %s is FALSE\n", fact)
				}
			}
		},
		ltre,
	)
}
