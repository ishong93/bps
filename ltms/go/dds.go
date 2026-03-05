// Dependency-Directed Search facility.
// Converted from dds.lisp.
package ltms

import "fmt"

// DebugDDS controls debug output for DDS.
var DebugDDS bool

func debugDDS(msg string, args ...interface{}) {
	if DebugDDS {
		fmt.Printf(msg, args...)
	}
}

// DDSResult is used for signaling DDS contradictions.
type DDSResult struct {
	Losers []interface{}
}

// DDSearch performs dependency-directed search.
func DDSearch(choiceSets [][]interface{}, end func(), ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	old := CurrentLTRE
	CurrentLTRE = ltre
	defer func() { CurrentLTRE = old }()

	if len(choiceSets) == 0 {
		debugDDS("\n    DDS: Found solution.")
		end()
		return
	}

	choices := choiceSets[0]
	for _, choice := range choices {
		debugDDS("\n    DDS: Considering %v...", choice)
		if IsFalse(choice, ltre) {
			debugDDS("\n    DDS: %v already known nogood.", choice)
			continue
		}
		if IsTrue(choice, ltre) {
			debugDDS("\n    DDS: %v true by implication.", choice)
			DDSearch(choiceSets[1:], end, ltre)
			return
		}

		debugDDS("\n    DDS: Assuming %v.", choice)
		var answer *DDSResult

		func() {
			WithContradictionHandler(ltre.LTMS,
				func(clauses []*Clause, l *LTMS) bool {
					for _, cl := range clauses {
						asns := AssumptionsOfClause(cl)
						for _, asn := range asns {
							choiceView := ViewNode(asn)
							if EqualForms(choice, choiceView) ||
								isNegatedEqual(choice, choiceView) {
								// Build losers list
								losers := make([]interface{}, 0)
								for _, a := range asns {
									if a != asn {
										losers = append(losers, SignedViewNode(a))
									}
								}
								answer = &DDSResult{Losers: losers}
								return true
							}
						}
					}
					return false
				},
				func() {
					Assuming([]interface{}{choice}, ltre, func() {
						RunRules(ltre)
						DDSearch(choiceSets[1:], end, ltre)
					})
				})
		}()

		if answer != nil {
			debugDDS("\n    DDS: %v inconsistent with %v.", choice, answer.Losers)
			nogoodForm := []interface{}{":NOT",
				append([]interface{}{":AND", choice}, answer.Losers...)}
			Assert(nogoodForm, ":DD-SEARCH-NOGOOD", ltre)
		}
	}
}

func isNegatedEqual(choice, view interface{}) bool {
	if lst, ok := choice.([]interface{}); ok && len(lst) == 2 {
		if s, ok := lst[0].(string); ok && s == ":NOT" {
			return EqualForms(lst[1], view)
		}
	}
	return false
}

// TestDDSearch runs a simple DDS test.
func TestDDSearch(debugging bool) {
	InLTRE(CreateLTRE("DDS Test", debugging))

	// Rule: A and C => contradiction
	MakeRule([]interface{}{"A"}, "TRUE",
		func(bindings map[string]interface{}, node *TmsNode) {
			MakeRule([]interface{}{"C"}, "TRUE",
				func(bindings2 map[string]interface{}, node2 *TmsNode) {
					RAssert([]interface{}{":NOT", []interface{}{":AND", "A", "C"}},
						":DOMAIN-NOGOOD")
				})
		})

	// Rule: B and E => contradiction
	MakeRule([]interface{}{"B"}, "TRUE",
		func(bindings map[string]interface{}, node *TmsNode) {
			MakeRule([]interface{}{"E"}, "TRUE",
				func(bindings2 map[string]interface{}, node2 *TmsNode) {
					RAssert([]interface{}{":NOT", []interface{}{":AND", "B", "E"}},
						":DOMAIN-NOGOOD")
				})
		})

	DDSearch(
		[][]interface{}{{"A", "B"}, {"C", "D"}, {"E", "F"}},
		func() { ShowDDTestSolution() },
		CurrentLTRE,
	)
}

// ShowDDTestSolution displays the DD search test solution.
func ShowDDTestSolution() {
	var result []interface{}
	for _, v := range []string{"F", "E", "D", "C", "B", "A"} {
		if IsTrue(v, CurrentLTRE) {
			result = append([]interface{}{v}, result...)
		}
	}
	fmt.Printf("\n Consistent solution: %v.", result)
}
