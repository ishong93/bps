// LTRE: An LTMS-based Tiny Rule Engine.
// Indirect proof mechanism.
// Translated from indirect.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package ltms

// TryIndirectProof attempts an indirect proof for the given fact.
// It temporarily assumes the negation of the fact and checks if a
// contradiction arises. If the negated fact appears among the
// assumptions of a violated clause, the assumption is retracted and
// a nogood is recorded, effectively proving the original fact.
// Returns true if the fact becomes known after the attempt.
// Corresponds to the Lisp function try-indirect-proof.
func TryIndirectProof(fact interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if IsKnown(fact, ltre) {
		return true
	}
	WithContradictionHandler(ltre.LTMS,
		func(contradictions []*Clause, ltms *LTMS) bool {
			if len(contradictions) == 0 {
				return false
			}
			assumptions := AssumptionsOfClause(contradictions[0])
			// Find the TMS node for the fact among the assumptions.
			datum := Referent(fact, true, ltre)
			theNode := datum.TmsNode
			var found *TmsNode
			for _, a := range assumptions {
				if a == theNode {
					found = a
					break
				}
			}
			if found != nil {
				status := found.Label
				RetractAssumption(found)
				AddNogood(found, status, assumptions)
			}
			return true
		},
		func() {
			negatedFact := []interface{}{":NOT", fact}
			Assuming([]interface{}{negatedFact}, ltre, func() {
				RunRules(ltre)
			})
		},
	)
	return IsKnown(fact, ltre)
}

// IndirectProofExample demonstrates indirect proof.
// It asserts (P or Q), (P => R), and (Q => R), then uses
// indirect proof to derive R.
// Corresponds to the Lisp function indirect-proof-example.
func IndirectProofExample() bool {
	InLTRE(CreateLTRE("Indirect Proof Example", false))
	Assert([]interface{}{":OR", "p", "q"}, "user", CurrentLTRE)
	Assert([]interface{}{":IMPLIES", "p", "r"}, "user", CurrentLTRE)
	Assert([]interface{}{":IMPLIES", "q", "r"}, "user", CurrentLTRE)
	// At this point, r is not yet known.
	_ = IsKnown("r", CurrentLTRE)
	TryIndirectProof("r", CurrentLTRE)
	return IsKnown("r", CurrentLTRE)
}
