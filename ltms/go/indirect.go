// Indirect proof mechanism for LTRE.
// Converted from indirect.lisp.
package ltms

import "fmt"

// TryIndirectProof attempts to prove a fact by contradiction.
func TryIndirectProof(fact interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if IsKnown(fact, ltre) {
		return true
	}

	WithContradictionHandler(ltre.LTMS,
		func(contradictions []*Clause, l *LTMS) bool {
			assumptions := AssumptionsOfClause(contradictions[0])
			factDatum := Referent(fact, true, ltre)
			factNode := factDatum.TmsNode
			var theNode *TmsNode
			for _, asn := range assumptions {
				if asn == factNode {
					theNode = asn
					break
				}
			}
			if theNode != nil {
				status := theNode.Label
				RetractAssumption(theNode)
				AddNogood(theNode, status, assumptions)
				return true
			}
			return false
		},
		func() {
			Assuming([]interface{}{[]interface{}{":NOT", fact}}, ltre, func() {
				RunRules(ltre)
			})
		})

	return IsKnown(fact, ltre)
}

// IndirectProofExample demonstrates indirect proof.
func IndirectProofExample() {
	InLTRE(CreateLTRE("Indirect Proof Example", false))
	Assert([]interface{}{":OR", "p", "q"}, "user", CurrentLTRE)
	Assert([]interface{}{":IMPLIES", "p", "r"}, "user", CurrentLTRE)
	Assert([]interface{}{":IMPLIES", "q", "r"}, "user", CurrentLTRE)
	fmt.Printf("\nBefore indirect proof: r known? %v", IsKnown("r", CurrentLTRE))
	result := TryIndirectProof("r", CurrentLTRE)
	fmt.Printf("\nAfter indirect proof: r known? %v (result=%v)", IsKnown("r", CurrentLTRE), result)
}
