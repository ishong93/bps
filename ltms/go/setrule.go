// Set constraint rules for CWA.
// Converted from setrule.lisp.
package ltms

// LoadSetRules installs the rules for set membership reasoning.
func LoadSetRules(ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	old := CurrentLTRE
	CurrentLTRE = ltre
	defer func() { CurrentLTRE = old }()

	// Rule 1: When (set ?name) is true, install sub-rules for membership
	MakeRule([]interface{}{"SET", "?name"}, "TRUE",
		func(bindings map[string]interface{}, triggerNode *TmsNode) {
			name := bindings["?name"]

			// Sub-rule: When (?name MEMBERS ?construal1) is interned
			MakeRule([]interface{}{name, "MEMBERS", "?construal1"}, "INTERN",
				func(bindings2 map[string]interface{}, node2 *TmsNode) {
					construal1 := bindings2["?construal1"]

					// Sub-sub-rule: When (?name HAS-MEMBER ?new) is interned
					// and ?new is not in ?construal1
					MakeRule([]interface{}{name, "HAS-MEMBER", "?new"}, "INTERN",
						func(bindings3 map[string]interface{}, node3 *TmsNode) {
							newMember := bindings3["?new"]
							if construal1 == nil {
								return
							}
							// Test: new not in construal1
							if members, ok := construal1.([]interface{}); ok {
								for _, m := range members {
									if EqualForms(m, newMember) {
										return // already in set
									}
								}
							}
							// Get the datum nodes for the assertion
							setDatum := Referent([]interface{}{"SET", name}, false, CurrentLTRE)
							membersDatum := Referent([]interface{}{name, "MEMBERS", construal1}, false, CurrentLTRE)
							newDatum := Referent([]interface{}{name, "HAS-MEMBER", newMember}, false, CurrentLTRE)
							if setDatum != nil && membersDatum != nil && newDatum != nil {
								RAssert([]interface{}{":IMPLIES",
									[]interface{}{":AND",
										setDatum.TmsNode,
										membersDatum.TmsNode},
									[]interface{}{":NOT", newDatum.TmsNode}},
									":NOT-IN-SET")
							}
						})

					// Sub-sub-rule: Construal uniqueness
					MakeRule([]interface{}{name, "MEMBERS", "?construal2"}, "INTERN",
						func(bindings4 map[string]interface{}, node4 *TmsNode) {
							construal2 := bindings4["?construal2"]
							// Test: construal2 != construal1 and different sets
							if EqualForms(construal1, construal2) {
								return
							}
							// Check form ordering
							d1 := Referent([]interface{}{name, "MEMBERS", construal1}, false, CurrentLTRE)
							d2 := Referent([]interface{}{name, "MEMBERS", construal2}, false, CurrentLTRE)
							if d1 == nil || d2 == nil || d1.Counter >= d2.Counter {
								return
							}
							// Check symmetric difference
							if !hasSymmetricDifference(construal1, construal2) {
								return
							}
							setDatum := Referent([]interface{}{"SET", name}, false, CurrentLTRE)
							if setDatum != nil && d1 != nil && d2 != nil {
								RAssert([]interface{}{":NOT",
									[]interface{}{":AND",
										setDatum.TmsNode,
										d1.TmsNode,
										d2.TmsNode}},
									":CONSTRUAL-UNIQUENESS")
							}
						})
				})
		})

	// Rule 2: CWA-JUSTIFICATION rule
	MakeRule([]interface{}{"CWA-JUSTIFICATION", "?ante", "?conse"}, "INTERN",
		func(bindings map[string]interface{}, node *TmsNode) {
			ante := bindings["?ante"]
			conse := bindings["?conse"]
			cwajDatum := Referent([]interface{}{"CWA-JUSTIFICATION", ante, conse}, false, CurrentLTRE)
			if cwajDatum != nil {
				RAssert([]interface{}{":IMPLIES",
					cwajDatum.TmsNode,
					[]interface{}{":IMPLIES", ante, conse}},
					":CWA-JUSTIFICATION")
			}
		})
}

func hasSymmetricDifference(a, b interface{}) bool {
	aList, aOk := a.([]interface{})
	bList, bOk := b.([]interface{})
	if !aOk || !bOk {
		return !EqualForms(a, b)
	}
	// Check if there's any element in a not in b, or vice versa
	for _, ae := range aList {
		found := false
		for _, be := range bList {
			if EqualForms(ae, be) {
				found = true
				break
			}
		}
		if !found {
			return true
		}
	}
	for _, be := range bList {
		found := false
		for _, ae := range aList {
			if EqualForms(ae, be) {
				found = true
				break
			}
		}
		if !found {
			return true
		}
	}
	return false
}
