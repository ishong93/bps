// Marx Brothers puzzle data and rules.
// Converted from marxdata.lisp.
package ltms

// LoadMarxData loads the Marx Brothers puzzle constraints and rules.
func LoadMarxData(ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	old := CurrentLTRE
	CurrentLTRE = ltre
	defer func() { CurrentLTRE = old }()

	// Pairwise nogoods - attributes that can't be the same person
	RAssert([]interface{}{"PAIRWISE-NOGOOD", "PLAYS-PIANO", "PLAYS-HARP"}, "CONSTRAINT")
	RAssert([]interface{}{"PAIRWISE-NOGOOD", "PLAYS-PIANO", "SMOOTH-TALKER"}, "CONSTRAINT")
	RAssert([]interface{}{"PAIRWISE-NOGOOD", "PLAYS-HARP", "SMOOTH-TALKER"}, "CONSTRAINT")
	RAssert([]interface{}{"PAIRWISE-NOGOOD", "LIKES-GAMBLING", "LIKES-ANIMALS"}, "CONSTRAINT")
	RAssert([]interface{}{"PAIRWISE-NOGOOD", "SMOOTH-TALKER", "LIKES-GAMBLING"}, "CONSTRAINT")

	// Same entity constraints
	RAssert([]interface{}{"SAME-ENTITY", "LIKES-ANIMALS", "PLAYS-HARP"}, "CONSTRAINT")

	// Known facts
	RAssert([]interface{}{":NOT", []interface{}{"LIKES-ANIMALS", "GROUCHO"}}, "CONSTRAINT")
	RAssert([]interface{}{":NOT", []interface{}{"SMOOTH-TALKER", "HARPO"}}, "CONSTRAINT")
	RAssert([]interface{}{"PLAYS-PIANO", "CHICO"}, "CONSTRAINT")

	// Rule: pairwise-nogood means two attributes can't apply to the same object
	MakeRule([]interface{}{"PAIRWISE-NOGOOD", "?attribute1", "?attribute2"}, "TRUE",
		func(bindings map[string]interface{}, horNode *TmsNode) {
			attr1 := bindings["?attribute1"]
			attr2 := bindings["?attribute2"]

			MakeRule([]interface{}{attr1, "?obj"}, "TRUE",
				func(bindings2 map[string]interface{}, f1Node *TmsNode) {
					obj := bindings2["?obj"]

					MakeRule([]interface{}{attr2, obj}, "TRUE",
						func(bindings3 map[string]interface{}, f2Node *TmsNode) {
							RAssert([]interface{}{":NOT",
								[]interface{}{":AND",
									ViewNode(horNode),
									ViewNode(f1Node),
									ViewNode(f2Node)}},
								"PAIRWISE-NOGOOD-RULE")
						})
				})
		})

	// Rule: same-entity means if attribute1 applies to obj, so does attribute2
	MakeRule([]interface{}{"SAME-ENTITY", "?attribute1", "?attribute2"}, "TRUE",
		func(bindings map[string]interface{}, horNode *TmsNode) {
			attr1 := bindings["?attribute1"]
			attr2 := bindings["?attribute2"]

			MakeRule([]interface{}{attr1, "?obj"}, "TRUE",
				func(bindings2 map[string]interface{}, f1Node *TmsNode) {
					obj := bindings2["?obj"]
					RAssert([]interface{}{":IMPLIES",
						[]interface{}{":AND",
							ViewNode(horNode),
							ViewNode(f1Node)},
						[]interface{}{attr2, obj}},
						"SAME-ENTITY-RULE")
				})
		})

	RunRules(ltre)
}
