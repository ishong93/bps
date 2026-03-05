// LTRE: An LTMS-based Tiny Rule Engine.
// Set constraint rules module.
// Translated from setrule.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.
//
// Translated from Common Lisp to Go.

package ltms

// InstallSetRules installs the set constraint rules in the given LTRE.
// These rules enforce:
//   - NOT-IN-SET: If a set is true and has a known construal (members list),
//     then any has-member claim not in that construal is false.
//   - CONSTRUAL-UNIQUENESS: Two different construals of the same set
//     cannot both be true simultaneously.
//   - CWA-JUSTIFICATION: A CWA-justification fact implies that its
//     antecedent implies its consequent.
//
// Corresponds to the rules defined in setrule.lisp.
func InstallSetRules(ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}

	// Rule 1: (rule ((:TRUE (set ?name) :VAR ?f1)) ...)
	// Triggers on facts matching (set ?name) when they become TRUE.
	// The outer rule creates inner rules that enforce set constraints.
	InsertRule(
		GetDbclass("set", ltre),
		func(p interface{}) (bool, []interface{}, bool) {
			// Match pattern: (set ?name)
			list, ok := p.([]interface{})
			if !ok || len(list) != 2 {
				return false, nil, false
			}
			if s, ok := list[0].(string); !ok || s != "set" {
				return false, nil, false
			}
			name := list[1]
			// Return bindings: [name], hasNode=true (for :VAR ?f1)
			return true, []interface{}{name}, true
		},
		WrapTrueBody(func(args ...interface{}) {
			// args[0] = TMS node for (set ?name) (:VAR ?f1)
			// args[1] = ?name
			f1 := args[0].(*TmsNode)
			name := args[1]

			// Inner Rule 2: (rule ((:INTERN (?name members ?construal1) :VAR ?f2)) ...)
			// Matches facts like (?name members ?construal1).
			InsertRule(
				GetDbclass(name, ltre),
				func(p interface{}) (bool, []interface{}, bool) {
					list, ok := p.([]interface{})
					if !ok || len(list) != 3 {
						return false, nil, false
					}
					if !deepEqual(list[0], name) {
						return false, nil, false
					}
					if s, ok := list[1].(string); !ok || s != "members" {
						return false, nil, false
					}
					construal1 := list[2]
					return true, []interface{}{construal1}, true
				},
				func(args2 ...interface{}) {
					// args2[0] = TMS node for (?name members ?construal1) (:VAR ?f2)
					// args2[1] = ?construal1
					f2 := args2[0].(*TmsNode)
					construal1 := args2[1]

					// Inner Rule 3a: NOT-IN-SET rule
					// (rule ((:INTERN (?name has-member ?new) :VAR ?f3
					//         :TEST (not (member ?new ?construal1 :TEST #'equal))))
					//   (rassert! (:IMPLIES (:AND ?f1 ?f2) (:NOT ?f3)) :NOT-IN-SET))
					InsertRule(
						GetDbclass(name, ltre),
						func(p interface{}) (bool, []interface{}, bool) {
							list, ok := p.([]interface{})
							if !ok || len(list) != 3 {
								return false, nil, false
							}
							if !deepEqual(list[0], name) {
								return false, nil, false
							}
							if s, ok := list[1].(string); !ok || s != "has-member" {
								return false, nil, false
							}
							newEl := list[2]
							// :TEST (not (member ?new ?construal1 :TEST #'equal))
							// ?new must NOT be a member of ?construal1
							if isMemberOf(newEl, construal1) {
								return false, nil, false
							}
							return true, nil, true
						},
						func(args3 ...interface{}) {
							// args3[0] = TMS node for (?name has-member ?new) (:VAR ?f3)
							f3 := args3[0].(*TmsNode)
							// (rassert! (:IMPLIES (:AND ?f1 ?f2) (:NOT ?f3)) :NOT-IN-SET)
							RAssert(
								[]interface{}{":IMPLIES",
									[]interface{}{":AND", f1, f2},
									[]interface{}{":NOT", f3},
								},
								"NOT-IN-SET",
								ltre,
							)
						},
					)

					// Inner Rule 3b: CONSTRUAL-UNIQUENESS rule
					// (rule ((:INTERN (?name MEMBERS ?construal2) :VAR ?f3
					//          :TEST (and (form< ?f2 ?f3)
					//                     (set-exclusive-or ?construal1 ?construal2 :TEST 'equal))))
					//   (rassert! (:NOT (:AND ?f1 ?f2 ?f3)) :CONSTRUAL-UNIQUENESS))
					InsertRule(
						GetDbclass(name, ltre),
						func(p interface{}) (bool, []interface{}, bool) {
							list, ok := p.([]interface{})
							if !ok || len(list) != 3 {
								return false, nil, false
							}
							if !deepEqual(list[0], name) {
								return false, nil, false
							}
							if s, ok := list[1].(string); !ok || s != "members" {
								return false, nil, false
							}
							construal2 := list[2]
							// Get the datum for the current form to compare ordering.
							// form< checks that f2 was inserted before f3.
							thisForm := list
							membersForm := []interface{}{name, "members", construal1}
							if !FormLess(membersForm, thisForm, ltre) {
								return false, nil, false
							}
							// set-exclusive-or: construals must differ
							if !setExclusiveOr(construal1, construal2) {
								return false, nil, false
							}
							return true, nil, true
						},
						func(args3 ...interface{}) {
							// args3[0] = TMS node for (?name MEMBERS ?construal2) (:VAR ?f3)
							f3 := args3[0].(*TmsNode)
							// (rassert! (:NOT (:AND ?f1 ?f2 ?f3)) :CONSTRUAL-UNIQUENESS)
							RAssert(
								[]interface{}{":NOT",
									[]interface{}{":AND", f1, f2, f3},
								},
								"CONSTRUAL-UNIQUENESS",
								ltre,
							)
						},
					)
				},
			)
		}),
	)

	// Rule 4: CWA-JUSTIFICATION rule
	// (rule ((:INTERN (CWA-JUSTIFICATION ?ante ?conse) :VAR ?cwaj))
	//   (rassert! (:IMPLIES ?cwaj (:IMPLIES ?ante ?conse)) :CWA-JUSTIFICATION))
	InsertRule(
		GetDbclass("CWA-JUSTIFICATION", ltre),
		func(p interface{}) (bool, []interface{}, bool) {
			list, ok := p.([]interface{})
			if !ok || len(list) != 3 {
				return false, nil, false
			}
			if s, ok := list[0].(string); !ok || s != "CWA-JUSTIFICATION" {
				return false, nil, false
			}
			ante := list[1]
			conse := list[2]
			return true, []interface{}{ante, conse}, true
		},
		func(args ...interface{}) {
			// args[0] = TMS node for (CWA-JUSTIFICATION ?ante ?conse) (:VAR ?cwaj)
			// args[1] = ?ante
			// args[2] = ?conse
			cwaj := args[0].(*TmsNode)
			ante := args[1]
			conse := args[2]
			// (rassert! (:IMPLIES ?cwaj (:IMPLIES ?ante ?conse)) :CWA-JUSTIFICATION)
			RAssert(
				[]interface{}{":IMPLIES",
					cwaj,
					[]interface{}{":IMPLIES", ante, conse},
				},
				"CWA-JUSTIFICATION",
				ltre,
			)
		},
	)
}

// isMemberOf checks whether el is a member of the construal list.
// The construal is expected to be a []interface{} (a list of elements).
// Uses deepEqual for comparison, corresponding to Lisp's (member ... :TEST #'equal).
func isMemberOf(el interface{}, construal interface{}) bool {
	list, ok := construal.([]interface{})
	if !ok {
		return false
	}
	for _, item := range list {
		if deepEqual(el, item) {
			return true
		}
	}
	return false
}

// setExclusiveOr returns true if the two construals differ
// (i.e., their symmetric difference is non-empty).
// Corresponds to Lisp's (set-exclusive-or construal1 construal2 :TEST 'equal).
func setExclusiveOr(a, b interface{}) bool {
	aList, aOk := a.([]interface{})
	bList, bOk := b.([]interface{})
	if !aOk || !bOk {
		return !deepEqual(a, b)
	}
	// Check if there is any element in a not in b
	for _, ae := range aList {
		found := false
		for _, be := range bList {
			if deepEqual(ae, be) {
				found = true
				break
			}
		}
		if !found {
			return true
		}
	}
	// Check if there is any element in b not in a
	for _, be := range bList {
		found := false
		for _, ae := range aList {
			if deepEqual(ae, be) {
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
