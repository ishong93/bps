// Basic rules for JSAINT.
// AND/OR subgoal management rules.
// Translated from jsrules.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

// RegisterJSaintRules registers the bookkeeping rules for JSAINT.
// These rules manage AND/OR subgoal expansion, solution detection,
// and failure propagation.
// Corresponds to the rules defined in jsrules.lisp.
func RegisterJSaintRules(jtre *Jtre) {
	// Rule: When AND-SUBGOALS is IN, expand pointers
	andDbclass := JGetDbclass([]interface{}{"AND-SUBGOALS"}, jtre)
	InsertJRule(andDbclass,
		func(p interface{}) (bool, []interface{}, bool) {
			ps, ok := p.([]interface{})
			if !ok || len(ps) < 3 || ps[0] != "AND-SUBGOALS" {
				return false, nil, false
			}
			return true, []interface{}{ps[1], ps[2]}, true
		},
		func(args ...interface{}) {
			if len(args) < 3 {
				return
			}
			parent := args[1]
			children, ok := args[2].([]interface{})
			if !ok {
				return
			}
			for _, child := range children {
				JAssert([]interface{}{"PARENT-OF", child, parent, "AND"},
					[]interface{}{":DEF-OF-AND"}, jtre)
			}
			// Assert solved when all children solved
			solvedJusts := make([]interface{}, 0, len(children)+1)
			solvedJusts = append(solvedJusts, ":AND-SUCCESS")
			for _, child := range children {
				solvedJusts = append(solvedJusts, []interface{}{"SOLVED", child})
			}
			JAssert([]interface{}{"solved", parent}, solvedJusts, jtre)
		},
	)

	// Rule: When OR-SUBGOALS is IN, set up OR expansion
	orDbclass := JGetDbclass([]interface{}{"OR-SUBGOALS"}, jtre)
	InsertJRule(orDbclass,
		func(p interface{}) (bool, []interface{}, bool) {
			ps, ok := p.([]interface{})
			if !ok || len(ps) < 3 || ps[0] != "OR-SUBGOALS" {
				return false, nil, false
			}
			return true, []interface{}{ps[1], ps[2]}, true
		},
		func(args ...interface{}) {
			if len(args) < 3 {
				return
			}
			parent := args[1]
			children, ok := args[2].([]interface{})
			if !ok {
				return
			}
			for _, child := range children {
				JAssert([]interface{}{"PARENT-OF", child, parent, "OR"},
					[]interface{}{":DEF-OF-OR"}, jtre)
			}
		},
	)

	// Rule: When PARENT-OF is IN, mark child as relevant
	parentDbclass := JGetDbclass([]interface{}{"PARENT-OF"}, jtre)
	InsertJRule(parentDbclass,
		func(p interface{}) (bool, []interface{}, bool) {
			ps, ok := p.([]interface{})
			if !ok || len(ps) < 4 || ps[0] != "PARENT-OF" {
				return false, nil, false
			}
			return true, []interface{}{ps[1], ps[2], ps[3]}, true
		},
		func(args ...interface{}) {
			if len(args) < 4 {
				return
			}
			child := args[1]
			JAssert([]interface{}{"RELEVANT", child},
				[]interface{}{":STILL-WORKING-ON"}, jtre)
		},
	)

	// Rule: When SOLUTION-OF is IN, mark as SOLVED
	solnDbclass := JGetDbclass([]interface{}{"SOLUTION-OF"}, jtre)
	InsertJRule(solnDbclass,
		func(p interface{}) (bool, []interface{}, bool) {
			ps, ok := p.([]interface{})
			if !ok || len(ps) < 3 || ps[0] != "SOLUTION-OF" {
				return false, nil, false
			}
			return true, []interface{}{ps[1], ps[2]}, true
		},
		func(args ...interface{}) {
			if len(args) < 3 {
				return
			}
			problem := args[1]
			JAssert([]interface{}{"SOLVED", problem},
				[]interface{}{":FOUND-ANSWER"}, jtre)
		},
	)

	// Rule: When SOLVED, close the problem
	solvedDbclass := JGetDbclass([]interface{}{"SOLVED"}, jtre)
	InsertJRule(solvedDbclass,
		func(p interface{}) (bool, []interface{}, bool) {
			ps, ok := p.([]interface{})
			if !ok || len(ps) < 2 || ps[0] != "SOLVED" {
				return false, nil, false
			}
			return true, []interface{}{ps[1]}, true
		},
		func(args ...interface{}) {
			if len(args) < 2 {
				return
			}
			problem := args[1]
			JRetract([]interface{}{"OPEN", problem}, "EXPAND-AGENDA-ITEM", true, jtre)
		},
	)

	// Rule: When FAILED, close the problem
	failedDbclass := JGetDbclass([]interface{}{"FAILED"}, jtre)
	InsertJRule(failedDbclass,
		func(p interface{}) (bool, []interface{}, bool) {
			ps, ok := p.([]interface{})
			if !ok || len(ps) < 2 || ps[0] != "FAILED" {
				return false, nil, false
			}
			return true, []interface{}{ps[1]}, true
		},
		func(args ...interface{}) {
			if len(args) < 2 {
				return
			}
			problem := args[1]
			JRetract([]interface{}{"OPEN", problem}, "EXPAND-AGENDA-ITEM", true, jtre)
		},
	)
}
