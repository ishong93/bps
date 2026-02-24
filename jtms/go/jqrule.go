// N-Queens rules for JTRE.
// Translated from jqrule.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

// RegisterQueensRules sets up the contradiction and capture rules
// for the N-Queens puzzle.
// Corresponds to the jqrule.lisp file which declares:
//   (contradiction 'Queens-capture *jtre*)
//   (rule ((:IN (Queen ?column1 ?row1) :VAR ?Q1)
//          (:IN (Queen ?column2 ?row2) :VAR ?Q2
//            :TEST (not (or (= ?column1 ?column2)
//                           (queens-okay? ...)))))
//         (rassert! Queens-capture (Death ?Q1 ?Q2)))
func RegisterQueensRules(jtre *Jtre) {
	// Declare Queens-capture as a contradiction
	JContradiction([]interface{}{"Queens-capture"}, jtre)

	// Register the queen-capture detection rule.
	// When two queens are placed and they attack each other,
	// assert the Queens-capture contradiction.
	queenDbclass := JGetDbclass([]interface{}{"Queen"}, jtre)

	InsertJRule(queenDbclass,
		// Matcher: check if this is a (Queen col row) fact
		func(p interface{}) (bool, []interface{}, bool) {
			ps, ok := p.([]interface{})
			if !ok || len(ps) != 3 || ps[0] != "Queen" {
				return false, nil, false
			}
			return true, []interface{}{ps[1], ps[2]}, true // need node
		},
		// Body: for each queen placement, check against all others
		func(args ...interface{}) {
			if len(args) < 3 {
				return
			}
			q1Node := args[0].(*TmsNode)
			col1, _ := toInt(args[1])
			row1, _ := toInt(args[2])

			// Check against all other Queen facts
			for _, datum := range queenDbclass.Facts {
				qs, ok := datum.LispForm.([]interface{})
				if !ok || len(qs) != 3 {
					continue
				}
				col2, _ := toInt(qs[1])
				row2, _ := toInt(qs[2])
				if col1 == col2 {
					continue // same column, skip
				}
				if QueensOkay(col1, row1, col2, row2) {
					continue // non-attacking, skip
				}
				q2Node := datum.TmsNode
				// Assert Queens-capture justified by the two queen nodes
				captureNode := JReferent([]interface{}{"Queens-capture"}, true, jtre).TmsNode
				JustifyNode(
					[]interface{}{"Death", q1Node, q2Node},
					captureNode,
					[]*TmsNode{q1Node, q2Node},
				)
			}
		},
	)
}

// toInt converts an interface{} to int.
func toInt(x interface{}) (int, bool) {
	switch v := x.(type) {
	case int:
		return v, true
	case int64:
		return int(v), true
	case float64:
		return int(v), true
	default:
		return 0, false
	}
}
