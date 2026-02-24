// N-Queens puzzle solver using dependency-directed search with JTRE.
// Translated from jqueens.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

import (
	"fmt"
	"math"
	"time"
)

// Queens puzzle state
var (
	NAssumptions int
	Placements   [][]interface{}
)

// TestQueens runs the N-Queens solver from `from` to `to`.
// Corresponds to the Lisp function test-queens.
func TestQueens(from, to int) {
	for n := from; n <= to; n++ {
		start := time.Now()
		NQueens(n, false)
		elapsed := time.Since(start)
		fmt.Printf("\n For n=%d, %d solutions, %d assumptions. (%v)",
			n, len(Placements), NAssumptions, elapsed)
	}
}

// NQueens solves the N-Queens puzzle.
// Returns the number of solutions found.
// Corresponds to the Lisp function n-queens.
func NQueens(n int, debugging bool) int {
	jtre := SetupQueensPuzzle(n, debugging)
	choiceSets := MakeQueensChoiceSets(n)
	SolveQueensPuzzle(choiceSets, jtre)
	return len(Placements)
}

// SetupQueensPuzzle initializes the JTRE for the queens puzzle.
// Corresponds to the Lisp function setup-queens-puzzle.
func SetupQueensPuzzle(n int, debugging bool) *Jtre {
	jtre := CreateJtre(fmt.Sprintf("%d-Queens JTRE", n), debugging)
	Placements = nil
	NAssumptions = 0
	RegisterQueensRules(jtre)
	return jtre
}

// MakeQueensChoiceSets creates the choice sets for column placements.
// Each set contains all possible placements for one column.
// Corresponds to the Lisp function make-queens-choice-sets.
func MakeQueensChoiceSets(n int) [][]interface{} {
	choiceSets := make([][]interface{}, 0, n)
	for col := 1; col <= n; col++ {
		columnQueens := make([]interface{}, 0, n)
		for row := 1; row <= n; row++ {
			columnQueens = append(columnQueens, []interface{}{"Queen", col, row})
		}
		choiceSets = append(choiceSets, columnQueens)
	}
	return choiceSets
}

// SolveQueensPuzzle performs dependency-directed search.
// Corresponds to the Lisp function solve-queens-puzzle.
func SolveQueensPuzzle(choiceSets [][]interface{}, jtre *Jtre) {
	if len(choiceSets) == 0 {
		GatherQueensSolution(jtre)
		return
	}
	for _, choice := range choiceSets[0] {
		negChoice := []interface{}{"not", choice}
		if JIn(negChoice, jtre) {
			continue // respect nogood information
		}
		nogood, asns := TryInContext(choice, func() {
			SolveQueensPuzzle(choiceSets[1:], jtre)
		}, jtre)
		NAssumptions++
		if nogood {
			// This assumption lost, justify the negation
			filtered := removeFromSlice(choice, asns)
			justification := append([]interface{}{"Nogood"}, filtered...)
			JAssert(negChoice, justification, jtre)
		}
	}
}

// TryInContext attempts to assume a fact and execute a thunk.
// If a contradiction is found, returns (true, assumptions).
// Otherwise returns (false, nil).
// Corresponds to the Lisp function try-in-context.
func TryInContext(asn interface{}, thunk func(), jtre *Jtre) (bool, []interface{}) {
	tryMarker := []interface{}{"TRY", asn}

	type contradictionResult struct {
		asns []*TmsNode
	}

	var result *contradictionResult

	handler := func(jtmsArg *JTMS, contras []*TmsNode) {
		for _, cnode := range contras {
			asns := AssumptionsOfNode(cnode)
			asnNode := GetJTmsNode(asn, jtre)
			for _, a := range asns {
				if a == asnNode {
					JRetract(asn, tryMarker, true, jtre)
					result = &contradictionResult{asns: asns}
					return
				}
			}
		}
	}

	WithContradictionHandler(jtre.JTMS, handler, func() {
		if JIn(asn, jtre) {
			return
		}
		JAssume(asn, tryMarker, jtre)
		if result != nil {
			return
		}
		RunJRules(jtre)
		if result != nil {
			return
		}
		thunk()
		if result == nil {
			JRetract(asn, tryMarker, true, jtre)
		}
	})

	if result != nil {
		views := make([]interface{}, len(result.asns))
		for i, n := range result.asns {
			views[i] = ViewNode(n)
		}
		return true, views
	}
	return false, nil
}

// QueensOkay checks if two queen positions are non-attacking.
// Corresponds to the Lisp function queens-okay?.
func QueensOkay(x1, y1, x2, y2 int) bool {
	return y1 != y2 && math.Abs(float64(x1-x2)) != math.Abs(float64(y1-y2))
}

// GatherQueensSolution collects a valid solution.
// Corresponds to the Lisp function gather-queens-solution.
func GatherQueensSolution(jtre *Jtre) {
	candidates := JFetch([]interface{}{"Queen", "?c", "?r"}, jtre)
	var solution []interface{}
	for _, q := range candidates {
		if JIn(q, jtre) {
			solution = append(solution, q)
		}
	}
	Placements = append(Placements, solution)
}

// ShowQueensSolution displays a queens solution on the board.
// Corresponds to the Lisp function show-queens-solution.
func ShowQueensSolution(solution []interface{}) {
	n := len(solution)
	for i := 0; i < n; i++ {
		fmt.Println()
		for j := 0; j < n; j++ {
			found := false
			for _, q := range solution {
				qs, ok := q.([]interface{})
				if ok && len(qs) == 3 {
					if deepEqual(qs[1], i) && deepEqual(qs[2], j) {
						found = true
						break
					}
				}
			}
			if found {
				fmt.Print("Q")
			} else {
				fmt.Print("-")
			}
		}
	}
}

// removeFromSlice removes an element from a slice using deepEqual.
func removeFromSlice(elem interface{}, slice []interface{}) []interface{} {
	var result []interface{}
	for _, s := range slice {
		if !deepEqual(s, elem) {
			result = append(result, s)
		}
	}
	return result
}
