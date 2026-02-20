// -*- Mode: Go -*-

// Antecedent Planner (a.k.a. Plan-A)
// Translated from plan-a.lisp

// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms

import "fmt"

// PlanA performs antecedent planning from a start state to a goal.
// It does breadth-first search through the state space.
// Returns the plan (list of state, operator, state, ...) and the number of states examined.
func PlanA(start *Env, goal []interface{}, plnpr *Plnpr) ([]interface{}, int) {
	atre := plnpr.Atre
	queue := [][]interface{}{{start}}
	numberExamined := 1

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		currentState, ok := current[0].(*Env)
		if !ok {
			continue
		}

		// Check if current state satisfies goal
		satisfied, _ := SatisfiesGoal(currentState, goal, plnpr)
		if satisfied {
			plnpr.Plist["PLAN"] = current
			return current, numberExamined
		}

		// Try all applicable operators
		for _, opInst := range FindApplicableOperators(currentState, plnpr) {
			result := ApplyOperator(currentState, opInst, plnpr, atre)
			if result == nil {
				continue
			}

			// Check if result is already in the current path (avoid loops)
			alreadyVisited := false
			for _, item := range current {
				if env, ok := item.(*Env); ok && env == result {
					alreadyVisited = true
					break
				}
			}

			if !alreadyVisited {
				DebugPlnpr(plnpr, nil,
					"\n  Reaching %v via %v on %v..",
					result, opInst, currentState)

				// Build new path: result, opInst, current...
				newPath := make([]interface{}, 0, len(current)+2)
				newPath = append(newPath, result)
				newPath = append(newPath, fmt.Sprintf("%v", opInst))
				newPath = append(newPath, current...)
				queue = append(queue, newPath)
			}
		}

		numberExamined++
	}

	plnpr.Plist["PLAN"] = nil
	return nil, numberExamined
}
