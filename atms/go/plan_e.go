// -*- Mode: Go -*-

// ATMS-based Envisioner for planning problems
// Translated from plan-e.lisp

// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms

import (
	"fmt"
	"io"
	"os"
)

// Transition represents a state transition via an operator.
type Transition struct {
	OpInst   []interface{}
	NewState *Env
}

// StateEntry represents a state and its possible transitions.
type StateEntry struct {
	State       *Env
	Transitions []Transition
}

// Envision generates all states and transitions for a planning problem.
func Envision(plnpr *Plnpr) []StateEntry {
	choiceSets := make([][]*TmsNode, 0)
	for _, bs := range plnpr.BasisSet {
		var nodes []*TmsNode
		for _, choice := range bs {
			nodes = append(nodes, GetTmsNode(choice, plnpr.Atre))
		}
		choiceSets = append(choiceSets, nodes)
	}
	states := Interpretations(plnpr.Atre.ATMS, choiceSets, nil)
	plnpr.Plist["STATES"] = states

	transTable := applyAllOperators(states, plnpr)
	plnpr.Plist["TRANSITIONS"] = transTable
	return transTable
}

func applyAllOperators(states []*Env, plnpr *Plnpr) []StateEntry {
	atre := plnpr.Atre
	var result []StateEntry
	for _, state := range states {
		entry := StateEntry{State: state}
		for _, opInst := range FindApplicableOperators(state, plnpr) {
			newState := ApplyOperator(state, opInst, plnpr, atre)
			if newState != nil {
				entry.Transitions = append(entry.Transitions, Transition{
					OpInst:   opInst,
					NewState: newState,
				})
			}
		}
		result = append(result, entry)
	}
	return result
}

// ShowEnvisionment displays the generated states and transition table.
func ShowEnvisionment(plnpr *Plnpr, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	statesVal := plnpr.Plist["STATES"]
	states, ok := statesVal.([]*Env)
	if !ok || len(states) == 0 {
		fmt.Fprintf(w, "\nThe state space is empty.")
		return
	}

	fmt.Fprintf(w, "\n %d states have been generated:", len(states))
	for _, state := range states {
		PrintEnv(state, w)
	}

	fmt.Fprintf(w, "\nTransition Table:")
	transVal := plnpr.Plist["TRANSITIONS"]
	transTable, ok := transVal.([]StateEntry)
	if !ok || len(transTable) == 0 {
		fmt.Fprintf(w, " empty.")
		return
	}

	for _, entry := range transTable {
		fmt.Fprintf(w, "\n  %v: ", entry.State)
		for _, t := range entry.Transitions {
			fmt.Fprintf(w, "\n   %v -> %v", t.OpInst, t.NewState)
		}
	}
}

// FindPlan searches the envisionment for a plan from start to goals.
func FindPlan(start []interface{}, goals []interface{}, plnpr *Plnpr) []interface{} {
	goalStates := FetchStates(goals, plnpr)
	startStates := FetchStates(start, plnpr)

	DebugPlnpr(plnpr, os.Stdout, "\nInitial states are %v.", startStates)
	DebugPlnpr(plnpr, os.Stdout, "\nGoal states are %v.", goalStates)

	// Get transitions from plist
	transVal := plnpr.Plist["TRANSITIONS"]
	transTable, _ := transVal.([]StateEntry)

	// Initialize queue with start states
	queue := make([][]interface{}, 0)
	for _, state := range startStates {
		queue = append(queue, []interface{}{state})
	}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]

		currentState, ok := current[0].(*Env)
		if !ok {
			continue
		}

		// Check if current state is a goal state
		isGoal := false
		for _, gs := range goalStates {
			if gs == currentState {
				isGoal = true
				break
			}
		}
		if isGoal {
			plnpr.Plist["PLAN"] = current
			return current
		}

		// Find transitions for current state
		for _, entry := range transTable {
			if entry.State != currentState {
				continue
			}
			for _, trans := range entry.Transitions {
				// Avoid loops
				alreadyVisited := false
				for i := 2; i < len(current); i++ {
					if env, ok := current[i].(*Env); ok && env == trans.NewState {
						alreadyVisited = true
						break
					}
				}
				if !alreadyVisited {
					DebugPlnpr(plnpr, os.Stdout,
						"\n Can reach %v via %v from %v.",
						trans.NewState, trans.OpInst, currentState)
					newPath := make([]interface{}, 0, len(current)+2)
					newPath = append(newPath, trans.NewState)
					newPath = append(newPath, trans.OpInst)
					newPath = append(newPath, current...)
					queue = append(queue, newPath)
				}
			}
		}
	}

	plnpr.Plist["PLAN"] = nil
	return nil
}
