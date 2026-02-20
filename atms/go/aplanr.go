// -*- Mode: Go -*-

// ATMS-based planner using ATRE + ATMS
// Translated from aplanr.lisp

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

// Plnpr represents a planning problem.
type Plnpr struct {
	Title    string
	Atre     *Atre
	BasisSet [][][]interface{} // list of choice sets, each is a list of choices (facts)
	Operators map[string]*Operator // keyed by operator name
	Plist    map[string]interface{} // property list for caching results
}

// Operator represents a planning operator with preconditions and effects.
type Operator struct {
	Form          []interface{}
	Preconditions [][]interface{}
	AddList       [][]interface{}
	DeleteList    [][]interface{}
}

func (p *Plnpr) String() string {
	return fmt.Sprintf("<PlnPr: %s>", p.Title)
}

func (o *Operator) String() string {
	return fmt.Sprintf("<Operator %v>", o.Form)
}

// CreatePlanningProblem creates a new planning problem with the given title and basis set.
func CreatePlanningProblem(title string, basisSet [][][]interface{}) *Plnpr {
	plnpr := &Plnpr{
		Title:     title,
		Atre:      CreateAtre(fmt.Sprintf("ATRE(%s)", title), false),
		BasisSet:  basisSet,
		Operators: make(map[string]*Operator),
		Plist:     make(map[string]interface{}),
	}
	return plnpr
}

// SetupChoiceSets sets up the choice sets for the planning problem.
func SetupChoiceSets(plnpr *Plnpr) {
	informant := fmt.Sprintf("BASIS SET(%s)", plnpr.Title)
	atre := plnpr.Atre
	for _, choiceSet := range plnpr.BasisSet {
		for _, choice := range choiceSet {
			AssumeIfNeeded(choice, informant, atre)
		}
	}
	RunRules(atre)
}

// SetDebugPlnpr sets the debugging state for a planning problem.
func SetDebugPlnpr(state bool, plnpr *Plnpr) {
	plnpr.Plist["DEBUGGING"] = state
}

// DebugPlnpr prints debug info if debugging is enabled.
func DebugPlnpr(plnpr *Plnpr, w io.Writer, format string, args ...interface{}) {
	if debugging, ok := plnpr.Plist["DEBUGGING"].(bool); ok && debugging {
		fmt.Fprintf(w, format, args...)
	}
}

// DefineOperator registers an operator in the planning problem.
func DefineOperator(plnpr *Plnpr, form []interface{}, preconditions, addList, deleteList [][]interface{}) {
	opName := fmt.Sprintf("%v", form[0])
	op := &Operator{
		Form:          form,
		Preconditions: preconditions,
		AddList:       addList,
		DeleteList:    deleteList,
	}
	plnpr.Operators[opName] = op
}

// FindApplicableOperators finds all applicable operator instances in the given state.
func FindApplicableOperators(state *Env, plnpr *Plnpr) [][]interface{} {
	var result [][]interface{}
	pattern := []interface{}{"applicable", "?x"}
	candidates := Fetch(pattern, plnpr.Atre)
	for _, candidate := range candidates {
		if candList, ok := candidate.([]interface{}); ok {
			if InFact(candidate, state, plnpr.Atre) {
				if len(candList) > 1 {
					if opInst, ok := candList[1].([]interface{}); ok {
						result = append(result, opInst)
					}
				}
			}
		}
	}
	return result
}

// FetchOperator retrieves an operator by name.
func FetchOperator(opName string, plnpr *Plnpr) *Operator {
	return plnpr.Operators[opName]
}

// ApplyOperator applies an operator instance to a state, returning the new state.
func ApplyOperator(state *Env, opInst []interface{}, plnpr *Plnpr, atre *Atre) *Env {
	opName := fmt.Sprintf("%v", opInst[0])
	operator := FetchOperator(opName, plnpr)
	if operator == nil {
		return nil
	}
	vals := opInst[1:]
	assumptions := make([]*TmsNode, len(state.Assumptions))
	copy(assumptions, state.Assumptions)

	// Create bindings from operator form variables to values
	bindings := make(map[string]interface{})
	if len(operator.Form) > 1 {
		formVars := operator.Form[1:]
		for i := 0; i < len(formVars) && i < len(vals); i++ {
			if varName, ok := formVars[i].(string); ok {
				bindings[varName] = vals[i]
			}
		}
	}

	// Apply bindings to add and delete lists
	addList := substituteBindings(operator.AddList, bindings)
	deleteList := substituteBindings(operator.DeleteList, bindings)

	// Remove delete-list assumptions
	var newAssumptions []*TmsNode
	for _, a := range assumptions {
		datum, ok := a.Datum.(*Datum)
		if !ok {
			newAssumptions = append(newAssumptions, a)
			continue
		}
		shouldDelete := false
		for _, del := range deleteList {
			if SExprEqual(datum.LispForm, del) {
				shouldDelete = true
				break
			}
		}
		if !shouldDelete {
			newAssumptions = append(newAssumptions, a)
		}
	}

	// Add new assumptions from add-list
	for _, add := range addList {
		node := GetTmsNode(add, atre)
		newAssumptions = OrderedInsert(node, newAssumptions, AssumptionOrder)
	}

	return FindOrMakeEnv(newAssumptions, atre.ATMS)
}

func substituteBindings(patterns [][]interface{}, bindings map[string]interface{}) [][]interface{} {
	result := make([][]interface{}, len(patterns))
	for i, p := range patterns {
		result[i] = substitutePattern(p, bindings)
	}
	return result
}

func substitutePattern(pattern []interface{}, bindings map[string]interface{}) []interface{} {
	result := make([]interface{}, len(pattern))
	for i, elem := range pattern {
		if varName, ok := elem.(string); ok {
			if val, found := bindings[varName]; found {
				result[i] = val
			} else {
				result[i] = elem
			}
		} else if subList, ok := elem.([]interface{}); ok {
			result[i] = substitutePattern(subList, bindings)
		} else {
			result[i] = elem
		}
	}
	return result
}

// FetchStates returns all states consistent with the given facts.
func FetchStates(facts []interface{}, plnpr *Plnpr) []*Env {
	choiceSets := make([][]*TmsNode, 0)
	for _, fact := range facts {
		node := GetTmsNode(fact, plnpr.Atre)
		choiceSets = append(choiceSets, []*TmsNode{node})
	}
	for _, bs := range plnpr.BasisSet {
		var nodes []*TmsNode
		for _, choice := range bs {
			nodes = append(nodes, GetTmsNode(choice, plnpr.Atre))
		}
		choiceSets = append(choiceSets, nodes)
	}
	return Interpretations(plnpr.Atre.ATMS, choiceSets, nil)
}

// SatisfiesGoal checks if a state satisfies the given goals.
func SatisfiesGoal(state *Env, goals []interface{}, plnpr *Plnpr) (bool, []Binding) {
	return checkGoals(goals, state, nil, plnpr)
}

func checkGoals(goals []interface{}, state *Env, bindings []Binding, plnpr *Plnpr) (bool, []Binding) {
	if len(goals) == 0 {
		return true, bindings
	}
	candidates := Fetch(goals[0], plnpr.Atre)
	for _, candidate := range candidates {
		if InFact(candidate, state, plnpr.Atre) {
			newBindings, ok := Unify(goals[0], candidate, bindings)
			if ok {
				found, result := checkGoals(goals[1:], state, newBindings, plnpr)
				if found {
					return true, result
				}
			}
		}
	}
	return false, nil
}

// ShowPlan displays the plan.
func ShowPlan(plan []interface{}, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	// Plan is a list of alternating states and operators (reversed)
	reversed := make([]interface{}, len(plan))
	for i, v := range plan {
		reversed[len(plan)-1-i] = v
	}
	for i := 0; i < len(reversed); i += 2 {
		if env, ok := reversed[i].(*Env); ok {
			PrintEnv(env, w)
		}
		if i+1 < len(reversed) && reversed[i+1] != nil {
			fmt.Fprintf(w, "\n  then, by %v, ", reversed[i+1])
		}
	}
}
