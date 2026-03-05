// LTRE: An LTMS-based Tiny Rule Engine.
// Marx Brothers attribution problem.
// Translated from marx.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1996, Kenneth D. Forbus, Northwestern University.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package ltms

import "fmt"

// MarxAttributes are the attributes to assign to Marx Brothers.
var MarxAttributes = []string{
	"plays-piano", "plays-harp",
	"smooth-talker", "likes-gambling", "likes-animals",
}

// MarxObjects are the Marx Brothers.
var MarxObjects = []string{"groucho", "harpo", "chico"}

// MarxBrothers runs the Marx Brothers attribution problem.
// Corresponds to the Lisp function marx-brothers.
func MarxBrothers() {
	SolveAttributionProblem(MarxAttributes, MarxObjects, InstallMarxData)
}

// MakeAttributeChoiceSets creates choice sets for each attribute.
// Each attribute can apply to exactly one object.
// Corresponds to the Lisp function make-attribute-choice-sets.
func MakeAttributeChoiceSets(attributes []string, objects []string) [][]interface{} {
	var result [][]interface{}
	for _, attr := range attributes {
		var choices []interface{}
		for _, obj := range objects {
			choices = append(choices, []interface{}{attr, obj})
		}
		result = append(result, choices)
	}
	return result
}

// SolveAttributionProblem solves a general attribution problem
// using dependency-directed search.
// Corresponds to the Lisp function solve-attribution-problem.
func SolveAttributionProblem(attributes []string, objects []string, installConstraints func(*LTRE)) {
	ltre := CreateLTRE("Attribution Problem Scratchpad", false)
	InLTRE(ltre)
	installConstraints(ltre)
	choiceSets := MakeAttributeChoiceSets(attributes, objects)
	DDSearch(choiceSets, func() {
		ShowAttributeSolution(attributes, ltre)
	}, ltre)
}

// ShowAttributeSolution prints the current solution.
// Corresponds to the Lisp function show-attribute-solution.
func ShowAttributeSolution(attributes []string, ltre *LTRE) {
	fmt.Println("\nSolution:")
	for _, attr := range attributes {
		matches := Fetch([]interface{}{attr, "?object"}, ltre)
		for _, match := range matches {
			if IsTrue(match, ltre) {
				fmt.Printf("  %v\n", match)
			}
		}
	}
}
