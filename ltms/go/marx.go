// Marx Brothers puzzle.
// Converted from marx.lisp.
package ltms

import "fmt"

var marxAttributes = []string{
	"PLAYS-PIANO", "PLAYS-HARP", "SMOOTH-TALKER",
	"LIKES-GAMBLING", "LIKES-ANIMALS",
}

var marxObjects = []string{"GROUCHO", "HARPO", "CHICO"}

// MarxBrothers solves the Marx Brothers attribution problem.
func MarxBrothers() {
	SolveAttributionProblem(marxAttributes, marxObjects)
}

// MakeAttributeChoiceSets creates choice sets for DD-Search.
func MakeAttributeChoiceSets(attributes, objects []string) [][]interface{} {
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

// SolveAttributionProblem solves an attribution problem using DD-Search.
func SolveAttributionProblem(attributes, objects []string) {
	InLTRE(CreateLTRE("Attribution Problem Scratchpad", false))
	LoadMarxData(CurrentLTRE)
	DDSearch(
		MakeAttributeChoiceSets(attributes, objects),
		func() { ShowAttributeSolution(attributes) },
		CurrentLTRE,
	)
}

// ShowAttributeSolution displays the solution.
func ShowAttributeSolution(attributes []string) {
	fmt.Println("\nSolution:")
	for _, attr := range attributes {
		matches := Fetch([]interface{}{attr, "?object"}, CurrentLTRE)
		for _, match := range matches {
			if IsTrue(match, CurrentLTRE) {
				fmt.Printf("\n  %v", match)
			}
		}
	}
}
