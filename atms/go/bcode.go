// -*- Mode: Go -*-

// Test code for ATRE Blocksworld system
// Translated from bcode.lisp

// Copyright (c) 1990-1992 Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms

// BuildBlocksProblem creates and initializes a blocks world planning problem.
func BuildBlocksProblem(title string, blocksList []string, debugging bool) *Plnpr {
	basisSet := MakeBlocksBasisSet(blocksList)
	plnpr := CreatePlanningProblem(title, basisSet)
	SetDebugPlnpr(debugging, plnpr)

	atre := plnpr.Atre

	// Register blocks world rules and operators
	RegisterBlocksWorldRules(plnpr, atre)

	// Assert that each item is a block
	for _, block := range blocksList {
		AssertFact([]interface{}{"block", block}, []interface{}{"Definition"}, atre)
	}

	RunRules(atre)
	SetupChoiceSets(plnpr)

	return plnpr
}

// MakeBlocksBasisSet generates the choice sets for a blocks world problem.
// For each block, it generates:
// 1. Where the block can be (holding, on table, or on another block)
// 2. What can be on the block (holding, clear, or another block on it)
// Plus a hand status choice set (hand-empty or holding some block)
func MakeBlocksBasisSet(blocks []string) [][][]interface{} {
	var basis [][][]interface{}

	for _, block := range blocks {
		// What the block can be on
		onChoices := [][]interface{}{
			{"Holding", block},
			{"On", block, "Table"},
		}
		for _, other := range blocks {
			if other != block {
				onChoices = append(onChoices, []interface{}{"On", block, other})
			}
		}
		basis = append(basis, onChoices)

		// What can be on the block
		topChoices := [][]interface{}{
			{"Holding", block},
			{"Clear", block},
		}
		for _, other := range blocks {
			if other != block {
				topChoices = append(topChoices, []interface{}{"ON", other, block})
			}
		}
		basis = append(basis, topChoices)
	}

	// Hand status
	handChoices := [][]interface{}{
		{"HAND-EMPTY"},
	}
	for _, block := range blocks {
		handChoices = append(handChoices, []interface{}{"HOLDING", block})
	}
	basis = append([][][]interface{}{handChoices}, basis...)

	return basis
}
