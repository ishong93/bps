// -*- Mode: Go -*-

// ATRE: Tiny Rule Engine, with ATMS interface
// Module listing and initialization
// Translated from atre.lisp

// Copyright (c) 1992, Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, the Xerox Corporation
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms

// AtreModules lists the ATRE module names.
var AtreModules = []string{
	"atms",   // ATMS
	"ainter", // Interface
	"adata",  // Database
	"arules", // Rule system
	"unify",  // Variables and pattern matching
	"funify", // Open-coding of unification
	"atret",  // Test procedures
}

// PlannerModules lists the planner module names.
var PlannerModules = []string{
	"aplanr", // Utilities
	"plan-a", // Antecedent planner
	"plan-e", // Envisioner
	"bcode",  // Blocks World support
	"blocks", // Rules for Blocks World
}

// InitPlanner initializes the planner subsystem.
// In Lisp, this was compile-planner which compiled and loaded files.
// In Go, all modules are compiled together, so this just ensures
// a default planning problem exists.
func InitPlanner() *Plnpr {
	return CreatePlanningProblem("DUMMY", nil)
}
