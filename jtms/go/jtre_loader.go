// JTRE loader and module listing.
// Translated from jtre.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

// JtreFiles lists the core JTRE module files.
// Corresponds to the Lisp variable *jtre-files*.
var JtreFiles = []string{
	"jtms",   // JTMS
	"jtre",   // Interface (jinter)
	"jdata",  // Database
	"jrules", // Rule system
	"funify", // Open-coding unification
}

// JQueensFiles lists the N-Queens module files.
// Corresponds to the Lisp variable *jqueens-files*.
var JQueensFiles = []string{
	"jqueens", // JTRE version of N-queens puzzle
	"jqrule",  // Contradiction detection rule
}

// JSaintFiles lists the JSAINT module files.
// Corresponds to the Lisp variable *jsaint-files*.
var JSaintFiles = []string{
	"jsaint",   // JSAINT main program
	"match",    // Math-oriented pattern matcher
	"simplify", // Algebraic simplifier
	"jsrules",  // Bookkeeping rules
	"jsops",    // Sample integration library
}

// InitJQueens initializes the JTRE for the N-Queens puzzle.
func InitJQueens(jtre *Jtre) {
	RegisterQueensRules(jtre)
}
