// Loader for LTMS/LTRE system.
// Converted from ltre.lisp.
package ltms

// LoadLTRE initializes the full LTRE system with complete LTMS support.
func LoadLTRE(title string, debugging bool) *LTRE {
	ltre := CreateLTRE(title, debugging)
	InitCompleteLTMS(ltre.LTMS)
	return ltre
}

// LTREFiles lists the source files in the LTRE system.
var LTREFiles = []string{
	"ltms.go",     // Core LTMS
	"ltre.go",     // LTRE interface (linter)
	"ldata.go",    // Database
	"lrules.go",   // Rule system
	"funify.go",   // Unification
	"cltms.go",    // Complete LTMS
	"cwa.go",      // Closed World Assumptions
	"dds.go",      // Dependency-Directed Search
	"indirect.go", // Indirect proof
	"setrule.go",  // Set rules
}

// ExampleFiles lists example/test files.
var ExampleFiles = []string{
	"laccept.go",  // Acceptance tests
	"ltms_ex.go",  // LTMS examples
	"marx.go",     // Marx Brothers puzzle
	"marxdata.go", // Marx Brothers data
}
