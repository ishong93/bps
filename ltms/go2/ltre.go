// LTRE: An LTMS-based Tiny Rule Engine.
// Definitions and interface module.
// Translated from linter.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package ltms

import (
	"fmt"
	"io"
	"os"
)

// LTRE represents an LTMS-based Tiny Rule Engine.
// It wraps an LTMS and provides a database of facts (datums organized
// into dbclasses) plus a rule system for forward-chaining inference.
// Corresponds to the Lisp defstruct ltre in linter.lisp.
type LTRE struct {
	Title        string
	LTMS         *LTMS
	DbclassTable map[string]*Dbclass
	DatumCounter int
	RuleCounter  int
	Debugging    bool
	Queue        []*RuleInvocation
	RulesRun     int
}

// String implements fmt.Stringer for LTRE.
func (l *LTRE) String() string {
	return fmt.Sprintf("<LTRE: %s>", l.Title)
}

// CurrentLTRE is the default LTRE, analogous to the Lisp *LTRE* variable.
var CurrentLTRE *LTRE

// WithLTRE executes fn with CurrentLTRE temporarily set to ltre.
// Corresponds to the Lisp macro with-LTRE.
func WithLTRE(ltre *LTRE, fn func()) {
	old := CurrentLTRE
	CurrentLTRE = ltre
	defer func() { CurrentLTRE = old }()
	fn()
}

// InLTRE sets the current LTRE, analogous to in-package.
// Corresponds to the Lisp function In-LTRE.
func InLTRE(ltre *LTRE) {
	CurrentLTRE = ltre
}

// CreateLTRE creates a new LTRE with the given title and options.
// Corresponds to the Lisp function create-ltre.
func CreateLTRE(title string, debugging bool) *LTRE {
	l := &LTRE{
		Title:        title,
		DbclassTable: make(map[string]*Dbclass),
		Debugging:    debugging,
	}
	l.LTMS = CreateLTMS(
		fmt.Sprintf("LTMS-OF %s", title),
		OptionNodeString(func(node *TmsNode) string {
			return MakeNodeString(node)
		}),
		OptionCacheDatums(false),
	)
	// Install the enqueue procedure so the LTMS feeds rule
	// triggers back into this LTRE's queue.
	ChangeLTMS(l.LTMS, OptionEnqueueProcedure(func(pair interface{}) {
		LtreEnqueue(pair, l)
	}))
	CurrentLTRE = l
	return l
}

// ChangeLTRE updates optional fields of the LTRE.
// Corresponds to the Lisp function change-ltre.
func ChangeLTRE(ltre *LTRE, debugging *bool) {
	if debugging != nil {
		ltre.Debugging = *debugging
	}
}

// UAssert asserts a fact, then runs all queued rules.
// Corresponds to the Lisp function uassert!.
func UAssert(fact interface{}, just interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if just == nil {
		just = "user"
	}
	Assert(fact, just, ltre)
	RunRules(ltre)
}

// UAssume assumes a fact, then runs all queued rules.
// Corresponds to the Lisp function uassume!.
func UAssume(fact interface{}, reason interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	Assume(fact, reason, ltre)
	RunRules(ltre)
}

// RunForms evaluates a sequence of functions, running rules after each.
// Corresponds to the Lisp function run-forms.
func RunForms(forms []func(), ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	for _, form := range forms {
		form()
		RunRules(ltre)
	}
}

// Show displays the current state of the LTRE.
// Corresponds to the Lisp function show.
func Show(ltre *LTRE, w io.Writer) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\nFor LTRE %s:", ltre.Title)
	ShowData(ltre, w)
	ShowRules(ltre, w)
}

// ShowByInformant displays all clauses added by a specific informant.
// Corresponds to the Lisp function show-by-informant.
func ShowByInformant(informant interface{}, ltre *LTRE) int {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	count := 0
	for _, clause := range ltre.LTMS.Clauses {
		match := false
		if infSlice, ok := clause.Informant.([]interface{}); ok {
			if len(infSlice) >= 3 && deepEqual(infSlice[2], informant) {
				match = true
			}
		} else {
			if deepEqual(clause.Informant, informant) {
				match = true
			}
		}
		if match {
			count++
			fmt.Printf("\n%v", ViewClause(clause))
		}
	}
	return count
}

// ViewClause returns a human-readable representation of a clause.
// Corresponds to the Lisp function view-clause.
func ViewClause(cl *Clause) interface{} {
	result := []interface{}{"OR"}
	for _, lit := range cl.Literals {
		if lit.Sign == LabelFalse {
			// :FALSE literal => (NOT <view>)
			result = append(result, []interface{}{"NOT", LViewNode(lit.Node)})
		} else {
			result = append(result, LViewNode(lit.Node))
		}
	}
	return result
}
