// JTRE: A JTMS-based Tiny Rule Engine.
// Definitions and interface module.
// Translated from jinter.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package jtms

import (
	"fmt"
	"io"
	"os"
)

// Jtre represents a JTMS-based Tiny Rule Engine.
// It wraps a JTMS and provides a database of facts (datums organized
// into dbclasses) plus a rule system for forward-chaining inference.
// Corresponds to the Lisp defstruct jtre in jinter.lisp.
type Jtre struct {
	Title        string                // Pretty name
	JTMS         *JTMS                 // Pointer to its JTMS
	DbclassTable map[string]*JDbclass  // Table of dbclasses
	DatumCounter int                   // Unique ID for asserts
	RuleCounter  int                   // Unique ID for rules
	Debugging    bool                  // If true, show basic operations
	Queue        []*QueueEntry         // Rule queue
	RulesRun     int                   // Statistic: total rules executed
}

// QueueEntry represents an enqueued rule body plus its bindings.
// When run-rules fires, it calls Body(Bindings...).
// Corresponds to the (cons body bindings) entries in the Lisp queue.
type QueueEntry struct {
	Body     func(args ...interface{})
	Bindings []interface{}
}

// String implements fmt.Stringer for Jtre.
func (j *Jtre) String() string {
	return fmt.Sprintf("<JTRE: %s>", j.Title)
}

// CreateJtre creates a new JTRE with the given title and options.
// Corresponds to the Lisp function create-jtre.
func CreateJtre(title string, debugging bool) *Jtre {
	j := &Jtre{
		Title:        title,
		DbclassTable: make(map[string]*JDbclass),
		Debugging:    debugging,
	}
	j.JTMS = CreateJTMS(
		fmt.Sprintf("JTMS-OF %s", title),
		WithNodeString(func(node *TmsNode) string {
			return ViewNode(node)
		}),
	)
	// Install the enqueue procedure so the JTMS feeds rule
	// triggers back into this JTRE's queue.
	ChangeJTMS(j.JTMS, WithEnqueueProcedure(func(rule interface{}) {
		JtreEnqueue(rule, j)
	}))
	return j
}

// ChangeJtre updates optional fields of the JTRE.
// Corresponds to the Lisp function change-jtre.
func ChangeJtre(jtre *Jtre, debugging *bool) {
	if debugging != nil {
		jtre.Debugging = *debugging
	}
}

// debuggingJtre prints a debug message if debugging is enabled.
// Corresponds to the Lisp macro debugging-jtre.
func debuggingJtre(jtre *Jtre, msg string, args ...interface{}) {
	if jtre.Debugging {
		fmt.Fprintf(os.Stderr, msg, args...)
	}
}

// Uassert asserts a fact, then runs all queued rules.
// Corresponds to the Lisp function uassert!.
func Uassert(fact interface{}, just interface{}, jtre *Jtre) *JDatum {
	datum := JAssert(fact, just, jtre)
	RunJRules(jtre)
	return datum
}

// Uassume assumes a fact, then runs all queued rules.
// Corresponds to the Lisp function uassume!.
func Uassume(fact interface{}, reason interface{}, jtre *Jtre) *JDatum {
	datum := JAssume(fact, reason, jtre)
	RunJRules(jtre)
	return datum
}

// RunForms evaluates a sequence of functions, running rules after each.
// Corresponds to the Lisp function run-forms.
func RunForms(forms []func(), jtre *Jtre) {
	for _, form := range forms {
		form()
		RunJRules(jtre)
	}
}

// ShowJtre displays the current state of the JTRE.
// Corresponds to the Lisp function show.
func ShowJtre(jtre *Jtre, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	ShowJData(jtre, w)
	ShowJRules(jtre, w)
}

// JtreEnqueue adds an item to the JTRE's rule queue.
// This is the callback installed in the JTMS's enqueue procedure.
// It accepts either a *QueueEntry directly or a raw interface{}
// that will be wrapped.
func JtreEnqueue(item interface{}, jtre *Jtre) {
	switch entry := item.(type) {
	case *QueueEntry:
		jtre.Queue = append([]*QueueEntry{entry}, jtre.Queue...)
	default:
		// Wrap raw items -- this handles the JTMS InRules callback
		// which passes rule closures as plain interface{}.
		if fn, ok := item.(func(args ...interface{})); ok {
			jtre.Queue = append([]*QueueEntry{{Body: fn}}, jtre.Queue...)
		}
	}
}
