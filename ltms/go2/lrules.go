// LTRE: An LTMS-based Tiny Rule Engine.
// Rule system module.
// Translated from lrules.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package ltms

import (
	"fmt"
	"io"
	"os"
)

// Rule represents a rule in the LTRE system.
// Corresponds to the Lisp defstruct rule in lrules.lisp.
type Rule struct {
	Counter int      // Unique ID for easy lookup
	LTRE    *LTRE    // The LTRE it is part of
	Dbclass *Dbclass // Dbclass of associated pattern
	// Matcher takes a datum's lisp-form and returns:
	//   ok       - whether the pattern matched
	//   bindings - variable bindings produced by the match
	//   hasNode  - whether the datum's TMS node should be prepended to bindings
	Matcher func(p interface{}) (bool, []interface{}, bool)
	// Body is the procedure that does the rule's work.
	Body func(args ...interface{})
}

// String implements fmt.Stringer for Rule.
func (r *Rule) String() string {
	return fmt.Sprintf("<Rule %d>", r.Counter)
}

// RuleInvocation represents a queued rule body together with its bindings.
// When RunRules fires, it calls Body(Bindings...).
// Corresponds to the (cons body bindings) entries in the Lisp queue.
type RuleInvocation struct {
	Body     func(args ...interface{})
	Bindings []interface{}
}

// InsertRule creates and registers a new rule in the given dbclass.
// The rule's matcher is tried against all existing facts in the dbclass.
// Corresponds to the Lisp function insert-rule.
func InsertRule(
	dbclass *Dbclass,
	matcher func(p interface{}) (bool, []interface{}, bool),
	body func(args ...interface{}),
) *Rule {
	ltre := dbclass.LTRE
	ltre.RuleCounter++
	rule := &Rule{
		Counter: ltre.RuleCounter,
		LTRE:    ltre,
		Dbclass: dbclass,
		Matcher: matcher,
		Body:    body,
	}
	// Index the rule: prepend to dbclass rules list.
	dbclass.Rules = append([]*Rule{rule}, dbclass.Rules...)
	// Try the new rule against all existing facts in this dbclass.
	for _, candidate := range dbclass.Facts {
		TryRuleOn(rule, candidate)
	}
	return rule
}

// TryRules tries all rules in the datum's dbclass against the datum.
// Called when a new datum is inserted into the database.
// Corresponds to the Lisp function try-rules.
func TryRules(datum *Datum) {
	for _, rule := range datum.Dbclass.Rules {
		TryRuleOn(rule, datum)
	}
}

// TryRuleOn tries a single rule against a datum.
// If the matcher succeeds, the rule body is enqueued with the bindings.
// When the matcher indicates hasNode, the datum's TMS node is prepended
// to the bindings so that the body can check the node's belief state.
// Corresponds to the Lisp function try-rule-on.
func TryRuleOn(rule *Rule, datum *Datum) {
	ltre := datum.LTRE
	ok, bindings, hasNode := rule.Matcher(datum.LispForm)
	if !ok {
		return
	}
	if hasNode {
		bindings = append([]interface{}{datum.TmsNode}, bindings...)
	}
	Enqueue(&RuleInvocation{Body: rule.Body, Bindings: bindings}, ltre)
}

// RunRules executes all queued rules in the LTRE.
// Returns the number of rules executed during this call.
// Corresponds to the Lisp function run-rules.
func RunRules(ltre *LTRE) int {
	counter := 0
	for {
		entry := Dequeue(ltre)
		if entry == nil {
			break
		}
		entry.Body(entry.Bindings...)
		counter++
	}
	debuggingLtre(ltre, "\n    %d rules run.", counter)
	ltre.RulesRun += counter
	return counter
}

// RunOneRule dequeues and executes a single rule.
// Returns true if there are more rules waiting.
// Corresponds to the Lisp function run-one-rule.
func RunOneRule(ltre *LTRE) bool {
	entry := Dequeue(ltre)
	if entry != nil {
		debuggingLtre(ltre, "\n     Executing single rule.")
		ltre.RulesRun++
		entry.Body(entry.Bindings...)
	}
	return RulesWaiting(ltre)
}

// RulesWaiting returns true if there are rules queued for execution.
// Corresponds to the Lisp function rules-waiting?.
func RulesWaiting(ltre *LTRE) bool {
	return len(ltre.Queue) > 0
}

// Enqueue adds a new rule invocation to the front of the LTRE's queue.
// This mimics the Lisp (push new (ltre-queue *LTRE*)) behavior.
// Corresponds to the Lisp function enqueue.
func Enqueue(entry *RuleInvocation, ltre *LTRE) {
	ltre.Queue = append([]*RuleInvocation{entry}, ltre.Queue...)
}

// LtreEnqueue is the callback suitable for use as the LTMS enqueue-procedure.
// It accepts either a *RuleInvocation directly or wraps a raw function.
func LtreEnqueue(item interface{}, ltre *LTRE) {
	switch entry := item.(type) {
	case *RuleInvocation:
		ltre.Queue = append([]*RuleInvocation{entry}, ltre.Queue...)
	default:
		if fn, ok := item.(func(args ...interface{})); ok {
			ltre.Queue = append([]*RuleInvocation{{Body: fn}}, ltre.Queue...)
		}
	}
}

// Dequeue removes and returns the first entry from the LTRE's queue.
// Returns nil if the queue is empty.
// Corresponds to the Lisp function dequeue (which uses pop, i.e. front removal).
func Dequeue(ltre *LTRE) *RuleInvocation {
	if len(ltre.Queue) == 0 {
		return nil
	}
	entry := ltre.Queue[0]
	ltre.Queue = ltre.Queue[1:]
	return entry
}

// ShowRules displays all rules in the LTRE, organized by dbclass.
// Returns the total number of rules found.
// Corresponds to the Lisp function show-rules.
func ShowRules(ltre *LTRE, w io.Writer) int {
	if w == nil {
		w = os.Stdout
	}
	counter := 0
	fmt.Fprintf(w, "\nThe rules in %s are:", ltre.Title)
	MapDbclass(func(dbclass *Dbclass) {
		for _, rule := range dbclass.Rules {
			counter++
			PrintRule(rule, w)
		}
	}, ltre)
	return counter
}

// PrintRule prints a single rule.
// Corresponds to the Lisp function print-rule.
func PrintRule(rule *Rule, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n %s", rule)
}

// GetRule finds a rule by its counter (ID) across all dbclasses.
// Returns nil if no rule with the given counter is found.
// Corresponds to the Lisp function get-rule.
func GetRule(num int, ltre *LTRE) *Rule {
	var result *Rule
	MapDbclass(func(dbclass *Dbclass) {
		for _, rule := range dbclass.Rules {
			if rule.Counter == num {
				result = rule
			}
		}
	}, ltre)
	return result
}

// debuggingLtre prints a debug message if the LTRE has debugging enabled.
// Corresponds to the Lisp macro debugging-ltre.
func debuggingLtre(ltre *LTRE, format string, args ...interface{}) {
	if ltre.Debugging {
		fmt.Printf(format, args...)
	}
}

// WrapTrueBody wraps a body function for a :TRUE trigger.
// When the rule fires, if the node (first arg) is already TRUE, the body
// executes immediately. Otherwise, it saves the invocation to node.TrueRules
// so it fires when the node later becomes TRUE.
func WrapTrueBody(body func(args ...interface{})) func(args ...interface{}) {
	return func(args ...interface{}) {
		if len(args) == 0 {
			return
		}
		node, ok := args[0].(*TmsNode)
		if !ok {
			body(args...)
			return
		}
		if TrueNode(node) {
			body(args...)
		} else {
			node.TrueRules = append(node.TrueRules,
				&RuleInvocation{Body: body, Bindings: args})
		}
	}
}

// WrapFalseBody wraps a body function for a :FALSE trigger.
// When the rule fires, if the node (first arg) is already FALSE, the body
// executes immediately. Otherwise, it saves the invocation to node.FalseRules
// so it fires when the node later becomes FALSE.
func WrapFalseBody(body func(args ...interface{})) func(args ...interface{}) {
	return func(args ...interface{}) {
		if len(args) == 0 {
			return
		}
		node, ok := args[0].(*TmsNode)
		if !ok {
			body(args...)
			return
		}
		if FalseNode(node) {
			body(args...)
		} else {
			node.FalseRules = append(node.FalseRules,
				&RuleInvocation{Body: body, Bindings: args})
		}
	}
}
