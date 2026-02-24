// JTRE: A JTMS-based Tiny Rule Engine.
// Rule system module.
// Translated from jrules.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

import (
	"fmt"
	"io"
	"os"
)

// JRule represents a rule in the JTRE system.
// Corresponds to the Lisp defstruct rule in jrules.lisp.
type JRule struct {
	ID      int
	Jtre    *Jtre
	Dbclass *JDbclass
	Matcher func(p interface{}) (bool, []interface{}, bool)
	Body    func(args ...interface{})
}

// String implements fmt.Stringer for JRule.
func (r *JRule) String() string {
	return fmt.Sprintf("<Rule %d>", r.ID)
}

// InsertJRule creates and registers a new rule in the given dbclass.
// The rule's matcher is tried against all existing facts in the dbclass.
// Corresponds to the Lisp function insert-rule.
func InsertJRule(dbclass *JDbclass, matcher func(p interface{}) (bool, []interface{}, bool), body func(args ...interface{})) *JRule {
	jtre := dbclass.Jtre
	jtre.RuleCounter++
	rule := &JRule{
		ID:      jtre.RuleCounter,
		Jtre:    jtre,
		Dbclass: dbclass,
		Matcher: matcher,
		Body:    body,
	}
	dbclass.Rules = append([]*JRule{rule}, dbclass.Rules...)
	for _, candidate := range dbclass.Facts {
		tryJRuleOn(rule, candidate)
	}
	return rule
}

// JTryRules tries all rules in the datum's dbclass against the datum.
// Corresponds to the Lisp function try-rules.
func JTryRules(datum *JDatum) {
	for _, rule := range datum.Dbclass.Rules {
		tryJRuleOn(rule, datum)
	}
}

// tryJRuleOn tries a single rule against a datum.
// If the matcher succeeds, the rule body is enqueued with the bindings.
// Corresponds to the Lisp function try-rule-on.
func tryJRuleOn(rule *JRule, datum *JDatum) {
	jtre := datum.Dbclass.Jtre
	ok, bindings, needsNode := rule.Matcher(datum.LispForm)
	if !ok {
		return
	}
	if needsNode {
		bindings = append([]interface{}{datum.TmsNode}, bindings...)
	}
	JtreEnqueue(&QueueEntry{Body: rule.Body, Bindings: bindings}, jtre)
}

// RunJRules executes all queued rules in the JTRE.
// Corresponds to the Lisp function run-rules.
func RunJRules(jtre *Jtre) {
	counter := 0
	for len(jtre.Queue) > 0 {
		entry := jtre.Queue[0]
		jtre.Queue = jtre.Queue[1:]
		entry.Body(entry.Bindings...)
		counter++
	}
	debuggingJtre(jtre, "\n    %d rules run.", counter)
	jtre.RulesRun += counter
}

// JRulesWaiting returns true if there are rules queued for execution.
// Corresponds to the Lisp function rules-waiting?.
func JRulesWaiting(jtre *Jtre) bool {
	return len(jtre.Queue) > 0
}

// JEnqueue adds a new entry to the JTRE's rule queue.
// Corresponds to the Lisp function enqueue.
func JEnqueue(entry *QueueEntry, jtre *Jtre) {
	jtre.Queue = append([]*QueueEntry{entry}, jtre.Queue...)
}

// JDequeue removes and returns the first entry from the queue.
// Corresponds to the Lisp function dequeue.
func JDequeue(jtre *Jtre) *QueueEntry {
	if len(jtre.Queue) == 0 {
		return nil
	}
	entry := jtre.Queue[0]
	jtre.Queue = jtre.Queue[1:]
	return entry
}

// ShowJRules displays all rules in the JTRE.
// Corresponds to the Lisp function show-rules.
func ShowJRules(jtre *Jtre, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\nThere are %d rules in %s:", jtre.RuleCounter, jtre.Title)
	if len(jtre.Queue) == 0 {
		fmt.Fprintf(w, "\n None queued.")
	} else {
		fmt.Fprintf(w, "\n %d queued.", len(jtre.Queue))
	}
	JMapDbclass(func(dbclass *JDbclass) {
		for _, rule := range dbclass.Rules {
			PrintJRule(rule, w)
		}
	}, jtre)
}

// PrintJRule prints a single rule.
// Corresponds to the Lisp function print-rule.
func PrintJRule(rule *JRule, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n %s", rule)
}

// GetJRule finds a rule by its ID.
// Corresponds to the Lisp function get-rule.
func GetJRule(num int, jtre *Jtre) *JRule {
	var result *JRule
	JMapDbclass(func(dbclass *JDbclass) {
		for _, rule := range dbclass.Rules {
			if rule.ID == num {
				result = rule
			}
		}
	}, jtre)
	return result
}
