// ATRE rule system for rule definition, matching, execution, and scheduling.
// Translated from arules.lisp, last edited 1/29/93 by KDF.
//
// Copyright (c) 1990-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package atms

import (
	"fmt"
	"io"
	"os"
)

// ConditionType represents the type of condition for a rule trigger.
type ConditionType string

const (
	ConditionIntern    ConditionType = "INTERN"
	ConditionIn        ConditionType = "IN"
	ConditionImpliedBy ConditionType = "IMPLIED-BY"
)

// MatchResult holds the result of a rule matcher function.
// Ok is true if the pattern matched.
// Bindings holds the variable bindings produced by the match.
// Condition indicates the rule condition type.
type MatchResult struct {
	Ok        bool
	Bindings  []interface{}
	Condition ConditionType
}

// RuleMatcher is the function type for a rule's match procedure.
// It takes a datum's lisp-form and returns a MatchResult.
type RuleMatcher func(datum interface{}) MatchResult

// RuleBody is the function type for a rule's body procedure.
// It is called with the variable bindings when the rule fires.
type RuleBody func(args ...interface{})

// Trigger represents a parsed rule trigger, pairing a pattern
// with optional variable and test bindings.
type Trigger struct {
	Pattern interface{} // The pattern to match against
	Var     string      // Optional bound variable name
	Test    interface{} // Optional extra test expression
}

// RuleBuilder provides a runtime rule registration API that
// corresponds to the Lisp compile-time rule macro system.
// In Lisp, the rule macro generates match and body procedures
// at compile time. Here we provide equivalent functionality
// at runtime.
type RuleBuilder struct {
	FilePrefix  string
	FileCounter int
}

// NewRuleBuilder creates a new RuleBuilder, corresponding to
// the (Rule-File prefix) macro in the Lisp source.
func NewRuleBuilder(prefix string) *RuleBuilder {
	return &RuleBuilder{
		FilePrefix:  prefix,
		FileCounter: 0,
	}
}

// GenerateRuleProcedureName generates a unique name for a rule
// procedure, corresponding to generate-rule-procedure-name.
func (rb *RuleBuilder) GenerateRuleProcedureName(pattern interface{}) string {
	rb.FileCounter++
	return fmt.Sprintf("%s-%v-%d", rb.FilePrefix, pattern, rb.FileCounter)
}

// ParseTriggers parses a list of trigger specifications into
// Trigger structs. In Lisp, trigger-list is a flat list of
// alternating patterns and options (:TEST, :VAR). Here each
// TriggerSpec bundles a pattern with its options.
//
// Corresponds to parse-triggers and parse-trigger-options.
type TriggerSpec struct {
	Pattern interface{}
	Var     string
	Test    interface{}
}

// ParseTriggerSpecs converts TriggerSpec values into Trigger values.
func ParseTriggerSpecs(specs []TriggerSpec) []Trigger {
	triggers := make([]Trigger, len(specs))
	for i, spec := range specs {
		triggers[i] = Trigger{
			Pattern: spec.Pattern,
			Var:     spec.Var,
			Test:    spec.Test,
		}
	}
	return triggers
}

// BuildAndInsertRule creates a rule from a matcher and body, then
// registers it with the given dbclass. This is the runtime equivalent
// of the Lisp build-rule + insert-rule pipeline.
//
// Parameters:
//   - dbclass: the database class to register the rule under
//   - matcher: function that tests whether a datum matches the rule's pattern
//   - body: function that executes the rule's action when fired
//   - inNodes: TMS nodes that must have a jointly non-empty label
//   - impNodes: TMS nodes that must be implied by the current focus
//
// Returns the newly created Rule.
func BuildAndInsertRule(dbclass *Dbclass, matcher RuleMatcher, body RuleBody,
	inNodes []*TmsNode, impNodes []*TmsNode) *Rule {
	// Wrap the typed matcher into the interface{}-based signature
	// expected by InsertRule.
	wrappedMatcher := func(datum interface{}) (bool, []interface{}, string) {
		result := matcher(datum)
		return result.Ok, result.Bindings, string(result.Condition)
	}
	return InsertRule(dbclass, wrappedMatcher, func(args ...interface{}) {
		body(args...)
	}, inNodes, impNodes)
}

// RegisterRule is a convenience function that combines trigger parsing,
// dbclass lookup, and rule insertion. It corresponds to the full
// expansion of the Lisp (rule ...) macro at runtime.
//
// Parameters:
//   - atre: the ATRE instance
//   - condition: the condition type (INTERN, IN, or IMPLIED-BY)
//   - triggerDBClassName: the name used to look up the dbclass
//   - matcher: the match procedure for the rule
//   - body: the body procedure for the rule
//   - inNodes: TMS nodes required to be jointly believed
//   - impNodes: TMS nodes required to be implied by focus
//
// Returns the newly created Rule.
func RegisterRule(atre *Atre, condition ConditionType,
	triggerDBClassName string, matcher RuleMatcher, body RuleBody,
	inNodes []*TmsNode, impNodes []*TmsNode) *Rule {
	dbclass := GetDbclass(triggerDBClassName, atre)
	return BuildAndInsertRule(dbclass, matcher, body, inNodes, impNodes)
}

// QueuedRule represents a rule that has been queued for execution.
// It bundles together the body function, bindings from matching,
// and the node conditions (in-nodes and imp-nodes).
type QueuedRule struct {
	Body     RuleBody
	Bindings []interface{}
	InNodes  []*TmsNode
	ImpNodes []*TmsNode
}

// EnqueueRule adds a rule to the ATRE queue as a QueuedRule.
// This corresponds to the enqueue call in try-rule-on.
func EnqueueRule(body RuleBody, bindings []interface{},
	inNodes []*TmsNode, impNodes []*TmsNode, atre *Atre) {
	qr := &QueuedRule{
		Body:     body,
		Bindings: bindings,
		InNodes:  inNodes,
		ImpNodes: impNodes,
	}
	// Also enqueue as the generic triple format expected by ExecuteRule.
	triple := []interface{}{
		func(args ...interface{}) { body(args...) },
		bindings,
		[]interface{}{inNodes, impNodes},
	}
	_ = qr // QueuedRule kept for typed access if needed
	Enqueue(triple, atre)
}

// MakeNestedRuleFn creates a closure that, when called, registers
// an inner rule. This corresponds to make-nested-rule in the Lisp
// source, which builds nested rule forms for multi-trigger rules.
//
// For a rule with multiple triggers, each trigger after the first
// creates an inner rule that is registered when the outer rule fires.
func MakeNestedRuleFn(atre *Atre, condition ConditionType,
	innerTriggerDBClassName string, innerMatcher RuleMatcher,
	innerBody RuleBody, inNodes []*TmsNode, impNodes []*TmsNode) RuleBody {
	return func(args ...interface{}) {
		dbclass := GetDbclass(innerTriggerDBClassName, atre)
		BuildAndInsertRule(dbclass, innerMatcher, innerBody, inNodes, impNodes)
	}
}

// TryRuleOnTyped is a typed version of TryRuleOn that works with
// the RuleMatcher type directly. It tests a rule against a datum
// and enqueues it if the matcher succeeds.
func TryRuleOnTyped(rule *Rule, datum *Datum) {
	if rule.Matcher == nil {
		return
	}
	okay, bindings, condition := rule.Matcher(datum.LispForm)
	if !okay {
		return
	}
	if condition == "IN" || condition == "IMPLIED-BY" {
		bindings = append([]interface{}{datum.TmsNode}, bindings...)
	}
	var nodePair []interface{}
	switch condition {
	case "IN":
		inNodes := append([]*TmsNode{datum.TmsNode}, rule.InNodes...)
		nodePair = []interface{}{inNodes, rule.ImpNodes}
	case "IMPLIED-BY":
		impNodes := append([]*TmsNode{datum.TmsNode}, rule.ImpNodes...)
		nodePair = []interface{}{rule.InNodes, impNodes}
	case "INTERN":
		nodePair = []interface{}{rule.InNodes, rule.ImpNodes}
	}
	Enqueue([]interface{}{rule.Body, bindings, nodePair}, datum.Atre)
}

// ExecuteQueuedRule executes a QueuedRule, checking belief conditions
// first. If the in-triggers or implied-by triggers are not ready,
// the rule is re-queued to the appropriate pending list.
func ExecuteQueuedRule(qr *QueuedRule, atre *Atre) {
	if !InTriggersReady(qr.InNodes, atre, nil) {
		triple := []interface{}{
			func(args ...interface{}) { qr.Body(args...) },
			qr.Bindings,
			[]interface{}{qr.InNodes, qr.ImpNodes},
		}
		atre.InRules = append([]interface{}{triple}, atre.InRules...)
		return
	}
	if !ImpliedByTriggersReady(qr.ImpNodes, atre) {
		triple := []interface{}{
			func(args ...interface{}) { qr.Body(args...) },
			qr.Bindings,
			[]interface{}{qr.InNodes, qr.ImpNodes},
		}
		atre.ImpRules = append([]interface{}{triple}, atre.ImpRules...)
		return
	}
	qr.Body(qr.Bindings...)
}

// ShowRulesDetailed displays a detailed summary of rules in the ATRE,
// including the distribution across dbclasses and pending rule counts.
// This provides the same output as the Lisp show-rules function.
func ShowRulesDetailed(atre *Atre, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	counter := 0
	type distEntry struct {
		Name  string
		Count int
	}
	var dist []distEntry
	for _, dbclass := range atre.Dbclasses {
		inc := len(dbclass.Rules)
		if inc > 0 {
			dist = append(dist, distEntry{dbclass.Name, inc})
			counter += inc
		}
	}
	in := len(atre.InRules)
	imp := len(atre.ImpRules)
	queued := len(atre.Queue)
	counter += in + imp
	fmt.Fprintf(w, "\n %s has %d rules in all.", atre.Title, counter)
	if queued > 0 {
		fmt.Fprintf(w, "\n  %d queued.", queued)
	} else {
		fmt.Fprintf(w, "\n  None queued.")
	}
	if in+imp > 0 {
		inStr := "No"
		if in > 0 {
			inStr = fmt.Sprintf("%d", in)
		}
		impStr := "No"
		if imp > 0 {
			impStr = fmt.Sprintf("%d", imp)
		}
		fmt.Fprintf(w, "  Pending: %s in, %s implied-by.", inStr, impStr)
	} else {
		fmt.Fprintf(w, "  None pending.")
	}
	if len(dist) > 0 {
		fmt.Fprintf(w, "\n Cached under dbclasses:")
		for _, entry := range dist {
			fmt.Fprintf(w, "\n    %s: %d", entry.Name, entry.Count)
		}
	}
}

// PrintRulesAll prints all rules in the ATRE with their matcher
// and body information. Returns the count of rules printed.
// Corresponds to print-rules in the Lisp source.
func PrintRulesAll(atre *Atre, w io.Writer) int {
	if w == nil {
		w = os.Stdout
	}
	counter := 0
	fmt.Fprintf(w, "\nThe rules in %s are:", atre.Title)
	for _, rule := range atre.Rules {
		counter++
		PrintRuleDetail(rule, w)
	}
	return counter
}

// PrintRuleDetail prints a single rule with its matcher and body.
// Corresponds to print-rule in the Lisp source.
func PrintRuleDetail(rule *Rule, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n %s: matcher=%v, body=%v", rule, rule.Matcher, rule.Body)
}

// GetRuleByCounter returns the rule with the given counter value,
// or nil if not found. Corresponds to get-rule in the Lisp source.
func GetRuleByCounter(num int, atre *Atre) *Rule {
	for _, rule := range atre.Rules {
		if rule.Counter == num {
			return rule
		}
	}
	return nil
}
