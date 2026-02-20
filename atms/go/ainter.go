// ATRE: A Tiny Rule Engine, ATMS-based version.
// Definitions and interface module.
// Last edited: 1/29/93, KDF
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package atms

import (
	"fmt"
	"io"
	"os"
)

// Atre represents A Tiny Rule Engine built on top of an ATMS.
type Atre struct {
	Title              string
	ATMS               *ATMS
	Dbclasses          []*Dbclass
	DbclassTable       map[string]*Dbclass
	DatumCounter       int
	Rules              []*Rule
	RuleCounter        int
	Debugging          bool
	Queue              []interface{}
	RulesRun           int
	InRules            []interface{}
	Focus              *Env
	ContradictionRules []interface{}
	ImpRules           []interface{}
}

// String implements the Stringer interface for Atre.
func (a *Atre) String() string {
	return fmt.Sprintf("<ATRE: %s>", a.Title)
}

// IsAtre returns true if the given value is a non-nil *Atre.
func IsAtre(x interface{}) bool {
	_, ok := x.(*Atre)
	return ok
}

// Dbclass groups facts and rules that share a common predicate name.
type Dbclass struct {
	Name  string
	Atre  *Atre
	Facts []*Datum
	Rules []*Rule
}

// String implements the Stringer interface for Dbclass.
func (d *Dbclass) String() string {
	return fmt.Sprintf("<Dbclass %s>", d.Name)
}

// Datum represents a single fact in the ATRE database.
type Datum struct {
	Counter    int
	Atre       *Atre
	LispForm   interface{}
	TmsNode    *TmsNode
	Dbclass    *Dbclass
	Assumption interface{} // nil if not an assumption; otherwise the reason
	Plist      map[string]interface{}
}

// String implements the Stringer interface for Datum.
func (d *Datum) String() string {
	return fmt.Sprintf("<Datum %d>", d.Counter)
}

// IsAssumption returns true if the datum was assumed.
func (d *Datum) IsAssumption() bool {
	return d.Assumption != nil
}

// Rule represents a rule in the ATRE.
type Rule struct {
	Counter  int
	Atre     *Atre
	Dbclass  *Dbclass
	Matcher  func(interface{}) (bool, []interface{}, string)
	Body     func(args ...interface{})
	InNodes  []*TmsNode
	ImpNodes []*TmsNode
}

// String implements the Stringer interface for Rule.
func (r *Rule) String() string {
	return fmt.Sprintf("<Rule %d>", r.Counter)
}

// CreateAtre creates a new ATRE with the given title and options.
func CreateAtre(title string, debugging bool) *Atre {
	atre := &Atre{
		Title:        title,
		DbclassTable: make(map[string]*Dbclass),
		Debugging:    debugging,
	}
	atre.ATMS = CreateATMS(
		fmt.Sprintf("ATMS-OF %s", title),
		func(a *ATMS) {
			a.NodeString = func(n *TmsNode) string {
				return StringifyNode(n)
			}
		},
	)
	ChangeATMS(atre.ATMS, nil,
		func(pair interface{}) {
			Enqueue(pair, atre)
		},
		nil,
	)
	// Create the FALSE datum linked to the contradiction node.
	falseDbclass := GetDbclass("FALSE", atre)
	atre.DatumCounter++
	falseDatum := &Datum{
		Counter:  atre.DatumCounter,
		Atre:     atre,
		LispForm: "FALSE",
		Dbclass:  falseDbclass,
	}
	falseDatum.TmsNode = atre.ATMS.ContraNode
	atre.ATMS.ContraNode.Datum = falseDatum
	falseDbclass.Facts = append(falseDbclass.Facts, falseDatum)
	return atre
}

// ChangeAtre updates optional fields of the ATRE.
func ChangeAtre(atre *Atre, debugging *bool) {
	if debugging != nil {
		atre.Debugging = *debugging
	}
}

// debuggingAtre prints a debug message if debugging is enabled on the ATRE.
func debuggingAtre(atre *Atre, msg string, args ...interface{}) {
	if atre.Debugging {
		fmt.Fprintf(os.Stderr, msg, args...)
	}
}

// RunRules runs all queued rules until the queue is empty.
func RunRules(atre *Atre) (int, int) {
	atre.Queue = append(atre.Queue, atre.InRules...)
	atre.InRules = nil
	counter := 0
	for {
		form := Dequeue(atre)
		if form == nil {
			break
		}
		ExecuteRule(form, atre)
		counter++
	}
	debuggingAtre(atre, "\n    %d rules run.", counter)
	atre.RulesRun += counter
	return counter, atre.RulesRun
}

// ExecuteRule executes a single queued rule, checking belief conditions.
// A queued rule is a []interface{}{procedure, arguments, nodeList}.
func ExecuteRule(queuedRule interface{}, atre *Atre) {
	triple, ok := queuedRule.([]interface{})
	if !ok || len(triple) < 3 {
		return
	}
	procedure, _ := triple[0].(func(args ...interface{}))
	arguments, _ := triple[1].([]interface{})
	nodePair, _ := triple[2].([]interface{})

	var inNodes []*TmsNode
	var impNodes []*TmsNode
	if nodePair != nil && len(nodePair) >= 2 {
		if in, ok := nodePair[0].([]*TmsNode); ok {
			inNodes = in
		}
		if imp, ok := nodePair[1].([]*TmsNode); ok {
			impNodes = imp
		}
	}

	if !InTriggersReady(inNodes, atre, nil) {
		atre.InRules = append([]interface{}{queuedRule}, atre.InRules...)
		return
	}
	if !ImpliedByTriggersReady(impNodes, atre) {
		atre.ImpRules = append([]interface{}{queuedRule}, atre.ImpRules...)
		return
	}
	if procedure != nil {
		procedure(arguments...)
	}
}

// InTriggersReady checks if all :IN trigger nodes are simultaneously
// satisfiable. If env is nil, the ATMS empty env is used.
func InTriggersReady(nodes []*TmsNode, atre *Atre, env *Env) bool {
	if env == nil {
		env = atre.ATMS.EmptyEnv
	}
	if env.IsNogood() {
		return false
	}
	if len(nodes) == 0 {
		return true
	}
	for _, newEnv := range nodes[0].Label {
		u := UnionEnv(newEnv, env)
		if u != nil && InTriggersReady(nodes[1:], atre, u) {
			return true
		}
	}
	return false
}

// ImpliedByTriggersReady checks if all :IMPLIED-BY trigger nodes are
// in the current focus environment.
func ImpliedByTriggersReady(nodes []*TmsNode, atre *Atre) bool {
	if len(nodes) == 0 {
		return true
	}
	if !FocusOkay(atre) {
		return false
	}
	for _, n := range nodes {
		if !InNode(n, atre.Focus) {
			return false
		}
	}
	return true
}

// RulesWaiting returns true if there are rules in the queue.
func RulesWaiting(atre *Atre) bool {
	return len(atre.Queue) > 0
}

// Enqueue adds an item to the ATRE's rule queue.
func Enqueue(item interface{}, atre *Atre) {
	atre.Queue = append([]interface{}{item}, atre.Queue...)
}

// Dequeue removes and returns the first item from the ATRE's rule queue.
// Returns nil if the queue is empty.
func Dequeue(atre *Atre) interface{} {
	if len(atre.Queue) == 0 {
		return nil
	}
	item := atre.Queue[0]
	atre.Queue = atre.Queue[1:]
	return item
}

// InsertRule creates a new rule and indexes it in the database.
func InsertRule(dbclass *Dbclass, matcher func(interface{}) (bool, []interface{}, string),
	body func(args ...interface{}), inNodes []*TmsNode, impNodes []*TmsNode) *Rule {
	atre := dbclass.Atre
	atre.RuleCounter++
	rule := &Rule{
		Matcher:  matcher,
		Atre:     atre,
		Body:     body,
		Dbclass:  dbclass,
		Counter:  atre.RuleCounter,
		InNodes:  inNodes,
		ImpNodes: impNodes,
	}
	atre.Rules = append([]*Rule{rule}, atre.Rules...)
	dbclass.Rules = append([]*Rule{rule}, dbclass.Rules...)
	for _, candidate := range dbclass.Facts {
		TryRuleOn(rule, candidate)
	}
	return rule
}

// TryRules tries all rules of the datum's dbclass on the datum.
func TryRules(datum *Datum) {
	for _, rule := range datum.Dbclass.Rules {
		TryRuleOn(rule, datum)
	}
}

// TryRuleOn tries a single rule on a datum.
func TryRuleOn(rule *Rule, datum *Datum) {
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

// Show displays the current state of the ATRE.
func Show(atre *Atre, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	focusStr := "empty"
	if atre.Focus != nil && !atre.Focus.IsNogood() {
		focusStr = atre.Focus.String()
	}
	fmt.Fprintf(w, "For ATRE %s:\n Focus = %s.", atre.Title, focusStr)
	ShowData(atre, w)
	ShowRules(atre, w)
}

// ShowRules displays a summary of rules in the ATRE.
func ShowRules(atre *Atre, w io.Writer) {
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

// PrintRules prints all rules in the ATRE.
func PrintRules(atre *Atre, w io.Writer) int {
	if w == nil {
		w = os.Stdout
	}
	counter := 0
	fmt.Fprintf(w, "\nThe rules in %s are:", atre.Title)
	for _, rule := range atre.Rules {
		counter++
		PrintRule(rule, w)
	}
	return counter
}

// PrintRule prints a single rule.
func PrintRule(rule *Rule, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n %s", rule)
}

// GetRule returns the rule with the given counter, or nil.
func GetRule(num int, atre *Atre) *Rule {
	for _, rule := range atre.Rules {
		if rule.Counter == num {
			return rule
		}
	}
	return nil
}

// Solutions computes interpretations for the given choice sets.
// Each choice set is a slice of fact forms; the function resolves them
// to TMS nodes and calls Interpretations.
func Solutions(atre *Atre, choiceSets [][]interface{}) []*Env {
	tmsChoiceSets := make([][]*TmsNode, len(choiceSets))
	for i, cs := range choiceSets {
		nodes := make([]*TmsNode, len(cs))
		for j, f := range cs {
			nodes[j] = GetTmsNode(f, atre)
		}
		tmsChoiceSets[i] = nodes
	}
	return Interpretations(atre.ATMS, tmsChoiceSets, nil)
}

// ChangeFocus sets the focus environment for the ATRE.
// Returns the environment if successful, nil otherwise.
// If env is nil, the focus is cleared.
func ChangeFocus(env *Env, atre *Atre) *Env {
	if env == nil {
		atre.Focus = nil
		return nil
	}
	if env.IsNogood() {
		return nil
	}
	atre.Focus = env
	atre.Queue = append(atre.Queue, atre.ImpRules...)
	atre.ImpRules = nil
	return env
}

// FocusOkay returns true if the focus environment is set and not nogood.
func FocusOkay(atre *Atre) bool {
	return atre.Focus != nil && !atre.Focus.IsNogood()
}

// WithFocus temporarily sets the focus environment, executes fn, then
// restores the old focus.
func WithFocus(focus *Env, atre *Atre, fn func()) {
	oldFocus := atre.Focus
	ChangeFocus(focus, atre)
	defer ChangeFocus(oldFocus, atre)
	fn()
}

// ContradictionRule registers a procedure to run when an environment
// becomes nogood, or enqueues it immediately if already nogood.
func ContradictionRule(env *Env, proc func(args ...interface{}), atre *Atre) {
	triple := []interface{}{proc, []interface{}{env}, nil}
	if env.IsNogood() {
		Enqueue(triple, atre)
	} else {
		env.Rules = append(env.Rules, triple)
	}
}
