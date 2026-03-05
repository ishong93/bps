// LTRE: An LTMS-based Tiny Rule Engine.
// Database module.
// Translated from ldata.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package ltms

import (
	"fmt"
	"io"
	"os"
	"strings"
)

// Dbclass groups facts and rules that share a common predicate name.
// Corresponds to the Lisp defstruct dbclass in ldata.lisp.
type Dbclass struct {
	Name  string
	LTRE  *LTRE
	Facts []*Datum
	Rules []*Rule
}

// String implements fmt.Stringer for Dbclass.
func (d *Dbclass) String() string {
	return fmt.Sprintf("<Dbclass %s>", d.Name)
}

// Datum represents a single fact in the LTRE database.
// Corresponds to the Lisp defstruct datum in ldata.lisp.
type Datum struct {
	Counter    int
	LTRE       *LTRE
	LispForm   interface{}
	TmsNode    *TmsNode
	Dbclass    *Dbclass
	Assumption interface{}            // nil or informant
	Plist      map[string]interface{} // local property list
}

// String implements fmt.Stringer for Datum.
func (d *Datum) String() string {
	return fmt.Sprintf("<Datum %d>", d.Counter)
}

// ConnectiveList is the list of logical connectives recognized by the LTRE.
// Corresponds to the Lisp variable *connective-list*.
var ConnectiveList = []string{":IMPLIES", ":AND", ":OR", ":IFF", ":NOT", ":TAXONOMY"}

// isConnective returns true if s is a logical connective.
func isConnective(s string) bool {
	for _, c := range ConnectiveList {
		if c == s {
			return true
		}
	}
	return false
}

// SimpleProposition returns true if x is a simple proposition
// (not a connective formula).
// Corresponds to the Lisp function simple-proposition?.
func SimpleProposition(x interface{}) bool {
	list, ok := x.([]interface{})
	if !ok {
		return true
	}
	if len(list) == 0 {
		return true
	}
	s, ok := list[0].(string)
	if !ok {
		return true
	}
	return !isConnective(s)
}

// NegatedProposition returns true if form is (:NOT <simple-proposition>).
// Corresponds to the Lisp function negated-proposition?.
func NegatedProposition(form interface{}) bool {
	list, ok := form.([]interface{})
	if !ok {
		return false
	}
	if len(list) < 2 {
		return false
	}
	s, ok := list[0].(string)
	if !ok {
		return false
	}
	return s == ":NOT" && SimpleProposition(list[1])
}

// innerForm extracts the inner form from a possibly negated proposition.
// If the form is (:NOT x), returns x. Otherwise returns the form itself.
func innerForm(fact interface{}) interface{} {
	if NegatedProposition(fact) {
		return fact.([]interface{})[1]
	}
	return fact
}

// Assert asserts a fact with a justification in the LTRE.
// Corresponds to the Lisp function assert!.
func Assert(fact interface{}, just interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	debuggingLtre(ltre, "\n    Asserting %v via %v.", fact, just)
	formula := BuildTmsFormula(fact, ltre)
	AddFormula(ltre.LTMS, formula, just)
}

// QuietAssert asserts a fact without triggering contradiction checks.
// Corresponds to the Lisp function quiet-assert!.
func QuietAssert(fact interface{}, just interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if just == nil {
		just = "user"
	}
	WithoutContradictionCheck(ltre.LTMS, func() {
		Assert(fact, just, ltre)
	})
}

// RAssert is the function equivalent of the rassert! macro.
// Corresponds to the Lisp macro rassert!.
func RAssert(fact interface{}, just interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if just == nil {
		just = "user"
	}
	Assert(fact, just, ltre)
}

// Assume marks a fact as an assumption in the LTRE.
// Panics if the fact was already assumed for a different reason.
// Corresponds to the Lisp function assume!.
func Assume(fact interface{}, reason interface{}, ltre *LTRE) *Datum {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	// Get the datum for the inner form (strip :NOT if present)
	datum := Referent(innerForm(fact), true, ltre)
	node := datum.TmsNode
	debuggingLtre(ltre, "\n    Assuming %v via %v.", fact, reason)

	// Unless the fact is a negated proposition or a simple proposition,
	// install clauses corresponding to the assumption with proper logical scoping.
	if !NegatedProposition(fact) && !SimpleProposition(fact) {
		formula := []interface{}{":IMPLIES", node, BuildTmsFormula(fact, ltre)}
		AddFormula(ltre.LTMS, formula, reason)
	}

	if datum.Assumption == nil {
		datum.Assumption = reason
		ConvertToAssumption(node)
		if NegatedProposition(fact) {
			EnableAssumption(node, LabelFalse)
		} else {
			EnableAssumption(node, LabelTrue)
		}
	} else if deepEqual(reason, datum.Assumption) {
		// Same reason, do nothing.
	} else {
		panic(fmt.Sprintf(
			"Fact %s assumed because of %v assumed again because of %v",
			ShowDatum(datum), datum.Assumption, reason))
	}
	return datum
}

// AlreadyAssumed returns true if the fact is already assumed.
// Corresponds to the Lisp function already-assumed?.
func AlreadyAssumed(fact interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	datum := Referent(fact, true, ltre)
	return datum.Assumption != nil
}

// Retract retracts a previously assumed fact.
// Corresponds to the Lisp function retract!.
func Retract(fact interface{}, just interface{}, ltre *LTRE, quiet bool) *TmsNode {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	datum := Referent(fact, true, ltre)
	node := datum.TmsNode
	if !node.IsAssumption {
		if !quiet {
			fmt.Printf("\n%s isn't an assumption.", ShowDatum(datum))
		}
	} else if !KnownNode(node) {
		if !quiet {
			fmt.Printf("\nThe assumption %v is not currently in.", fact)
		}
	} else if deepEqual(just, datum.Assumption) {
		debuggingLtre(ltre, "\n    Retracting %v via %v.", fact, just)
		datum.Assumption = nil
		RetractAssumption(node)
	} else if !quiet {
		fmt.Printf("\n%v not source of assumption for %v", just, fact)
	}
	return node
}

// RRetract is the function equivalent of the rretract! macro.
// Corresponds to the Lisp macro rretract!.
func RRetract(fact interface{}, just interface{}, ltre *LTRE) *TmsNode {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if just == nil {
		just = "USER"
	}
	return Retract(fact, just, ltre, true)
}

// Contradiction marks a conjunction of facts as contradictory.
// Corresponds to the Lisp function contradiction.
func Contradiction(losers []interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	var trues []*TmsNode
	var falses []*TmsNode
	for _, fact := range losers {
		datum := Referent(innerForm(fact), true, ltre)
		if NegatedProposition(fact) {
			falses = append(falses, datum.TmsNode)
		} else {
			trues = append(trues, datum.TmsNode)
		}
	}
	AddClause(trues, falses, "DECLARED-CONTRADICTION")
}

// Assuming temporarily enables assumptions for the given facts,
// executes body, then retracts them.
// Corresponds to the Lisp macro assuming.
func Assuming(factsToAssume []interface{}, ltre *LTRE, body func()) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	type avPair struct {
		node  *TmsNode
		label NodeLabel
	}
	var pairs []avPair
	for _, fact := range factsToAssume {
		datum := Referent(innerForm(fact), true, ltre)
		node := datum.TmsNode
		ConvertToAssumption(node)
		label := LabelTrue
		if NegatedProposition(fact) {
			label = LabelFalse
		}
		pairs = append(pairs, avPair{node: node, label: label})
	}
	// Enable all assumptions
	for _, av := range pairs {
		EnableAssumption(av.node, av.label)
	}
	defer func() {
		// Retract all assumptions on exit
		for _, av := range pairs {
			RetractAssumption(av.node)
		}
	}()
	body()
}

// BuildTmsFormula recursively converts a logical formula into TMS terms.
// Connective forms are preserved with their children converted;
// atomic/simple propositions are replaced by their TMS nodes.
// Corresponds to the Lisp function build-tms-formula.
func BuildTmsFormula(formula interface{}, ltre *LTRE) interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if list, ok := formula.([]interface{}); ok && len(list) > 0 {
		if s, ok := list[0].(string); ok && isConnective(s) {
			result := make([]interface{}, len(list))
			result[0] = list[0]
			for i := 1; i < len(list); i++ {
				result[i] = BuildTmsFormula(list[i], ltre)
			}
			return result
		}
	}
	// Atomic/simple proposition: return the TMS node
	return Referent(formula, true, ltre).TmsNode
}

// --- Database system ---

// GetDbclass retrieves or creates a Dbclass for the given fact.
// Corresponds to the Lisp function get-dbclass.
func GetDbclass(fact interface{}, ltre *LTRE) *Dbclass {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if fact == nil {
		panic("nil can't be a dbclass")
	}
	switch f := fact.(type) {
	case []interface{}:
		if len(f) == 0 {
			panic("empty list can't be a dbclass")
		}
		if NegatedProposition(fact) {
			return GetDbclass(f[1], ltre)
		}
		return GetDbclass(f[0], ltre)
	case string:
		if dbclass, ok := ltre.DbclassTable[f]; ok {
			return dbclass
		}
		dbclass := &Dbclass{
			Name: f,
			LTRE: ltre,
		}
		ltre.DbclassTable[f] = dbclass
		return dbclass
	default:
		key := fmt.Sprintf("%v", fact)
		if dbclass, ok := ltre.DbclassTable[key]; ok {
			return dbclass
		}
		dbclass := &Dbclass{
			Name: key,
			LTRE: ltre,
		}
		ltre.DbclassTable[key] = dbclass
		return dbclass
	}
}

// Referent finds or creates the datum for a fact.
// If virtual is true, the datum is inserted if not found.
// Corresponds to the Lisp function referent.
func Referent(fact interface{}, virtual bool, ltre *LTRE) *Datum {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if virtual {
		datum, _ := Insert(fact, ltre)
		return datum
	}
	return referent1(fact, ltre)
}

// referent1 looks up an existing datum by its lisp form.
// Corresponds to the Lisp function referent1.
func referent1(fact interface{}, ltre *LTRE) *Datum {
	form := innerForm(fact)
	dbclass := GetDbclass(fact, ltre)
	for _, candidate := range dbclass.Facts {
		if deepEqual(candidate.LispForm, form) {
			return candidate
		}
	}
	return nil
}

// Insert inserts a fact into the database, creating a new datum and
// TMS node if it doesn't already exist. Returns the datum and a boolean
// indicating whether the datum already existed.
// Corresponds to the Lisp function insert.
func Insert(fact interface{}, ltre *LTRE) (*Datum, bool) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	existing := referent1(fact, ltre)
	if existing != nil {
		return existing, true
	}
	form := innerForm(fact)
	ltre.DatumCounter++
	datum := &Datum{
		Counter:  ltre.DatumCounter,
		LTRE:     ltre,
		LispForm: form,
		Dbclass:  GetDbclass(form, ltre),
		Plist:    make(map[string]interface{}),
	}
	datum.TmsNode = TmsCreateNode(ltre.LTMS, datum, false)
	datum.Dbclass.Facts = append([]*Datum{datum}, datum.Dbclass.Facts...)
	TryRules(datum)
	return datum, false
}

// Fetch returns all facts matching the given pattern via unification.
// Returns a slice of substituted patterns.
// Corresponds to the Lisp function fetch.
func Fetch(pattern interface{}, ltre *LTRE) []interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	form := innerForm(pattern)
	var unifiers []interface{}
	candidates := GetCandidates(form, ltre)
	for _, candidate := range candidates {
		bindings := Unify(form, candidate.LispForm)
		if bindings != FailSymbol {
			unifiers = append(unifiers, Sublis(bindings.(map[string]interface{}), pattern))
		}
	}
	return unifiers
}

// GetCandidates returns all facts in the dbclass for the given pattern.
// Corresponds to the Lisp function get-candidates.
func GetCandidates(pattern interface{}, ltre *LTRE) []*Datum {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	return GetDbclass(pattern, ltre).Facts
}

// MapDbclass applies a function to every dbclass in the LTRE.
// Corresponds to the Lisp function map-dbclass.
func MapDbclass(proc func(*Dbclass), ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	for _, dbclass := range ltre.DbclassTable {
		proc(dbclass)
	}
}

// --- Interface and display of data ---

// IsTrue returns true if the fact exists and its TMS node is TRUE.
// Corresponds to the Lisp function true?.
func IsTrue(fact interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	r := Referent(fact, false, ltre)
	if r == nil {
		return false
	}
	if NegatedProposition(fact) {
		return FalseNode(r.TmsNode)
	}
	return TrueNode(r.TmsNode)
}

// IsFalse returns true if the fact exists and its TMS node is FALSE.
// Corresponds to the Lisp function false?.
func IsFalse(fact interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	r := Referent(fact, false, ltre)
	if r == nil {
		return false
	}
	if NegatedProposition(fact) {
		return TrueNode(r.TmsNode)
	}
	return FalseNode(r.TmsNode)
}

// IsKnown returns true if the fact exists and its TMS node label is known.
// Corresponds to the Lisp function known?.
func IsKnown(fact interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	r := Referent(fact, false, ltre)
	if r == nil {
		return false
	}
	return KnownNode(r.TmsNode)
}

// IsUnknown returns true if the fact does not exist or its TMS node
// label is UNKNOWN.
// Corresponds to the Lisp function unknown?.
func IsUnknown(fact interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	r := Referent(fact, false, ltre)
	if r == nil {
		return true
	}
	return UnknownNode(r.TmsNode)
}

// LabelOf returns the label of the TMS node for a fact, or LabelUnknown
// if the fact is not in the database.
// Corresponds to the Lisp function label-of.
func LabelOf(fact interface{}, ltre *LTRE) NodeLabel {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	r := Referent(fact, false, ltre)
	if r == nil {
		return LabelUnknown
	}
	return r.TmsNode.Label
}

// WhyFact prints the explanation for why a fact is believed.
// Corresponds to the Lisp function why?.
func WhyFact(fact interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	r := Referent(fact, false, ltre)
	if r != nil {
		WhyNode(r.TmsNode)
		if KnownNode(r.TmsNode) && r.Assumption != nil {
			fmt.Printf(" (%v)", r.Assumption)
		}
	} else {
		fmt.Printf("\n%v not in database.", fact)
	}
}

// GetTmsNode returns the TMS node for a fact, inserting it if necessary.
// Corresponds to the Lisp function get-tms-node.
func GetTmsNode(fact interface{}, ltre *LTRE) *TmsNode {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	return Referent(fact, true, ltre).TmsNode
}

// LViewNode returns the lisp-form stored in a TMS node's datum.
// Corresponds to the Lisp function view-node.
func LViewNode(node *TmsNode) interface{} {
	if d, ok := node.Datum.(*Datum); ok {
		return d.LispForm
	}
	return node.Datum
}

// SignedViewNode returns the lisp-form for creating terms in clauses,
// wrapping in (:NOT ...) if the node is false.
// Corresponds to the Lisp function signed-view-node.
func SignedViewNode(node *TmsNode) interface{} {
	if FalseNode(node) {
		return []interface{}{":NOT", LViewNode(node)}
	}
	if TrueNode(node) {
		return LViewNode(node)
	}
	panic(fmt.Sprintf("SignedViewNode requires knowing label: %v", node))
}

// ShowDatum returns a string representation of a datum's lisp form.
// Corresponds to the Lisp function show-datum.
func ShowDatum(datum *Datum) string {
	return fmt.Sprintf("%v", datum.LispForm)
}

// MakeNodeString returns a string representation of a TMS node's datum.
// Used as the node-string function for the LTMS.
// Corresponds to the Lisp function make-node-string.
func MakeNodeString(node *TmsNode) string {
	if d, ok := node.Datum.(*Datum); ok {
		return ShowDatum(d)
	}
	return fmt.Sprintf("%v", node.Datum)
}

// AssumptionsOf returns the lisp-forms of all assumptions
// supporting a fact.
// Corresponds to the Lisp function assumptions-of.
func AssumptionsOf(fact interface{}, ltre *LTRE) []interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	datum := Referent(fact, true, ltre)
	nodes := AssumptionsOfNode(datum.TmsNode)
	result := make([]interface{}, len(nodes))
	for i, n := range nodes {
		result[i] = LViewNode(n)
	}
	return result
}

// Consequences prints the consequences of a fact.
// Corresponds to the Lisp function consequences.
func Consequences(fact interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if !IsKnown(fact, ltre) {
		return
	}
	ShowNodeConsequences(GetTmsNode(fact, ltre))
}

// Explore starts interactive exploration of the support network for a fact.
// Corresponds to the Lisp function explore.
func Explore(fact interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	ExploreNetwork(GetTmsNode(fact, ltre))
}

// --- Global interrogatives ---

// ShowData displays all facts in the LTRE.
// Corresponds to the Lisp function show-data.
func ShowData(ltre *LTRE, w io.Writer) int {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if w == nil {
		w = os.Stdout
	}
	counter := 0
	fmt.Fprintf(w, "\n%d facts total.", ltre.DatumCounter)
	for _, dbclass := range ltre.DbclassTable {
		for _, datum := range dbclass.Facts {
			counter++
			var status string
			if TrueNode(datum.TmsNode) {
				status = "TRUE"
			} else if FalseNode(datum.TmsNode) {
				status = "FALSE"
			} else {
				status = "UNKNOWN"
			}
			fmt.Fprintf(w, "\n%s: %s", ShowDatum(datum), status)
		}
	}
	return counter
}

// GetDatum finds a datum by its ID number.
// Corresponds to the Lisp function get-datum.
func GetDatum(num int, ltre *LTRE) *Datum {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	for _, dbclass := range ltre.DbclassTable {
		for _, datum := range dbclass.Facts {
			if datum.Counter == num {
				return datum
			}
		}
	}
	return nil
}

// GetClause finds a clause by its index number.
// Corresponds to the Lisp function get-clause.
func GetClause(num int, ltre *LTRE) *Clause {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	for _, clause := range ltre.LTMS.Clauses {
		if clause.Index == num {
			return clause
		}
	}
	return nil
}

// FetchGlobal returns all facts matching pattern across all dbclasses,
// optionally filtered by status.
// Corresponds to the Lisp function fetch-global.
func FetchGlobal(pattern interface{}, status NodeLabel, filterByStatus bool, ltre *LTRE) []interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	var results []interface{}
	MapDbclass(func(dbclass *Dbclass) {
		for _, datum := range dbclass.Facts {
			statusOk := true
			if filterByStatus {
				switch status {
				case LabelTrue:
					statusOk = TrueNode(datum.TmsNode)
				case LabelFalse:
					statusOk = FalseNode(datum.TmsNode)
				case LabelUnknown:
					statusOk = UnknownNode(datum.TmsNode)
				}
			}
			if statusOk {
				bindings := Unify(datum.LispForm, pattern)
				if bindings != FailSymbol {
					results = append(results, datum.LispForm)
				}
			}
		}
	}, ltre)
	return results
}

// FormLess returns true if the datum for x was inserted before the datum for y.
// Corresponds to the Lisp function form<.
func FormLess(x, y interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	rx := Referent(x, true, ltre)
	ry := Referent(y, true, ltre)
	return rx.Counter < ry.Counter
}

// --- Deep equality helper ---

// deepEqual performs structural equality comparison on interface values.
func deepEqual(a, b interface{}) bool {
	if a == b {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	aSlice, aOk := a.([]interface{})
	bSlice, bOk := b.([]interface{})
	if aOk && bOk {
		if len(aSlice) != len(bSlice) {
			return false
		}
		for i := range aSlice {
			if !deepEqual(aSlice[i], bSlice[i]) {
				return false
			}
		}
		return true
	}
	return fmt.Sprintf("%v", a) == fmt.Sprintf("%v", b)
}

// --- String helper for formatting ---

// formatForm returns a human-readable string for an interface value.
func formatForm(v interface{}) string {
	switch f := v.(type) {
	case []interface{}:
		parts := make([]string, len(f))
		for i, elem := range f {
			parts[i] = formatForm(elem)
		}
		return "(" + strings.Join(parts, " ") + ")"
	case string:
		return f
	default:
		return fmt.Sprintf("%v", f)
	}
}
