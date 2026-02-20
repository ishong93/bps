// ATRE: A Tiny Rule Engine, ATMS-based version.
// Database module.
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

// Assert asserts a fact with a justification in the ATRE.
// The just parameter should be a slice: the first element is the informant,
// and the remaining elements are antecedent facts.
// If just is not a slice, it is wrapped in one.
func Assert(fact interface{}, just interface{}, atre *Atre) *Datum {
	datum := Referent(fact, true, atre)
	node := datum.TmsNode
	justSlice, ok := just.([]interface{})
	if !ok {
		justSlice = []interface{}{just}
	}
	debuggingAtre(atre, "\n    Asserting %v via %v.", fact, justSlice)
	var antecedents []*TmsNode
	for _, f := range justSlice[1:] {
		antecedents = append(antecedents, Referent(f, true, atre).TmsNode)
	}
	JustifyNode(justSlice[0], node, antecedents)
	return datum
}

// Assume marks a fact as an assumption in the ATRE.
// Returns the datum. Panics if the fact was already assumed for a different
// reason.
func Assume(fact interface{}, reason interface{}, atre *Atre) *Datum {
	datum := Referent(fact, true, atre)
	node := datum.TmsNode
	if !datum.IsAssumption() {
		datum.Assumption = reason
		debuggingAtre(atre, "\n    Assuming %v via %v.", fact, reason)
		AssumeNode(node)
	} else if reason != datum.Assumption {
		panic(fmt.Sprintf("Fact %s assumed because of %v assumed again because of %v",
			ShowDatum(datum), datum.Assumption, reason))
	}
	return datum
}

// AlreadyAssumed returns true if the fact's TMS node is an assumption.
func AlreadyAssumed(fact interface{}, atre *Atre) bool {
	node := GetTmsNode(fact, atre)
	return node.IsAssumption
}

// AssumeIfNeeded assumes a fact only if it is not already an assumption.
func AssumeIfNeeded(fact interface{}, reason interface{}, atre *Atre) *Datum {
	if AlreadyAssumed(fact, atre) {
		return nil
	}
	return Assume(fact, reason, atre)
}

// Contradiction marks a fact as contradictory.
func Contradiction(fact interface{}, atre *Atre) {
	MakeContradiction(Referent(fact, true, atre).TmsNode)
}

// GetDbclass retrieves or creates a Dbclass for the given fact.
// For a list-like fact (represented as *Cons), it recurses on the Car.
// For a string starting with "?", it is treated as a variable (error if unbound).
// For a plain string, it looks up or creates the dbclass in the table.
func GetDbclass(fact interface{}, atre *Atre) *Dbclass {
	if fact == nil {
		panic("nil can't be a dbclass")
	}
	switch f := fact.(type) {
	case *Cons:
		return GetDbclass(f.Car, atre)
	case string:
		if IsVariable(f) {
			panic(fmt.Sprintf("Dbclass unbound variable: %s", f))
		}
		if dbclass, ok := atre.DbclassTable[f]; ok {
			return dbclass
		}
		dbclass := &Dbclass{
			Name: f,
			Atre: atre,
		}
		atre.DbclassTable[f] = dbclass
		atre.Dbclasses = append(atre.Dbclasses, dbclass)
		return dbclass
	default:
		// Try to use fmt.Sprintf to get a string key for other types.
		key := fmt.Sprintf("%v", fact)
		if dbclass, ok := atre.DbclassTable[key]; ok {
			return dbclass
		}
		dbclass := &Dbclass{
			Name: key,
			Atre: atre,
		}
		atre.DbclassTable[key] = dbclass
		atre.Dbclasses = append(atre.Dbclasses, dbclass)
		return dbclass
	}
}

// Referent finds or creates the datum for a fact.
// If virtual is true, the datum is inserted if not found.
// If virtual is false, only an existing datum is returned (may return nil).
func Referent(fact interface{}, virtual bool, atre *Atre) *Datum {
	if virtual {
		datum, _ := Insert(fact, atre)
		return datum
	}
	return Referent1(fact, atre)
}

// Referent1 looks up an existing datum by its lisp form.
func Referent1(fact interface{}, atre *Atre) *Datum {
	dbclass := GetDbclass(fact, atre)
	for _, candidate := range dbclass.Facts {
		if SExprEqual(candidate.LispForm, fact) {
			return candidate
		}
	}
	return nil
}

// Insert inserts a fact into the database, creating a new datum and TMS node
// if it doesn't already exist. Returns the datum and a boolean indicating
// whether the datum already existed (true = already existed).
func Insert(fact interface{}, atre *Atre) (*Datum, bool) {
	datum := Referent1(fact, atre)
	if datum != nil {
		return datum, true
	}
	atre.DatumCounter++
	datum = &Datum{
		Counter:  atre.DatumCounter,
		Atre:     atre,
		LispForm: fact,
		Dbclass:  GetDbclass(fact, atre),
	}
	datum.TmsNode = TmsCreateNode(atre.ATMS, datum, TMSNodeOpts{})
	datum.Dbclass.Facts = append(datum.Dbclass.Facts, datum)
	TryRules(datum)
	return datum, false
}

// Fetch returns all facts matching the given pattern via unification.
// Returns a slice of substituted patterns.
func Fetch(pattern interface{}, atre *Atre) []interface{} {
	var unifiers []interface{}
	candidates := GetCandidates(pattern, atre)
	for _, candidate := range candidates {
		bindings, ok := Unify(pattern, candidate.LispForm, nil)
		if ok {
			unifiers = append(unifiers, Sublis(bindings, pattern))
		}
	}
	return unifiers
}

// GetCandidates returns all facts in the dbclass for the given pattern.
func GetCandidates(pattern interface{}, atre *Atre) []*Datum {
	return GetDbclass(pattern, atre).Facts
}

// True returns true if the fact exists and its TMS node is true
// (i.e., has label starting with the empty environment).
func True(fact interface{}, atre *Atre) bool {
	r := Referent(fact, false, atre)
	if r == nil {
		return false
	}
	return TrueNode(r.TmsNode)
}

// In returns true if the fact exists and its TMS node is in the given
// environment.
func In(fact interface{}, env *Env, atre *Atre) bool {
	r := Referent(fact, false, atre)
	if r == nil {
		return false
	}
	return InNode(r.TmsNode, env)
}

// Out returns true if the fact exists and its TMS node is out of the
// given environment.
func Out(fact interface{}, env *Env, atre *Atre) bool {
	r := Referent(fact, false, atre)
	if r == nil {
		return false
	}
	return OutNode(r.TmsNode, env)
}

// ConsistentWith returns true if the fact exists and its TMS node is
// consistent with the given environment.
func ConsistentWith(fact interface{}, env *Env, atre *Atre) bool {
	r := Referent(fact, false, atre)
	if r == nil {
		return false
	}
	return NodeConsistentWith(r.TmsNode, env)
}

// Why prints the explanation for why a fact is believed.
func Why(fact interface{}, atre *Atre, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	r := Referent(fact, false, atre)
	if r != nil {
		WhyNode(r.TmsNode, w, "")
	}
}

// EnvironmentOf constructs an environment from a list of assumed facts.
// Returns the environment and nil on success.
// If the result is nogood, returns nil and the nogood environment.
func EnvironmentOf(facts []interface{}, atre *Atre) (*Env, *Env) {
	env := atre.ATMS.EmptyEnv
	for _, fact := range facts {
		node := GetTmsNode(fact, atre)
		if !node.IsAssumption {
			panic(fmt.Sprintf("Non-assumption in EnvironmentOf: %v", fact))
		}
		env = ConsEnv(node, env)
		if env.IsNogood() {
			return nil, env
		}
	}
	return env, nil
}

// EnvironmentCons extends an environment with a fact's TMS node.
func EnvironmentCons(fact interface{}, env *Env, atre *Atre) *Env {
	return ConsEnv(GetTmsNode(fact, atre), env)
}

// ViewEnv returns the lisp forms of all assumptions in the environment.
func ViewEnv(env *Env) []interface{} {
	result := make([]interface{}, len(env.Assumptions))
	for i, a := range env.Assumptions {
		result[i] = ViewNode(a)
	}
	return result
}

// Justifications prints the justifications for a fact.
func Justifications(fact interface{}, atre *Atre, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	NodeJustifications(GetTmsNode(fact, atre), w)
}

// TheE looks up an environment by index number in the ATRE's ATMS.
func TheE(num int, atre *Atre) *Env {
	return E(atre.ATMS, num)
}

// GetTmsNode returns the TMS node for a fact, inserting it if necessary.
func GetTmsNode(fact interface{}, atre *Atre) *TmsNode {
	return Referent(fact, true, atre).TmsNode
}

// ViewNode returns the lisp form stored in a TMS node's datum.
func ViewNode(node *TmsNode) interface{} {
	if d, ok := node.Datum.(*Datum); ok {
		return d.LispForm
	}
	return node.Datum
}

// StringifyNode returns the string representation of a TMS node's datum.
func StringifyNode(node *TmsNode) string {
	return fmt.Sprintf("%v", ViewNode(node))
}

// AssumptionsOf returns the label (list of environments) for a fact's TMS node.
func AssumptionsOf(fact interface{}, atre *Atre) []*Env {
	return Referent(fact, true, atre).TmsNode.Label
}

// GetDatum finds a datum by its counter number.
func GetDatum(num int, atre *Atre) *Datum {
	for _, dbclass := range atre.Dbclasses {
		for _, datum := range dbclass.Facts {
			if datum.Counter == num {
				return datum
			}
		}
	}
	return nil
}

// GetJust finds a justification by its index number.
func GetJust(num int, atre *Atre) *Just {
	for _, just := range atre.ATMS.Justs {
		if just.Index == num {
			return just
		}
	}
	return nil
}

// ShowDatum returns a string representation of a datum's lisp form.
func ShowDatum(datum *Datum) string {
	return fmt.Sprintf("%v", datum.LispForm)
}

// ShowData displays all facts in the ATRE. Returns the total count displayed.
func ShowData(atre *Atre, w io.Writer) int {
	if w == nil {
		w = os.Stdout
	}
	counter := 0
	fmt.Fprintf(w, "\n%d facts total.", atre.DatumCounter)
	for _, dbclass := range atre.Dbclasses {
		for _, datum := range dbclass.Facts {
			counter++
			fmt.Fprintf(w, "\n%s: %v", ShowDatum(datum), AssumptionsOf(datum.LispForm, atre))
		}
	}
	return counter
}

// ShowContext displays all facts that are in the given environment.
// Returns the count of facts displayed.
func ShowContext(env *Env, atre *Atre, w io.Writer) int {
	if w == nil {
		w = os.Stdout
	}
	counter := 0
	for _, dbclass := range atre.Dbclasses {
		for _, datum := range dbclass.Facts {
			if InNode(datum.TmsNode, env) {
				counter++
				fmt.Fprintf(w, "\n%s", ShowDatum(datum))
			}
		}
	}
	fmt.Fprintf(w, "\n%d facts total.", counter)
	return counter
}

// ShowDbclasses displays a summary of all database classes.
// Returns the count of dbclasses.
func ShowDbclasses(atre *Atre, w io.Writer) int {
	if w == nil {
		w = os.Stdout
	}
	counter := 0
	for _, dbclass := range atre.Dbclasses {
		counter++
		fmt.Fprintf(w, "\n %s: %d facts, %d rules",
			dbclass.Name,
			len(dbclass.Facts),
			len(dbclass.Rules))
	}
	return counter
}
