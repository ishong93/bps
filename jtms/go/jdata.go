// JTRE: A JTMS-based Tiny Rule Engine.
// Database module.
// Translated from jdata.lisp in "Building Problem Solvers"
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

// JDbclass groups facts and rules that share a common predicate name.
// Corresponds to the Lisp defstruct dbclass in jdata.lisp.
type JDbclass struct {
	Name  string     // Corresponding symbol
	Jtre  *Jtre      // JTRE it is part of
	Facts []*JDatum  // Associated facts
	Rules []*JRule   // Associated rules
}

// String implements fmt.Stringer for JDbclass.
func (d *JDbclass) String() string {
	return fmt.Sprintf("<Dbclass %s>", d.Name)
}

// JDatum represents a single fact in the JTRE database.
// Corresponds to the Lisp defstruct datum in jdata.lisp.
type JDatum struct {
	ID         int                    // Unique ID for easy lookup
	LispForm   interface{}            // Expression for pattern-matching
	TmsNode    *TmsNode               // Pointer into TMS
	Dbclass    *JDbclass              // Dbclass of the corresponding pattern
	Assumption interface{}            // If non-nil, indicates informant
	Plist      map[string]interface{} // Local property list
}

// String implements fmt.Stringer for JDatum.
func (d *JDatum) String() string {
	return fmt.Sprintf("<Datum %d>", d.ID)
}

// IsAssumption returns true if this datum has been assumed.
func (d *JDatum) IsAssumption() bool {
	return d.Assumption != nil
}

// JAssert asserts a fact with a justification in the JTRE.
// The just parameter can be a single informant or a slice where
// the first element is the informant and the rest are antecedent facts.
// Corresponds to the Lisp function assert! in jdata.lisp.
func JAssert(fact interface{}, just interface{}, jtre *Jtre) *JDatum {
	datum := JReferent(fact, true, jtre)
	node := datum.TmsNode
	justSlice, ok := just.([]interface{})
	if !ok {
		justSlice = []interface{}{just}
	}
	debuggingJtre(jtre, "\n    Asserting %v via %v.", fact, justSlice)
	var antecedents []*TmsNode
	for _, f := range justSlice[1:] {
		antecedents = append(antecedents, JReferent(f, true, jtre).TmsNode)
	}
	JustifyNode(justSlice[0], node, antecedents)
	return datum
}

// QuietAssert asserts a fact without triggering contradiction checks.
// Corresponds to the Lisp function quiet-assert!.
func QuietAssert(fact interface{}, just interface{}, jtre *Jtre) *JDatum {
	var datum *JDatum
	WithoutContradictionCheck(jtre.JTMS, func() {
		datum = JAssert(fact, just, jtre)
	})
	return datum
}

// JAssume marks a fact as an assumption in the JTRE.
// Panics if the fact was already assumed for a different reason.
// Corresponds to the Lisp function assume! in jdata.lisp.
func JAssume(fact interface{}, reason interface{}, jtre *Jtre) *JDatum {
	datum := JReferent(fact, true, jtre)
	node := datum.TmsNode
	if !datum.IsAssumption() {
		datum.Assumption = reason
		debuggingJtre(jtre, "\n    Assuming %v via %v.", fact, reason)
		AssumeNode(node)
		EnableAssumption(node)
	} else if reason == datum.Assumption {
		// Same reason, do nothing.
	} else {
		panic(fmt.Sprintf(
			"Fact %s assumed because of %v assumed again because of %v",
			ShowJDatum(datum), datum.Assumption, reason))
	}
	return datum
}

// AlreadyAssumed returns true if the fact is already assumed.
// Corresponds to the Lisp function already-assumed?.
func AlreadyAssumed(fact interface{}, jtre *Jtre) bool {
	datum := JReferent(fact, true, jtre)
	return datum.IsAssumption()
}

// JRetract retracts a previously assumed fact.
// Corresponds to the Lisp function retract! in jdata.lisp.
func JRetract(fact interface{}, just interface{}, quiet bool, jtre *Jtre) *TmsNode {
	datum := JReferent(fact, true, jtre)
	node := datum.TmsNode
	if !node.Assumption {
		if !quiet {
			fmt.Printf("\n%s isn't an assumption.", ShowJDatum(datum))
		}
	} else if !InNode(node) {
		if !quiet {
			fmt.Printf("\nThe assumption %v is not currently in.", fact)
		}
	} else if just == datum.Assumption {
		debuggingJtre(jtre, "\n    Retracting %v via %v.", fact, just)
		datum.Assumption = nil
		RetractAssumption(node)
	} else if !quiet {
		fmt.Printf("\n%v not source of assumption for %v", just, fact)
	}
	return node
}

// JContradiction marks a fact as contradictory.
// Corresponds to the Lisp function contradiction.
func JContradiction(fact interface{}, jtre *Jtre) {
	MakeContradiction(JReferent(fact, true, jtre).TmsNode)
}

// JIn returns true if the fact exists and its TMS node is IN.
// Corresponds to the Lisp function in?.
func JIn(fact interface{}, jtre *Jtre) bool {
	r := JReferent(fact, false, jtre)
	if r == nil {
		return false
	}
	return InNode(r.TmsNode)
}

// JOut returns true if the fact exists and its TMS node is OUT.
// Corresponds to the Lisp function out?.
func JOut(fact interface{}, jtre *Jtre) bool {
	r := JReferent(fact, false, jtre)
	if r == nil {
		return false
	}
	return OutNode(r.TmsNode)
}

// JWhy prints the explanation for why a fact is believed.
// Corresponds to the Lisp function why?.
func JWhy(fact interface{}, jtre *Jtre) {
	r := JReferent(fact, false, jtre)
	if r != nil {
		WhyNode(r.TmsNode)
	}
}

// JAssumptionsOf returns the lisp-forms of all assumptions
// supporting a fact.
// Corresponds to the Lisp function assumptions-of.
func JAssumptionsOf(fact interface{}, jtre *Jtre) []interface{} {
	datum := JReferent(fact, true, jtre)
	nodes := AssumptionsOfNode(datum.TmsNode)
	result := make([]interface{}, len(nodes))
	for i, n := range nodes {
		result[i] = ViewNode(n)
	}
	return result
}

// JFetch returns all facts matching the given pattern via unification.
// Returns a slice of substituted patterns.
// Corresponds to the Lisp function fetch.
//
// NOTE: This is a simplified version. A full implementation would
// require a unification module (funify). Here we do exact equality
// matching and return matching lisp-forms.
func JFetch(pattern interface{}, jtre *Jtre) []interface{} {
	var results []interface{}
	candidates := JGetCandidates(pattern, jtre)
	for _, candidate := range candidates {
		// Simplified: exact match only. A full port would use Unify.
		if deepEqual(pattern, candidate.LispForm) {
			results = append(results, candidate.LispForm)
		}
	}
	return results
}

// Wfs performs a "walk from support" trace for a fact.
// Corresponds to the Lisp function wfs.
func Wfs(fact interface{}, jtre *Jtre) interface{} {
	if JOut(fact, jtre) {
		fmt.Printf("\n %v is OUT.", fact)
		return nil
	}
	startNode := GetJTmsNode(fact, jtre)
	queue := []*TmsNode{startNode}
	soFar := map[*TmsNode]bool{startNode: true}
	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		WhyNode(current)
		if !OutNode(current) && !current.Assumption {
			if just, ok := current.Support.(*Just); ok {
				for _, ante := range just.Antecedents {
					if !soFar[ante] {
						soFar[ante] = true
						queue = append(queue, ante)
					}
				}
			}
		}
	}
	fmt.Printf("\n--------")
	return fact
}

// SayDatumBelief prints the belief status of a fact.
// Corresponds to the Lisp function say-datum-belief.
func SayDatumBelief(fact interface{}, jtre *Jtre, indent string) {
	node := GetJTmsNode(fact, jtre)
	status := "OUT"
	if InNode(node) {
		status = "IN"
	}
	fmt.Printf("\n%s%v: %s", indent, fact, status)
}

// ShowJustifications prints all justifications for a fact.
// Corresponds to the Lisp function show-justifications.
func ShowJustifications(fact interface{}, jtre *Jtre) {
	fmt.Printf("\n %v::", fact)
	node := GetJTmsNode(fact, jtre)
	justs := node.Justs
	if len(justs) == 0 {
		fmt.Printf(" No justifications.")
		return
	}
	for _, j := range justs {
		fmt.Printf("\n %v", j.Informant)
		if len(j.Antecedents) > 0 {
			fmt.Printf(", on:")
			for _, ante := range j.Antecedents {
				SayDatumBelief(ViewNode(ante), jtre, "  ")
			}
			fmt.Printf(".")
		} else {
			fmt.Printf(".")
		}
	}
}

// ShowJData displays all facts in the JTRE.
// Corresponds to the Lisp function show-data.
func ShowJData(jtre *Jtre, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n%d facts total.", jtre.DatumCounter)
	JMapDbclass(func(dbclass *JDbclass) {
		for _, datum := range dbclass.Facts {
			status := "OUT"
			if InNode(datum.TmsNode) {
				status = "IN"
			}
			fmt.Fprintf(w, "\n%s: %s", ShowJDatum(datum), status)
		}
	}, jtre)
}

// JGetDbclass retrieves or creates a JDbclass for the given fact.
// For a slice (list-like fact), it recurses on the first element.
// For a plain string, it looks up or creates the dbclass in the table.
// Corresponds to the Lisp function get-dbclass.
func JGetDbclass(fact interface{}, jtre *Jtre) *JDbclass {
	if fact == nil {
		panic("nil can't be a dbclass")
	}
	switch f := fact.(type) {
	case []interface{}:
		if len(f) == 0 {
			panic("empty list can't be a dbclass")
		}
		return JGetDbclass(f[0], jtre)
	case string:
		if dbclass, ok := jtre.DbclassTable[f]; ok {
			return dbclass
		}
		dbclass := &JDbclass{
			Name: f,
			Jtre: jtre,
		}
		jtre.DbclassTable[f] = dbclass
		return dbclass
	default:
		key := fmt.Sprintf("%v", fact)
		if dbclass, ok := jtre.DbclassTable[key]; ok {
			return dbclass
		}
		dbclass := &JDbclass{
			Name: key,
			Jtre: jtre,
		}
		jtre.DbclassTable[key] = dbclass
		return dbclass
	}
}

// JReferent finds or creates the datum for a fact.
// If virtual is true, the datum is inserted if not found.
// If virtual is false, only an existing datum is returned (may return nil).
// Corresponds to the Lisp function referent.
func JReferent(fact interface{}, virtual bool, jtre *Jtre) *JDatum {
	if virtual {
		datum, _ := JInsert(fact, jtre)
		return datum
	}
	return jReferent1(fact, jtre)
}

// jReferent1 looks up an existing datum by its lisp form.
// Corresponds to the Lisp function referent1.
func jReferent1(fact interface{}, jtre *Jtre) *JDatum {
	dbclass := JGetDbclass(fact, jtre)
	for _, candidate := range dbclass.Facts {
		if deepEqual(candidate.LispForm, fact) {
			return candidate
		}
	}
	return nil
}

// JInsert inserts a fact into the database, creating a new datum and
// TMS node if it doesn't already exist. Returns the datum and a boolean
// indicating whether the datum already existed.
// Corresponds to the Lisp function insert.
func JInsert(fact interface{}, jtre *Jtre) (*JDatum, bool) {
	datum := jReferent1(fact, jtre)
	if datum != nil {
		return datum, true
	}
	jtre.DatumCounter++
	datum = &JDatum{
		ID:       jtre.DatumCounter,
		LispForm: fact,
		Dbclass:  JGetDbclass(fact, jtre),
		Plist:    make(map[string]interface{}),
	}
	datum.TmsNode = TmsCreateNode(jtre.JTMS, datum, false, false)
	datum.Dbclass.Facts = append([]*JDatum{datum}, datum.Dbclass.Facts...)
	JTryRules(datum)
	return datum, false
}

// JGetCandidates returns all facts in the dbclass for the given pattern.
// Corresponds to the Lisp function get-candidates.
func JGetCandidates(pattern interface{}, jtre *Jtre) []*JDatum {
	return JGetDbclass(pattern, jtre).Facts
}

// JMapDbclass applies a function to every dbclass in the JTRE.
// Corresponds to the Lisp function map-dbclass.
func JMapDbclass(proc func(*JDbclass), jtre *Jtre) {
	for _, dbclass := range jtre.DbclassTable {
		proc(dbclass)
	}
}

// GetJTmsNode returns the TMS node for a fact, inserting it if necessary.
// Corresponds to the Lisp function get-tms-node.
func GetJTmsNode(fact interface{}, jtre *Jtre) *TmsNode {
	return JReferent(fact, true, jtre).TmsNode
}

// ViewNode returns the lisp-form stored in a TMS node's datum.
// Corresponds to the Lisp function view-node.
func ViewNode(node *TmsNode) string {
	if d, ok := node.Datum.(*JDatum); ok {
		return fmt.Sprintf("%v", d.LispForm)
	}
	return fmt.Sprintf("%v", node.Datum)
}

// ShowJDatum returns a string representation of a datum's lisp form.
// Corresponds to the Lisp function show-datum.
func ShowJDatum(datum *JDatum) string {
	return fmt.Sprintf("%v", datum.LispForm)
}

// GetJDatum finds a datum by its ID number.
// Corresponds to the Lisp function get-datum.
func GetJDatum(num int, jtre *Jtre) *JDatum {
	var result *JDatum
	JMapDbclass(func(dbclass *JDbclass) {
		for _, datum := range dbclass.Facts {
			if datum.ID == num {
				result = datum
			}
		}
	}, jtre)
	return result
}

// GetJJust finds a justification by its index number.
// Corresponds to the Lisp function get-just.
func GetJJust(num int, jtre *Jtre) *Just {
	for _, just := range jtre.JTMS.Justs {
		if just.Index == num {
			return just
		}
	}
	return nil
}

// deepEqual performs structural equality comparison on interface values.
// It handles slices, strings, and other comparable types.
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
