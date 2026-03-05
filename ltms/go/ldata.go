// LTRE database operations.
// Converted from ldata.lisp.
package ltms

import (
	"fmt"
	"strings"
)

// Dbclass represents a database class for indexing facts.
type Dbclass struct {
	Name  string
	LTRE  *LTRE
	Facts []*Datum
	Rules []*Rule
}

// Datum represents a database entry.
type Datum struct {
	Counter    int
	LTRE       *LTRE
	LispForm   interface{}
	TmsNode    *TmsNode
	Dbclass    *Dbclass
	Assumption interface{} // nil or informant
}

var connectives = map[string]bool{
	":IMPLIES": true, ":AND": true, ":OR": true,
	":IFF": true, ":NOT": true, ":TAXONOMY": true,
}

// SimpleProposition checks if x is a simple (non-connective) proposition.
func SimpleProposition(x interface{}) bool {
	lst, ok := x.([]interface{})
	if !ok {
		return true
	}
	if len(lst) == 0 {
		return true
	}
	if s, ok := lst[0].(string); ok {
		return !connectives[s]
	}
	return true
}

// NegatedProposition checks if form is (:NOT simple-prop).
func NegatedProposition(form interface{}) bool {
	lst, ok := form.([]interface{})
	if !ok {
		return false
	}
	if len(lst) != 2 {
		return false
	}
	s, ok := lst[0].(string)
	if !ok || s != ":NOT" {
		return false
	}
	return SimpleProposition(lst[1])
}

// Assert asserts a fact by adding it as a formula.
func Assert(fact, just interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	formula := BuildTmsFormula(fact, ltre)
	AddFormula(ltre.LTMS, formula, just)
}

// Assume assumes a fact.
func Assume(fact, reason interface{}, ltre *LTRE) *Datum {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	var datum *Datum
	var isNeg bool

	if NegatedProposition(fact) {
		lst := fact.([]interface{})
		datum = Referent(lst[1], true, ltre)
		isNeg = true
	} else {
		datum = Referent(fact, true, ltre)
		isNeg = false
	}
	node := datum.TmsNode

	if !isNeg && !SimpleProposition(fact) {
		formula := []interface{}{":IMPLIES", node, BuildTmsFormula(fact, ltre)}
		AddFormula(ltre.LTMS, formula, reason)
	}

	if datum.Assumption == nil {
		datum.Assumption = reason
		ConvertToAssumption(node)
		if isNeg {
			EnableAssumption(node, LabelFalse)
		} else {
			EnableAssumption(node, LabelTrue)
		}
	} else if datum.Assumption != reason {
		panic(fmt.Sprintf("Fact %s assumed because of %v assumed again because of %v",
			ShowDatum(datum), datum.Assumption, reason))
	}
	return datum
}

// Retract retracts a fact.
func Retract(fact, just interface{}, ltre *LTRE, quiet bool) *TmsNode {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	datum := Referent(fact, true, ltre)
	node := datum.TmsNode
	if !node.IsAssumption {
		if !quiet {
			fmt.Printf("\nCan't retract non-assumption: %s", ShowDatum(datum))
		}
		return node
	}
	if !KnownNode(node) {
		if !quiet {
			fmt.Printf("\nNode not believed: %s", ShowDatum(datum))
		}
		return node
	}
	if just == datum.Assumption {
		datum.Assumption = nil
		RetractAssumption(node)
	} else if !quiet {
		fmt.Printf("\nWrong informant for retraction of %s", ShowDatum(datum))
	}
	return node
}

// AlreadyAssumed returns the assumption informant if assumed, nil otherwise.
func AlreadyAssumed(fact interface{}, ltre *LTRE) interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	d := Referent(fact, false, ltre)
	if d == nil {
		return nil
	}
	return d.Assumption
}

// BuildTmsFormula converts a formula to TMS-level representation.
func BuildTmsFormula(formula interface{}, ltre *LTRE) interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if lst, ok := formula.([]interface{}); ok && len(lst) > 0 {
		if s, ok := lst[0].(string); ok && connectives[s] {
			result := make([]interface{}, len(lst))
			result[0] = s
			for i := 1; i < len(lst); i++ {
				result[i] = BuildTmsFormula(lst[i], ltre)
			}
			return result
		}
	}
	datum := Referent(formula, true, ltre)
	return datum.TmsNode
}

// GetDbclass returns the Dbclass for a fact.
func GetDbclass(fact interface{}, ltre *LTRE) *Dbclass {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	name := dbclassName(fact)
	if db, ok := ltre.DbclassTable[name]; ok {
		return db
	}
	db := &Dbclass{
		Name: name,
		LTRE: ltre,
	}
	ltre.DbclassTable[name] = db
	return db
}

func dbclassName(fact interface{}) string {
	if lst, ok := fact.([]interface{}); ok && len(lst) > 0 {
		return fmt.Sprintf("%v", lst[0])
	}
	return fmt.Sprintf("%v", fact)
}

// Referent returns the Datum for a fact, creating it if virtual is true.
func Referent(fact interface{}, virtual bool, ltre *LTRE) *Datum {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	db := GetDbclass(fact, ltre)
	for _, d := range db.Facts {
		if EqualForms(d.LispForm, fact) {
			return d
		}
	}
	if !virtual {
		return nil
	}
	datum, _ := Insert(fact, ltre)
	return datum
}

// Insert inserts a new datum.
func Insert(fact interface{}, ltre *LTRE) (*Datum, bool) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	db := GetDbclass(fact, ltre)
	for _, d := range db.Facts {
		if EqualForms(d.LispForm, fact) {
			return d, false
		}
	}
	ltre.DatumCounter++
	datum := &Datum{
		Counter:  ltre.DatumCounter,
		LTRE:     ltre,
		LispForm: fact,
		Dbclass:  db,
	}
	datum.TmsNode = TmsCreateNode(ltre.LTMS, datum, false)
	db.Facts = append(db.Facts, datum)
	TryRules(datum)
	return datum, true
}

// Fetch returns all facts matching a pattern.
func Fetch(pattern interface{}, ltre *LTRE) []interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	var results []interface{}
	db := GetDbclass(pattern, ltre)
	for _, d := range db.Facts {
		bindings := Unify(pattern, d.LispForm)
		if bindings != FailSymbol {
			results = append(results, d.LispForm)
		}
	}
	return results
}

// IsTrue checks if a fact is true.
func IsTrue(fact interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	r := Referent(fact, false, ltre)
	if r == nil {
		return false
	}
	return TrueNode(r.TmsNode)
}

// IsFalse checks if a fact is false.
func IsFalse(fact interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	r := Referent(fact, false, ltre)
	if r == nil {
		return false
	}
	return FalseNode(r.TmsNode)
}

// IsKnown checks if a fact is known (true or false).
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

// IsUnknown checks if a fact is unknown.
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

// ShowData displays all data in the LTRE.
func ShowData(ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	count := 0
	for _, db := range ltre.DbclassTable {
		for _, d := range db.Facts {
			count++
			fmt.Printf("\n %s: %s", ShowDatum(d), d.TmsNode.Label)
		}
	}
	fmt.Printf("\n %d facts total.", count)
}

// ShowDatum returns a string representation of a datum.
func ShowDatum(datum *Datum) string {
	return fmt.Sprintf("%v", datum.LispForm)
}

// MakeNodeString returns a string for a TMS node used in LTRE context.
func MakeNodeString(node *TmsNode) string {
	if datum, ok := node.Datum.(*Datum); ok {
		return ShowDatum(datum)
	}
	return fmt.Sprintf("%v", node.Datum)
}

// ViewNode returns a human-readable representation of a node.
func ViewNode(node *TmsNode) interface{} {
	if datum, ok := node.Datum.(*Datum); ok {
		return datum.LispForm
	}
	return node.Datum
}

// SignedViewNode returns a signed view of a node.
func SignedViewNode(node *TmsNode) interface{} {
	v := ViewNode(node)
	if TrueNode(node) {
		return v
	}
	return []interface{}{":NOT", v}
}

// GetTmsNode returns the TMS node for a fact.
func GetTmsNode(fact interface{}, ltre *LTRE) *TmsNode {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	d := Referent(fact, true, ltre)
	return d.TmsNode
}

// GetDatum gets datum by counter number.
func GetDatum(num int, ltre *LTRE) *Datum {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	for _, db := range ltre.DbclassTable {
		for _, d := range db.Facts {
			if d.Counter == num {
				return d
			}
		}
	}
	return nil
}

// FetchGlobal fetches across all dbclasses.
func FetchGlobal(pattern interface{}, status NodeLabel, ltre *LTRE) []interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	var results []interface{}
	for _, db := range ltre.DbclassTable {
		for _, d := range db.Facts {
			bindings := Unify(pattern, d.LispForm)
			if bindings != FailSymbol {
				if status == LabelUnknown || d.TmsNode.Label == status {
					results = append(results, d.LispForm)
				}
			}
		}
	}
	return results
}

// AssumptionsOf returns assumptions supporting a fact.
func AssumptionsOf(fact interface{}, ltre *LTRE) []*TmsNode {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	d := Referent(fact, false, ltre)
	if d == nil {
		return nil
	}
	return AssumptionsOfNode(d.TmsNode)
}

// Consequences returns consequences of a fact.
func Consequences(fact interface{}, ltre *LTRE) []*TmsNode {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	d := Referent(fact, false, ltre)
	if d == nil {
		return nil
	}
	return NodeConsequences(d.TmsNode)
}

// WhyFact explains why a fact has its current status.
func WhyFact(fact interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	d := Referent(fact, false, ltre)
	if d == nil {
		fmt.Printf("\n%v is unknown.", fact)
		return
	}
	WhyNode(d.TmsNode)
}

// RAssert is the rule-body version of assert.
func RAssert(fact, just interface{}) {
	Assert(fact, just, CurrentLTRE)
}

// RRetract is the rule-body version of retract.
func RRetract(fact, just interface{}) {
	Retract(fact, just, CurrentLTRE, true)
}

// MapDbclass applies fn to all dbclasses.
func MapDbclass(fn func(*Dbclass), ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	for _, db := range ltre.DbclassTable {
		fn(db)
	}
}

// EqualForms checks structural equality of two forms.
func EqualForms(a, b interface{}) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	switch av := a.(type) {
	case string:
		bv, ok := b.(string)
		return ok && av == bv
	case int:
		bv, ok := b.(int)
		return ok && av == bv
	case float64:
		bv, ok := b.(float64)
		return ok && av == bv
	case bool:
		bv, ok := b.(bool)
		return ok && av == bv
	case *TmsNode:
		return a == b
	case *Datum:
		return a == b
	case []interface{}:
		bv, ok := b.([]interface{})
		if !ok || len(av) != len(bv) {
			return false
		}
		for i := range av {
			if !EqualForms(av[i], bv[i]) {
				return false
			}
		}
		return true
	default:
		return fmt.Sprintf("%v", a) == fmt.Sprintf("%v", b)
	}
}

// IsVariable checks if x is a pattern variable (starts with ?).
func IsVariable(x interface{}) bool {
	s, ok := x.(string)
	return ok && len(s) > 0 && strings.HasPrefix(s, "?")
}
