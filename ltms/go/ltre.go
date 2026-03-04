// LTRE (LTMS-based Tiny Rule Engine) interface.
// Converted from linter.lisp.
package ltms

import "fmt"

// LTRE is the LTMS-based Tiny Rule Engine.
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

// CurrentLTRE is the global current LTRE.
var CurrentLTRE *LTRE

func (l *LTRE) String() string {
	return fmt.Sprintf("<LTRE: %s>", l.Title)
}

func debuggingLtre(msg string, args ...interface{}) {
	if CurrentLTRE != nil && CurrentLTRE.Debugging {
		fmt.Printf(msg, args...)
	}
}

// CreateLTRE creates a new LTRE.
func CreateLTRE(title string, debugging bool) *LTRE {
	l := &LTRE{
		Title:        title,
		DbclassTable: make(map[string]*Dbclass),
		Debugging:    debugging,
	}
	l.LTMS = CreateLTMS(fmt.Sprintf("(:LTMS-OF %s)", title), LTMSOptions{
		NodeString:             MakeNodeString,
		Debugging:              false,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            false,
		DelaySat:               true,
	})
	l.LTMS.SetEnqueueProcedure(func(pair interface{}) {
		Enqueue(pair, l)
	})
	CurrentLTRE = l
	return l
}

// ChangeLTRE changes LTRE options.
func ChangeLTRE(l *LTRE, debugging bool) {
	l.Debugging = debugging
}

// InLTRE sets the current LTRE.
func InLTRE(l *LTRE) {
	CurrentLTRE = l
}

// WithLTRE runs fn with ltre as the current LTRE.
func WithLTRE(ltre *LTRE, fn func()) {
	old := CurrentLTRE
	CurrentLTRE = ltre
	defer func() { CurrentLTRE = old }()
	fn()
}

// UAssert asserts a fact and runs rules.
func UAssert(fact interface{}, just interface{}) {
	if just == nil {
		just = "user"
	}
	Assert(fact, just, CurrentLTRE)
	RunRules(CurrentLTRE)
}

// UAssume assumes a fact and runs rules.
func UAssume(fact interface{}, reason interface{}) {
	Assume(fact, reason, CurrentLTRE)
	RunRules(CurrentLTRE)
}

// RunForms evaluates forms and runs rules after each.
func RunForms(forms []func(), ltre *LTRE) {
	old := CurrentLTRE
	CurrentLTRE = ltre
	defer func() { CurrentLTRE = old }()
	for _, form := range forms {
		form()
		RunRules(ltre)
	}
}

// Show displays LTRE contents.
func Show(ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	fmt.Printf("For LTRE %s:", ltre.Title)
	ShowData(ltre)
	ShowRules(ltre)
}

// ShowByInformant shows clauses with a given informant.
func ShowByInformant(informant interface{}, ltre *LTRE) int {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	count := 0
	if clauses, ok := ltre.LTMS.Clauses.([]*Clause); ok {
		for _, clause := range clauses {
			match := false
			if inf, ok := clause.Informant.([]interface{}); ok && len(inf) >= 3 {
				match = inf[2] == informant
			} else {
				match = clause.Informant == informant
			}
			if match {
				count++
				fmt.Printf("%v\n", ViewClause(clause))
			}
		}
	}
	return count
}

// ViewClause returns a human-readable representation of a clause.
func ViewClause(cl *Clause) interface{} {
	result := []interface{}{"OR"}
	for _, lit := range cl.Literals {
		if lit.Sign == LabelFalse {
			result = append(result, []interface{}{"NOT", ViewNode(lit.Node)})
		} else {
			result = append(result, ViewNode(lit.Node))
		}
	}
	return result
}

// Assuming enables assumptions, runs body, then retracts.
func Assuming(facts []interface{}, ltre *LTRE, body func()) {
	old := CurrentLTRE
	CurrentLTRE = ltre
	defer func() { CurrentLTRE = old }()

	var nodes []*TmsNode
	for _, fact := range facts {
		var node *TmsNode
		var label NodeLabel
		if negated := isNegatedProp(fact); negated != nil {
			datum := Referent(negated, true, ltre)
			node = datum.TmsNode
			label = LabelFalse
		} else {
			datum := Referent(fact, true, ltre)
			node = datum.TmsNode
			label = LabelTrue
		}
		if !node.IsAssumption {
			ConvertToAssumption(node)
		}
		EnableAssumption(node, label)
		nodes = append(nodes, node)
	}
	defer func() {
		for _, n := range nodes {
			RetractAssumption(n)
		}
	}()
	body()
}

func isNegatedProp(fact interface{}) interface{} {
	if lst, ok := fact.([]interface{}); ok && len(lst) == 2 {
		if op, ok := lst[0].(string); ok && op == ":NOT" {
			return lst[1]
		}
	}
	return nil
}
