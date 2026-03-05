// Rule package for LTRE.
// Converted from lrules.lisp.
package ltms

import "fmt"

// Rule represents a rule in the LTRE.
type Rule struct {
	Counter int
	LTRE    *LTRE
	Dbclass *Dbclass
	Matcher func(interface{}) (bool, []interface{}, bool)
	Body    func(args ...interface{})
}

// RuleInvocation is a queued rule application.
type RuleInvocation struct {
	Body     func(args ...interface{})
	Bindings []interface{}
}

func (r *Rule) String() string {
	return fmt.Sprintf("<Rule %d>", r.Counter)
}

// InsertRule creates and indexes a rule.
func InsertRule(dbclass *Dbclass, matcher func(interface{}) (bool, []interface{}, bool), body func(args ...interface{})) *Rule {
	ltre := dbclass.LTRE
	ltre.RuleCounter++
	rule := &Rule{
		Counter: ltre.RuleCounter,
		LTRE:    ltre,
		Dbclass: dbclass,
		Matcher: matcher,
		Body:    body,
	}
	dbclass.Rules = append(dbclass.Rules, rule)
	for _, candidate := range dbclass.Facts {
		TryRuleOn(rule, candidate)
	}
	return rule
}

// TryRules tries all rules for a datum's dbclass.
func TryRules(datum *Datum) {
	for _, rule := range datum.Dbclass.Rules {
		TryRuleOn(rule, datum)
	}
}

// TryRuleOn tries a single rule on a datum.
func TryRuleOn(rule *Rule, datum *Datum) {
	ltre := datum.LTRE
	ok, bindings, hasNode := rule.Matcher(datum.LispForm)
	if ok {
		if hasNode {
			bindings = append([]interface{}{datum.TmsNode}, bindings...)
		}
		Enqueue(&RuleInvocation{Body: rule.Body, Bindings: bindings}, ltre)
	}
}

// RunRules executes all queued rules.
func RunRules(ltre *LTRE) int {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	old := CurrentLTRE
	CurrentLTRE = ltre
	defer func() { CurrentLTRE = old }()

	counter := 0
	for {
		form := Dequeue(ltre)
		if form == nil {
			break
		}
		form.Body(form.Bindings...)
		counter++
	}
	debuggingLtre("\n    %d rules run.", counter)
	ltre.RulesRun += counter
	return counter
}

// RunOneRule executes a single queued rule.
func RunOneRule(ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	rule := Dequeue(ltre)
	if rule != nil {
		debuggingLtre("\n     Executing single rule.")
		ltre.RulesRun++
		rule.Body(rule.Bindings...)
	}
	return RulesWaiting(ltre)
}

// RulesWaiting checks if rules are queued.
func RulesWaiting(ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	return len(ltre.Queue) > 0
}

// Enqueue adds a rule invocation to the queue.
func Enqueue(item interface{}, ltre *LTRE) {
	if ri, ok := item.(*RuleInvocation); ok {
		ltre.Queue = append([]*RuleInvocation{ri}, ltre.Queue...)
	}
}

// Dequeue removes and returns the first rule invocation.
func Dequeue(ltre *LTRE) *RuleInvocation {
	if len(ltre.Queue) == 0 {
		return nil
	}
	item := ltre.Queue[0]
	ltre.Queue = ltre.Queue[1:]
	return item
}

// ShowRules displays all rules.
func ShowRules(ltre *LTRE) int {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	counter := 0
	fmt.Printf("\nThe rules in %s are:", ltre.Title)
	MapDbclass(func(dbclass *Dbclass) {
		for _, rule := range dbclass.Rules {
			counter++
			PrintRule(rule)
		}
	}, ltre)
	return counter
}

// PrintRule prints a single rule.
func PrintRule(rule *Rule) {
	fmt.Printf("\n %s: dbclass=%s", rule, rule.Dbclass.Name)
}

// GetRule finds a rule by counter.
func GetRule(num int, ltre *LTRE) *Rule {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	var found *Rule
	MapDbclass(func(db *Dbclass) {
		for _, r := range db.Rules {
			if r.Counter == num {
				found = r
			}
		}
	}, ltre)
	return found
}

// MakeRule is a helper to create rules programmatically.
// triggerCondition: "TRUE", "FALSE", or "INTERN"
func MakeRule(triggerPattern interface{}, triggerCondition string,
	bodyFn func(bindings map[string]interface{}, triggerNode *TmsNode)) {

	db := GetDbclass(triggerPattern, CurrentLTRE)
	matcher := func(datum interface{}) (bool, []interface{}, bool) {
		ok, bindings := MatchPattern(triggerPattern, datum)
		if !ok {
			return false, nil, false
		}
		bindingsList := []interface{}{bindings}
		needsNode := triggerCondition != "INTERN"
		return true, bindingsList, needsNode
	}
	body := func(args ...interface{}) {
		var node *TmsNode
		var bindings map[string]interface{}
		idx := 0
		if triggerCondition != "INTERN" {
			if len(args) > 0 {
				node, _ = args[0].(*TmsNode)
			}
			idx = 1
		}
		if idx < len(args) {
			bindings, _ = args[idx].(map[string]interface{})
		}
		if bindings == nil {
			bindings = make(map[string]interface{})
		}

		switch triggerCondition {
		case "TRUE":
			if node != nil && TrueNode(node) {
				bodyFn(bindings, node)
			} else if node != nil {
				node.TrueRules = append(node.TrueRules,
					&RuleInvocation{Body: func(a ...interface{}) { bodyFn(bindings, node) }})
			}
		case "FALSE":
			if node != nil && FalseNode(node) {
				bodyFn(bindings, node)
			} else if node != nil {
				node.FalseRules = append(node.FalseRules,
					&RuleInvocation{Body: func(a ...interface{}) { bodyFn(bindings, node) }})
			}
		case "INTERN":
			bodyFn(bindings, nil)
		}
	}
	InsertRule(db, matcher, body)
}
