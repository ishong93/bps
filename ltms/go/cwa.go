// Closed World Assumptions.
// Converted from cwa.lisp.
package ltms

import "fmt"

// SetMembers returns the members and CWA of a set.
func SetMembers(setName interface{}, ltre *LTRE) (interface{}, interface{}, bool) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	pattern := []interface{}{setName, "MEMBERS", "?elements"}
	matches := Fetch(pattern, ltre)
	for _, mform := range matches {
		if IsTrue(mform, ltre) {
			if lst, ok := mform.([]interface{}); ok && len(lst) >= 3 {
				cwa := FindCWAForSet(mform, ltre)
				return lst[2], cwa, true
			}
		}
	}
	return nil, nil, false
}

// CloseSetIfNeeded closes a set if not already closed.
func CloseSetIfNeeded(setName interface{}, ltre *LTRE) (interface{}, interface{}, bool) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	members, cwa, found := SetMembers(setName, ltre)
	if found && cwa != nil {
		return members, cwa, false
	}
	return CloseSet(setName, ltre)
}

// CloseSet closes a set with CWA.
func CloseSet(setName interface{}, ltre *LTRE) (interface{}, interface{}, bool) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	nonContradictory := false
	defer func() {
		if !nonContradictory {
			RetractCWAs(setName, ltre)
		}
	}()

	knownMembers, knownNot := GetSetInformation(setName, ltre)
	cwaForm := MakeCWAForm(setName, knownMembers)
	membersForm := []interface{}{setName, "MEMBERS", knownMembers}

	RetractCWAs(setName, ltre)
	AssumeCWAIfNeeded(cwaForm, ltre)
	JustifyCWAIfNeeded(setName, knownMembers, knownNot, cwaForm, membersForm, ltre)
	nonContradictory = true
	return knownMembers, cwaForm, true
}

// FindCWAForSet finds the CWA assumption for a membership statement.
func FindCWAForSet(ms interface{}, ltre *LTRE) interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	msLst, ok := ms.([]interface{})
	if !ok || len(msLst) == 0 {
		return nil
	}
	setName := msLst[0]
	asns := AssumptionsOf(ms, ltre)
	for _, asn := range asns {
		v := ViewNode(asn)
		if CWAFormQ(v) {
			if lst, ok := v.([]interface{}); ok && len(lst) > 0 {
				if EqualForms(lst[0], setName) {
					return v
				}
			}
		}
	}
	return nil
}

// CWAFormQ checks if a form is a CWA form.
func CWAFormQ(form interface{}) bool {
	lst, ok := form.([]interface{})
	if !ok || len(lst) < 2 {
		return false
	}
	s, ok := lst[1].(string)
	return ok && s == "CWA"
}

// MakeCWAForm creates a CWA form.
func MakeCWAForm(setName, members interface{}) interface{} {
	return []interface{}{setName, "CWA", members}
}

// ParseCWAForm extracts set name and members from a CWA form.
func ParseCWAForm(cwaForm interface{}) (interface{}, interface{}) {
	if lst, ok := cwaForm.([]interface{}); ok && len(lst) >= 3 {
		return lst[0], lst[2]
	}
	return nil, nil
}

// GetSetInformation returns known members and known non-members.
func GetSetInformation(setName interface{}, ltre *LTRE) ([]interface{}, []interface{}) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	var knownIn, knownOut []interface{}
	pattern := []interface{}{setName, "HAS-MEMBER", "?member"}
	matches := Fetch(pattern, ltre)
	for _, possible := range matches {
		if IsTrue(possible, ltre) {
			if lst, ok := possible.([]interface{}); ok && len(lst) >= 3 {
				knownIn = append(knownIn, lst[2])
			}
		} else if IsFalse(possible, ltre) {
			if lst, ok := possible.([]interface{}); ok && len(lst) >= 3 {
				knownOut = append(knownOut, lst[2])
			}
		}
	}
	return knownIn, knownOut
}

// AssumeCWAIfNeeded assumes a CWA form.
func AssumeCWAIfNeeded(cwaForm interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if IsFalse(cwaForm, ltre) {
		node := GetTmsNode(cwaForm, ltre)
		PropagateUnknownness(node)
	}
	Assume(cwaForm, ":CWA", ltre)
}

// JustifyCWAIfNeeded creates the justification for a CWA.
func JustifyCWAIfNeeded(name interface{}, members, notMembers []interface{},
	cwaForm, membersForm interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	anteParts := []interface{}{":AND", []interface{}{"SET", name}}
	for _, el := range members {
		anteParts = append(anteParts, []interface{}{name, "HAS-MEMBER", el})
	}
	for _, el := range notMembers {
		anteParts = append(anteParts, []interface{}{":NOT", []interface{}{name, "HAS-MEMBER", el}})
	}
	anteParts = append(anteParts, cwaForm)
	Assert([]interface{}{"CWA-JUSTIFICATION", anteParts, membersForm},
		":SET-CWA-CLOSURE", ltre)
}

// FetchCWAFor returns the CWA form for a set name.
func FetchCWAFor(name interface{}, ltre *LTRE) interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	pattern := []interface{}{name, "CWA", "?x"}
	matches := Fetch(pattern, ltre)
	for _, cwa := range matches {
		if IsTrue(cwa, ltre) {
			return cwa
		}
	}
	return nil
}

// SetCWAHandler handles contradictions involving CWAs.
func SetCWAHandler(clauses []*Clause, l *LTMS, setName, cwa interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	cwaNode := GetTmsNode(cwa, ltre)
	for _, cl := range clauses {
		asns := AssumptionsOfClause(cl)
		hasCWA := false
		for _, asn := range asns {
			if asn == cwaNode {
				hasCWA = true
				break
			}
		}
		if hasCWA && CWAInvalid(cwa, ltre) {
			RetractCWA(cwa, ltre)
			return true
		}
	}
	return false
}

// RetractCWA retracts a CWA.
func RetractCWA(cwa interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	assumed := AlreadyAssumed(cwa, ltre)
	if assumed != nil {
		Retract(cwa, assumed, ltre, true)
	}
}

// CWAInvalid checks if a CWA is no longer valid.
func CWAInvalid(cwa interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	setName, presumedEls := ParseCWAForm(cwa)
	if els, ok := presumedEls.([]interface{}); ok {
		for _, el := range els {
			if !IsTrue([]interface{}{setName, "HAS-MEMBER", el}, ltre) {
				return true
			}
		}
	}
	pattern := []interface{}{setName, "HAS-MEMBER", "?el"}
	matches := Fetch(pattern, ltre)
	for _, hmForm := range matches {
		if IsTrue(hmForm, ltre) {
			if lst, ok := hmForm.([]interface{}); ok && len(lst) >= 3 {
				member := lst[2]
				if els, ok := presumedEls.([]interface{}); ok {
					found := false
					for _, el := range els {
						if EqualForms(member, el) {
							found = true
							break
						}
					}
					if !found {
						return true
					}
				}
			}
		}
	}
	return false
}

// RetractCWAs retracts all CWAs for a set.
func RetractCWAs(set interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	pattern := []interface{}{set, "CWA", "?members"}
	matches := Fetch(pattern, ltre)
	for _, cwa := range matches {
		if IsKnown(cwa, ltre) {
			assumed := AlreadyAssumed(cwa, ltre)
			if assumed != nil {
				Retract(cwa, assumed, ltre, true)
			}
		}
	}
}

// WithClosedSet runs body with a closed set.
func WithClosedSet(setName interface{}, ltre *LTRE, body func() interface{}) (bool, interface{}) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	members, cwa, _ := CloseSetIfNeeded(setName, ltre)
	type lostCWA struct{}
	caught := false

	WithContradictionHandler(ltre.LTMS,
		func(clauses []*Clause, l *LTMS) bool {
			if SetCWAHandler(clauses, l, setName, cwa, ltre) {
				caught = true
				return true
			}
			return false
		},
		func() {
			defer func() {
				if r := recover(); r != nil {
					if _, ok := r.(lostCWA); ok {
						caught = true
					} else {
						panic(r)
					}
				}
			}()
			body()
		})

	if caught {
		return false, nil
	}
	return true, members
}

// CWAInteractiveTest runs a simple CWA test.
func CWAInteractiveTest(debugging bool) {
	InLTRE(CreateLTRE("CWA Test", debugging))
	LoadSetRules(CurrentLTRE)
	data := []interface{}{
		[]interface{}{"SET", []interface{}{"Parts", "System"}},
		[]interface{}{[]interface{}{"Parts", "System"}, "HAS-MEMBER", "valve"},
		[]interface{}{[]interface{}{"Parts", "System"}, "HAS-MEMBER", "meter"},
		[]interface{}{[]interface{}{"Parts", "System"}, "HAS-MEMBER", "pump"},
	}
	for _, d := range data {
		Assume(d, ":INITIAL-OBSERVATIONS", CurrentLTRE)
	}
	RunRules(CurrentLTRE)
	ok, members := WithClosedSet([]interface{}{"Parts", "System"}, CurrentLTRE, func() interface{} {
		return Fetch([]interface{}{[]interface{}{"Parts", "System"}, "MEMBERS", "?els"}, CurrentLTRE)
	})
	if ok {
		fmt.Printf("\nParts are: %v", members)
	}
}

// CWAShakedown runs the CWA shakedown test.
func CWAShakedown() {
	InLTRE(CreateLTRE("CWA Test One", true))
	LoadSetRules(CurrentLTRE)
	UAssert([]interface{}{"SET", "foo"}, "USER")
	WithClosedSet("foo", CurrentLTRE, func() interface{} {
		UAssume([]interface{}{"foo", "HAS-MEMBER", "a"}, ":A-IN")
		return nil
	})
	WithClosedSet("foo", CurrentLTRE, func() interface{} {
		UAssume([]interface{}{"foo", "HAS-MEMBER", "b"}, ":B-IN")
		return nil
	})
	WithClosedSet("foo", CurrentLTRE, func() interface{} {
		RunRules(CurrentLTRE)
		return nil
	})
	if IsTrue([]interface{}{"foo", "MEMBERS", []interface{}{"b", "a"}}, CurrentLTRE) {
		fmt.Println("\n (A B) closure okay.")
	}
}
