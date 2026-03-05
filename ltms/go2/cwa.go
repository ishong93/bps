// LTRE: An LTMS-based Tiny Rule Engine.
// Closed World Assumptions module.
// Translated from cwa.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.
//
// Translated from Common Lisp to Go.

package ltms

// SetMembersResult holds the result of SetMembers.
type SetMembersResult struct {
	Members interface{} // The members list (third element of the members form)
	CWA     interface{} // The CWA form, or nil if none found
}

// SetMembers returns the currently believed members of a set and its CWA form.
// Returns nil if no true members form exists.
// Corresponds to the Lisp function set-members.
func SetMembers(setName interface{}, ltre *LTRE) *SetMembersResult {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	// (fetch `(,set-name MEMBERS ?elements))
	mforms := Fetch([]interface{}{setName, "members", "?elements"}, ltre)
	var ms interface{}
	for _, mform := range mforms {
		if IsTrue(mform, ltre) {
			ms = mform
			break
		}
	}
	if ms == nil {
		return nil
	}
	// (third m-s) = members list
	msList := ms.([]interface{})
	members := msList[2]
	cwa := FindCWAForSet(ms, ltre)
	return &SetMembersResult{
		Members: members,
		CWA:     cwa,
	}
}

// CloseSetResult holds the result of CloseSetIfNeeded or CloseSet.
type CloseSetResult struct {
	Members interface{} // The known members of the set
	CWA     interface{} // The CWA form
	New     bool        // True if a new closure was performed
}

// CloseSetIfNeeded closes a set if it has not already been closed.
// Returns the construal, CWA form, and whether a new closure was made.
// Corresponds to the Lisp function close-set-if-needed.
func CloseSetIfNeeded(setName interface{}, ltre *LTRE) *CloseSetResult {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	result := SetMembers(setName, ltre)
	if result != nil && result.CWA != nil {
		return &CloseSetResult{
			Members: result.Members,
			CWA:     result.CWA,
			New:     false,
		}
	}
	return CloseSet(setName, ltre)
}

// CloseSet performs the closed world assumption on a set.
// It determines current membership, retracts any old CWAs, and
// installs a new CWA assumption and justification.
// Corresponds to the Lisp function close-set.
func CloseSet(setName interface{}, ltre *LTRE) *CloseSetResult {
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
	membersForm := []interface{}{setName, "members", knownMembers}

	RetractCWAs(setName, ltre)
	AssumeCWAIfNeeded(cwaForm, ltre)
	JustifyCWAIfNeeded(setName, knownMembers, knownNot, cwaForm, membersForm, ltre)

	nonContradictory = true
	return &CloseSetResult{
		Members: knownMembers,
		CWA:     cwaForm,
		New:     true,
	}
}

// FindCWAForSet finds the CWA assumption form for a given members form.
// Returns the CWA form if found, nil otherwise.
// Corresponds to the Lisp function find-cwa-for-set.
func FindCWAForSet(ms interface{}, ltre *LTRE) interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	msList := ms.([]interface{})
	msName := msList[0]
	assumptions := AssumptionsOf(ms, ltre)
	for _, asn := range assumptions {
		if IsCWAForm(asn) {
			asnList := asn.([]interface{})
			if deepEqual(asnList[0], msName) {
				return asn
			}
		}
	}
	return nil
}

// IsCWAForm returns true if form is a CWA form, i.e., (name CWA members).
// Corresponds to the Lisp function cwa-form?.
func IsCWAForm(form interface{}) bool {
	list, ok := form.([]interface{})
	if !ok || len(list) < 2 {
		return false
	}
	s, ok := list[1].(string)
	return ok && s == "CWA"
}

// MakeCWAForm constructs a CWA form: (set-name CWA members).
// Corresponds to the Lisp function make-cwa-form.
func MakeCWAForm(setName interface{}, members interface{}) interface{} {
	return []interface{}{setName, "CWA", members}
}

// ParseCWAForm extracts the set name and members from a CWA form.
// Corresponds to the Lisp function parse-cwa-form.
func ParseCWAForm(cwaForm interface{}) (setName interface{}, members interface{}) {
	list := cwaForm.([]interface{})
	return list[0], list[2]
}

// GetSetInformation returns the known members and known non-members of a set.
// Corresponds to the Lisp function get-set-information.
func GetSetInformation(setName interface{}, ltre *LTRE) (knownIn []interface{}, knownOut []interface{}) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	// (fetch `(,set-name HAS-MEMBER ?member))
	possibles := Fetch([]interface{}{setName, "has-member", "?member"}, ltre)
	for _, possible := range possibles {
		pList := possible.([]interface{})
		member := pList[2]
		if IsTrue(possible, ltre) {
			knownIn = append(knownIn, member)
		} else if IsFalse(possible, ltre) {
			knownOut = append(knownOut, member)
		}
	}
	return knownIn, knownOut
}

// AssumeCWAIfNeeded assumes a CWA form if needed.
// If the CWA form is currently false, propagates unknownness first.
// Corresponds to the Lisp function assume-cwa-if-needed.
func AssumeCWAIfNeeded(cwaForm interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	if IsFalse(cwaForm, ltre) {
		node := GetTmsNode(cwaForm, ltre)
		PropagateUnknownness(node)
	}
	Assume(cwaForm, "CWA", ltre)
}

// JustifyCWAIfNeeded asserts the CWA justification linking the set
// membership information to the members form.
// Corresponds to the Lisp function justify-cwa-if-needed.
func JustifyCWAIfNeeded(name interface{}, members []interface{}, notMembers []interface{}, cwaForm interface{}, membersForm interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	// Build the antecedent:
	// (:AND (SET ,name)
	//   ,@(mapcar (lambda (el) (,name HAS-MEMBER ,el)) members)
	//   ,@(mapcar (lambda (el) (:NOT (,name HAS-MEMBER ,el))) not-members)
	//   ,cwa-form)
	andParts := []interface{}{":AND", []interface{}{"set", name}}
	for _, el := range members {
		andParts = append(andParts, []interface{}{name, "has-member", el})
	}
	for _, el := range notMembers {
		andParts = append(andParts, []interface{}{":NOT", []interface{}{name, "has-member", el}})
	}
	andParts = append(andParts, cwaForm)

	// (assert! `(CWA-JUSTIFICATION ,ante ,members-form) :SET-CWA-CLOSURE)
	Assert(
		[]interface{}{"CWA-JUSTIFICATION", andParts, membersForm},
		"SET-CWA-CLOSURE",
		ltre,
	)
}

// FetchCWAFor finds the currently true CWA form for a given set name.
// Corresponds to the Lisp function fetch-cwa-for.
func FetchCWAFor(name interface{}, ltre *LTRE) interface{} {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	cwas := Fetch([]interface{}{name, "CWA", "?x"}, ltre)
	for _, cwa := range cwas {
		if IsTrue(cwa, ltre) {
			return cwa
		}
	}
	return nil
}

// SetCWAHandler is a contradiction handler for CWA-based sets.
// When a contradiction involves a CWA assumption that is now invalid,
// it retracts the CWA and signals this by returning true.
//
// The setName and cwa parameters identify which CWA to check.
// The tag parameter is used as a panic value for non-local exit
// (callers should use recover to catch it).
//
// Corresponds to the Lisp function set-cwa-handler.
func SetCWAHandler(clauses []*Clause, ltms *LTMS, setName interface{}, cwa interface{}, tag interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	cwaNode := GetTmsNode(cwa, ltre)
	for _, cl := range clauses {
		asns := AssumptionsOfClause(cl)
		cwaFound := false
		for _, a := range asns {
			if a == cwaNode {
				cwaFound = true
				break
			}
		}
		if cwaFound && CWAInvalid(cwa, ltre) {
			RetractCWA(cwa, ltre)
			panic(tag)
		}
	}
	return false
}

// RetractCWA retracts a CWA assumption.
// Corresponds to the Lisp function retract-CWA.
func RetractCWA(cwa interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	datum := Referent(innerForm(cwa), false, ltre)
	if datum != nil && datum.Assumption != nil {
		Retract(cwa, datum.Assumption, ltre, true)
	}
}

// CWAInvalid returns true if a CWA assumption is no longer valid.
// A CWA is invalid if any of the presumed members are no longer true,
// or if there is a new member not in the presumed list.
// Corresponds to the Lisp function CWA-invalid?.
func CWAInvalid(cwa interface{}, ltre *LTRE) bool {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	setName, presumedEls := ParseCWAForm(cwa)

	// Check that every presumed element is still true as a member
	if elList, ok := presumedEls.([]interface{}); ok {
		for _, el := range elList {
			if !IsTrue([]interface{}{setName, "has-member", el}, ltre) {
				return true
			}
		}
	}

	// Check for new members not in the presumed list
	hmForms := Fetch([]interface{}{setName, "has-member", "?el"}, ltre)
	for _, hmForm := range hmForms {
		if IsTrue(hmForm, ltre) {
			hmList := hmForm.([]interface{})
			member := hmList[2]
			if !isMemberOf(member, presumedEls) {
				return true
			}
		}
	}
	return false
}

// RetractCWAs retracts all known CWA assumptions for a set.
// Corresponds to the Lisp function retract-CWAs.
func RetractCWAs(setName interface{}, ltre *LTRE) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	cwas := Fetch([]interface{}{setName, "CWA", "?members"}, ltre)
	for _, cwa := range cwas {
		if IsKnown(cwa, ltre) {
			datum := Referent(innerForm(cwa), false, ltre)
			if datum != nil && datum.Assumption != nil {
				Retract(cwa, datum.Assumption, ltre, true)
			}
		}
	}
}

// WithClosedSet executes body with the set closed under the CWA.
// If a contradiction invalidates the CWA, the CWA is retracted and
// the function returns false. Otherwise it returns true.
//
// This corresponds to the Lisp macro that uses catch/throw for
// non-local exit when the CWA becomes invalid.
func WithClosedSet(setName interface{}, ltre *LTRE, body func(members interface{}, cwaForm interface{})) (ok bool) {
	if ltre == nil {
		ltre = CurrentLTRE
	}
	result := CloseSetIfNeeded(setName, ltre)
	if result == nil {
		return false
	}

	tag := &struct{ name interface{} }{name: setName} // unique tag for panic/recover

	ok = true
	func() {
		defer func() {
			if r := recover(); r != nil {
				if r == tag {
					ok = false
				} else {
					panic(r) // re-panic for unrelated panics
				}
			}
		}()

		WithContradictionHandler(ltre.LTMS, func(clauses []*Clause, ltms *LTMS) bool {
			return SetCWAHandler(clauses, ltms, setName, result.CWA, tag, ltre)
		}, func() {
			body(result.Members, result.CWA)
		})
	}()

	return ok
}
