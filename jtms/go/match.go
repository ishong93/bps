// Pattern matcher for algebraic manipulation systems.
// Translated from match.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// This is a pattern matcher supporting element variables (? name restriction)
// and segment variables (?? name restriction).

package jtms

import (
	"fmt"
	"math"
)

// MatchFail is the sentinel value indicating a match failure.
const MatchFail = ":FAIL"

// Tolerance for floating-point comparison.
var MatchTolerance = 1.0e-6

// DictEntry represents a binding in the match dictionary.
// For element variables: {Name, Value, nil, false}
// For segment variables: {Name, Beg, End, true}
type DictEntry struct {
	Name      string
	Value     interface{} // For element vars
	Beg       []interface{} // For segment vars
	End       []interface{} // For segment vars
	IsSegment bool
}

// Dict is a match dictionary (list of bindings).
type Dict []*DictEntry

// Match attempts to match a pattern against a datum expression.
// Returns the updated dictionary or nil for failure.
// Corresponds to the Lisp function match.
func Match(pat interface{}, dat interface{}, dict Dict) (Dict, bool) {
	if dict == nil && pat == nil && dat == nil {
		return Dict{}, true
	}
	// Easy win: identical
	if pat == dat {
		if dict == nil {
			return Dict{}, true
		}
		return dict, true
	}
	// Element variable
	if IsElementVar(pat) {
		return matchElementVar(pat, dat, dict)
	}
	// Non-list pattern
	patSlice, patIsList := pat.([]interface{})
	if !patIsList {
		if MatchEqual(pat, dat) {
			if dict == nil {
				return Dict{}, true
			}
			return dict, true
		}
		return nil, false
	}
	// Segment variable at head
	if len(patSlice) > 0 && IsSegmentVar(patSlice[0]) {
		return matchSegmentVar(patSlice, dat, dict)
	}
	// Both must be lists
	datSlice, datIsList := dat.([]interface{})
	if !datIsList {
		return nil, false
	}
	if len(patSlice) == 0 && len(datSlice) == 0 {
		if dict == nil {
			return Dict{}, true
		}
		return dict, true
	}
	if len(patSlice) == 0 || len(datSlice) == 0 {
		return nil, false
	}
	// Recurse: match car, then cdr
	newDict, ok := Match(patSlice[0], datSlice[0], dict)
	if !ok {
		return nil, false
	}
	return Match(patSlice[1:], datSlice[1:], newDict)
}

// matchElementVar matches an element variable against a datum.
func matchElementVar(pat interface{}, dat interface{}, dict Dict) (Dict, bool) {
	entry := LookupVar(pat, dict)
	if entry != nil {
		if MatchEqual(entry.Value, dat) {
			return dict, true
		}
		return nil, false
	}
	pred := VarRestriction(pat)
	if pred == nil || pred(dat) {
		if dict == nil {
			dict = Dict{}
		}
		return append(Dict{{Name: VarName(pat), Value: dat}}, dict...), true
	}
	return nil, false
}

// matchSegmentVar matches a segment variable at the head of the pattern.
func matchSegmentVar(patSlice []interface{}, dat interface{}, dict Dict) (Dict, bool) {
	entry := LookupVar(patSlice[0], dict)
	datSlice, _ := dat.([]interface{})
	if entry != nil {
		// Check for match against known segment
		rest, ok := checkSegment(datSlice, entry.Beg, entry.End)
		if !ok {
			return nil, false
		}
		return Match(patSlice[1:], rest, dict)
	}
	return trySegmentBindings(patSlice[0], patSlice[1:], datSlice, dict)
}

// checkSegment verifies that dat starts with the segment [beg, end).
func checkSegment(dat []interface{}, beg []interface{}, end []interface{}) ([]interface{}, bool) {
	if len(beg) == len(end) {
		return dat, true
	}
	if len(dat) == 0 {
		return nil, false
	}
	if len(beg) == 0 {
		return nil, false
	}
	if MatchEqual(dat[0], beg[0]) {
		return checkSegment(dat[1:], beg[1:], end)
	}
	return nil, false
}

// trySegmentBindings tries different segment lengths for a segment variable.
func trySegmentBindings(varPat interface{}, restPat []interface{}, dat []interface{}, dict Dict) (Dict, bool) {
	name := VarName(varPat)
	pred := VarRestriction(varPat)

	// Try each possible segment length from 0 to len(dat)
	for i := 0; i <= len(dat); i++ {
		seg := dat[:i]
		rest := dat[i:]
		if pred != nil && !pred(seg) {
			continue
		}
		newDict := dict
		if newDict == nil {
			newDict = Dict{}
		}
		newDict = append(Dict{{Name: name, Beg: dat, End: rest, IsSegment: true}}, newDict...)
		result, ok := Match(restPat, rest, newDict)
		if ok {
			_ = seg // segment was valid
			return result, true
		}
	}
	return nil, false
}

// IsElementVar returns true if x is an element variable (? name ...).
func IsElementVar(x interface{}) bool {
	s, ok := x.([]interface{})
	if !ok || len(s) < 2 {
		return false
	}
	sym, ok := s[0].(string)
	return ok && sym == "?"
}

// IsSegmentVar returns true if x is a segment variable (?? name ...).
func IsSegmentVar(x interface{}) bool {
	s, ok := x.([]interface{})
	if !ok || len(s) < 2 {
		return false
	}
	sym, ok := s[0].(string)
	return ok && sym == "??"
}

// IsPatternVariable returns true if x is any pattern variable.
func IsPatternVariable(x interface{}) bool {
	return IsElementVar(x) || IsSegmentVar(x)
}

// VarName returns the name of a pattern variable.
func VarName(x interface{}) string {
	s := x.([]interface{})
	return fmt.Sprintf("%v", s[1])
}

// VarRestriction returns the restriction predicate of a pattern variable, or nil.
func VarRestriction(x interface{}) func(interface{}) bool {
	s := x.([]interface{})
	if len(s) >= 3 {
		if fn, ok := s[2].(func(interface{}) bool); ok {
			return fn
		}
	}
	return nil
}

// LookupVar looks up a variable in the dictionary.
func LookupVar(varPat interface{}, dict Dict) *DictEntry {
	name := VarName(varPat)
	for _, entry := range dict {
		if entry.Name == name {
			return entry
		}
	}
	return nil
}

// VarValue returns the value of a variable from the dictionary.
func VarValue(varPat interface{}, dict Dict) interface{} {
	entry := LookupVar(varPat, dict)
	if entry == nil {
		panic(fmt.Sprintf("Not bound variable: %v", varPat))
	}
	if entry.IsSegment {
		return segmentToList(entry.Beg, entry.End)
	}
	return entry.Value
}

// segmentToList extracts elements from beg up to (but not including) end.
func segmentToList(beg []interface{}, end []interface{}) []interface{} {
	count := len(beg) - len(end)
	if count < 0 {
		count = 0
	}
	result := make([]interface{}, count)
	copy(result, beg[:count])
	return result
}

// MatchEqual compares two values for equality with floating-point tolerance.
func MatchEqual(a, b interface{}) bool {
	af, aIsFloat := toFloat(a)
	bf, bIsFloat := toFloat(b)
	if aIsFloat && bIsFloat {
		return math.Abs(af-bf) < MatchTolerance
	}
	return deepEqual(a, b)
}

// toFloat attempts to convert a value to float64.
func toFloat(x interface{}) (float64, bool) {
	switch v := x.(type) {
	case float64:
		return v, true
	case float32:
		return float64(v), true
	case int:
		return float64(v), true
	case int64:
		return float64(v), true
	default:
		return 0, false
	}
}

// SubstituteIn performs substitutions in an expression using a dictionary.
// Corresponds to the Lisp function substitute-in.
func SubstituteIn(exp interface{}, dict Dict) interface{} {
	if exp == nil {
		return nil
	}
	if IsElementVar(exp) {
		return VarValue(exp, dict)
	}
	expSlice, ok := exp.([]interface{})
	if !ok {
		return exp
	}
	if len(expSlice) > 0 && IsSegmentVar(expSlice[0]) {
		head := VarValue(expSlice[0], dict)
		headSlice, ok := head.([]interface{})
		if !ok {
			headSlice = []interface{}{head}
		}
		rest := SubstituteIn(expSlice[1:], dict)
		restSlice, ok := rest.([]interface{})
		if !ok {
			restSlice = []interface{}{}
		}
		result := make([]interface{}, 0, len(headSlice)+len(restSlice))
		result = append(result, headSlice...)
		result = append(result, restSlice...)
		return result
	}
	result := make([]interface{}, len(expSlice))
	for i, elem := range expSlice {
		result[i] = SubstituteIn(elem, dict)
	}
	return result
}
