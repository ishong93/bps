// Extra pattern-matching facilities for LTRE.
// Open-coding of unification for rule compilation.
// Translated from funify.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package ltms

import (
	"fmt"
	"reflect"
)

// ---------------------------------------------------------------------------
// S-expression infrastructure
// ---------------------------------------------------------------------------

// Symbol represents a Lisp symbol as a Go string constant.
type Symbol string

const (
	SymQuote   Symbol = "QUOTE"
	SymCons    Symbol = "CONS"
	SymEqual   Symbol = "EQUAL"
	SymNull    Symbol = "NULL"
	SymConsp   Symbol = "CONSP"
	SymAnd     Symbol = "AND"
	SymCar     Symbol = "CAR"
	SymCdr     Symbol = "CDR"
	SymNumberp Symbol = "NUMBERP"
	SymEval    Symbol = "EVAL"
)

// FailSymbol is the sentinel value returned by Unify on failure.
const FailSymbol = ":FAIL"

// Cons represents a Lisp cons cell for building s-expressions.
type Cons struct {
	Car interface{}
	Cdr interface{}
}

// String returns a Lisp-style representation of the cons cell.
func (c *Cons) String() string {
	if c == nil {
		return "NIL"
	}
	s := "("
	s += fmt.Sprintf("%v", c.Car)
	rest := c.Cdr
	for rest != nil {
		rc, ok := rest.(*Cons)
		if !ok {
			s += " . " + fmt.Sprintf("%v", rest)
			break
		}
		s += " " + fmt.Sprintf("%v", rc.Car)
		rest = rc.Cdr
	}
	s += ")"
	return s
}

// Binding represents an association of a variable to a value,
// corresponding to a Lisp (var . value) cons cell in the bindings alist.
type Binding struct {
	Var string
	Val interface{}
}

// ---------------------------------------------------------------------------
// Variable detection
// ---------------------------------------------------------------------------

// IsVariable returns true if x is a string starting with "?".
// Corresponds to the Lisp function variable?.
func IsVariable(x interface{}) bool {
	s, ok := x.(string)
	return ok && len(s) > 0 && s[0] == '?'
}

// ---------------------------------------------------------------------------
// S-expression construction helpers
// ---------------------------------------------------------------------------

// SExprList builds a proper Lisp list (a chain of Cons cells terminated by nil)
// from the given elements.
func SExprList(elems ...interface{}) *Cons {
	if len(elems) == 0 {
		return nil
	}
	result := &Cons{Car: elems[len(elems)-1], Cdr: nil}
	for i := len(elems) - 2; i >= 0; i-- {
		result = &Cons{Car: elems[i], Cdr: result}
	}
	return result
}

// SliceToSExpr converts a Go slice to a proper Lisp list.
func SliceToSExpr(elems []interface{}) interface{} {
	if len(elems) == 0 {
		return nil
	}
	var result interface{}
	for i := len(elems) - 1; i >= 0; i-- {
		result = &Cons{Car: elems[i], Cdr: result}
	}
	return result
}

// SExprToSlice converts a proper Lisp list to a Go slice.
func SExprToSlice(sexpr interface{}) []interface{} {
	var result []interface{}
	for sexpr != nil {
		cons, ok := sexpr.(*Cons)
		if !ok {
			result = append(result, sexpr)
			break
		}
		result = append(result, cons.Car)
		sexpr = cons.Cdr
	}
	return result
}

// Car returns the car of a Cons cell, or nil if not a Cons.
func Car(x interface{}) interface{} {
	if cons, ok := x.(*Cons); ok {
		return cons.Car
	}
	return nil
}

// Cdr returns the cdr of a Cons cell, or nil if not a Cons.
func Cdr(x interface{}) interface{} {
	if cons, ok := x.(*Cons); ok {
		return cons.Cdr
	}
	return nil
}

// SExprEqual performs deep equality comparison on s-expressions.
func SExprEqual(a, b interface{}) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	aCons, aOk := a.(*Cons)
	bCons, bOk := b.(*Cons)
	if aOk && bOk {
		return SExprEqual(aCons.Car, bCons.Car) && SExprEqual(aCons.Cdr, bCons.Cdr)
	}
	if aOk != bOk {
		return false
	}
	return reflect.DeepEqual(a, b)
}

// ---------------------------------------------------------------------------
// Quotize
// ---------------------------------------------------------------------------

// Quotize transforms a pattern into an s-expression that, when evaluated,
// reconstructs the pattern. Variables are left as-is, non-list atoms
// are quoted, :EVAL forms are unwrapped, and lists become nested CONS forms.
//
// Corresponds to:
//
//	(defun quotize (pattern) ...)
func Quotize(pattern interface{}) interface{} {
	if pattern == nil {
		return nil
	}
	if IsVariable(pattern) {
		return pattern
	}
	cons, ok := pattern.(*Cons)
	if !ok {
		// Non-list atom: (QUOTE pattern)
		return SExprList(SymQuote, pattern)
	}
	// Check for :EVAL
	if sym, ok := cons.Car.(Symbol); ok && sym == SymEval {
		return Car(cons.Cdr)
	}
	if s, ok := cons.Car.(string); ok && (s == ":EVAL" || s == "EVAL") {
		return Car(cons.Cdr)
	}
	// (CONS (quotize (car pattern)) (quotize (cdr pattern)))
	return SExprList(SymCons, Quotize(cons.Car), Quotize(cons.Cdr))
}

// ---------------------------------------------------------------------------
// Pattern free variables
// ---------------------------------------------------------------------------

// PatternFreeVariables returns the free variables in a pattern,
// given the current set of bound variables.
//
// Corresponds to:
//
//	(defun pattern-free-variables (pattern) ...)
func PatternFreeVariables(pattern interface{}, boundVars []string) []string {
	return patternFreeVars1(pattern, nil, boundVars)
}

// patternFreeVars1 accumulates free variables found in pattern.
//
// Corresponds to:
//
//	(defun pattern-free-vars1 (pattern vars) ...)
func patternFreeVars1(pattern interface{}, vars []string, boundVars []string) []string {
	if pattern == nil {
		return vars
	}
	if IsVariable(pattern) {
		s := pattern.(string)
		if stringMember(s, vars) || stringMember(s, boundVars) {
			return vars
		}
		return append(vars, s)
	}
	cons, ok := pattern.(*Cons)
	if !ok {
		return vars
	}
	vars = patternFreeVars1(cons.Car, vars, boundVars)
	return patternFreeVars1(cons.Cdr, vars, boundVars)
}

// ---------------------------------------------------------------------------
// Runtime Unification
// ---------------------------------------------------------------------------

// Unify attempts to unify a pattern with a datum.
// Returns a map of variable bindings on success, or FailSymbol on failure.
//
// This is the runtime unifier used by the LTRE for pattern matching
// in fetch and fetch-global operations. It handles variables (strings
// starting with "?"), Cons-cell lists, and atomic values.
func Unify(pattern, datum interface{}) interface{} {
	bindings := make(map[string]interface{})
	if unify1(pattern, datum, bindings) {
		return bindings
	}
	return FailSymbol
}

// unify1 recursively unifies pattern with datum, extending bindings.
// Returns true on success, false on failure.
func unify1(pattern, datum interface{}, bindings map[string]interface{}) bool {
	// If both are nil, they match.
	if pattern == nil && datum == nil {
		return true
	}

	// If pattern is a variable, try to bind or check consistency.
	if IsVariable(pattern) {
		return unifyVariable(pattern.(string), datum, bindings)
	}

	// If datum is a variable, try to bind or check consistency.
	if IsVariable(datum) {
		return unifyVariable(datum.(string), pattern, bindings)
	}

	// If both are Cons cells, unify car and cdr recursively.
	pCons, pOk := pattern.(*Cons)
	dCons, dOk := datum.(*Cons)
	if pOk && dOk {
		if !unify1(pCons.Car, dCons.Car, bindings) {
			return false
		}
		return unify1(pCons.Cdr, dCons.Cdr, bindings)
	}

	// If both are slices, unify element-by-element.
	pSlice, pSliceOk := pattern.([]interface{})
	dSlice, dSliceOk := datum.([]interface{})
	if pSliceOk && dSliceOk {
		if len(pSlice) != len(dSlice) {
			return false
		}
		for i := range pSlice {
			if !unify1(pSlice[i], dSlice[i], bindings) {
				return false
			}
		}
		return true
	}

	// Otherwise, they must be equal atoms.
	return reflect.DeepEqual(pattern, datum)
}

// unifyVariable handles binding a variable during unification.
// If the variable is already bound, unify its value with the expression.
// Otherwise, bind the variable to the expression.
func unifyVariable(v string, exp interface{}, bindings map[string]interface{}) bool {
	if val, found := bindings[v]; found {
		return unify1(val, exp, bindings)
	}
	// Check for occurs check: ensure the variable does not appear in exp.
	if occursIn(v, exp, bindings) {
		return false
	}
	bindings[v] = exp
	return true
}

// occursIn checks whether variable v occurs anywhere in exp,
// following bindings for any variables encountered.
func occursIn(v string, exp interface{}, bindings map[string]interface{}) bool {
	if exp == nil {
		return false
	}
	if s, ok := exp.(string); ok && s == v {
		return true
	}
	if IsVariable(exp) {
		if val, found := bindings[exp.(string)]; found {
			return occursIn(v, val, bindings)
		}
		return false
	}
	if cons, ok := exp.(*Cons); ok {
		return occursIn(v, cons.Car, bindings) || occursIn(v, cons.Cdr, bindings)
	}
	if slice, ok := exp.([]interface{}); ok {
		for _, elem := range slice {
			if occursIn(v, elem, bindings) {
				return true
			}
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// Substitution
// ---------------------------------------------------------------------------

// Sublis substitutes variable bindings from a map into a pattern.
// Variables found in the bindings map are replaced by their values.
// Cons cells are traversed recursively, and slices are handled element-wise.
func Sublis(bindings map[string]interface{}, pattern interface{}) interface{} {
	if pattern == nil {
		return nil
	}
	if s, ok := pattern.(string); ok {
		if val, found := bindings[s]; found {
			return val
		}
		return pattern
	}
	if cons, ok := pattern.(*Cons); ok {
		newCar := Sublis(bindings, cons.Car)
		newCdr := Sublis(bindings, cons.Cdr)
		return &Cons{Car: newCar, Cdr: newCdr}
	}
	if slice, ok := pattern.([]interface{}); ok {
		result := make([]interface{}, len(slice))
		for i, elem := range slice {
			result[i] = Sublis(bindings, elem)
		}
		return result
	}
	return pattern
}

// SublisBindings substitutes variable bindings from a Binding slice
// into an s-expression. This version operates on the Binding struct
// representation used by the match-body generator.
func SublisBindings(alist []Binding, expr interface{}) interface{} {
	if expr == nil {
		return nil
	}
	if s, ok := expr.(string); ok {
		for _, b := range alist {
			if b.Var == s {
				return b.Val
			}
		}
		return expr
	}
	if sym, ok := expr.(Symbol); ok {
		for _, b := range alist {
			if b.Var == string(sym) {
				return b.Val
			}
		}
		return expr
	}
	cons, ok := expr.(*Cons)
	if !ok {
		return expr
	}
	newCar := SublisBindings(alist, cons.Car)
	newCdr := SublisBindings(alist, cons.Cdr)
	return &Cons{Car: newCar, Cdr: newCdr}
}

// ---------------------------------------------------------------------------
// Generate match body (compile-time unification support)
// ---------------------------------------------------------------------------

// MatchBodyResult holds the results of GenerateMatchBody.
type MatchBodyResult struct {
	Tests        []interface{}
	BindingSpecs []interface{}
}

// GenerateMatchBody generates the test body for pattern matching.
// pattern is the pattern to match against, vars is the list of pattern
// variables, and extraTest is an optional additional test expression.
// boundVars is needed for checking free variables in the extra test.
//
// Corresponds to:
//
//	(defun generate-match-body (pattern vars extra-test ...) ...)
func GenerateMatchBody(pattern interface{}, vars []string, extraTest interface{}, boundVars []string) (*MatchBodyResult, error) {
	var structureTests []interface{}
	var varAlist []Binding
	var equalTests []interface{}
	var bindingSpecs []interface{}

	tests := GenerateUnifyTests(pattern, vars, nil, Symbol("P"))
	for _, rawTest := range SExprToSlice(tests) {
		testCons, ok := rawTest.(*Cons)
		if !ok {
			continue
		}
		first := testCons.Car
		if IsVariable(first) {
			// Variable entry: (?x path1 path2 ...)
			rest := SExprToSlice(testCons.Cdr)
			pairwise := GeneratePairwiseTests(testCons.Cdr)
			for _, pt := range SExprToSlice(pairwise) {
				equalTests = append(equalTests, pt)
			}
			if extraTest != nil && len(rest) > 0 {
				lastElem := rest[len(rest)-1]
				varAlist = append(varAlist, Binding{Var: first.(string), Val: lastElem})
			}
			if len(rest) > 0 {
				lastElem := rest[len(rest)-1]
				bindingSpecs = append(bindingSpecs, lastElem)
			}
		} else {
			structureTests = append(structureTests, rawTest)
		}
	}

	// Substitute variable alist into extra test.
	extraTest = SublisBindings(varAlist, extraTest)

	// Check for free variables in extra test.
	if extraTest != nil {
		freeVars := PatternFreeVariables(extraTest, boundVars)
		if len(freeVars) > 0 {
			return nil, fmt.Errorf("rule test includes free variable: %v", freeVars)
		}
	}

	var allTests []interface{}
	allTests = append(allTests, structureTests...)
	allTests = append(allTests, equalTests...)
	if extraTest != nil {
		allTests = append(allTests, extraTest)
	}

	return &MatchBodyResult{
		Tests:        allTests,
		BindingSpecs: bindingSpecs,
	}, nil
}

// ---------------------------------------------------------------------------
// Generate pairwise tests
// ---------------------------------------------------------------------------

// GeneratePairwiseTests generates EQUAL tests for consecutive pairs
// in the given list. E.g., (a b c) -> ((EQUAL a b) (EQUAL b c)).
//
// Corresponds to:
//
//	(defun generate-pairwise-tests (tests) ...)
func GeneratePairwiseTests(tests interface{}) interface{} {
	if tests == nil {
		return nil
	}
	cons, ok := tests.(*Cons)
	if !ok || cons.Cdr == nil {
		return nil
	}
	rest, ok := cons.Cdr.(*Cons)
	if !ok {
		return nil
	}
	return &Cons{
		Car: SExprList(SymEqual, cons.Car, rest.Car),
		Cdr: GeneratePairwiseTests(cons.Cdr),
	}
}

// ---------------------------------------------------------------------------
// Generate unify tests
// ---------------------------------------------------------------------------

// GenerateUnifyTests generates a list of explicit tests for matching
// the given pattern. Assumes that the pattern to be tested will be
// in variable "P". Tests are returned in backward order.
//
// Example:
//
//	(generate-unify-tests '(foo ?x) nil nil 'P)
//	  => ((NULL (CDR (CDR P)))
//	      (EQUAL ?X (CAR (CDR P)))
//	      (CONSP (CDR P))
//	      (EQUAL (QUOTE FOO) (CAR P))
//	      (CONSP P))
//
// Corresponds to:
//
//	(defun generate-unify-tests (pattern vars tests path) ...)
func GenerateUnifyTests(pattern interface{}, vars []string, tests interface{}, path interface{}) interface{} {
	// (null pattern)
	if pattern == nil {
		return &Cons{
			Car: SExprList(SymNull, path),
			Cdr: tests,
		}
	}

	if IsVariable(pattern) {
		s := pattern.(string)

		// (member pattern vars)
		if stringMember(s, vars) {
			previous := assocSExpr(s, tests)
			if previous != nil {
				// push path onto (cdr previous)
				pushCdr(previous, path)
				return tests
			}
			return &Cons{
				Car: SExprList(s, path),
				Cdr: tests,
			}
		}

		// Variable not in vars; test against its current value.
		// (equal pattern path)
		return &Cons{
			Car: SExprList(SymEqual, pattern, path),
			Cdr: tests,
		}
	}

	// (numberp pattern)
	if isNumber(pattern) {
		return &Cons{
			Car: SExprList(SymAnd, SExprList(SymNumberp, path), SExprList(Symbol("="), pattern, path)),
			Cdr: tests,
		}
	}

	// Non-list atom
	cons, ok := pattern.(*Cons)
	if !ok {
		return &Cons{
			Car: SExprList(SymEqual, SExprList(SymQuote, pattern), path),
			Cdr: tests,
		}
	}

	// Recurse on a list: first (consp path), then car, then cdr.
	innerTests := &Cons{
		Car: SExprList(SymConsp, path),
		Cdr: tests,
	}
	carPath := SExprList(SymCar, path)
	innerTests = toConsOrNil(GenerateUnifyTests(cons.Car, vars, innerTests, carPath))
	cdrPath := SExprList(SymCdr, path)
	return GenerateUnifyTests(cons.Cdr, vars, innerTests, cdrPath)
}

// ---------------------------------------------------------------------------
// RLet support
// ---------------------------------------------------------------------------

// RLetClause represents a single variable binding in an rlet form.
type RLetClause struct {
	Var  string
	Expr interface{}
}

// RLetExpand expands an rlet form at runtime. It processes variable specs,
// calling Quotize on non-:EVAL values, and expands the body with the
// bound variables in scope.
func RLetExpand(boundVars []string, varSpecs []RLetClause, bodyExpander func(boundVars []string) interface{}) interface{} {
	newBound := make([]string, len(boundVars), len(boundVars)+len(varSpecs))
	copy(newBound, boundVars)
	for _, spec := range varSpecs {
		newBound = append(newBound, spec.Var)
	}

	processedSpecs := make([]RLetClause, len(varSpecs))
	for i, spec := range varSpecs {
		expr := spec.Expr
		if cons, ok := expr.(*Cons); ok {
			if sym, ok := cons.Car.(Symbol); ok && sym == SymEval {
				expr = Car(cons.Cdr)
			} else {
				expr = Quotize(expr)
			}
		} else if expr != nil && !IsVariable(expr) {
			expr = Quotize(expr)
		}
		processedSpecs[i] = RLetClause{Var: spec.Var, Expr: expr}
	}

	body := bodyExpander(newBound)
	letBindings := make([]interface{}, len(processedSpecs))
	for i, spec := range processedSpecs {
		letBindings[i] = SExprList(spec.Var, spec.Expr)
	}
	return SExprList(Symbol("LET"), SliceToSExpr(letBindings), body)
}

// ---------------------------------------------------------------------------
// Internal helpers
// ---------------------------------------------------------------------------

// stringMember returns true if s is in the given slice.
func stringMember(s string, slice []string) bool {
	for _, v := range slice {
		if v == s {
			return true
		}
	}
	return false
}

// assocSExpr looks up a key (string) in a Lisp alist (list of lists).
func assocSExpr(key string, alist interface{}) *Cons {
	for alist != nil {
		cons, ok := alist.(*Cons)
		if !ok {
			return nil
		}
		entry, ok := cons.Car.(*Cons)
		if ok {
			if s, ok := entry.Car.(string); ok && s == key {
				return entry
			}
		}
		alist = cons.Cdr
	}
	return nil
}

// pushCdr appends a value to the cdr list of a Cons cell by
// prepending it right after the car. This mutates the cons cell in place,
// mimicking Lisp's (push path (cdr previous)).
func pushCdr(cons *Cons, val interface{}) {
	cons.Cdr = &Cons{Car: val, Cdr: cons.Cdr}
}

// isNumber returns true if x is a numeric type.
func isNumber(x interface{}) bool {
	switch x.(type) {
	case int, int8, int16, int32, int64,
		uint, uint8, uint16, uint32, uint64,
		float32, float64:
		return true
	default:
		return false
	}
}

// toConsOrNil converts an interface{} that should be a *Cons or nil to *Cons.
func toConsOrNil(x interface{}) *Cons {
	if x == nil {
		return nil
	}
	if c, ok := x.(*Cons); ok {
		return c
	}
	return nil
}
