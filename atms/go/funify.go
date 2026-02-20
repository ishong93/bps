// -*- Mode: Go; -*-

// Extra pattern-matching facilities for FTRE
// Open-coding unification for rule compilation
// Last edited: 1/29/93, KDF

// Copyright (c) 1988-1992, Kenneth D. Forbus and Johan de Kleer,
// All Rights Reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package atms

import "fmt"

// Symbol represents a Lisp symbol as a Go string constant.
// Symbols like QUOTE, CONS, EQUAL, NULL, CONSP, AND, CAR, CDR, NUMBERP
// are used in generated test expressions.
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

// Quotize transforms a pattern into an s-expression that, when evaluated,
// reconstructs the pattern. Variables are left as-is, non-list atoms
// are quoted, :EVAL forms are unwrapped, and lists become nested CONS forms.
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
		// (cadr pattern)
		return Car(cons.Cdr)
	}
	// (CONS (quotize (car pattern)) (quotize (cdr pattern)))
	return SExprList(SymCons, Quotize(cons.Car), Quotize(cons.Cdr))
}

// RLetClause represents a single variable binding in an rlet form.
type RLetClause struct {
	Var  string
	Expr interface{}
}

// RLetExpand expands an rlet form at runtime. It processes variable specs,
// calling Quotize on non-:EVAL values, and expands the body with the
// bound variables in scope.
// boundVars is the current set of bound variable names.
// varSpecs is a list of (variable, expression) pairs.
// bodyExpander is a function that expands the body given updated bound vars.
func RLetExpand(boundVars []string, varSpecs []RLetClause, bodyExpander func(boundVars []string) interface{}) interface{} {
	// Extend bound vars with the new variable names.
	newBound := make([]string, len(boundVars), len(boundVars)+len(varSpecs))
	copy(newBound, boundVars)
	for _, spec := range varSpecs {
		newBound = append(newBound, spec.Var)
	}

	// Process each let clause.
	processedSpecs := make([]RLetClause, len(varSpecs))
	for i, spec := range varSpecs {
		expr := spec.Expr
		// Check if expression is (:EVAL ...)
		if cons, ok := expr.(*Cons); ok {
			if sym, ok := cons.Car.(Symbol); ok && sym == SymEval {
				// Use the raw expression (cadr)
				expr = Car(cons.Cdr)
			} else {
				expr = Quotize(expr)
			}
		} else if expr != nil && !IsVariable(expr) {
			expr = Quotize(expr)
		}
		processedSpecs[i] = RLetClause{Var: spec.Var, Expr: expr}
	}

	// Build the let form as an s-expression.
	// (LET ((var1 expr1) (var2 expr2) ...) body...)
	body := bodyExpander(newBound)
	letBindings := make([]interface{}, len(processedSpecs))
	for i, spec := range processedSpecs {
		letBindings[i] = SExprList(spec.Var, spec.Expr)
	}
	return SExprList(Symbol("LET"), SliceToSExpr(letBindings), body)
}

// PatternFreeVariables returns the free variables in a pattern,
// given the current set of bound variables.
func PatternFreeVariables(pattern interface{}, boundVars []string) []string {
	return patternFreeVars1(pattern, nil, boundVars)
}

// patternFreeVars1 accumulates free variables found in pattern.
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
		// Atom (not a variable, not a list) => no free vars.
		return vars
	}
	vars = patternFreeVars1(cons.Car, vars, boundVars)
	return patternFreeVars1(cons.Cdr, vars, boundVars)
}

// MatchBodyResult holds the results of GenerateMatchBody.
type MatchBodyResult struct {
	Tests        []interface{}
	BindingSpecs []interface{}
}

// GenerateMatchBody generates the test body for pattern matching.
// pattern is the pattern to match against, vars is the list of pattern
// variables, and extraTest is an optional additional test expression.
// boundVars is needed for checking free variables in the extra test.
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
			// Test looks like (?x (nth p) (nth p) ...)
			rest := SExprToSlice(testCons.Cdr)
			pairwise := GeneratePairwiseTests(testCons.Cdr)
			for _, pt := range SExprToSlice(pairwise) {
				equalTests = append(equalTests, pt)
			}
			if extraTest != nil {
				lastElem := rest[len(rest)-1]
				varAlist = append(varAlist, Binding{Var: first.(string), Val: lastElem})
			}
			lastElem := rest[len(rest)-1]
			bindingSpecs = append(bindingSpecs, lastElem)
		} else {
			structureTests = append(structureTests, rawTest)
		}
	}

	// Substitute variable alist into extra test.
	extraTest = Sublis(varAlist, extraTest)

	// Check for free variables in extra test.
	if extraTest != nil {
		freeVars := PatternFreeVariables(extraTest, boundVars)
		if len(freeVars) > 0 {
			return nil, fmt.Errorf("rule test includes free variable: %v", freeVars)
		}
	}

	// Combine all tests.
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

// GeneratePairwiseTests generates EQUAL tests for consecutive pairs
// in the given list. E.g., (a b c) -> ((EQUAL a b) (EQUAL b c)).
func GeneratePairwiseTests(tests interface{}) interface{} {
	if tests == nil {
		return nil
	}
	cons, ok := tests.(*Cons)
	if !ok {
		return nil
	}
	if cons.Cdr == nil {
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
func GenerateUnifyTests(pattern interface{}, vars []string, tests interface{}, path interface{}) interface{} {
	if pattern == nil {
		// End of pattern: (null path)
		return &Cons{
			Car: SExprList(SymNull, path),
			Cdr: tests,
		}
	}

	if IsVariable(pattern) {
		s := pattern.(string)

		// Check if pattern is a member of vars.
		if stringMember(s, vars) {
			// Must see if the pattern has been bound elsewhere.
			previous := assocSExpr(s, tests)
			if previous != nil {
				// Add this position to test it:
				// push path onto (cdr previous)
				pushCdr(previous, path)
				return tests
			}
			// First occurrence: (pattern path)
			return &Cons{
				Car: SExprList(s, path),
				Cdr: tests,
			}
		}

		// Variable not in vars; it must be bound, so test against current value.
		// (equal pattern path)
		return &Cons{
			Car: SExprList(SymEqual, pattern, path),
			Cdr: tests,
		}
	}

	// Check for number.
	if isNumber(pattern) {
		// (and (numberp path) (= pattern path))
		return &Cons{
			Car: SExprList(SymAnd, SExprList(SymNumberp, path), SExprList(Symbol("="), pattern, path)),
			Cdr: tests,
		}
	}

	// Check for non-list atom.
	cons, ok := pattern.(*Cons)
	if !ok {
		// (equal (quote pattern) path)
		return &Cons{
			Car: SExprList(SymEqual, SExprList(SymQuote, pattern), path),
			Cdr: tests,
		}
	}

	// Recurse on a list.
	// First add (consp path) test, then recurse on car with (car path),
	// then recurse on cdr with (cdr path).
	innerTests := &Cons{
		Car: SExprList(SymConsp, path),
		Cdr: tests,
	}
	carPath := SExprList(SymCar, path)
	innerTests = toConsOrNil(GenerateUnifyTests(cons.Car, vars, innerTests, carPath))
	cdrPath := SExprList(SymCdr, path)
	return GenerateUnifyTests(cons.Cdr, vars, innerTests, cdrPath)
}

// --- Helper functions ---

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
			// Dotted pair: add the final atom.
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

// Sublis substitutes variable bindings throughout an s-expression.
// Each binding maps a variable name to a replacement value.
func Sublis(alist []Binding, expr interface{}) interface{} {
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
	newCar := Sublis(alist, cons.Car)
	newCdr := Sublis(alist, cons.Cdr)
	return &Cons{Car: newCar, Cdr: newCdr}
}

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
// Returns the matching cons cell or nil.
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

// pushCdr appends a value to the end of the cdr list of a Cons cell.
// This mutates the cons cell in place, mimicking Lisp's (push path (cdr previous)).
func pushCdr(cons *Cons, val interface{}) {
	// In the Lisp code, (push path (cdr previous)) prepends path to the cdr list.
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
