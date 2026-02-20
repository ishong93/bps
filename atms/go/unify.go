// -*- Mode: Go; -*-

// Variables and unification
// Last edited 1/29/93, by KDF

// Copyright (c) 1988-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package atms

import "reflect"

// Binding represents an association of a variable to a value,
// corresponding to a Lisp (var . value) cons cell in the bindings alist.
type Binding struct {
	Var string
	Val interface{}
}

// Cons represents a Lisp cons cell for building s-expressions.
type Cons struct {
	Car interface{}
	Cdr interface{}
}

// IsVariable returns true if x is a string starting with "?".
func IsVariable(x interface{}) bool {
	s, ok := x.(string)
	return ok && len(s) > 0 && s[0] == '?'
}

// Unify attempts to unify two s-expressions a and b given existing bindings.
// Returns the extended bindings and true on success, or nil and false on failure.
func Unify(a, b interface{}, bindings []Binding) ([]Binding, bool) {
	if SExprEqual(a, b) {
		return bindings, true
	}
	if IsVariable(a) {
		return unifyVariable(a.(string), b, bindings)
	}
	if IsVariable(b) {
		return unifyVariable(b.(string), a, bindings)
	}
	aCons, aOk := a.(*Cons)
	bCons, bOk := b.(*Cons)
	if !aOk || !bOk {
		return nil, false
	}
	bindings, ok := Unify(aCons.Car, bCons.Car, bindings)
	if !ok {
		return nil, false
	}
	return Unify(aCons.Cdr, bCons.Cdr, bindings)
}

// unifyVariable attempts to unify a variable with an expression.
// Must distinguish no value from a value of nil.
func unifyVariable(v string, exp interface{}, bindings []Binding) ([]Binding, bool) {
	val, found := assoc(v, bindings)
	if found {
		return Unify(val, exp, bindings)
	}
	// If safe, bind v to exp.
	if FreeIn(v, exp, bindings) {
		return append(bindings, Binding{Var: v, Val: exp}), true
	}
	return nil, false
}

// FreeIn returns true if var does not occur in exp, assuming the given bindings.
func FreeIn(v string, exp interface{}, bindings []Binding) bool {
	if exp == nil {
		return true
	}
	if SExprEqual(v, exp) {
		return false
	}
	if IsVariable(exp) {
		val, found := assoc(exp.(string), bindings)
		if found {
			return FreeIn(v, val, bindings)
		}
		return true
	}
	cons, ok := exp.(*Cons)
	if !ok {
		return true
	}
	if FreeIn(v, cons.Car, bindings) {
		return FreeIn(v, cons.Cdr, bindings)
	}
	return false
}

// assoc looks up a variable in the bindings list, returning its value and
// whether it was found.
func assoc(v string, bindings []Binding) (interface{}, bool) {
	for _, b := range bindings {
		if b.Var == v {
			return b.Val, true
		}
	}
	return nil, false
}

// SExprEqual performs deep equality comparison on s-expressions,
// handling Cons cells, slices, and primitive types.
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
