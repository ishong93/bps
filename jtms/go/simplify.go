// Algebraic simplifier.
// Translated from simplify.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

import (
	"fmt"
	"math"
	"sort"
	"strings"
)

// SimplifyCache caches previously simplified expressions.
var SimplifyCache = make(map[string]interface{})

// AlgebraRules holds the simplification rules.
// Each rule is a slice: [pattern, predicate, skeleton].
var AlgebraRules [][]interface{}

func init() {
	AlgebraRules = buildAlgebraRules()
}

// Simplify simplifies an algebraic expression using cached results.
// Corresponds to the Lisp function simplify.
func Simplify(exp interface{}) interface{} {
	key := fmt.Sprintf("%v", exp)
	if cached, ok := SimplifyCache[key]; ok {
		return cached
	}
	result := simplifyIt(exp, AlgebraRules)
	SimplifyCache[key] = result
	return result
}

// ClearSimplifyCache clears the simplification cache.
func ClearSimplifyCache() {
	SimplifyCache = make(map[string]interface{})
}

// simplifyIt recursively simplifies an expression by applying rules.
func simplifyIt(exp interface{}, rules [][]interface{}) interface{} {
	// First simplify subexpressions
	var simplified interface{}
	if expSlice, ok := exp.([]interface{}); ok {
		result := make([]interface{}, len(expSlice))
		for i, e := range expSlice {
			result[i] = Simplify(e)
		}
		simplified = result
	} else {
		simplified = exp
	}
	result := tryMatcherRules(simplified, rules)
	if deepEqual(result, exp) {
		return result
	}
	return simplifyIt(result, rules)
}

// tryMatcherRules tries to apply simplification rules to an expression.
func tryMatcherRules(exp interface{}, rules [][]interface{}) interface{} {
	for _, rule := range rules {
		if len(rule) < 3 {
			continue
		}
		pattern := rule[0]
		predicate := rule[1]
		skeleton := rule[2]

		bindings, ok := Match(pattern, exp, nil)
		if !ok {
			continue
		}
		if checkPredicate(predicate, bindings) {
			return SubstituteIn(skeleton, bindings)
		}
	}
	return exp
}

// checkPredicate evaluates a predicate with bindings.
func checkPredicate(pred interface{}, bindings Dict) bool {
	if pred == nil {
		return true
	}
	// For Go, predicates are represented as functions
	if fn, ok := pred.(func(Dict) bool); ok {
		return fn(bindings)
	}
	return true
}

// AlgLess is a sort predicate for algebraic expressions.
// Corresponds to the Lisp function alg<.
func AlgLess(e1, e2 interface{}) bool {
	if MatchEqual(e1, e2) {
		return false
	}
	e1s, e1IsList := e1.([]interface{})
	e2s, e2IsList := e2.([]interface{})
	if e1IsList {
		if e2IsList {
			if len(e1s) == 0 && len(e2s) == 0 {
				return false
			}
			if len(e1s) == 0 {
				return true
			}
			if len(e2s) == 0 {
				return false
			}
			if MatchEqual(e1s[0], e2s[0]) {
				return AlgLess(e1s[1:], e2s[1:])
			}
			return AlgLess(e1s[0], e2s[0])
		}
		return false
	}
	if e2IsList {
		return true
	}
	e1Str, e1IsStr := e1.(string)
	e2Str, e2IsStr := e2.(string)
	if e1IsStr {
		if e2IsStr {
			return strings.Compare(e1Str, e2Str) < 0
		}
		return false
	}
	if e2IsStr {
		return true
	}
	e1f, e1IsNum := toFloat(e1)
	e2f, e2IsNum := toFloat(e2)
	if e1IsNum && e2IsNum {
		return e1f < e2f
	}
	return false
}

// SameConstant checks if an expression equals a numeric constant.
func SameConstant(exp interface{}, constant float64) bool {
	f, ok := toFloat(exp)
	if !ok {
		return false
	}
	return math.Abs(f-constant) < MatchTolerance
}

// IsZero checks if an expression is zero.
func IsZero(exp interface{}) bool { return SameConstant(exp, 0) }

// IsOne checks if an expression is one.
func IsOne(exp interface{}) bool { return SameConstant(exp, 1) }

// IsPlusMult checks if exp is + or *.
func IsPlusMult(exp interface{}) bool {
	s, ok := exp.(string)
	return ok && (s == "+" || s == "*")
}

// OccursIn checks if exp1 occurs anywhere in exp2.
func OccursIn(exp1, exp2 interface{}) bool {
	if deepEqual(exp1, exp2) {
		return true
	}
	if exp2 == nil {
		return false
	}
	if s, ok := exp2.([]interface{}); ok {
		for _, e := range s {
			if OccursIn(exp1, e) {
				return true
			}
		}
	}
	return false
}

// IsSorted checks if a list is sorted by the given predicate.
func IsSorted(list []interface{}, less func(a, b interface{}) bool) bool {
	for i := 1; i < len(list); i++ {
		if less(list[i], list[i-1]) {
			return false
		}
	}
	return true
}

// SortAlg sorts a list by the algebraic ordering.
func SortAlg(list []interface{}) []interface{} {
	result := make([]interface{}, len(list))
	copy(result, list)
	sort.SliceStable(result, func(i, j int) bool {
		return AlgLess(result[i], result[j])
	})
	return result
}

// buildAlgebraRules creates the standard algebra simplification rules.
func buildAlgebraRules() [][]interface{} {
	// Simplified representation of algebra rules.
	// In the full Lisp version, these are match patterns with predicates.
	// Here we represent them as Go functions for practicality.
	return [][]interface{}{
		// Rules are applied in order; each is [pattern, predicate, skeleton]
		// For a full port, each pattern/skeleton would use the match.go system.
		// This is a structural placeholder showing the rule architecture.
	}
}
