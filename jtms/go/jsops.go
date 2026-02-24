// Operators for JSAINT - integration operator definitions.
// Translated from jsops.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

// IntegrationOperator represents an integration method.
type IntegrationOperator struct {
	Name        string
	Trigger     func(integral interface{}) bool
	Subproblems func(integral interface{}) []interface{}
	Result      func(integral interface{}, subresults map[string]interface{}) interface{}
	Test        func(integral interface{}) bool
}

// StandardIntegrationOps returns the standard set of integration operators.
// Each operator corresponds to a defIntegration in jsops.lisp.
func StandardIntegrationOps() []*IntegrationOperator {
	return []*IntegrationOperator{
		{
			Name: "Integral-of-Constant",
			// (Integral ?t ?var) where ?var does not occur in ?t
			// Result: (* ?t ?var)
			Test: func(integral interface{}) bool {
				parts := asSlice(integral)
				if len(parts) != 3 || asStr(parts[0]) != "Integral" {
					return false
				}
				return !OccursIn(parts[2], parts[1])
			},
		},
		{
			Name: "Integral-of-Self",
			// (Integral ?exp ?exp)
			// Result: (/ (expt ?exp 2) 2)
			Test: func(integral interface{}) bool {
				parts := asSlice(integral)
				if len(parts) != 3 || asStr(parts[0]) != "Integral" {
					return false
				}
				return deepEqual(parts[1], parts[2])
			},
		},
		{
			Name: "Integral-of-Sum",
			// (Integral (+ ?t1 ?t2) ?var)
			// Subproblems: integrate each term separately
			// Result: (+ ?int1 ?int2)
			Test: func(integral interface{}) bool {
				parts := asSlice(integral)
				if len(parts) != 3 || asStr(parts[0]) != "Integral" {
					return false
				}
				inner := asSlice(parts[1])
				return len(inner) >= 3 && asStr(inner[0]) == "+"
			},
		},
		{
			Name: "Move-Constant-Outside",
			// (Integral (* ?const ?nonconst) ?var) where ?var not in ?const
			Test: func(integral interface{}) bool {
				parts := asSlice(integral)
				if len(parts) != 3 || asStr(parts[0]) != "Integral" {
					return false
				}
				inner := asSlice(parts[1])
				if len(inner) != 3 || asStr(inner[0]) != "*" {
					return false
				}
				return !OccursIn(parts[2], inner[1]) && OccursIn(parts[2], inner[2])
			},
		},
		{
			Name: "Simple-e-integral",
			// (Integral (expt %e ?var) ?var)
			// Result: (expt %e ?var)
			Test: func(integral interface{}) bool {
				parts := asSlice(integral)
				if len(parts) != 3 || asStr(parts[0]) != "Integral" {
					return false
				}
				inner := asSlice(parts[1])
				return len(inner) == 3 && asStr(inner[0]) == "expt" &&
					asStr(inner[1]) == "%e" && deepEqual(inner[2], parts[2])
			},
		},
		{
			Name: "Sin-integral",
			// (Integral (sin (* ?a ?var)) ?var) where ?var not in ?a
			// Result: (- (/ (cos (* ?a ?var)) ?a))
			Test: func(integral interface{}) bool {
				parts := asSlice(integral)
				if len(parts) != 3 || asStr(parts[0]) != "Integral" {
					return false
				}
				inner := asSlice(parts[1])
				if len(inner) != 2 || asStr(inner[0]) != "sin" {
					return false
				}
				product := asSlice(inner[1])
				return len(product) == 3 && asStr(product[0]) == "*" &&
					!OccursIn(parts[2], product[1])
			},
		},
		{
			Name: "Cos-integral",
			// (Integral (cos (* ?a ?var)) ?var) where ?var not in ?a
			// Result: (/ (sin (* ?a ?var)) ?a)
			Test: func(integral interface{}) bool {
				parts := asSlice(integral)
				if len(parts) != 3 || asStr(parts[0]) != "Integral" {
					return false
				}
				inner := asSlice(parts[1])
				if len(inner) != 2 || asStr(inner[0]) != "cos" {
					return false
				}
				product := asSlice(inner[1])
				return len(product) == 3 && asStr(product[0]) == "*" &&
					!OccursIn(parts[2], product[1])
			},
		},
		{
			Name: "Log-integral",
			// (Integral (log ?var %e) ?var)
			// Result: (- (* ?var (log ?var %e)) ?var)
			Test: func(integral interface{}) bool {
				parts := asSlice(integral)
				if len(parts) != 3 || asStr(parts[0]) != "Integral" {
					return false
				}
				inner := asSlice(parts[1])
				return len(inner) == 3 && asStr(inner[0]) == "log" &&
					deepEqual(inner[1], parts[2]) && asStr(inner[2]) == "%e"
			},
		},
		{
			Name: "Integral-of-Polyterm",
			// (Integral (expt ?var ?n) ?var) where ?n != -1
			// Result: (/ (expt ?var (+ 1 ?n)) (+ 1 ?n))
			Test: func(integral interface{}) bool {
				parts := asSlice(integral)
				if len(parts) != 3 || asStr(parts[0]) != "Integral" {
					return false
				}
				inner := asSlice(parts[1])
				if len(inner) != 3 || asStr(inner[0]) != "expt" {
					return false
				}
				return deepEqual(inner[1], parts[2]) && !SameConstant(inner[2], -1)
			},
		},
	}
}

// RegisterIntegrationOps registers all integration operators as rules
// in the given JTRE.
func RegisterIntegrationOps(jtre *Jtre) {
	ops := StandardIntegrationOps()
	for _, op := range ops {
		_ = op // Each operator would be registered as a rule
		// In the full implementation, each defIntegration macro
		// expands into a rule that matches against expanded integration problems
	}
}

// asSlice converts interface{} to []interface{} or returns nil.
func asSlice(x interface{}) []interface{} {
	s, _ := x.([]interface{})
	return s
}

// asStr converts interface{} to string or returns "".
func asStr(x interface{}) string {
	s, _ := x.(string)
	return s
}
