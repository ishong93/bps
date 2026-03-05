// Open-coding unification and pattern matching.
// Converted from funify.lisp.
package ltms

import "fmt"

// FailSymbol indicates a failed unification.
const FailSymbol = ":FAIL"

// Unify attempts to unify pattern with datum, returning bindings or FailSymbol.
func Unify(pattern, datum interface{}) interface{} {
	bindings := make(map[string]interface{})
	if unify1(pattern, datum, bindings) {
		return bindings
	}
	return FailSymbol
}

func unify1(pattern, datum interface{}, bindings map[string]interface{}) bool {
	if pattern == nil && datum == nil {
		return true
	}
	if IsVariable(pattern) {
		varName := pattern.(string)
		if val, bound := bindings[varName]; bound {
			return EqualForms(val, datum)
		}
		bindings[varName] = datum
		return true
	}
	if IsVariable(datum) {
		varName := datum.(string)
		if val, bound := bindings[varName]; bound {
			return EqualForms(val, pattern)
		}
		bindings[varName] = pattern
		return true
	}
	pList, pIsList := pattern.([]interface{})
	dList, dIsList := datum.([]interface{})
	if pIsList && dIsList {
		if len(pList) != len(dList) {
			return false
		}
		for i := range pList {
			if !unify1(pList[i], dList[i], bindings) {
				return false
			}
		}
		return true
	}
	if pIsList || dIsList {
		return false
	}
	return EqualForms(pattern, datum)
}

// Sublis substitutes bindings into a pattern.
func Sublis(bindings map[string]interface{}, pattern interface{}) interface{} {
	if pattern == nil {
		return nil
	}
	if IsVariable(pattern) {
		varName := pattern.(string)
		if val, ok := bindings[varName]; ok {
			return val
		}
		return pattern
	}
	if lst, ok := pattern.([]interface{}); ok {
		result := make([]interface{}, len(lst))
		for i, el := range lst {
			result[i] = Sublis(bindings, el)
		}
		return result
	}
	return pattern
}

// Quotize converts a pattern for use in assertions.
func Quotize(pattern interface{}) interface{} {
	if pattern == nil {
		return nil
	}
	if IsVariable(pattern) {
		return pattern
	}
	if lst, ok := pattern.([]interface{}); ok {
		if len(lst) > 0 {
			if s, ok := lst[0].(string); ok && s == ":EVAL" {
				return lst[1]
			}
		}
		result := make([]interface{}, len(lst))
		for i, el := range lst {
			result[i] = Quotize(el)
		}
		return result
	}
	return pattern
}

// PatternFreeVariables returns free variables in a pattern.
func PatternFreeVariables(pattern interface{}) []string {
	return patternFreeVars1(pattern, nil, nil)
}

func patternFreeVars1(pattern interface{}, vars, boundVars []string) []string {
	if pattern == nil {
		return vars
	}
	if IsVariable(pattern) {
		v := pattern.(string)
		for _, existing := range vars {
			if existing == v {
				return vars
			}
		}
		for _, bv := range boundVars {
			if bv == v {
				return vars
			}
		}
		return append(vars, v)
	}
	if lst, ok := pattern.([]interface{}); ok {
		for _, el := range lst {
			vars = patternFreeVars1(el, vars, boundVars)
		}
	}
	return vars
}

// MatchPattern matches a pattern against a datum, returning bindings.
func MatchPattern(pattern, datum interface{}) (bool, map[string]interface{}) {
	result := Unify(pattern, datum)
	if result == FailSymbol {
		return false, nil
	}
	return true, result.(map[string]interface{})
}

// GenerateMatchBody generates match tests for a pattern (runtime version).
func GenerateMatchBody(pattern interface{}, vars []string) func(interface{}) (bool, map[string]interface{}) {
	return func(datum interface{}) (bool, map[string]interface{}) {
		return MatchPattern(pattern, datum)
	}
}

// FormToString converts a form to its string representation.
func FormToString(form interface{}) string {
	switch v := form.(type) {
	case nil:
		return "nil"
	case string:
		return v
	case int:
		return fmt.Sprintf("%d", v)
	case float64:
		return fmt.Sprintf("%g", v)
	case []interface{}:
		parts := make([]string, len(v))
		for i, el := range v {
			parts[i] = FormToString(el)
		}
		return fmt.Sprintf("(%s)", joinStrings(parts, " "))
	default:
		return fmt.Sprintf("%v", v)
	}
}

func joinStrings(strs []string, sep string) string {
	if len(strs) == 0 {
		return ""
	}
	result := strs[0]
	for _, s := range strs[1:] {
		result += sep + s
	}
	return result
}
