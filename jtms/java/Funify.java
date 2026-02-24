// Open-coding unification for rule compilation.
// Translated from funify.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.ArrayList;
import java.util.List;

/**
 * Funify provides pattern-matching utilities including quotize,
 * pattern free variable detection, and unification test generation.
 */
public class Funify {

    /** Check if x is a variable (string starting with ?). */
    public static boolean isVariable(Object x) {
        return x instanceof String && ((String) x).startsWith("?");
    }

    /**
     * Quotize transforms a pattern for reconstruction.
     * Variables are left as-is, atoms are quoted, lists become cons.
     * Corresponds to the Lisp function quotize.
     */
    @SuppressWarnings("unchecked")
    public static Object quotize(Object pattern) {
        if (pattern == null) return null;
        if (isVariable(pattern)) return pattern;
        if (pattern instanceof List) {
            List<Object> list = (List<Object>) pattern;
            if (list.isEmpty()) return null;
            if (":EVAL".equals(list.get(0)) || "EVAL".equals(list.get(0))) {
                return list.size() > 1 ? list.get(1) : null;
            }
            List<Object> result = new ArrayList<>();
            result.add("CONS");
            result.add(quotize(list.get(0)));
            result.add(quotize(list.subList(1, list.size())));
            return result;
        }
        List<Object> quoted = new ArrayList<>();
        quoted.add("QUOTE");
        quoted.add(pattern);
        return quoted;
    }

    /**
     * Find free variables in a pattern.
     * Corresponds to the Lisp function pattern-free-variables.
     */
    public static List<String> patternFreeVariables(Object pattern, List<String> boundVars) {
        return patternFreeVars1(pattern, new ArrayList<>(), boundVars);
    }

    @SuppressWarnings("unchecked")
    private static List<String> patternFreeVars1(Object pattern, List<String> vars, List<String> boundVars) {
        if (pattern == null) return vars;
        if (isVariable(pattern)) {
            String s = (String) pattern;
            if (vars.contains(s) || boundVars.contains(s)) return vars;
            List<String> newVars = new ArrayList<>(vars);
            newVars.add(s);
            return newVars;
        }
        if (!(pattern instanceof List)) return vars;
        List<Object> list = (List<Object>) pattern;
        for (Object elem : list) {
            vars = patternFreeVars1(elem, vars, boundVars);
        }
        return vars;
    }

    /**
     * Generate pairwise equality tests.
     * Corresponds to the Lisp function generate-pairwise-tests.
     */
    public static List<List<Object>> generatePairwiseTests(List<Object> tests) {
        List<List<Object>> result = new ArrayList<>();
        for (int i = 0; i < tests.size() - 1; i++) {
            List<Object> test = new ArrayList<>();
            test.add("EQUAL");
            test.add(tests.get(i));
            test.add(tests.get(i + 1));
            result.add(test);
        }
        return result;
    }
}
