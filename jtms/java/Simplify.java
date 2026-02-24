// Algebraic simplifier.
// Translated from simplify.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.*;

/**
 * Simplify provides algebraic simplification using rule-based rewriting.
 */
public class Simplify {

    private static Map<String, Object> cache = new HashMap<>();

    /** Simplify an expression with caching. Corresponds to simplify. */
    public static Object simplify(Object exp) {
        String key = String.valueOf(exp);
        if (cache.containsKey(key)) return cache.get(key);
        Object result = simplifyIt(exp);
        cache.put(key, result);
        return result;
    }

    /** Clear the simplification cache. */
    public static void clearCache() {
        cache.clear();
    }

    @SuppressWarnings("unchecked")
    private static Object simplifyIt(Object exp) {
        Object simplified;
        if (exp instanceof List) {
            List<Object> list = (List<Object>) exp;
            List<Object> result = new ArrayList<>();
            for (Object e : list) result.add(simplify(e));
            simplified = result;
        } else {
            simplified = exp;
        }
        // In a full implementation, algebra rules would be applied here
        return simplified;
    }

    /** Check if exp1 occurs in exp2. Corresponds to occurs-in?. */
    @SuppressWarnings("unchecked")
    public static boolean occursIn(Object exp1, Object exp2) {
        if (JData.deepEqual(exp1, exp2)) return true;
        if (exp2 == null) return false;
        if (exp2 instanceof List) {
            for (Object e : (List<Object>) exp2) {
                if (occursIn(exp1, e)) return true;
            }
        }
        return false;
    }

    /** Check if expression equals a numeric constant. Corresponds to same-constant?. */
    public static boolean sameConstant(Object exp, double constant) {
        if (!(exp instanceof Number)) return false;
        double val = ((Number) exp).doubleValue();
        return Math.abs(val - constant) < Match.TOLERANCE;
    }

    /** Check if expression is zero. */
    public static boolean isZero(Object exp) { return sameConstant(exp, 0); }

    /** Check if expression is one. */
    public static boolean isOne(Object exp) { return sameConstant(exp, 1); }

    /** Algebraic ordering predicate. Corresponds to alg<. */
    @SuppressWarnings("unchecked")
    public static boolean algLess(Object e1, Object e2) {
        if (Match.matchEqual(e1, e2)) return false;
        boolean e1IsList = e1 instanceof List;
        boolean e2IsList = e2 instanceof List;
        if (e1IsList) {
            if (e2IsList) {
                List<Object> l1 = (List<Object>) e1;
                List<Object> l2 = (List<Object>) e2;
                if (l1.isEmpty() && l2.isEmpty()) return false;
                if (l1.isEmpty()) return true;
                if (l2.isEmpty()) return false;
                if (Match.matchEqual(l1.get(0), l2.get(0))) {
                    return algLess(l1.subList(1, l1.size()), l2.subList(1, l2.size()));
                }
                return algLess(l1.get(0), l2.get(0));
            }
            return false;
        }
        if (e2IsList) return true;
        boolean e1IsStr = e1 instanceof String;
        boolean e2IsStr = e2 instanceof String;
        if (e1IsStr) {
            return e2IsStr && ((String) e1).compareTo((String) e2) < 0;
        }
        if (e2IsStr) return true;
        if (e1 instanceof Number && e2 instanceof Number) {
            return ((Number) e1).doubleValue() < ((Number) e2).doubleValue();
        }
        return false;
    }
}
