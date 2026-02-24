// Pattern matcher for algebraic manipulation systems.
// Translated from match.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

/**
 * Match provides pattern matching with element variables (? name)
 * and segment variables (?? name).
 */
public class Match {

    public static final double TOLERANCE = 1.0e-6;

    /** Dictionary entry for a variable binding. */
    public static class DictEntry {
        public String name;
        public Object value;       // for element vars
        public List<Object> beg;   // for segment vars
        public List<Object> end;   // for segment vars
        public boolean isSegment;

        public DictEntry(String name, Object value) {
            this.name = name;
            this.value = value;
            this.isSegment = false;
        }
        public DictEntry(String name, List<Object> beg, List<Object> end) {
            this.name = name;
            this.beg = beg;
            this.end = end;
            this.isSegment = true;
        }
    }

    /** Result of a match attempt. */
    public static class MatchResult {
        public boolean success;
        public List<DictEntry> dict;
        public MatchResult(boolean success, List<DictEntry> dict) {
            this.success = success;
            this.dict = dict;
        }
    }

    /** Match a pattern against datum. */
    @SuppressWarnings("unchecked")
    public static MatchResult match(Object pat, Object dat, List<DictEntry> dict) {
        if (dict == null) dict = new ArrayList<>();
        if (pat == dat || JData.deepEqual(pat, dat)) return new MatchResult(true, dict);
        if (isElementVar(pat)) return matchElementVar(pat, dat, dict);
        if (!(pat instanceof List)) {
            return matchEqual(pat, dat) ? new MatchResult(true, dict) : new MatchResult(false, null);
        }
        List<Object> patList = (List<Object>) pat;
        if (!patList.isEmpty() && isSegmentVar(patList.get(0))) {
            return matchSegmentVar(patList, dat, dict);
        }
        if (!(dat instanceof List)) return new MatchResult(false, null);
        List<Object> datList = (List<Object>) dat;
        if (patList.isEmpty() && datList.isEmpty()) return new MatchResult(true, dict);
        if (patList.isEmpty() || datList.isEmpty()) return new MatchResult(false, null);
        MatchResult carResult = match(patList.get(0), datList.get(0), dict);
        if (!carResult.success) return carResult;
        return match(patList.subList(1, patList.size()),
                     datList.subList(1, datList.size()), carResult.dict);
    }

    @SuppressWarnings("unchecked")
    private static MatchResult matchElementVar(Object pat, Object dat, List<DictEntry> dict) {
        DictEntry entry = lookupVar(pat, dict);
        if (entry != null) {
            return matchEqual(entry.value, dat) ? new MatchResult(true, dict) : new MatchResult(false, null);
        }
        List<Object> patList = (List<Object>) pat;
        Predicate<Object> pred = patList.size() >= 3 && patList.get(2) instanceof Predicate
            ? (Predicate<Object>) patList.get(2) : null;
        if (pred == null || pred.test(dat)) {
            List<DictEntry> newDict = new ArrayList<>(dict);
            newDict.add(0, new DictEntry(varName(pat), dat));
            return new MatchResult(true, newDict);
        }
        return new MatchResult(false, null);
    }

    @SuppressWarnings("unchecked")
    private static MatchResult matchSegmentVar(List<Object> patList, Object dat, List<DictEntry> dict) {
        List<Object> datList = dat instanceof List ? (List<Object>) dat : new ArrayList<>();
        DictEntry entry = lookupVar(patList.get(0), dict);
        if (entry != null) {
            // Verify known segment
            int segLen = entry.beg.size() - entry.end.size();
            if (datList.size() < segLen) return new MatchResult(false, null);
            for (int i = 0; i < segLen; i++) {
                if (!matchEqual(datList.get(i), entry.beg.get(i)))
                    return new MatchResult(false, null);
            }
            return match(patList.subList(1, patList.size()),
                        datList.subList(segLen, datList.size()), dict);
        }
        return trySegmentBindings(patList.get(0), patList.subList(1, patList.size()), datList, dict);
    }

    private static MatchResult trySegmentBindings(Object var, List<Object> restPat,
                                                   List<Object> dat, List<DictEntry> dict) {
        String name = varName(var);
        for (int i = 0; i <= dat.size(); i++) {
            List<DictEntry> newDict = new ArrayList<>(dict);
            newDict.add(0, new DictEntry(name, new ArrayList<>(dat), dat.subList(i, dat.size())));
            MatchResult result = match(restPat, dat.subList(i, dat.size()), newDict);
            if (result.success) return result;
        }
        return new MatchResult(false, null);
    }

    /** Check if x is an element variable (? name ...). */
    @SuppressWarnings("unchecked")
    public static boolean isElementVar(Object x) {
        if (!(x instanceof List)) return false;
        List<Object> list = (List<Object>) x;
        return list.size() >= 2 && "?".equals(list.get(0));
    }

    /** Check if x is a segment variable (?? name ...). */
    @SuppressWarnings("unchecked")
    public static boolean isSegmentVar(Object x) {
        if (!(x instanceof List)) return false;
        List<Object> list = (List<Object>) x;
        return list.size() >= 2 && "??".equals(list.get(0));
    }

    /** Get the name of a pattern variable. */
    @SuppressWarnings("unchecked")
    public static String varName(Object x) {
        return String.valueOf(((List<Object>) x).get(1));
    }

    /** Look up a variable in the dictionary. */
    public static DictEntry lookupVar(Object var, List<DictEntry> dict) {
        String name = varName(var);
        for (DictEntry entry : dict) {
            if (entry.name.equals(name)) return entry;
        }
        return null;
    }

    /** Compare with floating-point tolerance. */
    public static boolean matchEqual(Object a, Object b) {
        if (a instanceof Number && b instanceof Number) {
            double da = ((Number) a).doubleValue();
            double db = ((Number) b).doubleValue();
            return Math.abs(da - db) < TOLERANCE;
        }
        return JData.deepEqual(a, b);
    }

    /** Perform substitutions in an expression. Corresponds to substitute-in. */
    @SuppressWarnings("unchecked")
    public static Object substituteIn(Object exp, List<DictEntry> dict) {
        if (exp == null) return null;
        if (isElementVar(exp)) {
            DictEntry entry = lookupVar(exp, dict);
            if (entry == null) throw new RuntimeException("Unbound variable: " + exp);
            return entry.value;
        }
        if (!(exp instanceof List)) return exp;
        List<Object> list = (List<Object>) exp;
        if (!list.isEmpty() && isSegmentVar(list.get(0))) {
            DictEntry entry = lookupVar(list.get(0), dict);
            if (entry == null) throw new RuntimeException("Unbound variable: " + list.get(0));
            List<Object> seg = entry.beg.subList(0, entry.beg.size() - entry.end.size());
            List<Object> rest = (List<Object>) substituteIn(list.subList(1, list.size()), dict);
            List<Object> result = new ArrayList<>(seg);
            if (rest != null) result.addAll(rest);
            return result;
        }
        List<Object> result = new ArrayList<>();
        for (Object elem : list) {
            result.add(substituteIn(elem, dict));
        }
        return result;
    }
}
