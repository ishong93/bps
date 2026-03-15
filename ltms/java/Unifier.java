package ltms;

import java.util.*;

public class Unifier {
    public static final Object FAIL = new Object() { public String toString() { return ":FAIL"; } };

    public static boolean isVariable(Object x) {
        return x instanceof String && ((String) x).startsWith("?");
    }

    @SuppressWarnings("unchecked")
    public static Object unify(Object pattern, Object datum) {
        return unify1(pattern, datum, new HashMap<>());
    }

    @SuppressWarnings("unchecked")
    static Object unify1(Object pattern, Object datum, Map<String, Object> bindings) {
        if (bindings == FAIL) return FAIL;
        if (pattern == datum) return bindings;
        if (isVariable(pattern)) {
            String var = (String) pattern;
            if (bindings.containsKey(var)) {
                return unify1(bindings.get(var), datum, bindings);
            }
            bindings.put(var, datum);
            return bindings;
        }
        if (isVariable(datum)) {
            String var = (String) datum;
            if (bindings.containsKey(var)) {
                return unify1(pattern, bindings.get(var), bindings);
            }
            bindings.put(var, pattern);
            return bindings;
        }
        if (pattern instanceof List && datum instanceof List) {
            List<Object> pList = (List<Object>) pattern;
            List<Object> dList = (List<Object>) datum;
            if (pList.size() != dList.size()) return FAIL;
            for (int i = 0; i < pList.size(); i++) {
                Object result = unify1(pList.get(i), dList.get(i), bindings);
                if (result == FAIL) return FAIL;
            }
            return bindings;
        }
        if (pattern != null && pattern.equals(datum)) return bindings;
        return FAIL;
    }

    @SuppressWarnings("unchecked")
    public static Object sublis(Object bindings, Object pattern) {
        if (bindings == FAIL || bindings == null) return pattern;
        Map<String, Object> map = (Map<String, Object>) bindings;
        return sublis1(map, pattern);
    }

    @SuppressWarnings("unchecked")
    static Object sublis1(Map<String, Object> bindings, Object pattern) {
        if (isVariable(pattern)) {
            String var = (String) pattern;
            if (bindings.containsKey(var)) {
                return sublis1(bindings, bindings.get(var));
            }
            return pattern;
        }
        if (pattern instanceof List) {
            List<Object> lst = (List<Object>) pattern;
            List<Object> result = new ArrayList<>();
            for (Object item : lst) {
                result.add(sublis1(bindings, item));
            }
            return result;
        }
        return pattern;
    }
}
