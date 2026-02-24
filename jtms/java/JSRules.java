// Bookkeeping rules for JSAINT.
// AND/OR subgoal management.
// Translated from jsrules.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * JSRules registers the bookkeeping rules for JSAINT:
 * AND/OR subgoal expansion, solution detection, and failure propagation.
 */
public class JSRules {

    /** Register all JSAINT bookkeeping rules. */
    @SuppressWarnings("unchecked")
    public static void registerRules(Jtre jtre) {
        // Rule: AND-SUBGOALS expansion
        Dbclass andDbclass = JData.getDbclass(Arrays.asList("AND-SUBGOALS"), jtre);
        JRules.insertRule(andDbclass,
            p -> {
                List<Object> ps = asList(p);
                if (ps == null || ps.size() < 3 || !"AND-SUBGOALS".equals(ps.get(0)))
                    return new Rule.MatchResult(false, null, false);
                List<Object> bindings = new ArrayList<>();
                bindings.add(ps.get(1));
                bindings.add(ps.get(2));
                return new Rule.MatchResult(true, bindings, true);
            },
            args -> {
                if (args.size() < 3) return;
                Object parent = args.get(1);
                List<Object> children = asList(args.get(2));
                if (children == null) return;
                for (Object child : children) {
                    JData.jassert(Arrays.asList("PARENT-OF", child, parent, "AND"),
                        Arrays.asList(":DEF-OF-AND"), jtre);
                }
            });

        // Rule: OR-SUBGOALS expansion
        Dbclass orDbclass = JData.getDbclass(Arrays.asList("OR-SUBGOALS"), jtre);
        JRules.insertRule(orDbclass,
            p -> {
                List<Object> ps = asList(p);
                if (ps == null || ps.size() < 3 || !"OR-SUBGOALS".equals(ps.get(0)))
                    return new Rule.MatchResult(false, null, false);
                List<Object> bindings = new ArrayList<>();
                bindings.add(ps.get(1));
                bindings.add(ps.get(2));
                return new Rule.MatchResult(true, bindings, true);
            },
            args -> {
                if (args.size() < 3) return;
                Object parent = args.get(1);
                List<Object> children = asList(args.get(2));
                if (children == null) return;
                for (Object child : children) {
                    JData.jassert(Arrays.asList("PARENT-OF", child, parent, "OR"),
                        Arrays.asList(":DEF-OF-OR"), jtre);
                }
            });

        // Rule: PARENT-OF -> RELEVANT
        Dbclass parentDbclass = JData.getDbclass(Arrays.asList("PARENT-OF"), jtre);
        JRules.insertRule(parentDbclass,
            p -> {
                List<Object> ps = asList(p);
                if (ps == null || ps.size() < 4 || !"PARENT-OF".equals(ps.get(0)))
                    return new Rule.MatchResult(false, null, false);
                List<Object> bindings = new ArrayList<>();
                bindings.add(ps.get(1));
                return new Rule.MatchResult(true, bindings, true);
            },
            args -> {
                if (args.size() < 2) return;
                Object child = args.get(1);
                JData.jassert(Arrays.asList("RELEVANT", child),
                    Arrays.asList(":STILL-WORKING-ON"), jtre);
            });

        // Rule: SOLUTION-OF -> SOLVED
        Dbclass solnDbclass = JData.getDbclass(Arrays.asList("SOLUTION-OF"), jtre);
        JRules.insertRule(solnDbclass,
            p -> {
                List<Object> ps = asList(p);
                if (ps == null || ps.size() < 3 || !"SOLUTION-OF".equals(ps.get(0)))
                    return new Rule.MatchResult(false, null, false);
                List<Object> bindings = new ArrayList<>();
                bindings.add(ps.get(1));
                return new Rule.MatchResult(true, bindings, true);
            },
            args -> {
                if (args.size() < 2) return;
                JData.jassert(Arrays.asList("SOLVED", args.get(1)),
                    Arrays.asList(":FOUND-ANSWER"), jtre);
            });

        // Rule: SOLVED -> close problem
        Dbclass solvedDbclass = JData.getDbclass(Arrays.asList("SOLVED"), jtre);
        JRules.insertRule(solvedDbclass,
            p -> {
                List<Object> ps = asList(p);
                if (ps == null || ps.size() < 2 || !"SOLVED".equals(ps.get(0)))
                    return new Rule.MatchResult(false, null, false);
                List<Object> bindings = new ArrayList<>();
                bindings.add(ps.get(1));
                return new Rule.MatchResult(true, bindings, true);
            },
            args -> {
                if (args.size() < 2) return;
                JData.jretract(Arrays.asList("OPEN", args.get(1)),
                    "EXPAND-AGENDA-ITEM", true, jtre);
            });

        // Rule: FAILED -> close problem
        Dbclass failedDbclass = JData.getDbclass(Arrays.asList("FAILED"), jtre);
        JRules.insertRule(failedDbclass,
            p -> {
                List<Object> ps = asList(p);
                if (ps == null || ps.size() < 2 || !"FAILED".equals(ps.get(0)))
                    return new Rule.MatchResult(false, null, false);
                List<Object> bindings = new ArrayList<>();
                bindings.add(ps.get(1));
                return new Rule.MatchResult(true, bindings, true);
            },
            args -> {
                if (args.size() < 2) return;
                JData.jretract(Arrays.asList("OPEN", args.get(1)),
                    "EXPAND-AGENDA-ITEM", true, jtre);
            });
    }

    @SuppressWarnings("unchecked")
    private static List<Object> asList(Object x) {
        if (x instanceof List) return (List<Object>) x;
        return null;
    }
}
