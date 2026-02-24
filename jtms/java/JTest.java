// Simple shakedown procedure for JTRE.
// Translated from jtest.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * JTest provides a shakedown test for the JTRE system.
 */
public class JTest {

    /** Run shakedown tests. Corresponds to shakedown-jtre. */
    public static void shakedownJtre() {
        Jtre jtre = new Jtre("Test One", false);

        // Define INTERN rule: when (foo ?x) and (bar ?y) are both
        // interned with numeric values, assert (mumble ?x ?y)
        Dbclass fooDbclass = JData.getDbclass(Arrays.asList("foo"), jtre);
        JRules.insertRule(fooDbclass,
            p -> {
                if (!(p instanceof List)) return new Rule.MatchResult(false, null, false);
                List<?> ps = (List<?>) p;
                if (ps.size() != 2 || !"foo".equals(ps.get(0)))
                    return new Rule.MatchResult(false, null, false);
                if (!(ps.get(1) instanceof Number))
                    return new Rule.MatchResult(false, null, false);
                List<Object> bindings = new ArrayList<>();
                bindings.add(ps.get(1));
                return new Rule.MatchResult(true, bindings, false);
            },
            args -> {
                Object x = args.get(0);
                Dbclass barClass = JData.getDbclass(Arrays.asList("bar"), jtre);
                for (Datum d : barClass.getFacts()) {
                    if (!(d.getLispForm() instanceof List)) continue;
                    List<?> bs = (List<?>) d.getLispForm();
                    if (bs.size() != 2 || !"bar".equals(bs.get(0))) continue;
                    if (!(bs.get(1) instanceof Number)) continue;
                    JData.jassert(Arrays.asList("mumble", x, bs.get(1)),
                        Arrays.asList("Test-intern"), jtre);
                }
            });
        System.out.println(" :INTERN rule defined okay.");

        // Define IN rule: when (foo ?x) and (bar ?y) are both IN
        // with non-numeric values, assert (grumble ?x ?y)
        JRules.insertRule(fooDbclass,
            p -> {
                if (!(p instanceof List)) return new Rule.MatchResult(false, null, false);
                List<?> ps = (List<?>) p;
                if (ps.size() != 2 || !"foo".equals(ps.get(0)))
                    return new Rule.MatchResult(false, null, false);
                if (ps.get(1) instanceof Number)
                    return new Rule.MatchResult(false, null, false);
                List<Object> bindings = new ArrayList<>();
                bindings.add(ps.get(1));
                return new Rule.MatchResult(true, bindings, true);
            },
            args -> {
                JTMS.TmsNode triggerNode = (JTMS.TmsNode) args.get(0);
                Object x = args.get(1);
                if (!JTMS.inNode(triggerNode)) return;
                Dbclass barClass = JData.getDbclass(Arrays.asList("bar"), jtre);
                for (Datum d : barClass.getFacts()) {
                    if (!(d.getLispForm() instanceof List)) continue;
                    List<?> bs = (List<?>) d.getLispForm();
                    if (bs.size() != 2 || !"bar".equals(bs.get(0))) continue;
                    if (bs.get(1) instanceof Number) continue;
                    if (JTMS.inNode(d.getTmsNode())) {
                        JData.jassert(Arrays.asList("grumble", x, bs.get(1)),
                            Arrays.asList(":TEST-in"), jtre);
                    }
                }
            });
        System.out.println(" :IN rule defined okay.");

        // Test referent and fetch
        JData.referent(Arrays.asList("foo", 1), true, jtre);
        List<Object> results = JData.fetch(Arrays.asList("foo", 1), jtre);
        if (!results.isEmpty()) {
            System.out.println(" Referent worked okay.");
        } else {
            throw new RuntimeException("Referent failed.");
        }

        JData.referent(Arrays.asList("bar", 1), true, jtre);
        JRules.runRules(jtre);
        System.out.println(" No errors during attempted rule execution.");

        results = JData.fetch(Arrays.asList("mumble", 1, 1), jtre);
        if (!results.isEmpty()) {
            System.out.println(" :INTERN rule fired okay.");
        } else {
            throw new RuntimeException(":INTERN rule failed to fire.");
        }

        // Test IN rules with non-numeric values
        JData.referent(Arrays.asList("foo", "a"), true, jtre);
        JData.referent(Arrays.asList("bar", "a"), true, jtre);
        JRules.runRules(jtre);

        if (JData.isIn(Arrays.asList("grumble", "a", "a"), jtre)) {
            System.out.println(" Premature triggering of :IN rule.");
        }

        jtre.uassume(Arrays.asList("foo", "a"), "USER");
        jtre.uassume(Arrays.asList("bar", "a"), "USER");

        if (JData.isIn(Arrays.asList("grumble", "a", "a"), jtre)) {
            System.out.println(" :IN rule worked okay.");
        } else {
            System.out.println(" :IN rule failed to fire.");
        }

        jtre.uassume(Arrays.asList("foo", 1), "USER");
        jtre.uassume(Arrays.asList("bar", 1), "USER");
        if (!JData.isIn(Arrays.asList("mumble", 1, 1), jtre)) {
            System.out.println(" Reference or JTMS failure.");
        }

        System.out.println("\n JTRE shakedown complete.");
    }

    public static void main(String[] args) {
        shakedownJtre();
    }
}
