// JTRE Rule system module.
// Translated from jrules.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;

/**
 * JRules provides the rule system for JTRE:
 * inserting rules, trying rules against facts, and running the rule queue.
 */
public class JRules {

    /** Insert a new rule into a dbclass. Corresponds to insert-rule. */
    public static Rule insertRule(Dbclass dbclass, Rule.RuleMatcher matcher, Jtre.RuleBody body) {
        Jtre jtre = dbclass.getJtre();
        jtre.setRuleCounter(jtre.getRuleCounter() + 1);
        Rule rule = new Rule(jtre.getRuleCounter(), jtre, dbclass, matcher, body);
        dbclass.getRules().add(0, rule);
        for (Datum candidate : dbclass.getFacts()) {
            tryRuleOn(rule, candidate);
        }
        return rule;
    }

    /** Try all rules in a datum's dbclass against it. Corresponds to try-rules. */
    public static void tryRules(Datum datum) {
        for (Rule rule : datum.getDbclass().getRules()) {
            tryRuleOn(rule, datum);
        }
    }

    /** Try a single rule against a datum. Corresponds to try-rule-on. */
    public static void tryRuleOn(Rule rule, Datum datum) {
        Jtre jtre = datum.getDbclass().getJtre();
        Rule.MatchResult result = rule.getMatcher().match(datum.getLispForm());
        if (!result.ok) return;
        List<Object> bindings = result.bindings != null ?
            new ArrayList<>(result.bindings) : new ArrayList<>();
        if (result.needsNode) {
            bindings.add(0, datum.getTmsNode());
        }
        Jtre.enqueue(new Jtre.QueueEntry(rule.getBody(), bindings), jtre);
    }

    /** Run all queued rules. Corresponds to run-rules. */
    public static void runRules(Jtre jtre) {
        int counter = 0;
        while (!jtre.getQueue().isEmpty()) {
            Jtre.QueueEntry entry = jtre.getQueue().remove(0);
            entry.body.execute(entry.bindings);
            counter++;
        }
        jtre.debug("%n    %d rules run.", counter);
        jtre.setRulesRun(jtre.getRulesRun() + counter);
    }

    /** Check if rules are waiting. Corresponds to rules-waiting?. */
    public static boolean rulesWaiting(Jtre jtre) {
        return !jtre.getQueue().isEmpty();
    }

    /** Display all rules. Corresponds to show-rules. */
    public static void showRules(Jtre jtre, PrintStream out) {
        if (out == null) out = System.out;
        PrintStream finalOut = out;
        finalOut.printf("%nThere are %d rules in %s:", jtre.getRuleCounter(), jtre.getTitle());
        if (jtre.getQueue().isEmpty()) {
            finalOut.printf("%n None queued.");
        } else {
            finalOut.printf("%n %d queued.", jtre.getQueue().size());
        }
        JData.mapDbclass(dbclass -> {
            for (Rule rule : dbclass.getRules()) {
                finalOut.printf("%n %s", rule);
            }
        }, jtre);
    }

    /** Find a rule by ID. Corresponds to get-rule. */
    public static Rule getRule(int num, Jtre jtre) {
        final Rule[] result = {null};
        JData.mapDbclass(dbclass -> {
            for (Rule rule : dbclass.getRules()) {
                if (rule.getId() == num) { result[0] = rule; return; }
            }
        }, jtre);
        return result[0];
    }
}
