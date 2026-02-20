// -*- Mode: Java -*-

// Tiny Rule Engine, ATMS interface: Rules module
// Translated from arules.lisp

// Copyright (c) 1992, Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, the Xerox Corporation
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * ATRE rules module: rule registration, matching, execution, and scheduling.
 */
public class AtreRules {

    /**
     * A queued rule: body function, bindings, and node conditions.
     */
    public static class QueuedRule {
        public final Function<List<Object>, Object> body;
        public final List<Object> bindings;
        public final List<TmsNode> inNodes;
        public final List<TmsNode> impNodes;

        public QueuedRule(Function<List<Object>, Object> body,
                          List<Object> bindings,
                          List<TmsNode> inNodes,
                          List<TmsNode> impNodes) {
            this.body = body;
            this.bindings = bindings;
            this.inNodes = inNodes != null ? inNodes : new ArrayList<>();
            this.impNodes = impNodes != null ? impNodes : new ArrayList<>();
        }
    }

    /**
     * Result of a rule match attempt.
     */
    public static class MatchResult {
        public final boolean matched;
        public final List<Object> bindings;
        public final String condition;

        public MatchResult(boolean matched, List<Object> bindings, String condition) {
            this.matched = matched;
            this.bindings = bindings;
            this.condition = condition;
        }
    }

    /**
     * Insert a rule into a dbclass.
     */
    public static Rule insertRule(Dbclass dbclass,
                                   Function<Object, MatchResult> matcher,
                                   Function<List<Object>, Object> body,
                                   List<TmsNode> inNodes,
                                   List<TmsNode> impNodes) {
        Atre atre = dbclass.getAtre();
        atre.incrementRuleCounter();
        Rule rule = new Rule(atre.getRuleCounter(), atre, dbclass,
                             matcher, body, inNodes, impNodes);
        atre.getRules().add(0, rule);
        dbclass.getRules().add(0, rule);

        // Try rule on existing facts
        for (Datum candidate : dbclass.getFacts()) {
            tryRuleOn(rule, candidate);
        }
        return rule;
    }

    /**
     * Try all rules of a datum's dbclass on the datum.
     */
    public static void tryRules(Datum datum) {
        for (Rule rule : datum.getDbclass().getRules()) {
            tryRuleOn(rule, datum);
        }
    }

    /**
     * Try a specific rule on a datum.
     */
    @SuppressWarnings("unchecked")
    public static void tryRuleOn(Rule rule, Datum datum) {
        if (rule.getMatcher() == null) return;

        MatchResult result = ((Function<Object, MatchResult>) rule.getMatcher())
                .apply(datum.getLispForm());

        if (result != null && result.matched) {
            List<Object> bindings = result.bindings != null ?
                    new ArrayList<>(result.bindings) : new ArrayList<>();

            String condition = result.condition;

            if ("IN".equals(condition) || "IMPLIED-BY".equals(condition)) {
                bindings.add(0, datum.getTmsNode());
            }

            List<TmsNode> inNodes = new ArrayList<>();
            List<TmsNode> impNodes = new ArrayList<>();

            if ("IN".equals(condition)) {
                inNodes.add(datum.getTmsNode());
                if (rule.getInNodes() != null) inNodes.addAll(rule.getInNodes());
                if (rule.getImpNodes() != null) impNodes.addAll(rule.getImpNodes());
            } else if ("IMPLIED-BY".equals(condition)) {
                if (rule.getInNodes() != null) inNodes.addAll(rule.getInNodes());
                impNodes.add(datum.getTmsNode());
                if (rule.getImpNodes() != null) impNodes.addAll(rule.getImpNodes());
            } else { // INTERN
                if (rule.getInNodes() != null) inNodes.addAll(rule.getInNodes());
                if (rule.getImpNodes() != null) impNodes.addAll(rule.getImpNodes());
            }

            enqueue(new QueuedRule(
                    (Function<List<Object>, Object>) rule.getBody(),
                    bindings, inNodes, impNodes), datum.getAtre());
        }
    }

    /**
     * Run all queued rules.
     */
    public static int runRules(Atre atre) {
        // Move in-rules to main queue
        atre.getQueue().addAll(atre.getInRules());
        atre.getInRules().clear();

        int counter = 0;
        while (!atre.getQueue().isEmpty()) {
            QueuedRule form = dequeue(atre);
            if (form == null) break;
            executeRule(form, atre);
            counter++;
        }

        if (atre.isDebugging()) {
            System.out.printf("%n    %d rules run.", counter);
        }
        atre.addRulesRun(counter);
        return counter;
    }

    /**
     * Execute a queued rule after checking conditions.
     */
    public static void executeRule(QueuedRule queuedRule, Atre atre) {
        // Check in-triggers
        if (!inTriggersReady(queuedRule.inNodes, atre,
                atre.getAtms().getEmptyEnv())) {
            atre.getInRules().add(queuedRule);
            return;
        }

        // Check implied-by triggers
        if (!impliedByTriggersReady(queuedRule.impNodes, atre)) {
            atre.getImpRules().add(queuedRule);
            return;
        }

        // Execute the rule body
        if (queuedRule.body != null) {
            queuedRule.body.apply(queuedRule.bindings);
        }
    }

    /**
     * Check if in-trigger nodes have jointly non-empty labels.
     */
    public static boolean inTriggersReady(List<TmsNode> nodes, Atre atre, Env env) {
        if (env.isNogood()) return false;
        if (nodes == null || nodes.isEmpty()) return true;

        TmsNode first = nodes.get(0);
        List<TmsNode> rest = nodes.subList(1, nodes.size());

        for (Env newEnv : first.getLabel()) {
            Env u = atre.getAtms().unionEnv(newEnv, env);
            if (u != null && inTriggersReady(rest, atre, u)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Check if implied-by trigger nodes are all in the current focus.
     */
    public static boolean impliedByTriggersReady(List<TmsNode> nodes, Atre atre) {
        if (nodes == null || nodes.isEmpty()) return true;
        if (!atre.isFocusOkay()) return false;

        Env focus = atre.getFocus();
        for (TmsNode node : nodes) {
            if (!atre.getAtms().isInNode(node, focus)) {
                return false;
            }
        }
        return true;
    }

    /**
     * Check if there are rules waiting in the queue.
     */
    public static boolean rulesWaiting(Atre atre) {
        return !atre.getQueue().isEmpty();
    }

    /**
     * Add a rule to the queue.
     */
    public static void enqueue(QueuedRule rule, Atre atre) {
        atre.getQueue().add(0, rule);
    }

    /**
     * Remove and return the first rule from the queue.
     */
    public static QueuedRule dequeue(Atre atre) {
        if (atre.getQueue().isEmpty()) return null;
        return atre.getQueue().remove(0);
    }

    /**
     * Show rule statistics.
     */
    public static void showRules(Atre atre, PrintWriter stream) {
        int counter = 0;
        List<String> dist = new ArrayList<>();

        for (Dbclass dbclass : atre.getDbclasses()) {
            int inc = dbclass.getRules().size();
            if (inc > 0) {
                dist.add(String.format("%s: %d", dbclass.getName(), inc));
                counter += inc;
            }
        }

        int inCount = atre.getInRules().size();
        int impCount = atre.getImpRules().size();
        int queued = atre.getQueue().size();
        counter += inCount + impCount;

        stream.printf("%n %s has %d rules in all.", atre.getTitle(), counter);
        stream.printf("%n  %s queued.", queued > 0 ? String.valueOf(queued) : "None");

        if (inCount + impCount > 0) {
            stream.printf("  Pending: %s in, %s implied-by.",
                    inCount > 0 ? String.valueOf(inCount) : "No",
                    impCount > 0 ? String.valueOf(impCount) : "No");
        } else {
            stream.print("  None pending.");
        }

        if (!dist.isEmpty()) {
            stream.printf("%n Cached under dbclasses:");
            for (String entry : dist) {
                stream.printf("%n    %s", entry);
            }
        }
        stream.flush();
    }

    /**
     * Print all rules.
     */
    public static int printRules(Atre atre, PrintWriter stream) {
        int counter = 0;
        stream.printf("%nThe rules in %s are:", atre.getTitle());
        for (Rule rule : atre.getRules()) {
            counter++;
            printRule(rule, stream);
        }
        stream.flush();
        return counter;
    }

    /**
     * Print a single rule.
     */
    public static void printRule(Rule rule, PrintWriter stream) {
        stream.printf("%n %s: %s, %s", rule, rule.getMatcher(), rule.getBody());
    }

    /**
     * Get a rule by its counter.
     */
    public static Rule getRule(int num, Atre atre) {
        for (Rule rule : atre.getRules()) {
            if (rule.getCounter() == num) return rule;
        }
        return null;
    }
}
