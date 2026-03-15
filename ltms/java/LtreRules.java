package ltms;

import java.util.*;
import java.util.function.*;

public class LtreRules {

    // Rule represents a pattern-matching rule in LTRE
    public static class Rule {
        public int counter;
        public LTRE ltre;
        public Dbclass dbclass;
        public Function<Object, Object[]> matcher; // returns [bindings, needsNode] or null
        public BiConsumer<Map<String, Object>, TmsNode> body;

        public Rule(int counter, Dbclass dbclass, Function<Object, Object[]> matcher, BiConsumer<Map<String, Object>, TmsNode> body) {
            this.counter = counter;
            this.dbclass = dbclass;
            this.ltre = dbclass.ltre;
            this.matcher = matcher;
            this.body = body;
        }

        @Override public String toString() { return "<Rule " + counter + ">"; }
    }

    // insertRule installs a rule and tries it on existing data
    public static void insertRule(Dbclass dbclass, Function<Object, Object[]> matcher, BiConsumer<Map<String, Object>, TmsNode> body) {
        LTRE ltre = dbclass.ltre;
        ltre.ruleCounter++;
        Rule rule = new Rule(ltre.ruleCounter, dbclass, matcher, body);
        dbclass.rules.add(0, rule);
        for (Datum candidate : new ArrayList<>(dbclass.facts)) {
            tryRuleOn(rule, candidate);
        }
    }

    // tryRules tries all rules in the datum's dbclass
    public static void tryRules(Datum datum) {
        for (Rule rule : new ArrayList<>(datum.dbclass.rules)) {
            tryRuleOn(rule, datum);
        }
    }

    @SuppressWarnings("unchecked")
    static void tryRuleOn(Rule rule, Datum datum) {
        Object[] result = rule.matcher.apply(datum.lispForm);
        if (result != null) {
            Map<String, Object> bindings = (Map<String, Object>) result[0];
            boolean needsNode = result.length > 1 && Boolean.TRUE.equals(result[1]);
            if (needsNode) {
                // Enqueue with the datum's TMS node
                enqueueRule(rule.body, bindings, datum.tmsNode, datum.ltre);
            } else {
                // Enqueue without node (INTERN condition)
                enqueueRule(rule.body, bindings, null, datum.ltre);
            }
        }
    }

    static void enqueueRule(BiConsumer<Map<String, Object>, TmsNode> body, Map<String, Object> bindings, TmsNode node, LTRE ltre) {
        ltre.queue.add(0, new Object[]{body, bindings, node});
    }

    public static int runRules(LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        int counter = 0;
        while (!ltre.queue.isEmpty()) {
            Object[] form = (Object[]) ltre.queue.remove(ltre.queue.size() - 1);
            @SuppressWarnings("unchecked")
            BiConsumer<Map<String, Object>, TmsNode> body = (BiConsumer<Map<String, Object>, TmsNode>) form[0];
            @SuppressWarnings("unchecked")
            Map<String, Object> bindings = (Map<String, Object>) form[1];
            TmsNode node = (TmsNode) form[2];
            body.accept(bindings, node);
            counter++;
        }
        if (ltre.debugging) System.out.printf("\n    %d rules run.", counter);
        ltre.rulesRun += counter;
        return counter;
    }

    public static void showRules(LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        int count = 0;
        System.out.printf("\nThe rules in %s are:", ltre.title);
        for (Dbclass db : ltre.dbclassTable.values()) {
            for (Rule rule : db.rules) {
                count++;
                System.out.printf("\n %s", rule);
            }
        }
        System.out.printf("\n%d rules total.", count);
    }

    // makeRule is a convenience for creating rules with pattern matching
    // condition is "TRUE", "FALSE", or "INTERN"
    @SuppressWarnings("unchecked")
    public static void makeRule(Object pattern, String condition, BiConsumer<Map<String, Object>, TmsNode> body) {
        LTRE ltre = LTRE.currentLTRE;
        String dbName = LTRE.dbclassName(pattern);
        Dbclass dbclass = LTRE.getDbclass(pattern, ltre);

        List<String> vars = new ArrayList<>();
        collectVariables(pattern, vars);

        Function<Object, Object[]> matcher = (Object datumForm) -> {
            Object bindings = Unifier.unify(pattern, datumForm);
            if (bindings == Unifier.FAIL) return null;
            Map<String, Object> map = (Map<String, Object>) bindings;
            boolean needsNode = !condition.equals("INTERN");
            return new Object[]{map, needsNode};
        };

        BiConsumer<Map<String, Object>, TmsNode> wrappedBody;
        if (condition.equals("INTERN")) {
            wrappedBody = body;
        } else {
            wrappedBody = (bindings, triggerNode) -> {
                if (triggerNode == null) return;
                boolean ok;
                if (condition.equals("TRUE")) {
                    ok = triggerNode.isTrue();
                } else { // FALSE
                    ok = triggerNode.isFalse();
                }
                if (ok) {
                    body.accept(bindings, triggerNode);
                } else {
                    // Enqueue for later when the condition is met
                    if (condition.equals("TRUE")) {
                        triggerNode.trueRules.add(new Object[]{body, bindings});
                    } else {
                        triggerNode.falseRules.add(new Object[]{body, bindings});
                    }
                }
            };
        }

        insertRule(dbclass, matcher, wrappedBody);
    }

    static void collectVariables(Object pattern, List<String> vars) {
        if (Unifier.isVariable(pattern)) {
            String v = (String) pattern;
            if (!vars.contains(v)) vars.add(v);
        } else if (pattern instanceof List) {
            for (Object item : (List<?>) pattern) {
                collectVariables(item, vars);
            }
        }
    }
}
