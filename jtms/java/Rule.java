// Rule: represents a rule in the JTRE system.
// Translated from jrules.lisp in "Building Problem Solvers".
package jtms;

import java.util.List;

/**
 * Rule represents a rule in the JTRE system.
 * The matcher checks if a fact matches the rule's trigger pattern.
 * The body is executed when the matcher succeeds.
 */
public class Rule {
    private int id;
    private Jtre jtre;
    private Dbclass dbclass;
    private RuleMatcher matcher;
    private Jtre.RuleBody body;

    /** Result from a rule matcher. */
    public static class MatchResult {
        public boolean ok;
        public List<Object> bindings;
        public boolean needsNode;
        public MatchResult(boolean ok, List<Object> bindings, boolean needsNode) {
            this.ok = ok;
            this.bindings = bindings;
            this.needsNode = needsNode;
        }
    }

    @FunctionalInterface
    public interface RuleMatcher {
        MatchResult match(Object pattern);
    }

    public Rule(int id, Jtre jtre, Dbclass dbclass, RuleMatcher matcher, Jtre.RuleBody body) {
        this.id = id;
        this.jtre = jtre;
        this.dbclass = dbclass;
        this.matcher = matcher;
        this.body = body;
    }

    public int getId() { return id; }
    public Jtre getJtre() { return jtre; }
    public Dbclass getDbclass() { return dbclass; }
    public RuleMatcher getMatcher() { return matcher; }
    public Jtre.RuleBody getBody() { return body; }

    @Override
    public String toString() {
        return String.format("<Rule %d>", id);
    }
}
