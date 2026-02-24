// JTRE: A JTMS-based Tiny Rule Engine.
// Translated from jinter.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Jtre represents a JTMS-based Tiny Rule Engine.
 * It wraps a JTMS and provides a database of facts and a rule system.
 */
public class Jtre {
    private String title;
    private JTMS jtms;
    private Map<String, Dbclass> dbclassTable;
    private int datumCounter = 0;
    private int ruleCounter = 0;
    private boolean debugging = false;
    private List<QueueEntry> queue = new ArrayList<>();
    private int rulesRun = 0;

    /** Queue entry: a rule body + bindings. */
    public static class QueueEntry {
        public RuleBody body;
        public List<Object> bindings;
        public QueueEntry(RuleBody body, List<Object> bindings) {
            this.body = body;
            this.bindings = bindings;
        }
    }

    @FunctionalInterface
    public interface RuleBody {
        void execute(List<Object> args);
    }

    public Jtre(String title, boolean debugging) {
        this.title = title;
        this.debugging = debugging;
        this.dbclassTable = new HashMap<>();
        this.jtms = new JTMS(String.format("JTMS-OF %s", title));
        this.jtms.setNodeStringFunc(node -> JData.viewNode(node));
        Jtre self = this;
        this.jtms.setEnqueueProcedure(rule -> enqueue(rule, self));
    }

    // Getters / setters
    public String getTitle() { return title; }
    public JTMS getJtms() { return jtms; }
    public Map<String, Dbclass> getDbclassTable() { return dbclassTable; }
    public int getDatumCounter() { return datumCounter; }
    public void setDatumCounter(int c) { this.datumCounter = c; }
    public int getRuleCounter() { return ruleCounter; }
    public void setRuleCounter(int c) { this.ruleCounter = c; }
    public boolean isDebugging() { return debugging; }
    public void setDebugging(boolean d) { this.debugging = d; }
    public List<QueueEntry> getQueue() { return queue; }
    public int getRulesRun() { return rulesRun; }
    public void setRulesRun(int r) { this.rulesRun = r; }

    @Override
    public String toString() {
        return String.format("<JTRE: %s>", title);
    }

    /** Debug output if debugging is on. */
    public void debug(String fmt, Object... args) {
        if (debugging) {
            System.out.printf(fmt, args);
        }
    }

    /** Assert a fact then run rules. Corresponds to uassert!. */
    public Datum uassert(Object fact, Object just) {
        Datum d = JData.jassert(fact, just, this);
        JRules.runRules(this);
        return d;
    }

    /** Assume a fact then run rules. Corresponds to uassume!. */
    public Datum uassume(Object fact, Object reason) {
        Datum d = JData.jassume(fact, reason, this);
        JRules.runRules(this);
        return d;
    }

    /** Display data and rules. Corresponds to show. */
    public void show(PrintStream out) {
        if (out == null) out = System.out;
        JData.showData(this, out);
        JRules.showRules(this, out);
    }

    /** Enqueue a rule for execution. */
    public static void enqueue(Object item, Jtre jtre) {
        if (item instanceof QueueEntry) {
            jtre.queue.add(0, (QueueEntry) item);
        }
    }

    /** Dequeue the next entry. */
    public static QueueEntry dequeue(Jtre jtre) {
        if (jtre.queue.isEmpty()) return null;
        return jtre.queue.remove(0);
    }
}
