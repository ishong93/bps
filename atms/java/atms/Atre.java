// -*- Mode: Java -*-
//
// ATRE definitions and interface.
// Translated from ainter.lisp, last edited 1/29/93 by KDF.
//
// Copyright (c) 1990-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package atms;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * ATRE -- Assumption-based Truth maintenance Rule Engine.
 * Combines the struct definition and interface functions from ainter.lisp.
 *
 * <p>An ATRE wraps an ATMS and provides a rule-based inference engine
 * with a database of facts (datums) organized into dbclasses, plus
 * support for focus-based reasoning and contradiction handling.
 *
 * <p>Corresponds to the Lisp defstruct atre and the top-level
 * interface functions in ainter.lisp.
 */
public class Atre {

    private String title;                          // Pretty name
    private ATMS atms;                             // Pointer to its ATMS
    private List<Dbclass> dbclasses;               // List of dbclasses
    private Map<String, Dbclass> dbclassTable;     // Quick index into dbclasses
    private int datumCounter;                      // Unique ID for asserts
    private List<Rule> rules;                      // Index for rules
    private int ruleCounter;                       // Unique ID for rules
    private boolean debugging;                     // Show basic operations
    private LinkedList<Object[]> queue;            // General queue
    private int rulesRun;                          // Statistics
    private List<Object[]> inRules;                // in-rules to be executed
    private Env focus;                             // State of the search, if any
    private List<Object[]> contradictionRules;     // As in Focus paper (AAAI-88)
    private List<Object[]> impRules;               // Ibid.

    /**
     * Private constructor; use {@link #createAtre(String, boolean)} instead.
     */
    private Atre(String title, ATMS atms,
                 Map<String, Dbclass> dbclassTable, boolean debugging) {
        this.title = title;
        this.atms = atms;
        this.dbclassTable = dbclassTable;
        this.debugging = debugging;
        this.dbclasses = new ArrayList<>();
        this.datumCounter = 0;
        this.rules = new ArrayList<>();
        this.ruleCounter = 0;
        this.queue = new LinkedList<>();
        this.rulesRun = 0;
        this.inRules = new ArrayList<>();
        this.focus = null;
        this.contradictionRules = new ArrayList<>();
        this.impRules = new ArrayList<>();
    }

    // -----------------------------------------------------------------
    // Factory
    // -----------------------------------------------------------------

    /**
     * Creates a new ATRE with the given title.
     * Corresponds to the Lisp function create-atre.
     *
     * <p>Sets up the underlying ATMS, installs a default FALSE datum
     * wired to the ATMS contradiction node, and configures the ATMS
     * enqueue procedure to feed into this ATRE's queue.
     *
     * @param title     a descriptive name for this ATRE
     * @param debugging if true, trace basic operations
     * @return the newly created ATRE
     */
    public static Atre createAtre(String title, boolean debugging) {
        // Build the ATMS title as a list (:ATMS-OF <title>)
        String atmsTitle = ":ATMS-OF " + title;
        ATMS atms = ATMS.createAtms(atmsTitle, Atre::stringifyNode, debugging, null);

        Map<String, Dbclass> dbclassTable = new HashMap<>();
        Atre j = new Atre(title, atms, dbclassTable, debugging);

        // Install the enqueue procedure so the ATMS feeds rule
        // triggers back into this ATRE's queue.
        atms.setEnqueueProcedure(pair -> j.enqueue(pair));

        // Create a default contradiction datum wired to the ATMS
        // contradiction node.
        Dbclass falseDbclass = j.getDbclass("FALSE");
        Datum falseDatum = new Datum(++j.datumCounter, j, "FALSE", falseDbclass);
        falseDatum.setTmsNode(atms.getContraNode());
        atms.getContraNode().setDatum(falseDatum);
        falseDbclass.getFacts().add(0, falseDatum);

        return j;
    }

    /**
     * Convenience overload with debugging defaulting to false.
     */
    public static Atre createAtre(String title) {
        return createAtre(title, false);
    }

    // -----------------------------------------------------------------
    // Queue management
    // -----------------------------------------------------------------

    /**
     * Enqueues a rule-trigger triple onto the ATRE queue.
     * Corresponds to the Lisp function enqueue.
     *
     * @param pair the trigger information (typically a procedure
     *             plus environment context)
     */
    public void enqueue(Object pair) {
        if (pair instanceof Object[]) {
            queue.addLast((Object[]) pair);
        } else {
            queue.addLast(new Object[]{ pair });
        }
    }

    // -----------------------------------------------------------------
    // Focus management (implied-by rules)
    // -----------------------------------------------------------------

    /**
     * Changes the current focus environment.
     * Re-queues any accumulated implied-by rules for re-examination
     * under the new focus.
     * Corresponds to the Lisp function change-focus.
     *
     * @param env  the new focus environment
     * @return the new focus environment if the switch succeeded,
     *         or null if the environment was nogood or null
     */
    public Env changeFocus(Env env) {
        if (env != null && !env.isNogood()) {
            this.focus = env;
            // Re-queue implied-by rules
            this.queue.addAll(this.impRules);
            this.impRules = new ArrayList<>();
            return env;
        }
        return null;
    }

    /**
     * Returns true if the current focus is set and not nogood.
     * Corresponds to the Lisp function focus-okay?.
     */
    public boolean isFocusOkay() {
        return focus != null && !focus.isNogood();
    }

    // -----------------------------------------------------------------
    // Solutions
    // -----------------------------------------------------------------

    /**
     * Computes consistent interpretations across the given choice sets.
     * Each choice set is a list of facts; the method converts them to
     * TMS nodes and delegates to the ATMS interpretations procedure.
     * Corresponds to the Lisp function solutions.
     *
     * @param choiceSets a list of choice sets, where each choice set
     *                   is a list of fact objects
     * @return the list of consistent environments (interpretations)
     */
    public List<Env> solutions(List<List<Object>> choiceSets) {
        List<List<TmsNode>> nodeSets = new ArrayList<>();
        for (List<Object> cs : choiceSets) {
            List<TmsNode> nodes = new ArrayList<>();
            for (Object fact : cs) {
                nodes.add(AtreData.getTmsNode(fact, this));
            }
            nodeSets.add(nodes);
        }
        return atms.interpretations(nodeSets, null);
    }

    // -----------------------------------------------------------------
    // Contradiction rules
    // -----------------------------------------------------------------

    /**
     * Installs a contradiction handler rule on an environment.
     * If the environment is already nogood the rule fires immediately
     * (via enqueue); otherwise it is pushed onto the environment's
     * rule list to fire if the environment later becomes nogood.
     * Corresponds to the Lisp function contradiction-rule.
     *
     * @param env  the environment to monitor
     * @param proc the procedure to run on contradiction
     */
    public void contradictionRule(Env env, Object proc) {
        Object[] entry = new Object[]{ proc, new Object[]{ env }, null };
        if (env.isNogood()) {
            enqueue(entry);
        } else {
            env.getRules().add(0, entry);
        }
    }

    // -----------------------------------------------------------------
    // Dbclass lookup (used during creation; full version in AtreData)
    // -----------------------------------------------------------------

    /**
     * Retrieves or creates the dbclass for the given symbol name.
     * This is the core dbclass lookup used during ATRE construction
     * and by AtreData.
     * Corresponds to the Lisp function get-dbclass (symbol branch).
     *
     * @param name the symbol name
     * @return the existing or newly created Dbclass
     */
    public Dbclass getDbclass(String name) {
        if (name == null) {
            throw new IllegalArgumentException("null can't be a dbclass.");
        }
        Dbclass dbclass = dbclassTable.get(name);
        if (dbclass != null) {
            return dbclass;
        }
        dbclass = new Dbclass(name, this);
        dbclassTable.put(name, dbclass);
        dbclasses.add(0, dbclass);
        return dbclass;
    }

    // -----------------------------------------------------------------
    // Display / debugging helpers
    // -----------------------------------------------------------------

    /**
     * Prints a summary of this ATRE's state.
     * Corresponds to the Lisp function show.
     */
    public void show(PrintStream stream) {
        stream.printf("For ATRE %s:%n Focus = %s.%n",
            title,
            (focus != null) ? focus.toString() : "empty");
        AtreData.showData(this, stream);
        // showRules would go here when the rules module is translated.
    }

    /**
     * Convenience overload printing to stdout.
     */
    public void show() {
        show(System.out);
    }

    /**
     * Logs a debugging message if debugging is enabled.
     *
     * @param msg the message to print
     */
    public void debuggingAtre(String msg) {
        if (debugging) {
            System.out.print(msg);
        }
    }

    // -----------------------------------------------------------------
    // Node string callback for ATMS
    // -----------------------------------------------------------------

    /**
     * Returns the string representation of a TMS node's datum.
     * Installed as the ATMS node-string function.
     * Corresponds to the Lisp function stringify-node.
     */
    public static String stringifyNode(TmsNode node) {
        Object datum = node.getDatum();
        if (datum instanceof Datum) {
            return String.valueOf(((Datum) datum).getLispForm());
        }
        return String.valueOf(datum);
    }

    /**
     * Returns the lisp-form of a TMS node's datum.
     * Corresponds to the Lisp function view-node.
     */
    public static Object viewNode(TmsNode node) {
        Object datum = node.getDatum();
        if (datum instanceof Datum) {
            return ((Datum) datum).getLispForm();
        }
        return datum;
    }

    // -----------------------------------------------------------------
    // Accessors
    // -----------------------------------------------------------------

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public ATMS getAtms() {
        return atms;
    }

    public void setAtms(ATMS atms) {
        this.atms = atms;
    }

    public List<Dbclass> getDbclasses() {
        return dbclasses;
    }

    public void setDbclasses(List<Dbclass> dbclasses) {
        this.dbclasses = dbclasses;
    }

    public Map<String, Dbclass> getDbclassTable() {
        return dbclassTable;
    }

    public void setDbclassTable(Map<String, Dbclass> dbclassTable) {
        this.dbclassTable = dbclassTable;
    }

    public int getDatumCounter() {
        return datumCounter;
    }

    public void setDatumCounter(int datumCounter) {
        this.datumCounter = datumCounter;
    }

    public int incrementDatumCounter() {
        return ++datumCounter;
    }

    public List<Rule> getRules() {
        return rules;
    }

    public void setRules(List<Rule> rules) {
        this.rules = rules;
    }

    public int getRuleCounter() {
        return ruleCounter;
    }

    public void setRuleCounter(int ruleCounter) {
        this.ruleCounter = ruleCounter;
    }

    public int incrementRuleCounter() {
        return ++ruleCounter;
    }

    public boolean isDebugging() {
        return debugging;
    }

    public void setDebugging(boolean debugging) {
        this.debugging = debugging;
    }

    public LinkedList<Object[]> getQueue() {
        return queue;
    }

    public void setQueue(LinkedList<Object[]> queue) {
        this.queue = queue;
    }

    public int getRulesRun() {
        return rulesRun;
    }

    public void setRulesRun(int rulesRun) {
        this.rulesRun = rulesRun;
    }

    public List<Object[]> getInRules() {
        return inRules;
    }

    public void setInRules(List<Object[]> inRules) {
        this.inRules = inRules;
    }

    public Env getFocus() {
        return focus;
    }

    public void setFocus(Env focus) {
        this.focus = focus;
    }

    public List<Object[]> getContradictionRules() {
        return contradictionRules;
    }

    public void setContradictionRules(List<Object[]> contradictionRules) {
        this.contradictionRules = contradictionRules;
    }

    public List<Object[]> getImpRules() {
        return impRules;
    }

    public void setImpRules(List<Object[]> impRules) {
        this.impRules = impRules;
    }

    @Override
    public String toString() {
        return "<ATRE: " + title + ">";
    }
}
