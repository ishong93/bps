// -*- Mode: Java -*-
//
// Assumption-based truth maintenance system, version 61 of 7/21/92.
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package atms;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * Assumption-based Truth Maintenance System.
 * Corresponds to the Lisp defstruct atms and all top-level functions
 * in atms.lisp.
 */
public class ATMS {

    /**
     * Result of comparing two environments.
     */
    public enum CompareEnvResult {
        EQ,   // environments are identical
        S12,  // e1 is a proper subset of e2
        S21,  // e2 is a proper subset of e1
        NONE  // no subset relationship
    }

    // --- Fields corresponding to defstruct atms ---
    private String title;
    private int nodeCounter;
    private int justCounter;
    private int envCounter;
    private List<TmsNode> nodes;
    private List<Just> justs;
    private List<TmsNode> contradictions;
    private List<TmsNode> assumptions;
    private boolean debugging;
    private List<EnvTable.Entry> nogoodTable;
    private TmsNode contraNode;
    private List<EnvTable.Entry> envTable;
    private Env emptyEnv;
    private Function<TmsNode, String> nodeString;
    private Consumer<Object> enqueueProcedure;

    // --- Constructor (private; use createAtms factory) ---
    private ATMS(String title,
                 Function<TmsNode, String> nodeString,
                 boolean debugging,
                 Consumer<Object> enqueueProcedure) {
        this.title = title;
        this.nodeString = (nodeString != null) ? nodeString : ATMS::defaultNodeString;
        this.debugging = debugging;
        this.enqueueProcedure = enqueueProcedure;
        this.nodeCounter = 0;
        this.justCounter = 0;
        this.envCounter = 0;
        this.nodes = new ArrayList<>();
        this.justs = new ArrayList<>();
        this.contradictions = new ArrayList<>();
        this.assumptions = new ArrayList<>();
        this.nogoodTable = new ArrayList<>();
        this.envTable = new ArrayList<>();
    }

    // ========================================================
    // Accessors
    // ========================================================

    public String getTitle() { return title; }
    public void setTitle(String title) { this.title = title; }

    public int getNodeCounter() { return nodeCounter; }
    public int getJustCounter() { return justCounter; }
    public int getEnvCounter() { return envCounter; }

    public List<TmsNode> getNodes() { return nodes; }
    public List<Just> getJusts() { return justs; }
    public List<TmsNode> getContradictions() { return contradictions; }
    public List<TmsNode> getAssumptions() { return assumptions; }

    public boolean isDebugging() { return debugging; }
    public void setDebugging(boolean debugging) { this.debugging = debugging; }

    public List<EnvTable.Entry> getNogoodTable() { return nogoodTable; }
    public void setNogoodTable(List<EnvTable.Entry> nogoodTable) { this.nogoodTable = nogoodTable; }

    public TmsNode getContraNode() { return contraNode; }
    public void setContraNode(TmsNode contraNode) { this.contraNode = contraNode; }

    public List<EnvTable.Entry> getEnvTable() { return envTable; }
    public void setEnvTable(List<EnvTable.Entry> envTable) { this.envTable = envTable; }

    public Env getEmptyEnv() { return emptyEnv; }
    public void setEmptyEnv(Env emptyEnv) { this.emptyEnv = emptyEnv; }

    public Function<TmsNode, String> getNodeString() { return nodeString; }
    public void setNodeString(Function<TmsNode, String> nodeString) { this.nodeString = nodeString; }

    public Consumer<Object> getEnqueueProcedure() { return enqueueProcedure; }
    public void setEnqueueProcedure(Consumer<Object> enqueueProcedure) {
        this.enqueueProcedure = enqueueProcedure;
    }

    @Override
    public String toString() {
        return "<ATMS: " + title + ">";
    }

    // ========================================================
    // Utility functions
    // ========================================================

    /**
     * Default node-string function: returns String.valueOf(datum).
     */
    public static String defaultNodeString(TmsNode n) {
        return String.valueOf(n.getDatum());
    }

    /**
     * Ordered insert: inserts item into a sorted list.
     * If item is already present (by identity), the list is returned unchanged.
     * The test returns true if item should come before the compared element.
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> orderedInsert(T item, List<T> list,
                                            java.util.Comparator<T> lessThan) {
        if (list == null || list.isEmpty()) {
            List<T> result = new ArrayList<>();
            result.add(item);
            return result;
        }
        List<T> result = new ArrayList<>(list.size() + 1);
        boolean inserted = false;
        for (T existing : list) {
            if (existing == item) {
                // Already present; return original list
                return list;
            }
            if (!inserted && lessThan.compare(item, existing) < 0) {
                result.add(item);
                inserted = true;
            }
            result.add(existing);
        }
        if (!inserted) {
            result.add(item);
        }
        return result;
    }

    /**
     * Assumption ordering: by node index ascending.
     */
    public static int assumptionOrder(TmsNode a1, TmsNode a2) {
        return Integer.compare(a1.getIndex(), a2.getIndex());
    }

    /**
     * Environment ordering: by env index ascending.
     */
    public static int envOrder(Env e1, Env e2) {
        return Integer.compare(e1.getIndex(), e2.getIndex());
    }

    // ========================================================
    // Basic inference engine interface
    // ========================================================

    /**
     * Factory method to create a new ATMS. Corresponds to create-atms.
     */
    public static ATMS createAtms(String title,
                                  Function<TmsNode, String> nodeString,
                                  boolean debugging,
                                  Consumer<Object> enqueueProcedure) {
        ATMS atms = new ATMS(title, nodeString, debugging, enqueueProcedure);
        atms.contraNode = tmsCreateNode(atms, "The contradiction", false, true);
        atms.emptyEnv = createEnv(atms, new ArrayList<>());
        return atms;
    }

    /**
     * Convenience overload with defaults.
     */
    public static ATMS createAtms(String title) {
        return createAtms(title, null, false, null);
    }

    /**
     * Change ATMS settings. Corresponds to change-atms.
     */
    public void changeAtms(Function<TmsNode, String> nodeString,
                           Consumer<Object> enqueueProcedure,
                           Boolean debugging) {
        if (nodeString != null) this.nodeString = nodeString;
        if (debugging != null) this.debugging = debugging;
        if (enqueueProcedure != null) this.enqueueProcedure = enqueueProcedure;
    }

    // ========================================================
    // Node queries
    // ========================================================

    /**
     * Is this node true (believed in the empty environment)?
     * Corresponds to true-node?.
     */
    public static boolean isTrueNode(TmsNode node) {
        List<Env> label = node.getLabel();
        if (label.isEmpty()) return false;
        return label.get(0) == node.getAtms().getEmptyEnv();
    }

    /**
     * Is this node believed (has a non-empty label)?
     * If env is provided, checks whether any label env is a subset of env.
     * Corresponds to in-node?.
     */
    public static boolean isInNode(TmsNode n, Env env) {
        if (env != null) {
            for (Env le : n.getLabel()) {
                if (isSubsetEnv(le, env)) return true;
            }
            return false;
        } else {
            return !n.getLabel().isEmpty();
        }
    }

    /**
     * Convenience: is node in (believed at all)?
     */
    public static boolean isInNode(TmsNode n) {
        return isInNode(n, null);
    }

    /**
     * Is this node out (not believed) in the given environment?
     * Corresponds to out-node?.
     */
    public static boolean isOutNode(TmsNode n, Env env) {
        return !isInNode(n, env);
    }

    /**
     * Is node consistent with the given environment?
     * Corresponds to node-consistent-with?.
     */
    public static boolean isNodeConsistentWith(TmsNode n, Env env) {
        for (Env le : n.getLabel()) {
            Env u = unionEnv(le, env);
            if (!u.isNogood()) return true;
        }
        return false;
    }

    // ========================================================
    // Node creation
    // ========================================================

    /**
     * Create a new TMS node. Corresponds to tms-create-node.
     */
    public static TmsNode tmsCreateNode(ATMS atms, Object datum,
                                        boolean assumptionp,
                                        boolean contradictoryp) {
        TmsNode node = new TmsNode(++atms.nodeCounter, datum,
                                   assumptionp, contradictoryp, atms);
        atms.nodes.add(0, node);
        if (contradictoryp) {
            atms.contradictions.add(0, node);
        }
        if (assumptionp) {
            atms.assumptions.add(0, node);
            List<TmsNode> singleAssumption = new ArrayList<>();
            singleAssumption.add(node);
            node.getLabel().add(0, createEnv(atms, singleAssumption));
        }
        return node;
    }

    /**
     * Convert an existing node into an assumption. Corresponds to assume-node.
     */
    public static void assumeNode(TmsNode node) {
        if (!node.isAssumption()) {
            ATMS atms = node.getAtms();
            if (atms.debugging) {
                System.err.println("Converting " + node.nodeString()
                                   + " into an assumption");
            }
            node.setAssumption(true);
            atms.assumptions.add(0, node);
            List<TmsNode> singleAssumption = new ArrayList<>();
            singleAssumption.add(node);
            List<Env> newEnvs = new ArrayList<>();
            newEnvs.add(createEnv(atms, singleAssumption));
            update(newEnvs, node, "ASSUME-NODE");
        }
    }

    /**
     * Mark a node as contradictory. Corresponds to make-contradiction.
     */
    public static void makeContradiction(TmsNode node) {
        if (!node.isContradictory()) {
            ATMS atms = node.getAtms();
            node.setContradictory(true);
            atms.contradictions.add(0, node);
            while (true) {
                if (!node.getLabel().isEmpty()) {
                    Env nogood = node.getLabel().get(0);
                    newNogood(atms, nogood, "MAKE-CONTRADICTION");
                } else {
                    break;
                }
            }
        }
    }

    // ========================================================
    // Justification
    // ========================================================

    /**
     * Justify a node. Corresponds to justify-node.
     */
    public static Just justifyNode(Object informant, TmsNode consequence,
                                   List<TmsNode> antecedents) {
        ATMS atms = consequence.getAtms();
        Just just = new Just(++atms.justCounter, informant,
                             consequence, antecedents);
        consequence.getJusts().add(0, just);
        for (TmsNode node : antecedents) {
            node.getConsequences().add(0, just);
        }
        atms.justs.add(0, just);
        if (atms.debugging) {
            List<String> antNames = new ArrayList<>();
            for (TmsNode a : antecedents) {
                antNames.add(a.nodeString());
            }
            System.err.println("Justifying " + consequence.nodeString()
                               + " in terms of " + informant
                               + " on " + antNames);
        }
        List<Env> startEnvs = new ArrayList<>();
        startEnvs.add(atms.emptyEnv);
        propagate(just, null, startEnvs);
        return just;
    }

    /**
     * Declare a set of nodes as nogood. Corresponds to nogood-nodes.
     */
    public static Just nogoodNodes(Object informant, List<TmsNode> nodes) {
        return justifyNode(informant,
                           nodes.get(0).getAtms().getContraNode(),
                           nodes);
    }

    // ========================================================
    // Label updating
    // ========================================================

    /**
     * Propagate a justification. Corresponds to propagate.
     */
    public static void propagate(Just just, TmsNode antecedent,
                                 List<Env> envs) {
        List<Env> newEnvs = weave(antecedent, envs,
                                  just.getAntecedents());
        if (newEnvs != null && !newEnvs.isEmpty()) {
            update(newEnvs, just.getConsequence(), just);
        }
    }

    /**
     * Update a node with new environments. Corresponds to update.
     * The 'just' parameter is Object because in assume-node it is a String.
     */
    public static void update(List<Env> newEnvs, TmsNode consequence,
                              Object just) {
        ATMS atms = consequence.getAtms();

        // If consequence is contradictory, all new envs become nogoods.
        if (consequence.isContradictory()) {
            for (Env env : newEnvs) {
                newNogood(atms, env, just);
            }
            return;
        }

        // Update the label, filtering newEnvs
        newEnvs = updateLabel(consequence, newEnvs);
        if (newEnvs == null || newEnvs.isEmpty()) return;

        // Enqueue rules if needed
        Consumer<Object> enqueuef = atms.enqueueProcedure;
        if (enqueuef != null) {
            for (Object rule : consequence.getRules()) {
                enqueuef.accept(rule);
            }
            consequence.setRules(new ArrayList<>());
        }

        // Propagate to each justification that this node is an antecedent of
        for (Just supportedJust : consequence.getConsequences()) {
            propagate(supportedJust, consequence, newEnvs);

            // After propagation, remove envs no longer in the label
            // (they may have been subsumed)
            for (int i = 0; i < newEnvs.size(); i++) {
                if (newEnvs.get(i) != null
                    && !consequence.getLabel().contains(newEnvs.get(i))) {
                    newEnvs.set(i, null);
                }
            }
            newEnvs.removeIf(e -> e == null);
            if (newEnvs.isEmpty()) return;
        }
    }

    /**
     * Update the label of a node with new environments.
     * Corresponds to update-label.
     * Returns the list of actually-new environments that were added.
     */
    public static List<Env> updateLabel(TmsNode node, List<Env> newEnvs) {
        // Work with mutable copies to allow nulling out entries
        List<Env> newEnvsList = new ArrayList<>(newEnvs);
        List<Env> envs = new ArrayList<>(node.getLabel());

        for (int ni = 0; ni < newEnvsList.size(); ni++) {
            boolean addedToEnvs = false;
            for (int ei = 0; ei < envs.size(); ei++) {
                Env nenv = envs.get(ei);
                Env newE = newEnvsList.get(ni);
                if (nenv == null) continue;
                if (newE == null) break;
                CompareEnvResult cmp = compareEnv(newE, nenv);
                if (cmp == CompareEnvResult.EQ || cmp == CompareEnvResult.S21) {
                    // new-env is a superset or equal: discard it
                    newEnvsList.set(ni, null);
                    addedToEnvs = true; // don't add to envs
                    break;
                } else if (cmp == CompareEnvResult.S12) {
                    // new-env is a proper subset: discard old env
                    Env oldEnv = envs.get(ei);
                    oldEnv.getNodes().remove(node);
                    envs.set(ei, null);
                }
            }
            if (!addedToEnvs && newEnvsList.get(ni) != null) {
                envs.add(0, newEnvsList.get(ni));
            }
        }

        // Filter out nulls
        newEnvsList.removeIf(e -> e == null);
        for (Env newEnv : newEnvsList) {
            newEnv.getNodes().add(0, node);
        }
        envs.removeIf(e -> e == null);
        node.setLabel(envs);
        return newEnvsList;
    }

    /**
     * Weave environments through antecedents. Corresponds to weave.
     * Returns null if no consistent combination is possible.
     */
    public static List<Env> weave(TmsNode antecedent, List<Env> envs,
                                  List<TmsNode> antecedents) {
        List<Env> currentEnvs = new ArrayList<>(envs);
        for (TmsNode node : antecedents) {
            if (node == antecedent) continue;
            List<Env> newEnvs = new ArrayList<>();
            for (Env env : currentEnvs) {
                if (env == null) continue;
                for (Env nodeEnv : node.getLabel()) {
                    Env newEnv = unionEnv(env, nodeEnv);
                    if (!newEnv.isNogood()) {
                        boolean subsumed = false;
                        for (int i = 0; i < newEnvs.size(); i++) {
                            Env existing = newEnvs.get(i);
                            if (existing == null) continue;
                            CompareEnvResult cmp = compareEnv(newEnv, existing);
                            if (cmp == CompareEnvResult.EQ
                                || cmp == CompareEnvResult.S21) {
                                subsumed = true;
                                break;
                            } else if (cmp == CompareEnvResult.S12) {
                                newEnvs.set(i, null);
                            }
                        }
                        if (!subsumed) {
                            newEnvs.add(0, newEnv);
                        }
                    }
                }
            }
            newEnvs.removeIf(e -> e == null);
            currentEnvs = newEnvs;
            if (currentEnvs.isEmpty()) return null;
        }
        return currentEnvs;
    }

    /**
     * Check whether a set of antecedent nodes can all be believed together.
     * Corresponds to in-antecedent?.
     */
    public static boolean isInAntecedent(List<TmsNode> nodes) {
        if (nodes == null || nodes.isEmpty()) return true;
        return isWeave(nodes.get(0).getAtms().getEmptyEnv(), nodes);
    }

    /**
     * Recursive helper for in-antecedent?. Corresponds to weave?.
     */
    public static boolean isWeave(Env env, List<TmsNode> nodes) {
        if (nodes == null || nodes.isEmpty()) return true;
        TmsNode first = nodes.get(0);
        List<TmsNode> rest = nodes.subList(1, nodes.size());
        for (Env e : first.getLabel()) {
            Env newEnv = unionEnv(e, env);
            if (!newEnv.isNogood()) {
                if (isWeave(newEnv, rest)) return true;
            }
        }
        return false;
    }

    /**
     * Are all nodes believed in the given environment?
     * Corresponds to supporting-antecedent?.
     */
    public static boolean isSupportingAntecedent(List<TmsNode> nodes, Env env) {
        for (TmsNode node : nodes) {
            if (!isInNode(node, env)) return false;
        }
        return true;
    }

    // ========================================================
    // Node removal
    // ========================================================

    /**
     * Remove a node from the ATMS. Corresponds to remove-node.
     * The node must have no consequences.
     */
    public static void removeNode(TmsNode node) {
        if (!node.getConsequences().isEmpty()) {
            throw new IllegalStateException("Can't remove node with consequences");
        }
        ATMS atms = node.getAtms();
        atms.nodes.remove(node);
        for (Just just : node.getJusts()) {
            for (TmsNode ant : just.getAntecedents()) {
                ant.getConsequences().remove(just);
            }
        }
        for (Env env : node.getLabel()) {
            env.getNodes().remove(node);
        }
    }

    // ========================================================
    // Creating and extending environments
    // ========================================================

    /**
     * Create a new environment. Corresponds to create-env.
     */
    public static Env createEnv(ATMS atms, List<TmsNode> assumptions) {
        Env e = new Env(++atms.envCounter,
                        (assumptions != null) ? assumptions.size() : 0,
                        assumptions);
        atms.envTable = EnvTable.insertInTable(atms.envTable, e);
        setEnvContradictory(atms, e);
        return e;
    }

    /**
     * Compute the union of two environments. Corresponds to union-env.
     */
    public static Env unionEnv(Env e1, Env e2) {
        // Ensure e1 is the smaller
        if (e1.getCount() > e2.getCount()) {
            Env tmp = e1;
            e1 = e2;
            e2 = tmp;
        }
        for (TmsNode assume : e1.getAssumptions()) {
            e2 = consEnv(assume, e2);
            if (e2.isNogood()) return e2;
        }
        return e2;
    }

    /**
     * Add an assumption to an environment. Corresponds to cons-env.
     */
    public static Env consEnv(TmsNode assumption, Env env) {
        List<TmsNode> nassumes = orderedInsert(assumption,
                                               env.getAssumptions(),
                                               ATMS::assumptionOrder);
        Env found = lookupEnv(nassumes);
        if (found != null) return found;
        return createEnv(assumption.getAtms(), nassumes);
    }

    /**
     * Find or create an environment for the given assumptions.
     * Corresponds to find-or-make-env.
     */
    public static Env findOrMakeEnv(List<TmsNode> assumptions, ATMS atms) {
        if (assumptions == null || assumptions.isEmpty()) {
            return atms.emptyEnv;
        }
        Env found = lookupEnv(assumptions);
        if (found != null) return found;
        return createEnv(atms, assumptions);
    }

    // ========================================================
    // Env table operations
    // ========================================================

    /**
     * Look up an environment by its assumption list.
     * Corresponds to lookup-env.
     */
    public static Env lookupEnv(List<TmsNode> assumes) {
        if (assumes == null || assumes.isEmpty()) return null;
        ATMS atms = assumes.get(0).getAtms();
        EnvTable.Entry entry = EnvTable.lookup(atms.envTable, assumes.size());
        if (entry == null) return null;
        for (Env env : entry.getEnvs()) {
            if (env.getAssumptions().equals(assumes)) {
                return env;
            }
        }
        return null;
    }

    /**
     * Is e1 a subset of e2? Corresponds to subset-env?.
     */
    public static boolean isSubsetEnv(Env e1, Env e2) {
        if (e1 == e2) return true;
        if (e1.getCount() > e2.getCount()) return false;
        return e2.getAssumptions().containsAll(e1.getAssumptions());
    }

    /**
     * Compare two environments. Corresponds to compare-env.
     */
    public static CompareEnvResult compareEnv(Env e1, Env e2) {
        if (e1 == e2) return CompareEnvResult.EQ;
        if (e1.getCount() < e2.getCount()) {
            if (e2.getAssumptions().containsAll(e1.getAssumptions())) {
                return CompareEnvResult.S12;
            }
            return CompareEnvResult.NONE;
        }
        if (e1.getAssumptions().containsAll(e2.getAssumptions())) {
            return CompareEnvResult.S21;
        }
        return CompareEnvResult.NONE;
    }

    // ========================================================
    // Processing nogoods
    // ========================================================

    /**
     * Process a new nogood environment. Corresponds to new-nogood.
     * The 'just' parameter is Object because it can be a Just or a String.
     */
    public static void newNogood(ATMS atms, Env cenv, Object just) {
        if (atms.debugging) {
            System.err.println("  " + cenv + " new minimal nogood.");
        }
        cenv.setNogood(just);
        removeEnvFromLabels(cenv, atms);
        atms.nogoodTable = EnvTable.insertInTable(atms.nogoodTable, cenv);
        int count = cenv.getCount();

        // Remove supersets from nogood table
        for (EnvTable.Entry entry : atms.nogoodTable) {
            if (entry.getCount() > count) {
                List<Env> toRemove = new ArrayList<>();
                for (Env old : entry.getEnvs()) {
                    if (isSubsetEnv(cenv, old)) {
                        toRemove.add(old);
                    }
                }
                entry.getEnvs().removeAll(toRemove);
            }
        }

        // Mark supersets in env table as nogood
        for (EnvTable.Entry entry : atms.envTable) {
            if (entry.getCount() > count) {
                for (Env old : entry.getEnvs()) {
                    if (!old.isNogood() && isSubsetEnv(cenv, old)) {
                        old.setNogood(cenv);
                        removeEnvFromLabels(old, atms);
                    }
                }
            }
        }
    }

    /**
     * Check if an environment should be contradictory.
     * Corresponds to set-env-contradictory.
     */
    public static boolean setEnvContradictory(ATMS atms, Env env) {
        if (env.isNogood()) return true;
        int count = env.getCount();
        for (EnvTable.Entry entry : atms.nogoodTable) {
            if (entry.getCount() > count) {
                return false;
            }
            for (Env cenv : entry.getEnvs()) {
                if (isSubsetEnv(cenv, env)) {
                    env.setNogood(cenv);
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Remove an environment from all node labels.
     * Corresponds to remove-env-from-labels.
     */
    public static void removeEnvFromLabels(Env env, ATMS atms) {
        Consumer<Object> enqueuef = atms.enqueueProcedure;
        if (enqueuef != null) {
            for (Object rule : env.getRules()) {
                enqueuef.accept(rule);
            }
            env.setRules(new ArrayList<>());
        }
        for (TmsNode node : env.getNodes()) {
            node.getLabel().remove(env);
        }
    }

    // ========================================================
    // Interpretation construction
    // ========================================================

    /**
     * Compute interpretations from choice sets.
     * Corresponds to interpretations.
     *
     * @param atms       the ATMS
     * @param choiceSets list of choice sets; each choice set is a list of
     *                   TmsNode alternatives
     * @param defaults   optional list of default assumption nodes
     * @return list of consistent environments, or null if none
     */
    public static List<Env> interpretations(ATMS atms,
                                            List<List<TmsNode>> choiceSets,
                                            List<TmsNode> defaults) {
        if (atms.debugging) {
            System.err.println(" Constructing interpretations depth-first...");
        }
        List<Env> solutions = new ArrayList<>();

        // Expand choice sets: for each alt-set, collect all label envs
        List<List<Env>> expandedChoiceSets = new ArrayList<>();
        for (List<TmsNode> altSet : choiceSets) {
            List<Env> expanded = new ArrayList<>();
            for (TmsNode alt : altSet) {
                expanded.addAll(alt.getLabel());
            }
            expandedChoiceSets.add(expanded);
        }

        // Depth-first search
        if (!expandedChoiceSets.isEmpty()) {
            for (Env choice : expandedChoiceSets.get(0)) {
                getDepthSolutions1(choice,
                    expandedChoiceSets.subList(1, expandedChoiceSets.size()),
                    solutions);
            }
        }

        solutions.removeIf(e -> e == null);

        if (solutions.isEmpty()) {
            if (!choiceSets.isEmpty()) return null;
            solutions.add(atms.emptyEnv);
        }

        if (defaults != null && !defaults.isEmpty()) {
            List<Env> baseSolutions = new ArrayList<>(solutions);
            solutions.clear();
            for (Env solution : baseSolutions) {
                extendViaDefaults(solution, defaults, defaults, solutions);
            }
        }

        solutions.removeIf(e -> e == null);
        return solutions;
    }

    /**
     * Convenience overload without defaults.
     */
    public static List<Env> interpretations(ATMS atms,
                                            List<List<TmsNode>> choiceSets) {
        return interpretations(atms, choiceSets, null);
    }

    /**
     * Depth-first search for solutions.
     * Corresponds to get-depth-solutions1.
     */
    private static void getDepthSolutions1(Env solution,
                                           List<List<Env>> choiceSets,
                                           List<Env> solutions) {
        if (choiceSets.isEmpty()) {
            // Check if this solution is subsumed by an existing one
            boolean dominated = false;
            for (int i = 0; i < solutions.size(); i++) {
                Env old = solutions.get(i);
                if (old == null) continue;
                CompareEnvResult cmp = compareEnv(old, solution);
                if (cmp == CompareEnvResult.EQ || cmp == CompareEnvResult.S12) {
                    dominated = true;
                    break;
                } else if (cmp == CompareEnvResult.S21) {
                    solutions.set(i, null);
                }
            }
            if (!dominated) {
                solutions.add(0, solution);
            }
            return;
        }

        if (solution.isNogood()) return;

        List<Env> firstChoices = choiceSets.get(0);
        List<List<Env>> restChoices = choiceSets.subList(1, choiceSets.size());
        for (Env choice : firstChoices) {
            Env newSolution = unionEnv(solution, choice);
            if (!newSolution.isNogood()) {
                getDepthSolutions1(newSolution, restChoices, solutions);
            }
        }
    }

    /**
     * Extend a solution via defaults. Corresponds to extend-via-defaults.
     *
     * The Lisp do-loop iterates through remaining defaults. For each default,
     * if cons-env with the solution is not nogood, it recurses with the
     * extended solution and the tail of remaining. The loop always continues
     * to the next default. When all defaults are exhausted, the termination
     * clause checks if the solution is maximal and adds it to solutions.
     */
    private static void extendViaDefaults(Env solution,
                                          List<TmsNode> remaining,
                                          List<TmsNode> original,
                                          List<Env> solutions) {
        // Iterate through remaining defaults (mirrors Lisp do-loop)
        for (int i = 0; i < remaining.size(); i++) {
            TmsNode current = remaining.get(i);
            List<TmsNode> rest = remaining.subList(i + 1, remaining.size());
            Env newSolution = consEnv(current, solution);
            if (!newSolution.isNogood()) {
                extendViaDefaults(newSolution, rest, original, solutions);
            }
            // Loop always continues to the next default
        }
        // Termination clause: all defaults exhausted
        // Check if solution is already present (by identity)
        if (solutions.contains(solution)) return;
        // Check if solution is maximal: every original default is either
        // already in the solution or incompatible with it
        for (TmsNode def : original) {
            if (!solution.getAssumptions().contains(def)
                && !consEnv(def, solution).isNogood()) {
                return; // Not maximal; can still be extended
            }
        }
        solutions.add(0, solution);
    }

    // ========================================================
    // Generating explanations
    // ========================================================

    /**
     * Explain why a node is believed in an environment.
     * Returns a list of Objects, where each is either a Just or
     * a String[]{&quot;ASSUME&quot;, node.toString()} pair (represented as
     * an ExplanationEntry).
     * Corresponds to explain-node.
     */
    public static List<Object> explainNode(TmsNode node, Env env) {
        return explainNode1(env, node, new ArrayList<>(), new ArrayList<>());
    }

    /**
     * Represents an assumption entry in an explanation.
     */
    public static class AssumptionEntry {
        public final TmsNode node;

        public AssumptionEntry(TmsNode node) {
            this.node = node;
        }

        @Override
        public String toString() {
            return "(ASSUME . " + node + ")";
        }
    }

    /**
     * Recursive explanation builder. Corresponds to explain-node-1.
     *
     * @return an explanation list, or null if this path fails
     */
    private static List<Object> explainNode1(Env env, TmsNode node,
                                             List<TmsNode> queuedNodes,
                                             List<Object> explanation) {
        // If already queued, fail
        if (queuedNodes.contains(node)) return null;

        // If node is an assumption in env, add assumption entry
        if (node.isAssumption() && env.getAssumptions().contains(node)) {
            List<Object> result = new ArrayList<>(explanation);
            result.add(0, new AssumptionEntry(node));
            return result;
        }

        // If already explained, return current explanation
        for (Object entry : explanation) {
            if (entry instanceof AssumptionEntry) {
                if (((AssumptionEntry) entry).node == node) {
                    return explanation;
                }
            } else if (entry instanceof Just) {
                if (((Just) entry).getConsequence() == node) {
                    return explanation;
                }
            }
        }

        // Try each justification
        List<TmsNode> newQueued = new ArrayList<>(queuedNodes);
        newQueued.add(0, node);

        for (Just just : node.getJusts()) {
            // Check all antecedents are in the environment
            boolean allIn = true;
            for (TmsNode a : just.getAntecedents()) {
                if (!isInNode(a, env)) {
                    allIn = false;
                    break;
                }
            }
            if (!allIn) continue;

            // Try to build explanation through this justification
            List<Object> newExplanation = new ArrayList<>(explanation);
            boolean failed = false;
            for (TmsNode a : just.getAntecedents()) {
                newExplanation = explainNode1(env, a, newQueued, newExplanation);
                if (newExplanation == null) {
                    failed = true;
                    break;
                }
            }
            if (!failed) {
                newExplanation.add(0, just);
                return newExplanation;
            }
        }
        return null;
    }

    // ========================================================
    // Printing
    // ========================================================

    /**
     * Print why a node is believed. Corresponds to why-node.
     */
    public static void whyNode(TmsNode node, PrintWriter stream, String prefix) {
        stream.print("\n<" + prefix + node.getDatum() + ",{");
        for (Env e : node.getLabel()) {
            envString(e, stream);
        }
        stream.print("}>");
    }

    public static void whyNode(TmsNode node, PrintWriter stream) {
        whyNode(node, stream, "");
    }

    public static void whyNode(TmsNode node) {
        PrintWriter pw = new PrintWriter(System.out, true);
        whyNode(node, pw, "");
        pw.flush();
    }

    /**
     * Print all nodes. Corresponds to why-nodes.
     */
    public static void whyNodes(ATMS atms, PrintWriter stream) {
        List<TmsNode> reversed = new ArrayList<>(atms.nodes);
        Collections.reverse(reversed);
        for (TmsNode n : reversed) {
            whyNode(n, stream);
        }
    }

    public static void whyNodes(ATMS atms) {
        PrintWriter pw = new PrintWriter(System.out, true);
        whyNodes(atms, pw);
        pw.flush();
    }

    /**
     * Print justifications for a node. Corresponds to node-justifications.
     */
    public static void nodeJustifications(TmsNode node, PrintWriter stream) {
        stream.print("\n For " + node.nodeString() + ":");
        for (Just j : node.getJusts()) {
            printJustification(j, stream);
        }
    }

    public static void nodeJustifications(TmsNode node) {
        PrintWriter pw = new PrintWriter(System.out, true);
        nodeJustifications(node, pw);
        pw.flush();
    }

    /**
     * Print a single justification. Corresponds to print-justification.
     */
    public static void printJustification(Just j, PrintWriter stream) {
        stream.print("\n  " + j.getInformant() + ", ");
        for (TmsNode a : j.getAntecedents()) {
            whyNode(a, stream, "     ");
        }
    }

    public static void printJustification(Just j) {
        PrintWriter pw = new PrintWriter(System.out, true);
        printJustification(j, pw);
        pw.flush();
    }

    /**
     * Find an environment by index. Corresponds to e.
     */
    public Env findEnvByIndex(int n) {
        for (EnvTable.Entry bucket : envTable) {
            for (Env env : bucket.getEnvs()) {
                if (env.getIndex() == n) return env;
            }
        }
        return null;
    }

    /**
     * Print an environment. Corresponds to print-env.
     */
    public static void printEnv(Env e, PrintWriter stream) {
        stream.print("\n" + e + ":" + (e.isNogood() ? "* " : " "));
        envString(e, stream);
    }

    public static void printEnv(Env e) {
        PrintWriter pw = new PrintWriter(System.out, true);
        printEnv(e, pw);
        pw.flush();
    }

    /**
     * Print the string representation of an environment.
     * Corresponds to env-string.
     */
    public static void envString(Env e, PrintWriter stream) {
        List<TmsNode> assumptions = e.getAssumptions();
        Function<TmsNode, String> printer = null;
        if (!assumptions.isEmpty()) {
            printer = assumptions.get(0).getAtms().getNodeString();
        }
        List<String> strings = new ArrayList<>();
        if (printer != null) {
            for (TmsNode a : assumptions) {
                strings.add(printer.apply(a));
            }
        }
        Collections.sort(strings);
        StringBuilder sb = new StringBuilder("{");
        for (int i = 0; i < strings.size(); i++) {
            if (i > 0) sb.append(",");
            sb.append(strings.get(i));
        }
        sb.append("}");
        stream.print(sb.toString());
    }

    /**
     * Return the string representation of an environment.
     */
    public static String envString(Env e) {
        List<TmsNode> assumptions = e.getAssumptions();
        Function<TmsNode, String> printer = null;
        if (!assumptions.isEmpty()) {
            printer = assumptions.get(0).getAtms().getNodeString();
        }
        List<String> strings = new ArrayList<>();
        if (printer != null) {
            for (TmsNode a : assumptions) {
                strings.add(printer.apply(a));
            }
        }
        Collections.sort(strings);
        StringBuilder sb = new StringBuilder("{");
        for (int i = 0; i < strings.size(); i++) {
            if (i > 0) sb.append(",");
            sb.append(strings.get(i));
        }
        sb.append("}");
        return sb.toString();
    }

    // ========================================================
    // Printing global data
    // ========================================================

    /**
     * Print all nogood environments. Corresponds to print-nogoods.
     */
    public static void printNogoods(ATMS atms, PrintWriter stream) {
        printEnvTable(atms.nogoodTable, stream);
    }

    public static void printNogoods(ATMS atms) {
        PrintWriter pw = new PrintWriter(System.out, true);
        printNogoods(atms, pw);
        pw.flush();
    }

    /**
     * Print all environments. Corresponds to print-envs.
     */
    public static void printEnvs(ATMS atms, PrintWriter stream) {
        printEnvTable(atms.envTable, stream);
    }

    public static void printEnvs(ATMS atms) {
        PrintWriter pw = new PrintWriter(System.out, true);
        printEnvs(atms, pw);
        pw.flush();
    }

    /**
     * Print an environment table. Corresponds to print-env-table.
     */
    public static void printEnvTable(List<EnvTable.Entry> table,
                                     PrintWriter stream) {
        for (EnvTable.Entry bucket : table) {
            for (Env env : bucket.getEnvs()) {
                printEnv(env, stream);
            }
        }
    }

    /**
     * Print ATMS statistics. Corresponds to print-atms-statistics.
     */
    public static void printAtmsStatistics(ATMS atms) {
        PrintWriter pw = new PrintWriter(System.out, true);
        printTable(" For env table:", atms.envTable, pw);
        printTable(" For nogood table:", atms.nogoodTable, pw);
        pw.flush();
    }

    /**
     * Print a table summary. Corresponds to print-table.
     */
    public static void printTable(String msg, List<EnvTable.Entry> table,
                                  PrintWriter stream) {
        stream.print("\n" + msg);
        for (EnvTable.Entry entry : table) {
            stream.print("\n   Length " + entry.getCount()
                         + ", " + entry.getEnvs().size());
        }
    }
}
