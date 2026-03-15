package ltms;

import java.util.*;
import java.util.function.*;

/**
 * Complete LTMS with trie-based clause storage and IPIA
 * (Iterative Prime Implicate Algorithm) resolution.
 * Converted from cltms.lisp.
 */
public class CompleteLTMS {

    // --- Trie entry for clause storage ---

    /**
     * A trie entry maps a Literal (key) to either a sub-trie (List of TrieEntry)
     * or a leaf Clause.
     */
    public static class TrieEntry {
        public Literal key;
        public Object value; // List<TrieEntry> or Clause (leaf)

        public TrieEntry(Literal key, Object value) {
            this.key = key;
            this.value = value;
        }
    }

    // --- Install hooks into LTMS ---

    /**
     * Install complete LTMS hooks into the given LTMS instance.
     * Sets ipiaFn, propagateMoreUnknownnessFn, fullAddClauseFn, and walkTrieFn.
     */
    public static void installCompleteLTMS(LTMS ltms) {
        ltms.ipiaFn = CompleteLTMS::ipia;
        ltms.propagateMoreUnknownnessFn = CompleteLTMS::propagateMoreUnknownness;
        ltms.fullAddClauseFn = (l, lits, inf) -> fullAddClause(l, lits, inf);
        ltms.walkTrieFn = (fn, trie) -> walkTrie(fn, trie);
    }

    // --- Trie operations ---

    /**
     * Walk all clauses in the trie, calling fn on each leaf Clause.
     * Corresponds to walk-trie in cltms.lisp.
     */
    @SuppressWarnings("unchecked")
    public static void walkTrie(Consumer<Clause> fn, Object trie) {
        if (trie == null) return;
        if (trie instanceof Clause) {
            fn.accept((Clause) trie);
            return;
        }
        if (trie instanceof List) {
            for (TrieEntry entry : (List<TrieEntry>) trie) {
                walkTrie(fn, entry.value);
            }
        }
    }

    /**
     * Collect all clauses from the trie into a list.
     * Corresponds to collect in cltms.lisp.
     */
    public static List<Clause> collect(LTMS ltms) {
        List<Clause> result = new ArrayList<>();
        walkTrie(cl -> result.add(cl), ltms.clauses);
        return result;
    }

    /**
     * Build a trie from a list of literals terminating in a clause.
     * Corresponds to build-trie in cltms.lisp:
     *   (if (null lits) cl (cons (cons (car lits) (build-trie (cdr lits) cl)) nil))
     */
    @SuppressWarnings("unchecked")
    public static Object buildTrie(List<Literal> lits, Clause cl) {
        if (lits.isEmpty()) return cl;
        List<TrieEntry> result = new ArrayList<>();
        result.add(new TrieEntry(lits.get(0), buildTrie(lits.subList(1, lits.size()), cl)));
        return result;
    }

    /**
     * Check if literals are subsumed by any clause in the trie.
     * Corresponds to subsumed? in cltms.lisp.
     */
    @SuppressWarnings("unchecked")
    public static boolean subsumed(List<Literal> lits, Object trie) {
        if (trie == null) return false;
        if (!(trie instanceof List)) return false;
        List<TrieEntry> entries = (List<TrieEntry>) trie;
        for (TrieEntry entry : entries) {
            if (lits == null || lits.isEmpty()) return false;
            int idx = indexOf(entry.key, lits);
            if (idx >= 0) {
                List<Literal> rest = lits.subList(idx + 1, lits.size());
                if (!(entry.value instanceof List)) {
                    // Leaf clause found - subsumed
                    return true;
                }
                if (subsumed(rest, entry.value)) return true;
            }
        }
        return false;
    }

    /**
     * Add a clause to the trie.
     * Corresponds to add-to-trie in cltms.lisp.
     */
    @SuppressWarnings("unchecked")
    public static void addToTrie(Clause cl, LTMS ltms) {
        List<Literal> lits = cl.literals;
        if (ltms.clauses == null) {
            ltms.clauses = buildTrie(lits, cl);
            return;
        }
        if (ltms.clauses instanceof List) {
            List<?> list = (List<?>) ltms.clauses;
            if (list.isEmpty()) {
                ltms.clauses = buildTrie(lits, cl);
                return;
            }
            // Check if it's a List<TrieEntry> (trie) or List<Clause> (flat list from base LTMS)
            if (!list.isEmpty() && list.get(0) instanceof TrieEntry) {
                addToTrie1(lits, (List<TrieEntry>) ltms.clauses, cl);
                return;
            }
        }
        // First trie insertion when clauses was a flat list
        ltms.clauses = buildTrie(lits, cl);
    }

    /**
     * Internal trie insertion.
     * Walks the trie following literal keys, inserting new branches as needed.
     * Corresponds to the inner loop of add-to-trie in cltms.lisp.
     */
    @SuppressWarnings("unchecked")
    static void addToTrie1(List<Literal> lits, List<TrieEntry> trie, Clause cl) {
        List<TrieEntry> current = trie;

        for (int li = 0; li < lits.size(); li++) {
            Literal lit = lits.get(li);
            int litIndex = lit.node.index;
            TrieEntry slot = null;
            int insertPos = -1;

            for (int i = 0; i < current.size(); i++) {
                TrieEntry entry = current.get(i);
                if (sameLiteral(lit, entry.key)) {
                    slot = entry;
                    break;
                }
                if (entry.key.node.index > litIndex) {
                    insertPos = i;
                    break;
                }
                if (i == current.size() - 1) {
                    insertPos = current.size();
                    break;
                }
            }

            if (slot == null) {
                // Build remaining trie from lits[li..] and insert
                Object newBranch = buildTrie(lits.subList(li, lits.size()), cl);
                @SuppressWarnings("unchecked")
                List<TrieEntry> newEntries = (List<TrieEntry>) newBranch;
                TrieEntry newEntry = newEntries.get(0);

                if (insertPos >= 0 && insertPos < current.size()) {
                    // Insert before the entry with higher index
                    TrieEntry existing = current.get(insertPos);
                    current.add(insertPos, newEntry);
                } else {
                    current.add(newEntry);
                }
                return;
            }

            // Found matching slot - descend into it
            if (li == lits.size() - 1) {
                // Last literal - set leaf
                slot.value = cl;
                return;
            }

            if (slot.value instanceof List) {
                current = (List<TrieEntry>) slot.value;
            } else {
                // Was a leaf, need to extend
                List<TrieEntry> newSub = new ArrayList<>();
                Object sub = buildTrie(lits.subList(li + 1, lits.size()), cl);
                if (sub instanceof List) {
                    newSub.addAll((List<TrieEntry>) sub);
                }
                slot.value = newSub;
                return;
            }
        }
    }

    /**
     * Remove clauses from the trie that are subsumed by the given literals.
     * Corresponds to remove-subsumed in cltms.lisp.
     */
    @SuppressWarnings("unchecked")
    public static void removeSubsumed(Consumer<Clause> fn, List<Literal> lits, LTMS ltms) {
        if (ltms.clauses == null) return;
        if (removeSubsumed1(fn, lits, ltms.clauses)) {
            ltms.clauses = null;
        }
    }

    /**
     * Internal recursive subsumption removal.
     * Returns true if the entire sub-trie should be removed.
     * Corresponds to remove-subsumed-1 in cltms.lisp.
     */
    @SuppressWarnings("unchecked")
    static boolean removeSubsumed1(Consumer<Clause> fn, List<Literal> lits, Object trie) {
        if (lits.isEmpty()) {
            walkTrie(fn, trie);
            return true;
        }
        if (!(trie instanceof List)) return false;

        List<TrieEntry> entries = (List<TrieEntry>) trie;
        int au = lits.get(0).node.index;

        Iterator<TrieEntry> it = entries.iterator();
        while (it.hasNext()) {
            TrieEntry entry = it.next();
            boolean shouldRemove = false;

            if (entry.key.node.index >= au) {
                if (sameLiteral(lits.get(0), entry.key)) {
                    shouldRemove = removeSubsumed1(fn, lits.subList(1, lits.size()), entry.value);
                } else if (entry.key.node.index > au) {
                    break;
                }
            } else {
                shouldRemove = removeSubsumed1(fn, lits, entry.value);
            }

            if (shouldRemove) {
                if (entries.size() == 1) {
                    // This was the only entry - remove entire sub-trie
                    return true;
                }
                it.remove();
            }
        }
        return entries.isEmpty();
    }

    // --- Clause processing ---

    /**
     * Install a clause if it is not already subsumed.
     * Corresponds to install-clause in cltms.lisp.
     */
    public static Clause installClause(LTMS ltms, List<Literal> literals, Object informant) {
        if (subsumed(literals, ltms.clauses)) return null;
        return processClause(ltms, literals, informant, false);
    }

    /**
     * Process a new clause: BCP, remove subsumed, add to trie, index and queue.
     * Corresponds to process-clause in cltms.lisp.
     */
    public static Clause processClause(LTMS ltms, List<Literal> literals, Object informant,
                                        boolean internal) {
        Clause cl = LTMS.bcpAddClause(ltms, literals, informant, false);
        removeSubsumed(oldClause -> removeClause(oldClause, cl), literals, ltms);
        addToTrie(cl, ltms);
        if (internal) {
            // internal clauses are not queued
        } else if (ltms.delaySat && cl.isSatisfied()) {
            cl.status = ClauseStatus.DIRTY;
            indexClause(cl, ltms);
        } else {
            indexClause(cl, ltms);
            insertQueue(cl, ltms);
        }
        return cl;
    }

    /**
     * Index a clause in the connection graph (true/false clause lists of each node).
     * Corresponds to index-clause in cltms.lisp.
     */
    static void indexClause(Clause cl, LTMS ltms) {
        for (Literal term : cl.literals) {
            if (term.sign == NodeLabel.TRUE) {
                insertClause(cl, term.node.trueClauses);
            } else {
                insertClause(cl, term.node.falseClauses);
            }
        }
        LTMS.checkClauses(ltms, new ArrayList<>(Collections.singletonList(cl)));
    }

    /**
     * Insert clause into a sorted clause list (sorted by length ascending).
     * Corresponds to insert-clause macro in cltms.lisp.
     */
    static void insertClause(Clause cl, List<Clause> list) {
        int pos = 0;
        while (pos < list.size() && cl.length >= list.get(pos).length) {
            pos++;
        }
        list.add(pos, cl);
    }

    /**
     * Insert clause into the IPIA processing queue (sorted by clause length).
     * Corresponds to insert-queue in cltms.lisp.
     */
    static void insertQueue(Clause cl, LTMS ltms) {
        cl.status = ClauseStatus.QUEUED;
        for (int i = 0; i < ltms.queue.size(); i++) {
            LTMS.QueueSlot slot = ltms.queue.get(i);
            if (slot.length == cl.length) {
                slot.clauses.add(cl);
                return;
            }
            if (slot.length > cl.length) {
                LTMS.QueueSlot newSlot = new LTMS.QueueSlot(cl.length);
                newSlot.clauses.add(cl);
                ltms.queue.add(i, newSlot);
                return;
            }
        }
        LTMS.QueueSlot newSlot = new LTMS.QueueSlot(cl.length);
        newSlot.clauses.add(cl);
        ltms.queue.add(newSlot);
    }

    /**
     * Insert a clause into the sigma structure (list of length-bucketed clause lists).
     * Corresponds to insert-list / insert-list2 in cltms.lisp.
     */
    static List<List<Object>> insertList(Clause cl, List<List<Object>> list) {
        int clLen = cl.length;
        for (int i = 0; i < list.size(); i++) {
            List<Object> slot = list.get(i);
            int slotLen = (Integer) slot.get(0);
            if (slotLen == clLen) {
                slot.add(cl);
                return list;
            }
            if (slotLen > clLen) {
                List<Object> newSlot = new ArrayList<>();
                newSlot.add(clLen);
                newSlot.add(cl);
                list.add(i, newSlot);
                return list;
            }
        }
        List<Object> newSlot = new ArrayList<>();
        newSlot.add(clLen);
        newSlot.add(cl);
        list.add(newSlot);
        return list;
    }

    // --- Connection graph ---

    /**
     * Get connected clauses for a literal (opposite polarity).
     * Corresponds to literal-connections in cltms.lisp.
     */
    static List<Clause> literalConnections(Literal lit) {
        if (lit.sign == NodeLabel.TRUE) {
            return lit.node.falseClauses;
        }
        return lit.node.trueClauses;
    }

    // --- Consensus / Resolution ---

    /**
     * Compute the consensus (resolvent) of two clauses on a given literal.
     * Returns null if the consensus is a tautology or if informants match.
     * Corresponds to simplify-consensus in cltms.lisp.
     */
    static List<Literal> simplifyConsensus(Clause cl1, Clause cl2, Literal term,
                                            List<Literal> conses) {
        if (cl1.informant != null && cl1.informant.equals(cl2.informant)) return null;

        List<Literal> result = new ArrayList<>();
        int i = 0, j = 0;
        List<Literal> lits1 = cl1.literals;
        List<Literal> lits2 = cl2.literals;

        while (true) {
            if (i >= lits1.size()) {
                while (j < lits2.size()) result.add(lits2.get(j++));
                return result;
            }
            if (j >= lits2.size()) {
                while (i < lits1.size()) result.add(lits1.get(i++));
                return result;
            }

            Literal t1 = lits1.get(i);
            Literal t2 = lits2.get(j);

            if (sameLiteral(t1, t2)) {
                result.add(t1);
                i++;
                j++;
            } else if (t1.node.index == t2.node.index) {
                // Same node, different sign - complementary
                if (!sameLiteral(t1, term)) {
                    return null; // tautology on a non-resolution literal
                }
                // This is the resolution literal - skip both
                i++;
                j++;
            } else if (t1.node.index < t2.node.index) {
                result.add(t1);
                i++;
            } else {
                result.add(t2);
                j++;
            }
        }
    }

    /**
     * Compute consensus and check subsumption; if valid, create a new processed clause.
     * Corresponds to simplify-subsume-consensus in cltms.lisp.
     */
    static Clause simplifySubsumeConsensus(LTMS ltms, Clause cl1, Clause cl2, Literal p) {
        if (cl1.informant != null && cl1.informant.equals(cl2.informant)) return null;
        List<Literal> lits = simplifyConsensus(cl1, cl2, p, ltms.conses);
        if (lits == null || lits.isEmpty()) return null;
        if (subsumed(lits, ltms.clauses)) return null;
        return processClause(ltms, new ArrayList<>(lits),
                Arrays.asList("RESOLVE", cl1, cl2, p), true);
    }

    /**
     * Disjoin two sorted literal lists (merge without duplicates).
     * Returns null on complementary literals (failure).
     * Corresponds to disjoin-clauses in cltms.lisp.
     */
    public static List<Literal> disjoinClauses(List<Literal> terms1, List<Literal> terms2) {
        List<Literal> result = new ArrayList<>();
        int i = 0, j = 0;
        while (true) {
            if (i >= terms1.size()) {
                while (j < terms2.size()) result.add(terms2.get(j++));
                return result;
            }
            if (j >= terms2.size()) {
                while (i < terms1.size()) result.add(terms1.get(i++));
                return result;
            }
            Literal t1 = terms1.get(i);
            Literal t2 = terms2.get(j);
            if (sameLiteral(t1, t2)) {
                result.add(t1);
                i++;
                j++;
            } else if (t1.node.index == t2.node.index) {
                // Complementary literals => FAIL
                return null;
            } else if (t1.node.index < t2.node.index) {
                result.add(t1);
                i++;
            } else {
                result.add(t2);
                j++;
            }
        }
    }

    // --- Clause removal ---

    /**
     * Remove an old clause from the connection graph when it is subsumed by a new clause.
     * Corresponds to remove-clause in cltms.lisp.
     */
    static void removeClause(Clause oldClause, Clause newClause) {
        if (oldClause.status != ClauseStatus.NOT_INDEXED) {
            for (Literal term : oldClause.literals) {
                if (term.sign == NodeLabel.TRUE) {
                    term.node.trueClauses.remove(oldClause);
                } else {
                    term.node.falseClauses.remove(oldClause);
                }
            }
        }
        oldClause.status = ClauseStatus.SUBSUMED;

        TmsNode node = LTMS.clauseConsequent(oldClause);
        if (node != null) {
            // Check if the new clause also supports this node
            boolean found = false;
            for (Literal lit : newClause.literals) {
                if (lit.node == node) {
                    found = true;
                    break;
                }
            }
            if (found) {
                node.support = newClause;
            } else {
                // A contradiction is being introduced
                LTMS.findAlternativeSupport(node.ltms, LTMS.propagateUnknownness(node));
            }
        }
    }

    // --- IPIA (Iterative Prime Implicate Algorithm) ---

    /**
     * The main IPIA resolution loop.
     * Processes clauses from the queue, computing all prime implicates.
     * Corresponds to ipia in cltms.lisp.
     */
    public static void ipia(LTMS ltms) {
        while (!ltms.queue.isEmpty()) {
            LTMS.QueueSlot slot = ltms.queue.get(0);
            if (slot.clauses.isEmpty()) {
                ltms.queue.remove(0);
                continue;
            }

            Clause c = slot.clauses.remove(0);
            if (slot.clauses.isEmpty()) {
                ltms.queue.remove(0);
            }

            if (c.status != ClauseStatus.QUEUED) continue;
            if (ltms.delaySat && c.isSatisfied()) {
                c.status = ClauseStatus.DIRTY;
                continue;
            }

            // Build sigma (length-bucketed list) and pxs (connection lists per literal)
            List<List<Object>> sigma = new ArrayList<>();
            sigma = insertList(c, sigma);

            List<List<Clause>> pxs = new ArrayList<>();
            for (Literal lit : c.literals) {
                List<Clause> px = new ArrayList<>();
                for (Clause p : literalConnections(lit)) {
                    if (p.status == ClauseStatus.QUEUED) continue;
                    if (p.status == ClauseStatus.SUBSUMED) continue;
                    if (p.status == ClauseStatus.DIRTY && ltms.delaySat && p.isSatisfied()) continue;
                    if (simplifyConsensus(c, p, lit, ltms.conses) == null) continue;
                    if (ltms.delaySat && p.isSatisfied()) {
                        p.status = ClauseStatus.DIRTY;
                        continue;
                    }
                    px.add(p);
                }
                pxs.add(px);
            }

            // Process pxs against sigma
            Collections.reverse(pxs); // pxs was built in reverse in lisp (push)
            Collections.reverse(pxs); // actually lisp reverses with nreverse, so keep order
            int litIdx = 0;
            for (Literal lit : c.literals) {
                List<Clause> px = pxs.get(litIdx);
                if (!px.isEmpty()) {
                    for (List<Object> sigmaSlot : new ArrayList<>(sigma)) {
                        // Iterate over clauses in slot (skip first element which is the length)
                        for (int si = 1; si < sigmaSlot.size(); si++) {
                            Clause s = (Clause) sigmaSlot.get(si);
                            if (s.status == ClauseStatus.SUBSUMED) continue;
                            if (!containsLiteral(s, lit)) continue;
                            if (ltms.delaySat && s.isSatisfied()) {
                                s.status = ClauseStatus.DIRTY;
                                continue;
                            }
                            sigma = ipiaInner(ltms, sigma, px, s, lit);
                        }
                    }
                }
                litIdx++;
            }

            if (c.status == ClauseStatus.QUEUED) {
                c.status = ClauseStatus.NONE;
            }

            // Index newly created clauses
            for (List<Object> sigmaSlot : sigma) {
                for (int si = 1; si < sigmaSlot.size(); si++) {
                    Clause s = (Clause) sigmaSlot.get(si);
                    if (s.status == ClauseStatus.NOT_INDEXED) {
                        indexClause(s, ltms);
                        s.status = ClauseStatus.NONE;
                    }
                }
            }

            LTMS.checkForContradictions(ltms);
        }
    }

    /**
     * IPIA inner loop - resolve clause s against all clauses in px on the given literal.
     * Corresponds to ipia-inner in cltms.lisp.
     */
    static List<List<Object>> ipiaInner(LTMS ltms, List<List<Object>> sigma,
                                         List<Clause> px, Clause s, Literal lit) {
        List<Clause> sChildren = new ArrayList<>();
        for (Clause p : px) {
            if (ltms.delaySat && p.isSatisfied()) {
                p.status = ClauseStatus.DIRTY;
                continue;
            }
            if (p.status == ClauseStatus.SUBSUMED) continue;

            Clause consensus = simplifySubsumeConsensus(ltms, s, p, lit);
            if (consensus != null) {
                consensus.status = ClauseStatus.NOT_INDEXED;
                if (s.status == ClauseStatus.SUBSUMED) {
                    sigma = insertList(consensus, sigma);
                    sChildren.clear();
                    break;
                }
                sChildren.add(consensus);
            }
        }
        for (Clause child : sChildren) {
            if (child.status != ClauseStatus.SUBSUMED) {
                sigma = insertList(child, sigma);
            }
        }
        return sigma;
    }

    // --- LTMS entry point hooks ---

    /**
     * Run LTMS to completion using IPIA.
     * Corresponds to complete-ltms in cltms.lisp.
     */
    public static void completeLTMS(LTMS ltms) {
        LTMS.CompleteMode old = ltms.complete;
        try {
            ltms.complete = LTMS.CompleteMode.TRUE;
            ipia(ltms);
        } finally {
            ltms.complete = old;
            LTMS.checkForContradictions(ltms);
        }
    }

    /**
     * Propagate more unknownness for the complete LTMS.
     * When a node becomes unknown, update sats counts and re-queue dirty clauses.
     * Corresponds to propagate-more-unknownness in cltms.lisp.
     */
    public static void propagateMoreUnknownness(NodeLabel oldValue, TmsNode node, LTMS ltms) {
        List<Clause> clauses;
        if (oldValue == NodeLabel.TRUE) {
            clauses = node.trueClauses;
        } else {
            clauses = node.falseClauses;
        }
        for (Clause clause : new ArrayList<>(clauses)) {
            clause.sats--;
            if (clause.sats == 0 && clause.status == ClauseStatus.DIRTY) {
                insertQueue(clause, ltms);
            }
        }
    }

    /**
     * Full add clause for complete LTMS - install and run IPIA if not delaying.
     * Corresponds to full-add-clause in cltms.lisp.
     */
    public static void fullAddClause(LTMS ltms, List<Literal> literals, Object informant) {
        Clause cl = installClause(ltms, literals, informant);
        if (cl != null && ltms.complete != LTMS.CompleteMode.DELAY) {
            LTMS.checkForContradictions(ltms);
            ipia(ltms);
        }
    }

    // --- Helper methods ---

    /**
     * Check if two literals refer to the same node with the same sign.
     */
    static boolean sameLiteral(Literal a, Literal b) {
        return a == b || (a.node == b.node && a.sign == b.sign);
    }

    /**
     * Find the index of a literal in a list (by node and sign equality).
     */
    static int indexOf(Literal key, List<Literal> lits) {
        for (int i = 0; i < lits.size(); i++) {
            if (sameLiteral(lits.get(i), key)) return i;
        }
        return -1;
    }

    /**
     * Check if a clause contains a given literal.
     */
    static boolean containsLiteral(Clause clause, Literal lit) {
        for (Literal l : clause.literals) {
            if (sameLiteral(l, lit)) return true;
        }
        return false;
    }
}
