package ltms;

import java.util.*;
import java.util.function.*;

/**
 * Logic-based Truth Maintenance System.
 * Converted from ltms.lisp.
 */
public class LTMS {
    public static final String ENABLED_ASSUMPTION = ":ENABLED-ASSUMPTION";

    public String title;
    public int nodeCounter;
    public int clauseCounter;
    public Map<Object, TmsNode> nodes;  // null if not caching
    public Object clauses;              // List<Clause> or trie
    public boolean debugging;
    public boolean checkingContradictions = true;
    public Function<TmsNode, String> nodeString;
    public List<BiFunction<List<Clause>, LTMS, Boolean>> contradictionHandlers = new ArrayList<>();
    public List<Clause> pendingContradictions = new ArrayList<>();
    public Consumer<Object> enqueueProcedure;
    public CompleteMode complete = CompleteMode.NONE;
    public List<Clause> violatedClauses = new ArrayList<>();
    public List<QueueSlot> queue = new ArrayList<>();
    public List<Literal> conses = new ArrayList<>();
    public boolean delaySat = true;
    public int consSize;

    // Cross-module function hooks
    public Consumer<LTMS> ipiaFn;
    public TriConsumer<NodeLabel, TmsNode, LTMS> propagateMoreUnknownnessFn;
    public TriConsumer<LTMS, List<Literal>, Object> fullAddClauseFn;
    public BiConsumer<Consumer<Clause>, Object> walkTrieFn;

    public enum CompleteMode { NONE, TRUE, DELAY, COMPLETE }

    @FunctionalInterface
    public interface TriConsumer<A, B, C> {
        void accept(A a, B b, C c);
    }

    public static class QueueSlot {
        public int length;
        public List<Clause> clauses;
        public QueueSlot(int length) {
            this.length = length;
            this.clauses = new ArrayList<>();
        }
    }

    // --- Creation ---

    public LTMS(String title) {
        this.title = title;
        this.nodeString = TmsNode::nodeString;
        this.clauses = new ArrayList<Clause>();
    }

    public static LTMS createLTMS(String title, Function<TmsNode, String> nodeString,
                                   boolean debugging, boolean checkingContradictions,
                                   BiFunction<List<Clause>, LTMS, Boolean> contradictionHandler,
                                   Consumer<Object> enqueueProcedure,
                                   boolean cacheDatums, CompleteMode complete, boolean delaySat) {
        LTMS l = new LTMS(title);
        if (nodeString != null) l.nodeString = nodeString;
        l.debugging = debugging;
        l.checkingContradictions = checkingContradictions;
        if (contradictionHandler != null) {
            l.contradictionHandlers.add(contradictionHandler);
        } else {
            l.contradictionHandlers.add(LTMS::askUserHandler);
        }
        l.enqueueProcedure = enqueueProcedure;
        if (cacheDatums) l.nodes = new HashMap<>();
        l.complete = complete;
        l.delaySat = delaySat;
        return l;
    }

    public static LTMS createLTMS(String title) {
        return createLTMS(title, null, false, true, null, null, true, CompleteMode.NONE, true);
    }

    public void changeLTMS(Function<TmsNode, String> nodeString, Boolean debugging,
                           Boolean checkingContradictions,
                           BiFunction<List<Clause>, LTMS, Boolean> contradictionHandler,
                           Consumer<Object> enqueueProcedure,
                           CompleteMode complete, Boolean delaySat) {
        if (nodeString != null) this.nodeString = nodeString;
        if (debugging != null) this.debugging = debugging;
        if (checkingContradictions != null) this.checkingContradictions = checkingContradictions;
        if (contradictionHandler != null) {
            this.contradictionHandlers = new ArrayList<>();
            this.contradictionHandlers.add(contradictionHandler);
        }
        if (enqueueProcedure != null) this.enqueueProcedure = enqueueProcedure;
        if (complete != null) this.complete = complete;
        if (delaySat != null) this.delaySat = delaySat;
    }

    // --- Node creation ---

    public TmsNode tmsCreateNode(Object datum, boolean assumptionp) {
        if (nodes != null && nodes.containsKey(datum)) {
            throw new LtmsError("Two nodes with same datum: " + datum);
        }
        nodeCounter++;
        TmsNode node = new TmsNode(nodeCounter, datum, assumptionp, this);
        if (nodes != null) nodes.put(datum, node);
        if (complete != CompleteMode.NONE && nodeCounter > consSize) {
            conses.clear();
            consSize += 50;
            for (int i = 0; i < consSize; i++) {
                conses.add(new Literal(null, null));
            }
        }
        return node;
    }

    // --- Assumption operations ---

    public static void enableAssumption(TmsNode node, NodeLabel label) {
        if (!node.isAssumption) throw new LtmsError("Can't enable non-assumption: " + node);
        if (node.label == label) {
            node.support = ENABLED_ASSUMPTION;
        } else if (node.isUnknown()) {
            topSetTruth(node, label, ENABLED_ASSUMPTION);
        } else {
            throw new LtmsError("Can't set already set node: " + node);
        }
    }

    public static void convertToAssumption(TmsNode node) {
        if (!node.isAssumption) {
            if (node.ltms.debugging)
                System.out.printf("%nConverting %s into an assumption", node.nodeString());
            node.isAssumption = true;
        }
    }

    public static void retractAssumption(TmsNode node) {
        if (node.isKnown() && ENABLED_ASSUMPTION.equals(node.support)) {
            List<TmsNode> unknowns = propagateUnknownness(node);
            findAlternativeSupport(node.ltms, unknowns);
        }
    }

    // --- Formula normalization ---

    static LTMS normLTMS;

    public static void addFormula(LTMS l, Object formula, Object informant) {
        Object inf;
        if (informant == null) {
            inf = Arrays.asList(":IMPLIED-BY", formula);
        } else {
            inf = Arrays.asList(":IMPLIED-BY", formula, informant);
        }
        List<List<Literal>> clauses = normalize(l, formula);
        for (List<Literal> clause : clauses) {
            List<Literal> simplified = simplifyClause(clause);
            if (simplified != null) {
                addClauseInternal(simplified, inf, true);
            }
        }
        checkForContradictions(l);
    }

    public static List<Literal> simplifyClause(List<Literal> literals) {
        if (literals == null || literals.isEmpty()) return literals;
        literals = sortClause(literals);
        List<Literal> result = new ArrayList<>();
        result.add(literals.get(0));
        for (int i = 1; i < literals.size(); i++) {
            Literal prev = result.get(result.size() - 1);
            Literal curr = literals.get(i);
            if (prev.node != curr.node) {
                result.add(curr);
            } else if (prev.sign != curr.sign) {
                return null; // tautology
            }
        }
        return result;
    }

    public static List<Literal> sortClause(List<Literal> literals) {
        List<Literal> cp = new ArrayList<>(literals);
        cp.sort(Comparator.comparingInt(a -> a.node.index));
        return cp;
    }

    @SuppressWarnings("unchecked")
    public static List<List<Literal>> normalize(LTMS l, Object exp) {
        normLTMS = l;
        return normalize1(exp, false);
    }

    @SuppressWarnings("unchecked")
    static List<List<Literal>> normalize1(Object exp, boolean negate) {
        if (exp instanceof List) {
            List<Object> lst = (List<Object>) exp;
            if (!lst.isEmpty() && lst.get(0) instanceof String) {
                String op = (String) lst.get(0);
                switch (op) {
                    case ":IMPLIES":
                        if (negate) {
                            List<List<Literal>> r1 = normalize1(lst.get(1), false);
                            List<List<Literal>> r2 = normalize1(lst.get(2), true);
                            r1.addAll(r2);
                            return r1;
                        }
                        return disjoin(normalize1(lst.get(1), true), normalize1(lst.get(2), false));
                    case ":IFF":
                        return normalizeIff(lst, negate);
                    case ":OR":
                        return negate ? normalizeConjunction(lst, true) : normalizeDisjunction(lst, false);
                    case ":AND":
                        return negate ? normalizeDisjunction(lst, true) : normalizeConjunction(lst, false);
                    case ":NOT":
                        return normalize1(lst.get(1), !negate);
                    case ":TAXONOMY":
                        return normalizeTax(lst, negate);
                }
            }
        }
        TmsNode node = findNode(normLTMS, exp);
        List<Literal> single = new ArrayList<>();
        single.add(negate ? node.falseLiteral : node.trueLiteral);
        List<List<Literal>> result = new ArrayList<>();
        result.add(single);
        return result;
    }

    static List<List<Literal>> normalizeTax(List<Object> exp, boolean negate) {
        List<Object> items = exp.subList(1, exp.size());
        List<Object> orClause = new ArrayList<>();
        orClause.add(":OR");
        orClause.addAll(items);
        List<Object> andParts = new ArrayList<>();
        andParts.add(":AND");
        andParts.add(orClause);
        for (int i = 0; i < items.size(); i++) {
            for (int j = i + 1; j < items.size(); j++) {
                andParts.add(Arrays.asList(":NOT", Arrays.asList(":AND", items.get(i), items.get(j))));
            }
        }
        return normalize1(andParts, negate);
    }

    static List<List<Literal>> normalizeConjunction(List<Object> exp, boolean negate) {
        List<List<Literal>> result = new ArrayList<>();
        for (int i = 1; i < exp.size(); i++) {
            result.addAll(normalize1(exp.get(i), negate));
        }
        return result;
    }

    static List<List<Literal>> normalizeIff(List<Object> exp, boolean negate) {
        List<List<Literal>> r1 = normalize1(Arrays.asList(":IMPLIES", exp.get(1), exp.get(2)), negate);
        List<List<Literal>> r2 = normalize1(Arrays.asList(":IMPLIES", exp.get(2), exp.get(1)), negate);
        r1.addAll(r2);
        return r1;
    }

    static List<List<Literal>> normalizeDisjunction(List<Object> exp, boolean negate) {
        if (exp.size() < 2) {
            List<List<Literal>> r = new ArrayList<>();
            r.add(new ArrayList<>());
            return r;
        }
        List<List<Literal>> result = normalize1(exp.get(1), negate);
        for (int i = 2; i < exp.size(); i++) {
            result = disjoin(normalize1(exp.get(i), negate), result);
        }
        return result;
    }

    static List<List<Literal>> disjoin(List<List<Literal>> conj1, List<List<Literal>> conj2) {
        if (conj1.isEmpty() && conj2.isEmpty()) return new ArrayList<>();
        List<List<Literal>> result = new ArrayList<>();
        for (List<Literal> d1 : conj1) {
            for (List<Literal> d2 : conj2) {
                List<Literal> combined = new ArrayList<>(d1);
                combined.addAll(d2);
                result.add(combined);
            }
        }
        return result;
    }

    public static TmsNode findNode(LTMS l, Object name) {
        if (name instanceof TmsNode) return (TmsNode) name;
        if (l.nodes != null) {
            TmsNode n = l.nodes.get(name);
            if (n != null) return n;
        }
        return l.tmsCreateNode(name, false);
    }

    // --- Adding clauses ---

    public static void addClause(List<TmsNode> trueNodes, List<TmsNode> falseNodes, Object informant) {
        List<Literal> lits = new ArrayList<>();
        for (TmsNode n : trueNodes) lits.add(n.trueLiteral);
        for (TmsNode n : falseNodes) lits.add(n.falseLiteral);
        addClauseInternal(lits, informant, false);
    }

    @SuppressWarnings("unchecked")
    public static void addClauseInternal(List<Literal> literals, Object informant, boolean internal) {
        if (literals.isEmpty()) throw new LtmsError("Total contradiction: Null clause");
        LTMS l = literals.get(0).node.ltms;
        if (l.complete != CompleteMode.NONE && l.fullAddClauseFn != null) {
            l.fullAddClauseFn.accept(l, literals, informant);
        } else {
            Clause cl = bcpAddClause(l, literals, informant, true);
            List<Clause> clist = (List<Clause>) l.clauses;
            clist.add(0, cl);
        }
        if (!internal) checkForContradictions(l);
    }

    public static Clause bcpAddClause(LTMS l, List<Literal> literals, Object informant, boolean index) {
        l.clauseCounter++;
        Clause cl = new Clause(l.clauseCounter, literals, informant, literals.size());
        for (Literal term : literals) {
            NodeLabel label = term.node.label;
            if (label == NodeLabel.UNKNOWN) cl.pvs++;
            if (term.sign == NodeLabel.TRUE) {
                if (index) term.node.trueClauses.add(0, cl);
                if (label == NodeLabel.TRUE) { cl.sats++; cl.pvs++; }
            } else {
                if (index) term.node.falseClauses.add(0, cl);
                if (label == NodeLabel.FALSE) { cl.sats++; cl.pvs++; }
            }
        }
        if (index) checkClauses(l, new ArrayList<>(Collections.singletonList(cl)));
        return cl;
    }

    public static void addNogood(TmsNode culprit, NodeLabel sign, List<TmsNode> assumptions) {
        List<TmsNode> trues = new ArrayList<>(), falses = new ArrayList<>();
        for (TmsNode a : assumptions) {
            NodeLabel lbl = (a == culprit) ? sign : a.label;
            if (lbl == NodeLabel.TRUE) falses.add(a);
            else if (lbl == NodeLabel.FALSE) trues.add(a);
        }
        addClause(trues, falses, "NOGOOD");
    }

    // --- BCP ---

    public static void checkClauses(LTMS l, List<Clause> clausesToCheck) {
        if (l.debugging) System.out.print("\n Beginning propagation...");
        while (!clausesToCheck.isEmpty()) {
            Clause cl = clausesToCheck.remove(0);
            List<Clause> newClauses = checkClause(l, cl);
            if (newClauses != null) clausesToCheck.addAll(newClauses);
        }
    }

    static List<Clause> checkClause(LTMS l, Clause clause) {
        if (clause.isViolated()) {
            if (!l.violatedClauses.contains(clause))
                l.violatedClauses.add(clause);
            return null;
        }
        if (clause.pvs == 1) {
            Literal unknown = findUnknownPair(clause);
            if (unknown != null)
                return setTruth(unknown.node, unknown.sign, clause);
        }
        return null;
    }

    static Literal findUnknownPair(Clause clause) {
        for (Literal term : clause.literals)
            if (term.node.isUnknown()) return term;
        return null;
    }

    public static void topSetTruth(TmsNode node, NodeLabel value, Object reason) {
        List<Clause> clausesToCheck = setTruth(node, value, reason);
        checkClauses(node.ltms, clausesToCheck);
        checkForContradictions(node.ltms);
    }

    public static List<Clause> setTruth(TmsNode node, NodeLabel value, Object reason) {
        LTMS l = node.ltms;
        if (l.debugging)
            System.out.printf("%n  Setting %s to %s, via %s.", node.nodeString(), value, reason);
        node.support = reason;
        node.label = value;
        List<Clause> clausesToCheck = new ArrayList<>();
        if (value == NodeLabel.TRUE) {
            if (l.enqueueProcedure != null) {
                for (Object rule : node.trueRules) l.enqueueProcedure.accept(rule);
                node.trueRules.clear();
            }
            for (Clause cl : node.trueClauses) cl.sats++;
            for (Clause cl : node.falseClauses) { cl.pvs--; if (cl.pvs < 2) clausesToCheck.add(cl); }
        } else if (value == NodeLabel.FALSE) {
            if (l.enqueueProcedure != null) {
                for (Object rule : node.falseRules) l.enqueueProcedure.accept(rule);
                node.falseRules.clear();
            }
            for (Clause cl : node.falseClauses) cl.sats++;
            for (Clause cl : node.trueClauses) { cl.pvs--; if (cl.pvs < 2) clausesToCheck.add(cl); }
        }
        return clausesToCheck;
    }

    // --- Retraction ---

    public static List<TmsNode> propagateUnknownness(TmsNode inNode) {
        LTMS l = inNode.ltms;
        List<TmsNode> unknownQueue = new ArrayList<>();
        Deque<TmsNode> forgetQueue = new ArrayDeque<>();
        forgetQueue.add(inNode);
        while (!forgetQueue.isEmpty()) {
            TmsNode node = forgetQueue.poll();
            unknownQueue.add(node);
            if (l.debugging) System.out.printf("%n Retracting %s.", node.nodeString());
            NodeLabel oldValue = node.label;
            node.label = NodeLabel.UNKNOWN;
            node.support = null;
            List<Clause> clauseList = (oldValue == NodeLabel.TRUE) ? node.falseClauses : node.trueClauses;
            for (Clause clause : clauseList) {
                clause.pvs++;
                if (clause.pvs == 2) {
                    TmsNode n2 = clauseConsequent(clause);
                    if (n2 != null) forgetQueue.add(n2);
                }
            }
            if (l.complete != CompleteMode.NONE && l.propagateMoreUnknownnessFn != null) {
                l.propagateMoreUnknownnessFn.accept(oldValue, node, l);
            }
        }
        return unknownQueue;
    }

    public static TmsNode clauseConsequent(Clause clause) {
        for (Literal tp : clause.literals) {
            if (tp.node.label == tp.sign) {
                return (clause == tp.node.support) ? tp.node : null;
            }
        }
        return null;
    }

    public static void findAlternativeSupport(LTMS l, List<TmsNode> nodes) {
        for (TmsNode node : nodes) {
            if (node.isUnknown()) {
                checkClauses(l, new ArrayList<>(node.trueClauses));
                checkClauses(l, new ArrayList<>(node.falseClauses));
            }
        }
        if (l.complete == CompleteMode.TRUE && l.ipiaFn != null) l.ipiaFn.accept(l);
    }

    // --- Contradiction handling ---

    public static void checkForContradictions(LTMS l) {
        l.violatedClauses.removeIf(c -> !c.isViolated());
        if (!l.violatedClauses.isEmpty())
            handleContradiction(l, new ArrayList<>(l.violatedClauses));
    }

    static void handleContradiction(LTMS l, List<Clause> violated) {
        if (!l.checkingContradictions) {
            l.pendingContradictions.removeIf(c -> !c.isViolated());
            for (Clause vc : violated) {
                if (vc.isViolated() && !l.pendingContradictions.contains(vc))
                    l.pendingContradictions.add(vc);
            }
        } else {
            for (var handler : l.contradictionHandlers) {
                if (handler.apply(violated, l)) return;
            }
        }
    }

    public static void withContradictionCheck(LTMS l, Runnable body) {
        boolean old = l.checkingContradictions;
        l.checkingContradictions = true;
        try { body.run(); } finally { l.checkingContradictions = old; }
    }

    public static void withoutContradictionCheck(LTMS l, Runnable body) {
        boolean old = l.checkingContradictions;
        l.checkingContradictions = false;
        try { body.run(); } finally { l.checkingContradictions = old; }
    }

    public static void withContradictionHandler(LTMS l,
            BiFunction<List<Clause>, LTMS, Boolean> handler, Runnable body) {
        l.contradictionHandlers.add(0, handler);
        try { body.run(); } finally { if (!l.contradictionHandlers.isEmpty()) l.contradictionHandlers.remove(0); }
    }

    public static void withAssumptions(List<TmsNode> nodes, List<NodeLabel> labels, Runnable body) {
        for (int i = 0; i < nodes.size(); i++) enableAssumption(nodes.get(i), labels.get(i));
        try { body.run(); } finally { for (TmsNode n : nodes) retractAssumption(n); }
    }

    // --- Support queries ---

    public static List<TmsNode> assumptionsOfNode(TmsNode node) {
        if (ENABLED_ASSUMPTION.equals(node.support)) return Collections.singletonList(node);
        if (node.isKnown() && node.support instanceof Clause)
            return assumptionsOfClause((Clause) node.support);
        return Collections.emptyList();
    }

    public static List<TmsNode> assumptionsOfClause(Clause inClause) {
        Object mark = new Object();
        Deque<Clause> clauseQueue = new ArrayDeque<>();
        clauseQueue.add(inClause);
        List<TmsNode> assumptions = new ArrayList<>();
        while (!clauseQueue.isEmpty()) {
            Clause clause = clauseQueue.poll();
            for (Literal tp : clause.literals) {
                TmsNode node = tp.node;
                if (node.mark == mark) continue;
                if (node.label != tp.sign) {
                    if (ENABLED_ASSUMPTION.equals(node.support)) {
                        assumptions.add(node);
                    } else if (node.support == null) {
                        throw new LtmsError("Node is unknown: " + node);
                    } else if (node.support instanceof Clause) {
                        clauseQueue.add((Clause) node.support);
                    }
                }
                node.mark = mark;
            }
        }
        return assumptions;
    }

    // --- User interface ---

    public static boolean askUserHandler(List<Clause> contradictions, LTMS l) {
        for (Clause c : contradictions) {
            if (c.isViolated()) handleOneContradiction(c);
        }
        return true;
    }

    static void handleOneContradiction(Clause violated) {
        List<TmsNode> asns = assumptionsOfClause(violated);
        if (asns.isEmpty()) throw new LtmsError("Global contradiction");
        System.out.println("\nContradiction found:");
        for (int i = 0; i < asns.size(); i++)
            System.out.printf("%n%d %s", i + 1, asns.get(i).nodeString());
        System.out.println("\nRetracting first assumption.");
        retractAssumption(asns.get(0));
    }

    public static boolean avoidAll(List<Clause> contradictions, LTMS l) {
        for (Clause c : contradictions) {
            if (c.isViolated()) {
                List<TmsNode> culprits = assumptionsOfClause(c);
                if (culprits.isEmpty()) throw new LtmsError("Total contradiction");
                TmsNode culprit = culprits.get(0);
                NodeLabel sign = culprit.label;
                retractAssumption(culprit);
                addNogood(culprit, sign, culprits);
            }
        }
        return true;
    }

    // --- Display ---

    public void walkClauses(Consumer<Clause> f) {
        if (complete != CompleteMode.NONE && walkTrieFn != null) {
            walkTrieFn.accept(f, clauses);
        } else if (clauses instanceof List) {
            @SuppressWarnings("unchecked")
            List<Clause> clist = (List<Clause>) clauses;
            for (Clause cl : clist) f.accept(cl);
        }
    }

    public static void whyNode(TmsNode node) {
        if (node.isUnknown()) {
            System.out.printf("%n%s is unknown.", node.nodeString());
            return;
        }
        if (ENABLED_ASSUMPTION.equals(node.support)) {
            System.out.printf("%n%s is %s <%s>", node.nodeString(), node.label, ENABLED_ASSUMPTION);
            return;
        }
        if (node.support instanceof Clause) {
            Clause cl = (Clause) node.support;
            System.out.printf("%n%s is %s via %s on", node.nodeString(), node.label, cl.informant);
            for (Literal tp : cl.literals) {
                if (tp.node.label != tp.sign)
                    System.out.printf("%n   %s is %s", tp.node.nodeString(), tp.node.label);
            }
        }
    }

    public static void whyNodes(LTMS l) {
        if (l.nodes != null) for (TmsNode n : l.nodes.values()) whyNode(n);
    }

    public static void explainNode(TmsNode node) {
        if (node.isUnknown()) return;
        int[] lineCount = {0};
        if (node.ltms.nodes != null)
            for (TmsNode n : node.ltms.nodes.values()) n.mark = null;
        explain1(node, lineCount);
    }

    static Object explain1(TmsNode node, int[] lineCount) {
        if (node.mark != null) return node.mark;
        if (ENABLED_ASSUMPTION.equals(node.support)) {
            lineCount[0]++;
            String lbl = node.isTrue() ? node.nodeString() : "(:NOT " + node.nodeString() + ")";
            System.out.printf("%n%3d %-15s %-15s   Assumption", lineCount[0], lbl, "()");
            node.mark = lineCount[0];
            return lineCount[0];
        }
        if (!(node.support instanceof Clause)) return null;
        Clause clause = (Clause) node.support;
        List<Object> anteResults = new ArrayList<>();
        for (Literal tp : clause.literals) {
            if (tp.node.support != clause) anteResults.add(explain1(tp.node, lineCount));
        }
        lineCount[0]++;
        String lbl = node.isTrue() ? node.nodeString() : "(:NOT " + node.nodeString() + ")";
        System.out.printf("%n%3d %-15s %-15s  ", lineCount[0], lbl, anteResults);
        prettyPrintClause(clause);
        node.mark = lineCount[0];
        return lineCount[0];
    }

    public static void prettyPrintClauses(LTMS l) {
        l.walkClauses(cl -> { System.out.print("\n "); prettyPrintClause(cl); });
    }

    public static void prettyPrintClause(Clause clause) {
        System.out.print("(:OR");
        for (Literal lit : clause.literals) {
            if (lit.sign == NodeLabel.TRUE)
                System.out.printf(" %s", lit.node.nodeString());
            else
                System.out.printf(" (:NOT %s)", lit.node.nodeString());
        }
        System.out.print(")");
    }

    public static String signedNodeString(TmsNode node) {
        if (node.isTrue()) return node.nodeString();
        if (node.isFalse()) return "Not[" + node.nodeString() + "]";
        return "Unknown[" + node.nodeString() + "]";
    }

    public static List<TmsNode> nodeConsequences(TmsNode node) {
        List<Clause> clauses = (node.label == NodeLabel.TRUE) ? node.falseClauses : node.trueClauses;
        List<TmsNode> conseqs = new ArrayList<>();
        for (Clause cl : clauses) {
            if (cl != node.support) {
                TmsNode c = clauseConsequent(cl);
                if (c != null) conseqs.add(c);
            }
        }
        return conseqs;
    }

    public static List<TmsNode> clauseAntecedents(Clause clause) {
        List<TmsNode> result = new ArrayList<>();
        for (Literal pair : clause.literals) {
            if (pair.node.support != clause) result.add(pair.node);
        }
        return result;
    }

    @Override
    public String toString() {
        return "#<LTMS: " + title + ">";
    }
}
