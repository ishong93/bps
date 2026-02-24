// JTMS - Justification-based Truth Maintenance System
// Translated from the Common Lisp version in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * The main JTMS class containing the truth maintenance system,
 * along with inner classes TmsNode and Just.
 */
public class JTMS {

    // -----------------------------------------------------------------------
    // Label constants corresponding to :IN and :OUT in the Lisp version.
    // -----------------------------------------------------------------------
    public static final String IN = "IN";
    public static final String OUT = "OUT";

    /**
     * Sentinel object used as support for enabled assumptions
     * (corresponds to :ENABLED-ASSUMPTION in the Lisp version).
     */
    public static final Object ENABLED_ASSUMPTION = ":ENABLED-ASSUMPTION";

    // -----------------------------------------------------------------------
    // Inner class: TmsNode
    // Corresponds to (defstruct tms-node ...) in the Lisp version.
    // -----------------------------------------------------------------------
    public static class TmsNode {
        private int index;
        private Object datum;
        private String label = OUT;
        private Object support;          // null, Just, or ENABLED_ASSUMPTION
        private List<Just> justs = new ArrayList<>();
        private List<Just> consequences = new ArrayList<>();
        private Object mark;
        private boolean contradictory;
        private boolean assumption;
        private boolean isDefault;       // true when assumption? was :DEFAULT
        private List<Object> inRules = new ArrayList<>();
        private List<Object> outRules = new ArrayList<>();
        private JTMS jtms;

        public TmsNode(int index, Object datum, boolean assumption,
                       boolean contradictory, JTMS jtms) {
            this.index = index;
            this.datum = datum;
            this.assumption = assumption;
            this.contradictory = contradictory;
            this.jtms = jtms;
        }

        // Getters and setters
        public int getIndex() { return index; }
        public Object getDatum() { return datum; }
        public String getLabel() { return label; }
        public void setLabel(String label) { this.label = label; }
        public Object getSupport() { return support; }
        public void setSupport(Object support) { this.support = support; }
        public List<Just> getJusts() { return justs; }
        public List<Just> getConsequences() { return consequences; }
        public Object getMark() { return mark; }
        public void setMark(Object mark) { this.mark = mark; }
        public boolean isContradictory() { return contradictory; }
        public void setContradictory(boolean contradictory) { this.contradictory = contradictory; }
        public boolean isAssumption() { return assumption; }
        public void setAssumption(boolean assumption) { this.assumption = assumption; }
        public boolean isDefault() { return isDefault; }
        public void setDefault(boolean isDefault) { this.isDefault = isDefault; }
        public List<Object> getInRules() { return inRules; }
        public void setInRules(List<Object> inRules) { this.inRules = inRules; }
        public List<Object> getOutRules() { return outRules; }
        public void setOutRules(List<Object> outRules) { this.outRules = outRules; }
        public JTMS getJtms() { return jtms; }

        @Override
        public String toString() {
            return "#<Node: " + nodeString(this) + ">";
        }
    }

    // -----------------------------------------------------------------------
    // Inner class: Just
    // Corresponds to (defstruct just ...) in the Lisp version.
    // -----------------------------------------------------------------------
    public static class Just {
        private int index;
        private Object informant;
        private TmsNode consequence;
        private List<TmsNode> antecedents;

        public Just(int index, Object informant, TmsNode consequence,
                    List<TmsNode> antecedents) {
            this.index = index;
            this.informant = informant;
            this.consequence = consequence;
            this.antecedents = antecedents;
        }

        public int getIndex() { return index; }
        public Object getInformant() { return informant; }
        public TmsNode getConsequence() { return consequence; }
        public List<TmsNode> getAntecedents() { return antecedents; }

        @Override
        public String toString() {
            return "#<Just " + index + ">";
        }
    }

    // -----------------------------------------------------------------------
    // Exception used for contradiction signaling (simulates CL throw/catch).
    // -----------------------------------------------------------------------
    public static class ContradictionException extends RuntimeException {
        public ContradictionException() { super("JTMS contradiction"); }
    }

    // -----------------------------------------------------------------------
    // JTMS fields
    // Corresponds to (defstruct jtms ...) in the Lisp version.
    // -----------------------------------------------------------------------
    private String title;
    private int nodeCounter = 0;
    private int justCounter = 0;
    private List<TmsNode> nodes = new ArrayList<>();
    private List<Just> justs = new ArrayList<>();
    private boolean debugging = false;
    private List<TmsNode> contradictions = new ArrayList<>();
    private List<TmsNode> assumptions = new ArrayList<>();
    private boolean checkingContradictions = true;
    private Function<TmsNode, String> nodeStringFunc;
    private BiConsumer<JTMS, List<TmsNode>> contradictionHandler;
    private Consumer<Object> enqueueProcedure;

    /**
     * Package-level equivalent of *contra-assumptions* special variable.
     */
    private static List<TmsNode> contraAssumptions;

    // -----------------------------------------------------------------------
    // Constructors and factory
    // -----------------------------------------------------------------------

    /**
     * Creates a new JTMS (corresponds to create-jtms).
     */
    public JTMS(String title) {
        this.title = title;
        this.nodeStringFunc = JTMS::defaultNodeString;
        this.contradictionHandler = JTMS::askUserHandler;
    }

    public JTMS(String title,
                Function<TmsNode, String> nodeStringFunc,
                boolean debugging,
                boolean checkingContradictions,
                BiConsumer<JTMS, List<TmsNode>> contradictionHandler,
                Consumer<Object> enqueueProcedure) {
        this.title = title;
        this.nodeStringFunc = nodeStringFunc != null ? nodeStringFunc : JTMS::defaultNodeString;
        this.debugging = debugging;
        this.checkingContradictions = checkingContradictions;
        this.contradictionHandler = contradictionHandler != null ? contradictionHandler : JTMS::askUserHandler;
        this.enqueueProcedure = enqueueProcedure;
    }

    @Override
    public String toString() {
        return "#<JTMS: " + title + ">";
    }

    // -----------------------------------------------------------------------
    // Getters / Setters (corresponds to change-jtms)
    // -----------------------------------------------------------------------
    public String getTitle() { return title; }
    public List<TmsNode> getNodes() { return nodes; }
    public List<Just> getJusts() { return justs; }
    public List<TmsNode> getContradictions() { return contradictions; }
    public List<TmsNode> getAssumptions() { return assumptions; }
    public boolean isDebugging() { return debugging; }
    public boolean isCheckingContradictions() { return checkingContradictions; }

    public void setNodeStringFunc(Function<TmsNode, String> fn) { this.nodeStringFunc = fn; }
    public void setDebugging(boolean debugging) { this.debugging = debugging; }
    public void setCheckingContradictions(boolean c) { this.checkingContradictions = c; }
    public void setContradictionHandler(BiConsumer<JTMS, List<TmsNode>> h) { this.contradictionHandler = h; }
    public void setEnqueueProcedure(Consumer<Object> p) { this.enqueueProcedure = p; }

    /**
     * Corresponds to change-jtms: modifies JTMS settings.
     */
    public void changeJtms(Function<TmsNode, String> nodeStringFunc,
                           Boolean debugging,
                           Boolean checkingContradictions,
                           BiConsumer<JTMS, List<TmsNode>> contradictionHandler,
                           Consumer<Object> enqueueProcedure) {
        if (nodeStringFunc != null) this.nodeStringFunc = nodeStringFunc;
        if (debugging != null) this.debugging = debugging;
        if (checkingContradictions != null) this.checkingContradictions = checkingContradictions;
        if (contradictionHandler != null) this.contradictionHandler = contradictionHandler;
        if (enqueueProcedure != null) this.enqueueProcedure = enqueueProcedure;
    }

    // -----------------------------------------------------------------------
    // Utility / query functions
    // -----------------------------------------------------------------------

    /** Default node-string function. */
    public static String defaultNodeString(TmsNode n) {
        return String.valueOf(n.getDatum());
    }

    /** Returns the printable string for a node using the JTMS's nodeStringFunc. */
    public static String nodeString(TmsNode node) {
        return node.getJtms().nodeStringFunc.apply(node);
    }

    /** Prints a debugging message if debugging is enabled. */
    private void debuggingJtms(String msg, Object... args) {
        if (debugging) {
            System.err.printf(msg, args);
        }
    }

    /** Throws a RuntimeException with a formatted message about a node. */
    public static void tmsError(String msg, TmsNode node) {
        throw new RuntimeException(String.format(msg, nodeString(node)));
    }

    /** Returns true if the node is a premise. */
    public static boolean isTmsNodePremise(TmsNode node) {
        Object support = node.getSupport();
        if (support == null) return false;
        if (support == ENABLED_ASSUMPTION) return false;
        if (support instanceof Just) {
            return ((Just) support).getAntecedents().isEmpty();
        }
        return false;
    }

    /** Returns true if the node's label is IN. */
    public static boolean inNode(TmsNode node) {
        return IN.equals(node.getLabel());
    }

    /** Returns true if the node's label is OUT. */
    public static boolean outNode(TmsNode node) {
        return OUT.equals(node.getLabel());
    }

    // -----------------------------------------------------------------------
    // Node creation
    // -----------------------------------------------------------------------

    /**
     * Creates a new TMS node (corresponds to tms-create-node).
     */
    public TmsNode createNode(Object datum, boolean assumptionp, boolean contradictoryp) {
        nodeCounter++;
        TmsNode node = new TmsNode(nodeCounter, datum, assumptionp, contradictoryp, this);
        if (assumptionp) {
            assumptions.add(0, node);
        }
        if (contradictoryp) {
            contradictions.add(0, node);
        }
        nodes.add(0, node);
        return node;
    }

    /**
     * Creates a default-assumption node (assumption? = :DEFAULT equivalent).
     */
    public TmsNode createDefaultNode(Object datum, boolean contradictoryp) {
        TmsNode node = createNode(datum, true, contradictoryp);
        node.setDefault(true);
        return node;
    }

    // -----------------------------------------------------------------------
    // Assumption management
    // -----------------------------------------------------------------------

    /**
     * Converts a node into an assumption and enables it (corresponds to assume-node).
     */
    public void assumeNode(TmsNode node) {
        if (!node.isAssumption()) {
            debuggingJtms("%nConverting %s into an assumption", nodeString(node));
            node.setAssumption(true);
        }
        enableAssumption(node);
    }

    /**
     * Marks a node as contradictory (corresponds to make-contradiction).
     */
    public void makeContradiction(TmsNode node) {
        if (!node.isContradictory()) {
            node.setContradictory(true);
            contradictions.add(0, node);
            checkForContradictions();
        }
    }

    // -----------------------------------------------------------------------
    // Justification
    // -----------------------------------------------------------------------

    /**
     * Adds a justification (corresponds to justify-node).
     */
    public void justifyNode(Object informant, TmsNode consequence, List<TmsNode> antecedents) {
        justCounter++;
        Just just = new Just(justCounter, informant, consequence, new ArrayList<>(antecedents));
        consequence.getJusts().add(0, just);
        for (TmsNode node : antecedents) {
            node.getConsequences().add(0, just);
        }
        justs.add(0, just);

        if (debugging) {
            List<String> names = new ArrayList<>();
            for (TmsNode a : antecedents) {
                names.add(nodeString(a));
            }
            debuggingJtms("%nJustifying %s by %s using %s.",
                    nodeString(consequence), informant, names);
        }

        if (!antecedents.isEmpty() || outNode(consequence)) {
            if (checkJustification(just)) {
                installSupport(consequence, just);
            }
        } else {
            consequence.setSupport(just);
        }
        checkForContradictions();
    }

    /** Checks whether a justification can fire (corresponds to check-justification). */
    public static boolean checkJustification(Just just) {
        return outNode(just.getConsequence()) && justificationSatisfied(just);
    }

    /** Returns true if all antecedents are IN (corresponds to justification-satisfied?). */
    public static boolean justificationSatisfied(Just just) {
        for (TmsNode ant : just.getAntecedents()) {
            if (!inNode(ant)) return false;
        }
        return true;
    }

    /** Makes the consequence IN and propagates (corresponds to install-support). */
    private void installSupport(TmsNode conseq, Just just) {
        makeNodeIn(conseq, just);
        propagateInness(conseq);
    }

    /** Propagates belief from a newly-IN node (corresponds to propagate-inness). */
    private void propagateInness(TmsNode node) {
        List<TmsNode> q = new ArrayList<>();
        q.add(node);
        while (!q.isEmpty()) {
            TmsNode current = q.remove(0);
            debuggingJtms("%n   Propagating belief in %s.", nodeString(current));
            for (Just justification : current.getConsequences()) {
                if (checkJustification(justification)) {
                    makeNodeIn(justification.getConsequence(), justification);
                    q.add(justification.getConsequence());
                }
            }
        }
    }

    /** Marks a node as IN with the given reason (corresponds to make-node-in). */
    private void makeNodeIn(TmsNode conseq, Object reason) {
        if (debugging) {
            Object reasonStr;
            if (reason instanceof Just && reason != ENABLED_ASSUMPTION) {
                Just just = (Just) reason;
                List<String> antNames = new ArrayList<>();
                for (TmsNode a : just.getAntecedents()) {
                    antNames.add(nodeStringFunc.apply(a));
                }
                reasonStr = "(" + just.getInformant() + " " + String.join(" ", antNames) + ")";
            } else {
                reasonStr = reason;
            }
            debuggingJtms("%n     Making %s in via %s.",
                    nodeString(conseq), reasonStr);
        }

        conseq.setLabel(IN);
        conseq.setSupport(reason);
        if (enqueueProcedure != null) {
            for (Object inRule : conseq.getInRules()) {
                enqueueProcedure.accept(inRule);
            }
            conseq.setInRules(new ArrayList<>());
        }
    }

    // -----------------------------------------------------------------------
    // Retraction
    // -----------------------------------------------------------------------

    /** Retracts a previously enabled assumption (corresponds to retract-assumption). */
    public void retractAssumption(TmsNode node) {
        if (node.getSupport() != ENABLED_ASSUMPTION) {
            return;
        }
        debuggingJtms("%n  Retracting assumption %s.", nodeString(node));
        makeNodeOut(node);
        List<TmsNode> outQueue = propagateOutness(node);
        outQueue.add(0, node);
        findAlternativeSupport(outQueue);
    }

    /** Enables an assumption node (corresponds to enable-assumption). */
    public void enableAssumption(TmsNode node) {
        if (!node.isAssumption()) {
            tmsError("Can't enable the non-assumption %s", node);
        }
        debuggingJtms("%n  Enabling assumption %s.", nodeString(node));
        if (outNode(node)) {
            makeNodeIn(node, ENABLED_ASSUMPTION);
            propagateInness(node);
        } else if (node.getSupport() == ENABLED_ASSUMPTION) {
            // Already an enabled assumption, do nothing.
        } else if (node.getSupport() instanceof Just
                   && ((Just) node.getSupport()).getAntecedents().isEmpty()) {
            // Supported by a premise justification, do nothing.
        } else {
            node.setSupport(ENABLED_ASSUMPTION);
        }
        checkForContradictions();
    }

    /** Marks a node as OUT (corresponds to make-node-out). */
    private void makeNodeOut(TmsNode node) {
        debuggingJtms("%n     Retracting belief in %s.", nodeString(node));
        node.setSupport(null);
        node.setLabel(OUT);
        if (enqueueProcedure != null) {
            for (Object outRule : node.getOutRules()) {
                enqueueProcedure.accept(outRule);
            }
        }
        node.setOutRules(new ArrayList<>());
    }

    /** Propagates disbelief (corresponds to propagate-outness). */
    private List<TmsNode> propagateOutness(TmsNode node) {
        debuggingJtms("%n   Propagating disbelief in %s.", nodeString(node));
        List<TmsNode> outQueue = new ArrayList<>();
        List<Just> js = new ArrayList<>(node.getConsequences());
        while (!js.isEmpty()) {
            Just j = js.remove(0);
            TmsNode conseq = j.getConsequence();
            if (conseq.getSupport() == j) {
                makeNodeOut(conseq);
                outQueue.add(conseq);
                js.addAll(conseq.getConsequences());
            }
        }
        return outQueue;
    }

    /** Looks for alternative supports (corresponds to find-alternative-support). */
    private void findAlternativeSupport(List<TmsNode> outQueue) {
        debuggingJtms("%n   Looking for alternative supports.");
        for (TmsNode node : outQueue) {
            if (!inNode(node)) {
                for (Just just : node.getJusts()) {
                    if (checkJustification(just)) {
                        installSupport(just.getConsequence(), just);
                        break;
                    }
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // Contradiction handling
    // -----------------------------------------------------------------------

    /** Checks for contradictions (corresponds to check-for-contradictions). */
    public void checkForContradictions() {
        if (!checkingContradictions) return;
        List<TmsNode> found = new ArrayList<>();
        for (TmsNode cnode : contradictions) {
            if (inNode(cnode)) {
                found.add(cnode);
            }
        }
        if (!found.isEmpty() && contradictionHandler != null) {
            contradictionHandler.accept(this, found);
        }
    }

    /**
     * Runs body with contradiction checking disabled, then restores
     * (corresponds to without-contradiction-check macro).
     */
    public void withoutContradictionCheck(Runnable body) {
        boolean oldValue = checkingContradictions;
        checkingContradictions = false;
        try {
            body.run();
        } finally {
            checkingContradictions = oldValue;
        }
    }

    /**
     * Runs body with contradiction checking enabled, then restores
     * (corresponds to with-contradiction-check macro).
     */
    public void withContradictionCheck(Runnable body) {
        boolean oldValue = checkingContradictions;
        checkingContradictions = true;
        try {
            body.run();
        } finally {
            checkingContradictions = oldValue;
        }
    }

    /**
     * Runs body with a temporary contradiction handler, then restores
     * (corresponds to with-contradiction-handler macro).
     */
    public void withContradictionHandler(BiConsumer<JTMS, List<TmsNode>> handler, Runnable body) {
        BiConsumer<JTMS, List<TmsNode>> oldHandler = contradictionHandler;
        contradictionHandler = handler;
        try {
            body.run();
        } finally {
            contradictionHandler = oldHandler;
        }
    }

    /**
     * Enables default assumptions, retracting any that cause contradictions
     * (corresponds to default-assumptions).
     */
    public void defaultAssumptions() {
        withContradictionCheck(() -> {
            withContradictionHandler((j, c) -> {
                throw new ContradictionException();
            }, () -> {
                for (TmsNode assumption : assumptions) {
                    if (assumption.getSupport() == ENABLED_ASSUMPTION) {
                        continue;
                    }
                    if (!assumption.isDefault()) {
                        continue;
                    }
                    boolean caught = false;
                    try {
                        enableAssumption(assumption);
                    } catch (ContradictionException e) {
                        caught = true;
                    }
                    if (caught) {
                        retractAssumption(assumption);
                    }
                }
            });
        });
    }

    // -----------------------------------------------------------------------
    // Query / inspection functions
    // -----------------------------------------------------------------------

    /** Returns the support for a node (corresponds to supporting-justification-for-node). */
    public static Object supportingJustificationForNode(TmsNode node) {
        return node.getSupport();
    }

    /**
     * Traces back through justifications to find the enabled assumptions
     * supporting a node (corresponds to assumptions-of-node).
     */
    public static List<TmsNode> assumptionsOfNode(TmsNode node) {
        List<TmsNode> assumptionsList = new ArrayList<>();
        Object marker = new Object(); // unique marker
        List<TmsNode> nodeQueue = new ArrayList<>();
        nodeQueue.add(node);
        while (!nodeQueue.isEmpty()) {
            TmsNode current = nodeQueue.remove(0);
            if (current.getMark() == marker) {
                continue;
            }
            current.setMark(marker);
            if (current.getSupport() == ENABLED_ASSUMPTION) {
                assumptionsList.add(current);
            } else if (inNode(current)) {
                if (current.getSupport() instanceof Just) {
                    nodeQueue.addAll(((Just) current.getSupport()).getAntecedents());
                }
            }
        }
        return assumptionsList;
    }

    /** Returns all currently enabled assumptions (corresponds to enabled-assumptions). */
    public List<TmsNode> enabledAssumptions() {
        List<TmsNode> result = new ArrayList<>();
        for (TmsNode assumption : assumptions) {
            if (assumption.getSupport() == ENABLED_ASSUMPTION) {
                result.add(assumption);
            }
        }
        return result;
    }

    /** Prints the reason a node is IN or OUT (corresponds to why-node). */
    public static TmsNode whyNode(TmsNode node) {
        Object justification = node.getSupport();
        if (justification == ENABLED_ASSUMPTION) {
            System.out.printf("%n%s is an enabled assumption", nodeString(node));
        } else if (justification instanceof Just) {
            Just just = (Just) justification;
            System.out.printf("%n%s is IN via %s on", nodeString(node), just.getInformant());
            for (TmsNode anode : just.getAntecedents()) {
                System.out.printf("%n  %s", nodeString(anode));
            }
        } else {
            System.out.printf("%n%s is OUT.", nodeString(node));
        }
        return node;
    }

    /** Prints the reason for every node (corresponds to why-nodes). */
    public void whyNodes() {
        for (TmsNode node : nodes) {
            whyNode(node);
        }
    }

    // -----------------------------------------------------------------------
    // Contradiction handler (interactive)
    // -----------------------------------------------------------------------

    /** Default contradiction handler (corresponds to ask-user-handler). */
    public static void askUserHandler(JTMS jtms, List<TmsNode> contradictionsList) {
        handleOneContradiction(contradictionsList.get(0));
        jtms.checkForContradictions();
    }

    /**
     * Presents the user with conflicting assumptions and asks which to retract
     * (corresponds to handle-one-contradiction).
     */
    public static void handleOneContradiction(TmsNode contraNode) {
        contraAssumptions = assumptionsOfNode(contraNode);
        if (contraAssumptions.isEmpty()) {
            tmsError("%nThere is a flaw in the universe...%s", contraNode);
        }
        System.out.printf("%nContradiction found: %s", nodeString(contraNode));
        printContraList(contraAssumptions);
        System.out.printf("%nCall tmsAnswer(<number>) to retract assumption.");

        Scanner scanner = new Scanner(System.in);
        System.out.print("\nJTMS contradiction break> ");
        if (scanner.hasNextLine()) {
            String text = scanner.nextLine().trim();
            try {
                int num = Integer.parseInt(text);
                tmsAnswer(num);
            } catch (NumberFormatException e) {
                System.out.printf("%nIgnoring answer, must be an integer.");
            }
        }
    }

    /** Prints a numbered list of nodes (corresponds to print-contra-list). */
    public static void printContraList(List<TmsNode> nodesList) {
        for (int i = 0; i < nodesList.size(); i++) {
            System.out.printf("%n%d %s", i + 1, nodeString(nodesList.get(i)));
        }
    }

    /** Processes a user's answer to a contradiction (corresponds to tms-answer). */
    public static void tmsAnswer(int num) {
        if (num <= 0) {
            System.out.printf("%nIgnoring answer, too small");
            return;
        }
        if (num > contraAssumptions.size()) {
            System.out.printf("%nIgnoring answer, too big.");
            return;
        }
        TmsNode node = contraAssumptions.get(num - 1);
        node.getJtms().retractAssumption(node);
    }

    // -----------------------------------------------------------------------
    // Interactive network exploration
    // -----------------------------------------------------------------------

    /** Allows interactive exploration of the support network (corresponds to explore-network). */
    public static TmsNode exploreNetwork(TmsNode node) {
        if (!inNode(node)) {
            System.out.printf("%n Sorry, %s not believed.", nodeString(node));
            return node;
        }
        Scanner scanner = new Scanner(System.in);
        List<TmsNode> stack = new ArrayList<>();
        TmsNode current = node;
        while (true) {
            whyNode(current);
            List<TmsNode> options;
            if (current.getSupport() instanceof Just) {
                options = ((Just) current.getSupport()).getAntecedents();
            } else {
                options = new ArrayList<>();
            }
            int olen = options.size();

            while (true) {
                System.out.print("\n>>>");
                if (!scanner.hasNextLine()) {
                    return current;
                }
                String text = scanner.nextLine().trim();
                if ("q".equals(text)) {
                    return current;
                }
                try {
                    int choice = Integer.parseInt(text);
                    if (choice < 0 || choice > olen) {
                        System.out.printf("%n Must be q or an integer from 0 to %d.", olen);
                        continue;
                    }
                    if (choice == 0) {
                        if (!stack.isEmpty()) {
                            current = stack.remove(stack.size() - 1);
                        } else {
                            return current;
                        }
                    } else {
                        stack.add(current);
                        current = options.get(choice - 1);
                    }
                    break;
                } catch (NumberFormatException e) {
                    System.out.printf("%n Must be q or an integer from 0 to %d.", olen);
                }
            }
        }
    }
}
