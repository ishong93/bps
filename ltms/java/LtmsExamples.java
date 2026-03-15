package ltms;

import java.util.*;

/**
 * Test/example functions for the LTMS.
 * Converted from ltms-ex.lisp.
 */
public class LtmsExamples {

    public static void testExplain() {
        LTMS ltms = LTMS.createLTMS("Explain Example");
        ltms.tmsCreateNode("x", true);
        LTMS.addFormula(ltms, Arrays.asList(":OR", "x", "y"), null);
        LTMS.addFormula(ltms, Arrays.asList(":OR", Arrays.asList(":NOT", "y"), "z"), null);
        LTMS.addFormula(ltms, Arrays.asList(":OR", Arrays.asList(":NOT", "z"), "r"), null);
        LTMS.enableAssumption(LTMS.findNode(ltms, "x"), NodeLabel.FALSE);
        LTMS.explainNode(LTMS.findNode(ltms, "r"));
    }

    public static void testFormula(LTMS.CompleteMode complete) {
        LTMS ltms = LTMS.createLTMS("Formula", null, false, true, null, null, true,
                complete != null ? complete : LTMS.CompleteMode.NONE, true);
        TmsNode r = ltms.tmsCreateNode("r", false);
        TmsNode s = ltms.tmsCreateNode("s", false);
        TmsNode t = ltms.tmsCreateNode("t", false);
        TmsNode u = ltms.tmsCreateNode("u", false);
        LTMS.addFormula(ltms, Arrays.asList(":IMPLIES",
                Arrays.asList(":AND", r, Arrays.asList(":IMPLIES", s, t)), u), null);
    }

    public static void testAsk() {
        LTMS ltms = LTMS.createLTMS("Testing asking");
        TmsNode n1 = ltms.tmsCreateNode("N1", true);
        TmsNode n2 = ltms.tmsCreateNode("N2", true);
        LTMS.enableAssumption(n1, NodeLabel.FALSE);
        LTMS.enableAssumption(n2, NodeLabel.FALSE);
        LTMS.addFormula(ltms, Arrays.asList(":OR", n1, n2), null);
        LTMS.whyNodes(ltms);
    }

    public static void testAvoidAll() {
        LTMS ltms = LTMS.createLTMS("Testing avoid all", null, false, true,
                LTMS::avoidAll, null, true, LTMS.CompleteMode.NONE, true);
        TmsNode n1 = ltms.tmsCreateNode("N1", true);
        TmsNode n2 = ltms.tmsCreateNode("N2", true);
        LTMS.enableAssumption(n1, NodeLabel.FALSE);
        LTMS.enableAssumption(n2, NodeLabel.FALSE);
        LTMS.addFormula(ltms, Arrays.asList(":OR", n1, n2), null);
        LTMS.whyNodes(ltms);
    }

    public static void test1(boolean complete) {
        LTMS ltms = LTMS.createLTMS("TEST1", null, false, true, null, null, true,
                complete ? LTMS.CompleteMode.TRUE : LTMS.CompleteMode.NONE, true);
        if (complete) CompleteLTMS.installCompleteLTMS(ltms);
        TmsNode x = ltms.tmsCreateNode("x", false);
        TmsNode y = ltms.tmsCreateNode("y", false);
        LTMS.addFormula(ltms, Arrays.asList(":OR", x, y), null);
        LTMS.addFormula(ltms, Arrays.asList(":OR", x, Arrays.asList(":NOT", y)), null);
        CompleteLTMS.completeLTMS(ltms);
        if (!x.isTrue()) throw new LtmsError("TEST1 failed");
        System.out.println("TEST1 passed.");
    }

    public static void testBug() {
        LTMS ltms = LTMS.createLTMS("BUG check");
        TmsNode x = ltms.tmsCreateNode("x", true);
        TmsNode y = ltms.tmsCreateNode("y", true);
        TmsNode z = ltms.tmsCreateNode("z", false);
        LTMS.addFormula(ltms, Arrays.asList(":OR", x, z), null);
        LTMS.addFormula(ltms, Arrays.asList(":OR", y, z), null);
        LTMS.enableAssumption(x, NodeLabel.FALSE);
        LTMS.enableAssumption(y, NodeLabel.FALSE);
        LTMS.whyNodes(ltms);
        LTMS.retractAssumption(x);
        LTMS.whyNodes(ltms);
    }

    public static void testBug1(boolean complete) {
        LTMS ltms = LTMS.createLTMS("BUG check", null, false, true, null, null, true,
                complete ? LTMS.CompleteMode.TRUE : LTMS.CompleteMode.NONE, true);
        if (complete) CompleteLTMS.installCompleteLTMS(ltms);
        TmsNode x = ltms.tmsCreateNode("x", true);
        TmsNode y = ltms.tmsCreateNode("y", true);
        TmsNode z = ltms.tmsCreateNode("z", false);
        LTMS.addFormula(ltms, Arrays.asList(":OR", x, z), null);
        LTMS.addFormula(ltms, Arrays.asList(":OR", y, z), null);
        LTMS.enableAssumption(x, NodeLabel.FALSE);
        LTMS.enableAssumption(y, NodeLabel.FALSE);
        LTMS.whyNodes(ltms);
        LTMS.retractAssumption(x);
        LTMS.whyNodes(ltms);
    }

    public static void testTax(int n, boolean complete) {
        LTMS ltms = LTMS.createLTMS("taxing", null, false, true, null, null, true,
                complete ? LTMS.CompleteMode.TRUE : LTMS.CompleteMode.NONE, true);
        if (complete) CompleteLTMS.installCompleteLTMS(ltms);
        List<Object> nodes = new ArrayList<>();
        for (int i = 0; i < n; i++) nodes.add(ltms.tmsCreateNode(i, false));
        List<Object> formula = new ArrayList<>();
        formula.add(":TAXONOMY");
        formula.addAll(nodes);
        LTMS.addFormula(ltms, formula, null);
        System.out.printf("\n %d prime implicates%n", ltms.clauseCounter);
    }

    public static void testE(boolean complete) {
        LTMS ltms = LTMS.createLTMS("example", null, true, true, null, null, true,
                complete ? LTMS.CompleteMode.TRUE : LTMS.CompleteMode.NONE, true);
        if (complete) CompleteLTMS.installCompleteLTMS(ltms);
        TmsNode a = ltms.tmsCreateNode("a", true);
        TmsNode b = ltms.tmsCreateNode("b", true);
        TmsNode c = ltms.tmsCreateNode("c", true);
        TmsNode d = ltms.tmsCreateNode("d", true);
        TmsNode e = ltms.tmsCreateNode("e", true);
        LTMS.addFormula(ltms, Arrays.asList(":OR", Arrays.asList(":NOT", a), b), null);
        LTMS.addFormula(ltms, Arrays.asList(":OR", Arrays.asList(":NOT", c), d), null);
        LTMS.addFormula(ltms, Arrays.asList(":OR", Arrays.asList(":NOT", c), e), null);
        LTMS.addFormula(ltms, Arrays.asList(":OR", Arrays.asList(":NOT", b),
                Arrays.asList(":NOT", d), Arrays.asList(":NOT", e)), null);
    }

    public static void runTests() {
        System.out.println("Running LTMS tests...");
        testAsk();
        testAvoidAll();
        testBug();
        testBug1(true);
        testTax(3, true);
        System.out.println("\nAll LTMS tests completed.");
    }
}
