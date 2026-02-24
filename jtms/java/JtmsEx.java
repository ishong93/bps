// Examples for Justification-based TMS.
// Translated from jtms-ex.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.Arrays;

/**
 * JtmsEx contains example functions demonstrating JTMS usage.
 */
public class JtmsEx {

    /** Find a node by datum. Corresponds to get-node. */
    public static JTMS.TmsNode getNode(Object datum, JTMS jtms) {
        for (JTMS.TmsNode node : jtms.getNodes()) {
            if (JData.deepEqual(datum, node.getDatum())) return node;
        }
        return null;
    }

    /** Find a justification by index. Corresponds to get-justification. */
    public static JTMS.Just getJustification(int num, JTMS jtms) {
        for (JTMS.Just just : jtms.getJusts()) {
            if (just.getIndex() == num) return just;
        }
        return null;
    }

    /** Result holder for ex1. */
    public static class Ex1Result {
        public JTMS jtms;
        public JTMS.TmsNode na, nb, nc, nd, ne, nf, ng;
    }

    /** Example 1: Simple JTMS with assumptions and justifications. */
    public static Ex1Result ex1() {
        JTMS jtms = new JTMS("Simple Example", null, true, true, null, null);
        JTMS.TmsNode na = jtms.createNode("a", true, false);
        JTMS.TmsNode nb = jtms.createNode("b", true, false);
        JTMS.TmsNode nc = jtms.createNode("c", true, false);
        JTMS.TmsNode nd = jtms.createNode("d", true, false);
        JTMS.TmsNode ne = jtms.createNode("e", true, false);
        JTMS.TmsNode nf = jtms.createNode("f", true, false);
        JTMS.TmsNode ng = jtms.createNode("g", true, false);

        jtms.justifyNode("j1", nf, Arrays.asList(na, nb));
        jtms.justifyNode("j2", ne, Arrays.asList(nb, nc));
        jtms.justifyNode("j3", ng, Arrays.asList(na, ne));
        jtms.justifyNode("j4", ng, Arrays.asList(nd, ne));

        jtms.enableAssumption(na);
        jtms.enableAssumption(nb);
        jtms.enableAssumption(nc);
        jtms.enableAssumption(nd);

        Ex1Result r = new Ex1Result();
        r.jtms = jtms; r.na = na; r.nb = nb; r.nc = nc;
        r.nd = nd; r.ne = ne; r.nf = nf; r.ng = ng;
        return r;
    }

    /** Example 2: Add contradiction to ex1. */
    public static JTMS.TmsNode ex2(Ex1Result r) {
        JTMS.TmsNode contra = r.jtms.createNode("Loser", false, true);
        r.jtms.justifyNode("j5", contra, Arrays.asList(r.ne, r.nf));
        return contra;
    }

    /** Example 3: Multiple support with contradictions. */
    public static void ex3() {
        JTMS jtms = new JTMS("Multiple support example");
        JTMS.TmsNode assumptionA = jtms.createNode("A", true, false);
        JTMS.TmsNode assumptionC = jtms.createNode("C", true, false);
        JTMS.TmsNode assumptionE = jtms.createNode("E", true, false);
        JTMS.TmsNode nodeH = jtms.createNode("h", false, false);

        jtms.enableAssumption(assumptionA);
        jtms.enableAssumption(assumptionC);
        jtms.enableAssumption(assumptionE);

        jtms.justifyNode("R1", nodeH, Arrays.asList(assumptionC, assumptionE));

        JTMS.TmsNode nodeG = jtms.createNode("g", false, false);
        jtms.justifyNode("R2", nodeG, Arrays.asList(assumptionA, assumptionC));

        JTMS.TmsNode contradiction = jtms.createNode("CONTRADICTION", false, true);
        jtms.justifyNode("R3", contradiction, Arrays.asList(nodeG));

        System.out.println("Ex3 completed. Check node states with whyNodes.");
        jtms.whyNodes();
    }

    public static void main(String[] args) {
        System.out.println("=== Example 1 ===");
        Ex1Result r = ex1();
        r.jtms.whyNodes();

        System.out.println("\n=== Example 2 ===");
        ex2(r);

        System.out.println("\n=== Example 3 ===");
        ex3();
    }
}
