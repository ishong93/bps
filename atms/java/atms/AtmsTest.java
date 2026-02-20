// -*- Mode: Java -*-

// ATMS test code
// Translated from atest.lisp

// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collections;

/**
 * Test code demonstrating ATMS usage.
 */
public class AtmsTest {

    private ATMS atms;
    private TmsNode a, b, c, d, e, f;

    /**
     * Basic ATMS test with assumptions and justifications.
     * Creates nodes A-F, assumes A,B,C, and justifies:
     *   J1: D <- A, B
     *   J2: E <- B, C
     *   J3: F <- D, E
     */
    public void atmsTest1() {
        atms = ATMS.createATMS("atms-test1", true);

        a = atms.createNode("A", false, false);
        b = atms.createNode("B", false, false);
        c = atms.createNode("C", false, false);
        d = atms.createNode("D", false, false);
        e = atms.createNode("E", false, false);
        f = atms.createNode("F", false, false);

        atms.assumeNode(a);
        atms.assumeNode(b);
        atms.assumeNode(c);

        atms.justifyNode("J1", d, Arrays.asList(a, b));
        atms.justifyNode("J2", e, Arrays.asList(b, c));
        atms.justifyNode("J3", f, Arrays.asList(d, e));
    }

    /**
     * Add a simpler justification for D (run after test1).
     */
    public void atmsTest2() {
        atms.justifyNode("simpler", d, Collections.singletonList(a));
    }

    /**
     * Mark {A, B} as nogood (run after test1).
     */
    public void atmsTest3() {
        atms.nogoodNodes("atms-test3", Arrays.asList(a, b));
    }

    /**
     * Example from de Kleer's paper by Gitchang.
     * Demonstrates label maintenance through justifications and nogoods.
     */
    public static void step1() {
        PrintWriter out = new PrintWriter(System.out, true);

        ATMS atms = ATMS.createATMS("Step-1", false);

        TmsNode a = atms.createNode("A", false, false);
        TmsNode b = atms.createNode("B", false, false);
        TmsNode c = atms.createNode("C", false, false);
        TmsNode x1 = atms.createNode("x=1", false, false);
        TmsNode yx = atms.createNode("y=x", false, false);
        TmsNode xz = atms.createNode("x=z", false, false);
        TmsNode y1 = atms.createNode("y=1", false, false);
        TmsNode z1 = atms.createNode("z=1", false, false);

        atms.assumeNode(a);
        atms.assumeNode(b);
        atms.assumeNode(c);

        atms.justifyNode("j1", x1, Collections.singletonList(a));
        atms.justifyNode("j2", yx, Collections.singletonList(b));
        atms.justifyNode("j3", xz, Collections.singletonList(c));

        atms.whyNodes(out);
        atms.printEnvs(out);

        out.println("\n\nNow register nogood{A,B}");
        atms.nogoodNodes("NOGOOD", Arrays.asList(a, b));
        atms.whyNodes(out);
        atms.printEnvs(out);

        out.println("\n\nx=1, y=x => y=1");
        atms.justifyNode("j4", y1, Arrays.asList(x1, yx));
        atms.whyNodes(out);
        atms.printEnvs(out);

        out.println("\n\nWe have a premise z=1");
        atms.justifyNode("Premise", z1, Collections.emptyList());
        atms.whyNodes(out);
        atms.printEnvs(out);

        out.println("\n\nz=1, x=z => x=1");
        atms.justifyNode("j5", x1, Arrays.asList(z1, xz));
        atms.whyNodes(out);
        atms.printEnvs(out);
    }

    /**
     * Run all tests.
     */
    public static void main(String[] args) {
        System.out.println("=== ATMS Test 1 ===");
        AtmsTest test = new AtmsTest();
        test.atmsTest1();
        PrintWriter out = new PrintWriter(System.out, true);
        test.atms.whyNodes(out);
        test.atms.printEnvs(out);

        System.out.println("\n\n=== Step 1 (de Kleer) ===");
        step1();
    }
}
