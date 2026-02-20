// -*- Mode: Java -*-

// Tests for ATRE
// Translated from atret.lisp

// Copyright (c) 1992 Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, the Xerox Corporation
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collections;

/**
 * Test code demonstrating ATRE usage with rules and inference.
 */
public class AtreTest {

    /**
     * Test 1: INTERN rules with assumptions.
     * Demonstrates forward-chaining inference:
     *   (implies (sentient-robot Robbie) (Human Robbie))
     *   (implies (human Robbie) (mortal Robbie))
     *   (sentient-robot Robbie)
     *   => (Human Robbie), (mortal Robbie)
     */
    public static Atre atreTest1(boolean debugging) {
        Atre atre = Atre.createAtre("Test ATRE", debugging);

        // In the Lisp version, a rule macro expands to register
        // a pattern-matching rule. Here we demonstrate the API calls directly.

        // Assume facts
        AtreData.assumeFact(
            Arrays.asList("implies",
                Arrays.asList("sentient-robot", "Robbie"),
                Arrays.asList("Human", "Robbie")),
            "no-bias", atre);

        AtreData.assumeFact(
            Arrays.asList("implies",
                Arrays.asList("human", "Robbie"),
                Arrays.asList("mortal", "Robbie")),
            "sigh", atre);

        AtreData.assumeFact(
            Arrays.asList("sentient-robot", "Robbie"),
            "sort-of", atre);

        AtreRules.runRules(atre);
        return atre;
    }

    /**
     * Test 2: INTERN rules with assertions (not assumptions).
     */
    public static Atre atreTest2(boolean debugging) {
        Atre atre = Atre.createAtre("Test ATRE", debugging);

        AtreData.assertFact(
            Arrays.asList("implies",
                Arrays.asList("sentient-robot", "Robbie"),
                Arrays.asList("Human", "Robbie")),
            Collections.singletonList("no-bias"), atre);

        AtreData.assertFact(
            Arrays.asList("implies",
                Arrays.asList("human", "Robbie"),
                Arrays.asList("mortal", "Robbie")),
            Collections.singletonList("sigh"), atre);

        AtreData.assumeFact(
            Arrays.asList("sentient-robot", "Robbie"),
            "sort-of", atre);

        AtreRules.runRules(atre);
        return atre;
    }

    /**
     * Test 3: IN rules.
     */
    public static Atre atreTest3(boolean debugging) {
        Atre atre = Atre.createAtre("Test ATRE", debugging);

        AtreData.assertFact(
            Arrays.asList("implies",
                Arrays.asList("sentient-robot", "Robbie"),
                Arrays.asList("Human", "Robbie")),
            Collections.singletonList("no-bias"), atre);

        AtreData.assertFact(
            Arrays.asList("implies",
                Arrays.asList("human", "Robbie"),
                Arrays.asList("mortal", "Robbie")),
            Collections.singletonList("sigh"), atre);

        AtreData.assumeFact(
            Arrays.asList("sentient-robot", "Robbie"),
            "sort-of", atre);

        AtreRules.runRules(atre);
        return atre;
    }

    /**
     * Test 4: IMPLIED-BY rules.
     */
    public static Atre atreTest4(boolean debugging) {
        PrintWriter out = new PrintWriter(System.out, true);
        Atre atre = Atre.createAtre("Test ATRE", debugging);

        AtreData.assumeFact(
            Arrays.asList("implies",
                Arrays.asList("sentient-robot", "Robbie"),
                Arrays.asList("Human", "Robbie")),
            "no-bias", atre);

        AtreData.assertFact(
            Arrays.asList("implies",
                Arrays.asList("human", "Robbie"),
                Arrays.asList("mortal", "Robbie")),
            Collections.singletonList("sigh"), atre);

        AtreData.assumeFact(
            Arrays.asList("sentient-robot", "Robbie"),
            "sort-of", atre);

        AtreRules.runRules(atre);

        AtreData.showData(atre, out);
        atre.getAtms().printEnvs(out);

        return atre;
    }

    /**
     * Test 5: Contradiction rules.
     * Tests mortality/immortality contradiction.
     */
    public static Atre atreTest5(boolean debugging) {
        PrintWriter out = new PrintWriter(System.out, true);
        Atre atre = Atre.createAtre("Test ATRE", debugging);

        AtreData.assumeFact(
            Arrays.asList("sentient", "Robbie"),
            "sort-of", atre);

        AtreData.assumeFact(
            Arrays.asList("immortal", "Robbie"),
            "why-not", atre);

        AtreData.assertFact(
            Arrays.asList("implies",
                Arrays.asList("sentient", "Robbie"),
                Arrays.asList("Human", "Robbie")),
            Collections.singletonList("no-bias"), atre);

        AtreData.assertFact(
            Arrays.asList("implies",
                Arrays.asList("human", "Robbie"),
                Arrays.asList("mortal", "Robbie")),
            Collections.singletonList("sigh"), atre);

        AtreRules.runRules(atre);

        out.println("\nNogoods:");
        atre.getAtms().printNogoods(out);

        return atre;
    }

    /**
     * Run all tests.
     */
    public static void main(String[] args) {
        System.out.println("=== ATRE Test 1 ===");
        atreTest1(true);

        System.out.println("\n\n=== ATRE Test 2 ===");
        atreTest2(true);

        System.out.println("\n\n=== ATRE Test 4 ===");
        atreTest4(true);

        System.out.println("\n\n=== ATRE Test 5 ===");
        atreTest5(true);
    }
}
