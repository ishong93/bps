package ltms;

import java.util.*;

public class LtmsAcceptTest {

    public static void testLTRE() {
        LTRE.inLTRE(LTRE.createLTRE("Debugging LTRE", false));
        System.out.println("\nTesting database/LTMS link...");
        testDatums();
        System.out.println("\nTesting LTMS...");
        testClauses();
        System.out.println("\nTesting Rule system...");
        testRules();
        System.out.println("\nAll tests passed!");
    }

    static void testDatums() {
        LTRE.assertFact("foo", "testing", LTRE.currentLTRE);
        if (!LTRE.isTrue("foo", LTRE.currentLTRE)) throw new LtmsError("Fact installation glitch");
        LTRE.assertFact(Arrays.asList(":NOT", "bar"), "testing", LTRE.currentLTRE);
        if (!LTRE.isFalse("bar", LTRE.currentLTRE)) throw new LtmsError("Negation glitch");
        System.out.println("  Datums: OK");
    }

    static void testClauses() {
        LTRE.assertFact(Arrays.asList(":OR", "a", "b"), "case-split", LTRE.currentLTRE);
        LTRE.assertFact(Arrays.asList(":IMPLIES", "a", "c"), "why-not?", LTRE.currentLTRE);
        LTRE.assumeFact(Arrays.asList(":IMPLIES", "c", "d"), "what-the-heck", LTRE.currentLTRE);
        LTRE.assumeFact(Arrays.asList(":NOT", "b"), "for-fun", LTRE.currentLTRE);
        if (!LTRE.isTrue("d", LTRE.currentLTRE)) throw new LtmsError("Propagation glitch");
        LTRE.retractFact(Arrays.asList(":NOT", "b"), "for-fun", LTRE.currentLTRE, false);
        if (!LTRE.isUnknown("d", LTRE.currentLTRE)) throw new LtmsError("Retraction glitch");
        LTRE.assumeFact(Arrays.asList(":NOT", "b"), "for-fun", LTRE.currentLTRE);
        if (!LTRE.isTrue("d", LTRE.currentLTRE)) throw new LtmsError("Unouting glitch");
        LTRE.retractFact(Arrays.asList(":IMPLIES", "c", "d"), "what-the-heck", LTRE.currentLTRE, false);
        if (!LTRE.isUnknown("d", LTRE.currentLTRE)) throw new LtmsError("Retraction glitch 2");
        LTRE.assumeFact(Arrays.asList(":IMPLIES", "c", "d"), "what-the-heck", LTRE.currentLTRE);
        if (!LTRE.isTrue("d", LTRE.currentLTRE)) throw new LtmsError("Unouting glitch 2");
        System.out.println("  Clauses: OK");
    }

    static void testRules() {
        // Rule 1: TRUE triggers
        LtreRules.makeRule(Arrays.asList("foo", "?x"), "TRUE",
            (bindings, f1Node) -> {
                Object x = bindings.get("?x");
                LtreRules.makeRule(Arrays.asList("bar", "?y"), "TRUE",
                    (bindings2, f2Node) -> {
                        Object y = bindings2.get("?y");
                        LTRE.assertFact(Arrays.asList(":IMPLIES",
                            Arrays.asList(":AND", LTRE.viewNode(f1Node), LTRE.viewNode(f2Node)),
                            Arrays.asList("mumble", x, y)),
                            "hack", LTRE.currentLTRE);
                    });
            });

        // Rule 2: INTERN triggers
        LtreRules.makeRule(Arrays.asList("foo", "?x"), "INTERN",
            (bindings, f1Node) -> {
                Object x = bindings.get("?x");
                LtreRules.makeRule(Arrays.asList("bar", "?y"), "INTERN",
                    (bindings2, f2Node) -> {
                        Object y = bindings2.get("?y");
                        LTRE.assertFact(Arrays.asList(":IMPLIES",
                            Arrays.asList(":AND",
                                LTRE.referent(Arrays.asList("foo", x), false, LTRE.currentLTRE).tmsNode,
                                LTRE.referent(Arrays.asList("bar", y), false, LTRE.currentLTRE).tmsNode),
                            Arrays.asList("grumble", x, y)),
                            "hack", LTRE.currentLTRE);
                    });
            });

        LTRE.referent(Arrays.asList("foo", 1), true, LTRE.currentLTRE);
        LTRE.referent(Arrays.asList("bar", 1), true, LTRE.currentLTRE);
        LtreRules.runRules(LTRE.currentLTRE);

        if (LTRE.referent(Arrays.asList("grumble", 1, 1), false, LTRE.currentLTRE) == null)
            throw new LtmsError("Intern triggering failure");

        LTRE.assumeFact(Arrays.asList("foo", 1), "why-not?", LTRE.currentLTRE);
        LTRE.assumeFact(Arrays.asList(":NOT", Arrays.asList("bar", 1)), "monkeywrench", LTRE.currentLTRE);
        LtreRules.runRules(LTRE.currentLTRE);

        if (LTRE.isTrue(Arrays.asList("mumble", 1, 1), LTRE.currentLTRE))
            throw new LtmsError("Badly conditioned triggering");

        LTRE.retractFact(Arrays.asList(":NOT", Arrays.asList("bar", 1)), "monkeywrench", LTRE.currentLTRE, false);
        LtreRules.runRules(LTRE.currentLTRE);

        LTRE.assumeFact(Arrays.asList("bar", 1), "why", LTRE.currentLTRE);
        LtreRules.runRules(LTRE.currentLTRE);

        if (!LTRE.isTrue(Arrays.asList("mumble", 1, 1), LTRE.currentLTRE))
            throw new LtmsError("Badly conditioned triggering - 2");

        System.out.println("  Rules: OK");
    }
}
