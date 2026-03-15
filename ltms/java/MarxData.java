package ltms;

import java.util.*;

public class MarxData {

    public static void loadMarxData(LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        LTRE old = LTRE.currentLTRE;
        LTRE.currentLTRE = ltre;
        try {
            // Pairwise nogoods
            LTRE.assertFact(Arrays.asList("PAIRWISE-NOGOOD", "PLAYS-PIANO", "PLAYS-HARP"), "CONSTRAINT", ltre);
            LTRE.assertFact(Arrays.asList("PAIRWISE-NOGOOD", "PLAYS-PIANO", "SMOOTH-TALKER"), "CONSTRAINT", ltre);
            LTRE.assertFact(Arrays.asList("PAIRWISE-NOGOOD", "PLAYS-HARP", "SMOOTH-TALKER"), "CONSTRAINT", ltre);
            LTRE.assertFact(Arrays.asList("PAIRWISE-NOGOOD", "LIKES-GAMBLING", "LIKES-ANIMALS"), "CONSTRAINT", ltre);
            LTRE.assertFact(Arrays.asList("PAIRWISE-NOGOOD", "SMOOTH-TALKER", "LIKES-GAMBLING"), "CONSTRAINT", ltre);

            // Same entity
            LTRE.assertFact(Arrays.asList("SAME-ENTITY", "LIKES-ANIMALS", "PLAYS-HARP"), "CONSTRAINT", ltre);

            // Known facts
            LTRE.assertFact(Arrays.asList(":NOT", Arrays.asList("LIKES-ANIMALS", "GROUCHO")), "CONSTRAINT", ltre);
            LTRE.assertFact(Arrays.asList(":NOT", Arrays.asList("SMOOTH-TALKER", "HARPO")), "CONSTRAINT", ltre);
            LTRE.assertFact(Arrays.asList("PLAYS-PIANO", "CHICO"), "CONSTRAINT", ltre);

            // Rule: pairwise-nogood
            LtreRules.makeRule(Arrays.asList("PAIRWISE-NOGOOD", "?attribute1", "?attribute2"), "TRUE",
                (bindings, horNode) -> {
                    Object attr1 = bindings.get("?attribute1");
                    Object attr2 = bindings.get("?attribute2");

                    LtreRules.makeRule(Arrays.asList(attr1, "?obj"), "TRUE",
                        (bindings2, f1Node) -> {
                            Object obj = bindings2.get("?obj");

                            LtreRules.makeRule(Arrays.asList(attr2, obj), "TRUE",
                                (bindings3, f2Node) -> {
                                    LTRE.assertFact(Arrays.asList(":NOT",
                                        Arrays.asList(":AND",
                                            LTRE.viewNode(horNode),
                                            LTRE.viewNode(f1Node),
                                            LTRE.viewNode(f2Node))),
                                        "PAIRWISE-NOGOOD-RULE", LTRE.currentLTRE);
                                });
                        });
                });

            // Rule: same-entity
            LtreRules.makeRule(Arrays.asList("SAME-ENTITY", "?attribute1", "?attribute2"), "TRUE",
                (bindings, horNode) -> {
                    Object attr1 = bindings.get("?attribute1");
                    Object attr2 = bindings.get("?attribute2");

                    LtreRules.makeRule(Arrays.asList(attr1, "?obj"), "TRUE",
                        (bindings2, f1Node) -> {
                            Object obj = bindings2.get("?obj");
                            LTRE.assertFact(Arrays.asList(":IMPLIES",
                                Arrays.asList(":AND",
                                    LTRE.viewNode(horNode),
                                    LTRE.viewNode(f1Node)),
                                Arrays.asList(attr2, obj)),
                                "SAME-ENTITY-RULE", LTRE.currentLTRE);
                        });
                });

            LtreRules.runRules(ltre);
        } finally {
            LTRE.currentLTRE = old;
        }
    }
}
