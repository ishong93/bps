package ltms;

import java.util.*;

public class DDS {
    public static boolean debugDDS = false;

    public static void ddSearch(List<List<Object>> choiceSets, Runnable end, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        if (choiceSets == null || choiceSets.isEmpty()) {
            if (debugDDS) System.out.println("\n    DDS: Found solution.");
            end.run();
            return;
        }

        List<Object> choices = choiceSets.get(0);
        List<List<Object>> remaining = choiceSets.subList(1, choiceSets.size());
        Object marker = new Object();
        final LTRE finalLtre = ltre;

        for (Object choice : choices) {
            if (debugDDS) System.out.printf("\n    DDS: Considering %s...", choice);

            if (LTRE.isFalse(choice, ltre)) {
                if (debugDDS) System.out.printf("\n    DDS: %s already known nogood.", choice);
                continue;
            }

            if (LTRE.isTrue(choice, ltre)) {
                if (debugDDS) System.out.printf("\n    DDS: %s true by implication.", choice);
                ddSearch(remaining, end, ltre);
                return;
            }

            if (debugDDS) System.out.printf("\n    DDS: Assuming %s.", choice);

            final Object[] answer = {null};
            final Object finalChoice = choice;

            LTMS.withContradictionHandler(ltre.ltms,
                (clauses, ltms) -> {
                    for (Clause cl : clauses) {
                        List<TmsNode> asns = LTMS.assumptionsOfClause(cl);
                        for (TmsNode asn : asns) {
                            Object viewedNode = LTRE.viewNode(asn);
                            boolean matchesChoice = LTRE.equalForms(finalChoice, viewedNode);
                            if (!matchesChoice && finalChoice instanceof List) {
                                List<?> choiceLst = (List<?>) finalChoice;
                                if (choiceLst.size() == 2 && ":NOT".equals(choiceLst.get(0))) {
                                    matchesChoice = LTRE.equalForms(choiceLst.get(1), viewedNode);
                                }
                            }
                            if (matchesChoice) {
                                List<Object> losers = new ArrayList<>();
                                for (TmsNode a : asns) {
                                    if (a != asn) losers.add(LTRE.signedViewNode(a));
                                }
                                answer[0] = losers;
                                return true;
                            }
                        }
                    }
                    return false;
                },
                () -> {
                    LTRE.assuming(Collections.singletonList(finalChoice), finalLtre, () -> {
                        LtreRules.runRules(finalLtre);
                        ddSearch(remaining, end, finalLtre);
                    });
                });

            if (answer[0] instanceof List) {
                @SuppressWarnings("unchecked")
                List<Object> losers = (List<Object>) answer[0];
                if (debugDDS) System.out.printf("\n    DDS: %s inconsistent.", choice);
                List<Object> nogoodParts = new ArrayList<>();
                nogoodParts.add(choice);
                nogoodParts.addAll(losers);
                List<Object> nogood = new ArrayList<>();
                nogood.add(":NOT");
                List<Object> andParts = new ArrayList<>();
                andParts.add(":AND");
                andParts.addAll(nogoodParts);
                nogood.add(andParts);
                LTRE.assertFact(nogood, ":DD-SEARCH-NOGOOD", finalLtre);
            }
        }
    }
}
