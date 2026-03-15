package ltms;

import java.util.*;

public class SetRule {

    public static void loadSetRules(LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        LTRE old = LTRE.currentLTRE;
        LTRE.currentLTRE = ltre;
        try {
            // Rule 1: When (SET ?name) is true, install sub-rules
            LtreRules.makeRule(Arrays.asList("SET", "?name"), "TRUE",
                (bindings, triggerNode) -> {
                    Object name = bindings.get("?name");

                    // Sub-rule: When (?name MEMBERS ?construal1) is interned
                    LtreRules.makeRule(Arrays.asList(name, "MEMBERS", "?construal1"), "INTERN",
                        (bindings2, node2) -> {
                            Object construal1 = bindings2.get("?construal1");

                            // Sub-sub-rule: (?name HAS-MEMBER ?new) not in construal1
                            LtreRules.makeRule(Arrays.asList(name, "HAS-MEMBER", "?new"), "INTERN",
                                (bindings3, node3) -> {
                                    Object newMember = bindings3.get("?new");
                                    if (construal1 == null) return;
                                    // Check not already in set
                                    if (construal1 instanceof List) {
                                        for (Object m : (List<?>) construal1) {
                                            if (LTRE.equalForms(m, newMember)) return;
                                        }
                                    }
                                    Datum setDatum = LTRE.referent(Arrays.asList("SET", name), false, LTRE.currentLTRE);
                                    Datum membersDatum = LTRE.referent(Arrays.asList(name, "MEMBERS", construal1), false, LTRE.currentLTRE);
                                    Datum newDatum = LTRE.referent(Arrays.asList(name, "HAS-MEMBER", newMember), false, LTRE.currentLTRE);
                                    if (setDatum != null && membersDatum != null && newDatum != null) {
                                        LTRE.assertFact(Arrays.asList(":IMPLIES",
                                            Arrays.asList(":AND", setDatum.tmsNode, membersDatum.tmsNode),
                                            Arrays.asList(":NOT", newDatum.tmsNode)),
                                            ":NOT-IN-SET", LTRE.currentLTRE);
                                    }
                                });

                            // Sub-sub-rule: Construal uniqueness
                            LtreRules.makeRule(Arrays.asList(name, "MEMBERS", "?construal2"), "INTERN",
                                (bindings4, node4) -> {
                                    Object construal2 = bindings4.get("?construal2");
                                    if (LTRE.equalForms(construal1, construal2)) return;
                                    Datum d1 = LTRE.referent(Arrays.asList(name, "MEMBERS", construal1), false, LTRE.currentLTRE);
                                    Datum d2 = LTRE.referent(Arrays.asList(name, "MEMBERS", construal2), false, LTRE.currentLTRE);
                                    if (d1 == null || d2 == null || d1.counter >= d2.counter) return;
                                    if (!hasSymmetricDifference(construal1, construal2)) return;
                                    Datum setDatum = LTRE.referent(Arrays.asList("SET", name), false, LTRE.currentLTRE);
                                    if (setDatum != null) {
                                        LTRE.assertFact(Arrays.asList(":NOT",
                                            Arrays.asList(":AND", setDatum.tmsNode, d1.tmsNode, d2.tmsNode)),
                                            ":CONSTRUAL-UNIQUENESS", LTRE.currentLTRE);
                                    }
                                });
                        });
                });

            // Rule 2: CWA-JUSTIFICATION rule
            LtreRules.makeRule(Arrays.asList("CWA-JUSTIFICATION", "?ante", "?conse"), "INTERN",
                (bindings, node) -> {
                    Object ante = bindings.get("?ante");
                    Object conse = bindings.get("?conse");
                    Datum cwajDatum = LTRE.referent(Arrays.asList("CWA-JUSTIFICATION", ante, conse), false, LTRE.currentLTRE);
                    if (cwajDatum != null) {
                        LTRE.assertFact(Arrays.asList(":IMPLIES",
                            cwajDatum.tmsNode,
                            Arrays.asList(":IMPLIES", ante, conse)),
                            ":CWA-JUSTIFICATION", LTRE.currentLTRE);
                    }
                });
        } finally {
            LTRE.currentLTRE = old;
        }
    }

    @SuppressWarnings("unchecked")
    static boolean hasSymmetricDifference(Object a, Object b) {
        if (!(a instanceof List) || !(b instanceof List)) return !LTRE.equalForms(a, b);
        List<Object> aList = (List<Object>) a;
        List<Object> bList = (List<Object>) b;
        for (Object ae : aList) {
            boolean found = false;
            for (Object be : bList) { if (LTRE.equalForms(ae, be)) { found = true; break; } }
            if (!found) return true;
        }
        for (Object be : bList) {
            boolean found = false;
            for (Object ae : aList) { if (LTRE.equalForms(ae, be)) { found = true; break; } }
            if (!found) return true;
        }
        return false;
    }
}
