package ltms;

import java.util.*;

public class IndirectProof {

    public static boolean tryIndirectProof(Object fact, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        if (LTRE.isKnown(fact, ltre)) return true;
        final LTRE finalLtre = ltre;

        LTMS.withContradictionHandler(ltre.ltms,
            (contradictions, ltms) -> {
                List<TmsNode> assumptions = LTMS.assumptionsOfClause(contradictions.get(0));
                TmsNode theNode = null;
                TmsNode factNode = LTRE.getTmsNode(fact, finalLtre);
                for (TmsNode asn : assumptions) {
                    if (asn == factNode) { theNode = asn; break; }
                }
                if (theNode != null) {
                    NodeLabel status = theNode.label;
                    LTMS.retractAssumption(theNode);
                    LTMS.addNogood(theNode, status, assumptions);
                    return true;
                }
                return false;
            },
            () -> {
                LTRE.assuming(Collections.singletonList(Arrays.asList(":NOT", fact)), finalLtre, () -> {
                    LtreRules.runRules(finalLtre);
                });
            });

        return LTRE.isKnown(fact, ltre);
    }
}
