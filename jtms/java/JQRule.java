// N-Queens rules for JTRE.
// Translated from jqrule.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * JQRule registers the queen capture detection rules.
 */
public class JQRule {

    /** Register queens rules. Corresponds to jqrule.lisp. */
    public static void registerQueensRules(Jtre jtre) {
        // Declare Queens-capture as contradiction
        JData.contradiction(Arrays.asList("Queens-capture"), jtre);

        // Register capture detection rule
        Dbclass queenDbclass = JData.getDbclass(Arrays.asList("Queen"), jtre);
        JRules.insertRule(queenDbclass,
            // Matcher
            p -> {
                if (!(p instanceof List)) return new Rule.MatchResult(false, null, false);
                List<?> ps = (List<?>) p;
                if (ps.size() != 3 || !"Queen".equals(ps.get(0)))
                    return new Rule.MatchResult(false, null, false);
                List<Object> bindings = new ArrayList<>();
                bindings.add(ps.get(1)); // column
                bindings.add(ps.get(2)); // row
                return new Rule.MatchResult(true, bindings, true);
            },
            // Body
            args -> {
                if (args.size() < 3) return;
                JTMS.TmsNode q1Node = (JTMS.TmsNode) args.get(0);
                int col1 = ((Number) args.get(1)).intValue();
                int row1 = ((Number) args.get(2)).intValue();

                for (Datum datum : queenDbclass.getFacts()) {
                    if (!(datum.getLispForm() instanceof List)) continue;
                    List<?> qs = (List<?>) datum.getLispForm();
                    if (qs.size() != 3 || !"Queen".equals(qs.get(0))) continue;
                    int col2 = ((Number) qs.get(1)).intValue();
                    int row2 = ((Number) qs.get(2)).intValue();
                    if (col1 == col2) continue;
                    if (JQueens.queensOkay(col1, row1, col2, row2)) continue;
                    JTMS.TmsNode q2Node = datum.getTmsNode();
                    JTMS.TmsNode captureNode = JData.referent(
                        Arrays.asList("Queens-capture"), true, jtre).getTmsNode();
                    jtre.getJtms().justifyNode(
                        Arrays.asList("Death", q1Node, q2Node),
                        captureNode,
                        Arrays.asList(q1Node, q2Node));
                }
            });
    }
}
