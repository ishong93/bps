// N-Queens puzzle solver using dependency-directed search with JTRE.
// Translated from jqueens.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * JQueens implements the N-Queens puzzle using JTRE
 * with dependency-directed backtracking.
 */
public class JQueens {
    private static int nAssumptions = 0;
    private static List<List<Object>> placements = new ArrayList<>();

    /** Run N-Queens from `from` to `to`. Corresponds to test-queens. */
    public static void testQueens(int from, int to) {
        for (int n = from; n <= to; n++) {
            long start = System.currentTimeMillis();
            nQueens(n, false);
            long elapsed = System.currentTimeMillis() - start;
            System.out.printf("%n For n=%d, %d solutions, %d assumptions. (%dms)",
                n, placements.size(), nAssumptions, elapsed);
        }
    }

    /** Solve N-Queens. Returns number of solutions. Corresponds to n-queens. */
    public static int nQueens(int n, boolean debugging) {
        Jtre jtre = setupQueensPuzzle(n, debugging);
        List<List<Object>> choiceSets = makeQueensChoiceSets(n);
        solveQueensPuzzle(choiceSets, jtre);
        return placements.size();
    }

    /** Setup the puzzle. Corresponds to setup-queens-puzzle. */
    public static Jtre setupQueensPuzzle(int n, boolean debugging) {
        Jtre jtre = new Jtre(String.format("%d-Queens JTRE", n), debugging);
        placements = new ArrayList<>();
        nAssumptions = 0;
        JQRule.registerQueensRules(jtre);
        return jtre;
    }

    /** Create choice sets. Corresponds to make-queens-choice-sets. */
    public static List<List<Object>> makeQueensChoiceSets(int n) {
        List<List<Object>> choiceSets = new ArrayList<>();
        for (int col = 1; col <= n; col++) {
            List<Object> columnQueens = new ArrayList<>();
            for (int row = 1; row <= n; row++) {
                columnQueens.add(Arrays.asList("Queen", col, row));
            }
            choiceSets.add(columnQueens);
        }
        return choiceSets;
    }

    /** Solve using dependency-directed search. Corresponds to solve-queens-puzzle. */
    public static void solveQueensPuzzle(List<List<Object>> choiceSets, Jtre jtre) {
        if (choiceSets.isEmpty()) {
            gatherQueensSolution(jtre);
            return;
        }
        for (Object choice : choiceSets.get(0)) {
            List<Object> negChoice = Arrays.asList("not", choice);
            if (JData.isIn(negChoice, jtre)) continue;

            boolean[] nogood = {false};
            List<Object>[] asns = new List[]{null};

            tryInContext(choice, () -> {
                solveQueensPuzzle(choiceSets.subList(1, choiceSets.size()), jtre);
            }, jtre, nogood, asns);

            nAssumptions++;
            if (nogood[0] && asns[0] != null) {
                List<Object> filtered = asns[0].stream()
                    .filter(a -> !JData.deepEqual(a, choice))
                    .collect(Collectors.toList());
                List<Object> justification = new ArrayList<>();
                justification.add("Nogood");
                justification.addAll(filtered);
                JData.jassert(negChoice, justification, jtre);
            }
        }
    }

    /** Try assumption in context with contradiction handler. */
    private static void tryInContext(Object asn, Runnable thunk, Jtre jtre,
                                      boolean[] nogoodOut, List<Object>[] asnsOut) {
        Object tryMarker = Arrays.asList("TRY", asn);
        final boolean[] done = {false};

        jtre.getJtms().withContradictionHandler((jtmsArg, contras) -> {
            for (JTMS.TmsNode cnode : contras) {
                List<JTMS.TmsNode> assumptions = jtmsArg.assumptionsOfNode(cnode);
                JTMS.TmsNode asnNode = JData.getTmsNode(asn, jtre);
                if (assumptions.contains(asnNode)) {
                    JData.jretract(asn, tryMarker, true, jtre);
                    nogoodOut[0] = true;
                    asnsOut[0] = assumptions.stream()
                        .map(n -> (Object) JData.viewNode(n))
                        .collect(Collectors.toList());
                    done[0] = true;
                    return;
                }
            }
        }, () -> {
            if (JData.isIn(asn, jtre)) return;
            JData.jassume(asn, tryMarker, jtre);
            if (done[0]) return;
            JRules.runRules(jtre);
            if (done[0]) return;
            thunk.run();
            if (!done[0]) {
                JData.jretract(asn, tryMarker, true, jtre);
            }
        });
    }

    /** Check if two queens are non-attacking. Corresponds to queens-okay?. */
    public static boolean queensOkay(int x1, int y1, int x2, int y2) {
        return y1 != y2 && Math.abs(x1 - x2) != Math.abs(y1 - y2);
    }

    /** Gather a solution. Corresponds to gather-queens-solution. */
    private static void gatherQueensSolution(Jtre jtre) {
        // Collect all Queen facts that are IN
        List<Object> solution = new ArrayList<>();
        Dbclass queenClass = JData.getDbclass(Arrays.asList("Queen"), jtre);
        for (Datum d : queenClass.getFacts()) {
            if (JTMS.inNode(d.getTmsNode())) {
                solution.add(d.getLispForm());
            }
        }
        placements.add(solution);
    }

    /** Display a solution. Corresponds to show-queens-solution. */
    @SuppressWarnings("unchecked")
    public static void showQueensSolution(List<Object> solution) {
        int n = solution.size();
        for (int i = 0; i < n; i++) {
            System.out.println();
            for (int j = 0; j < n; j++) {
                boolean found = false;
                for (Object q : solution) {
                    if (q instanceof List) {
                        List<Object> ql = (List<Object>) q;
                        if (ql.size() == 3 && JData.deepEqual(ql.get(1), i)
                            && JData.deepEqual(ql.get(2), j)) {
                            found = true;
                            break;
                        }
                    }
                }
                System.out.print(found ? "Q" : "-");
            }
        }
    }

    public static int getAssumptions() { return nAssumptions; }
    public static List<List<Object>> getPlacements() { return placements; }
}
