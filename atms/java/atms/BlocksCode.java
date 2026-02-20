// -*- Mode: Java -*-

// Test code for ATRE Blocksworld system
// Translated from bcode.lisp

// Copyright (c) 1990-1992 Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

import java.util.*;

/**
 * Blocks world problem builder and basis set generator.
 */
public class BlocksCode {

    /**
     * Build a complete blocks world planning problem.
     */
    public static Planner buildBlocksProblem(String title, List<String> blocksList,
                                               boolean debugging) {
        List<List<List<Object>>> basisSet = makeBlocksBasisSet(blocksList);
        Planner planner = Planner.createPlanningProblem(title, basisSet);
        planner.setDebug(debugging);

        // Register blocks world rules and operators
        Blocks.registerBlocksWorldRules(planner);

        Atre atre = planner.getAtre();

        // Assert that each item is a block
        for (String block : blocksList) {
            AtreData.assertFact(
                Arrays.asList("block", block),
                Collections.singletonList("Definition"),
                atre);
        }

        AtreRules.runRules(atre);
        planner.setupChoiceSets();

        return planner;
    }

    /**
     * Generate the choice sets (basis set) for a blocks world problem.
     * For each block generates:
     * 1. Where the block can be (holding, on table, or on another block)
     * 2. What can be on the block (holding, clear, or another block on it)
     * Plus a hand status choice set (hand-empty or holding some block)
     */
    public static List<List<List<Object>>> makeBlocksBasisSet(List<String> blocks) {
        List<List<List<Object>>> basis = new ArrayList<>();

        for (String block : blocks) {
            // What the block can be on
            List<List<Object>> onChoices = new ArrayList<>();
            onChoices.add(Arrays.asList("Holding", block));
            onChoices.add(Arrays.asList("On", block, "Table"));
            for (String other : blocks) {
                if (!other.equals(block)) {
                    onChoices.add(Arrays.asList("On", block, other));
                }
            }
            basis.add(onChoices);

            // What can be on the block
            List<List<Object>> topChoices = new ArrayList<>();
            topChoices.add(Arrays.asList("Holding", block));
            topChoices.add(Arrays.asList("Clear", block));
            for (String other : blocks) {
                if (!other.equals(block)) {
                    topChoices.add(Arrays.asList("ON", other, block));
                }
            }
            basis.add(topChoices);
        }

        // Hand status
        List<List<Object>> handChoices = new ArrayList<>();
        handChoices.add(Collections.singletonList("HAND-EMPTY"));
        for (String block : blocks) {
            handChoices.add(Arrays.asList("HOLDING", block));
        }
        basis.add(0, handChoices);

        return basis;
    }
}
