// -*- Mode: Java -*-

// Blocks world rules for ATRE
// Translated from blocks.lisp

// Copyright (c) 1988-1992 Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

import java.util.*;

/**
 * Blocks world domain rules and operators.
 */
public class Blocks {

    /**
     * Register all blocks world constraint rules and operators.
     * In Lisp, these were macro-expanded rules loaded at runtime.
     * Here they are registered programmatically.
     */
    public static void registerBlocksWorldRules(Planner planner) {
        // Define operators

        // Pickup ?x: pick up block from table
        planner.defineOperator(
            Arrays.asList("Pickup", "?x"),
            Arrays.asList(
                Arrays.asList("on", "?x", "Table"),
                Arrays.asList("clear", "?x"),
                Collections.singletonList("hand-empty")),
            Collections.singletonList(
                Arrays.asList("holding", "?x")),
            Arrays.asList(
                Arrays.asList("on", "?x", "Table"),
                Arrays.asList("clear", "?x"),
                Collections.singletonList("hand-empty"))
        );

        // Putdown ?x: put block on table
        planner.defineOperator(
            Arrays.asList("Putdown", "?x"),
            Collections.singletonList(
                Arrays.asList("holding", "?x")),
            Arrays.asList(
                Arrays.asList("on", "?x", "Table"),
                Arrays.asList("clear", "?x"),
                Collections.singletonList("hand-empty")),
            Collections.singletonList(
                Arrays.asList("holding", "?x"))
        );

        // Stack ?x ?y: put ?x on ?y
        planner.defineOperator(
            Arrays.asList("Stack", "?x", "?y"),
            Arrays.asList(
                Arrays.asList("holding", "?x"),
                Arrays.asList("clear", "?y")),
            Arrays.asList(
                Collections.singletonList("hand-empty"),
                Arrays.asList("on", "?x", "?y"),
                Arrays.asList("clear", "?x")),
            Arrays.asList(
                Arrays.asList("holding", "?x"),
                Arrays.asList("clear", "?y"))
        );

        // Unstack ?x ?y: remove ?x from ?y
        planner.defineOperator(
            Arrays.asList("Unstack", "?x", "?y"),
            Arrays.asList(
                Collections.singletonList("hand-empty"),
                Arrays.asList("clear", "?x"),
                Arrays.asList("on", "?x", "?y")),
            Arrays.asList(
                Arrays.asList("holding", "?x"),
                Arrays.asList("clear", "?y")),
            Arrays.asList(
                Collections.singletonList("hand-empty"),
                Arrays.asList("clear", "?x"),
                Arrays.asList("on", "?x", "?y"))
        );
    }
}
