// -*- Mode: Java -*-

// ATRE: Tiny Rule Engine, with ATMS interface
// Module listing and initialization
// Translated from atre.lisp

// Copyright (c) 1992, Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, the Xerox Corporation
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

/**
 * Module listing and initialization for the ATRE system.
 * In Lisp this loaded and compiled files; in Java all classes
 * are compiled together, so this serves as documentation and
 * provides an initialization entry point.
 */
public class AtreLoader {

    /** ATRE core module names. */
    public static final String[] ATRE_MODULES = {
        "ATMS",       // ATMS core
        "Atre",       // Interface
        "AtreData",   // Database
        "AtreRules",  // Rule system
        "Unify",      // Variables and pattern matching
        "Funify",     // Open-coding of unification
        "AtreTest"    // Test procedures
    };

    /** Planner module names. */
    public static final String[] PLANNER_MODULES = {
        "Planner",      // Planning utilities
        "PlanA",        // Antecedent planner
        "PlanE",        // Envisioner
        "BlocksCode",   // Blocks World support
        "Blocks"        // Rules for Blocks World
    };

    /**
     * Initialize the planner subsystem.
     * Creates a dummy planning problem if none exists.
     */
    public static Planner initPlanner() {
        return Planner.createPlanningProblem("DUMMY", null);
    }
}
