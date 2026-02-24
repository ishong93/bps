// JTRE Loader and module listing.
// Translated from jtre.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.Arrays;
import java.util.List;

/**
 * JtreLoader lists module files and provides initialization helpers.
 */
public class JtreLoader {

    /** Core JTRE module files. Corresponds to *jtre-files*. */
    public static final List<String> JTRE_FILES = Arrays.asList(
        "jtms",   // JTMS
        "jinter", // Interface
        "jdata",  // Database
        "jrules", // Rule system
        "funify"  // Open-coding unification
    );

    /** N-Queens module files. Corresponds to *jqueens-files*. */
    public static final List<String> JQUEENS_FILES = Arrays.asList(
        "jqueens", // JTRE version of N-queens puzzle
        "jqrule"   // Contradiction detection rule
    );

    /** JSAINT module files. Corresponds to *jsaint-files*. */
    public static final List<String> JSAINT_FILES = Arrays.asList(
        "jsaint",   // JSAINT main program
        "match",    // Math-oriented pattern matcher
        "simplify", // Algebraic simplifier
        "jsrules",  // Bookkeeping rules
        "jsops"     // Sample integration library
    );

    /** Initialize JTRE for N-Queens. */
    public static void initQueens(Jtre jtre) {
        JQRule.registerQueensRules(jtre);
    }

    /** Initialize JTRE for JSAINT. */
    public static void initJSaint(Jtre jtre) {
        JSRules.registerRules(jtre);
    }

    public static void main(String[] args) {
        System.out.println("JTRE Modules: " + JTRE_FILES);
        System.out.println("JQueens Modules: " + JQUEENS_FILES);
        System.out.println("JSaint Modules: " + JSAINT_FILES);
    }
}
