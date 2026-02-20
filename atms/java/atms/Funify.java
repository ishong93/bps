// -*- Mode: Java -*-
//
// Extra pattern-matching facilities for FTRE.
// Translated from funify.lisp, last edited 1/29/93 by KDF.
//
// Copyright (c) 1988-1992, Kenneth D. Forbus and Johan de Kleer.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package atms;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import atms.Unify.Cons;

/**
 * Extra pattern-matching facilities for FTRE.
 *
 * <p>Provides compile-time-style helpers that manipulate symbolic
 * s-expression patterns at runtime.  The functions here build
 * explicit test and binding structures used by the rule compiler.
 *
 * <p>In the original Lisp code these are macro-expansion helpers;
 * here they are translated as regular static methods that operate
 * on {@link Cons}-based s-expressions.
 */
public class Funify {

    // ---------------------------------------------------------------
    // Thread-local *bound-vars* — the Lisp special variable.
    // ---------------------------------------------------------------

    /**
     * Corresponds to the Lisp special variable {@code *bound-vars*}.
     * Holds the list of variables that are considered bound in the
     * current expansion context.  Use {@link #getBoundVars()} and
     * {@link #setBoundVars(List)} to access it.
     */
    private static final ThreadLocal<List<String>> boundVars =
        ThreadLocal.withInitial(ArrayList::new);

    /** Get the current bound-vars list. */
    public static List<String> getBoundVars() {
        return boundVars.get();
    }

    /** Set the current bound-vars list. */
    public static void setBoundVars(List<String> vars) {
        boundVars.set(vars);
    }

    // ---------------------------------------------------------------
    // Result type for generate-match-body (multiple return values).
    // ---------------------------------------------------------------

    /**
     * Return type for {@link #generateMatchBody}, corresponding to
     * the Lisp {@code (values ...)} of that function.
     */
    public static class MatchBody {
        private final List<Object> tests;
        private final List<Object> bindingSpecs;

        public MatchBody(List<Object> tests, List<Object> bindingSpecs) {
            this.tests = tests;
            this.bindingSpecs = bindingSpecs;
        }

        /** The combined list of structure, equality, and extra tests. */
        public List<Object> getTests() { return tests; }

        /** The list of binding path specifications. */
        public List<Object> getBindingSpecs() { return bindingSpecs; }
    }

    // ---------------------------------------------------------------
    // quotize
    // ---------------------------------------------------------------

    /**
     * Translate a pattern into an s-expression that, when evaluated,
     * would reconstruct the pattern with variables left unquoted.
     *
     * <p>Corresponds to:
     * <pre>
     * (defun quotize (pattern) ...)
     * </pre>
     *
     * @param pattern an s-expression (possibly containing variables)
     * @return an s-expression with QUOTE/cons wrappers
     */
    public static Object quotize(Object pattern) {
        // (null pattern) => nil
        if (pattern == null) {
            return null;
        }
        // (variable? pattern) => pattern
        if (Unify.isVariable(pattern)) {
            return pattern;
        }
        // (not (listp pattern)) => (list 'QUOTE pattern)
        if (!(pattern instanceof Cons)) {
            return Unify.list("QUOTE", pattern);
        }
        Cons c = (Cons) pattern;
        // ((eq (car pattern) :EVAL) => (cadr pattern))
        if (":EVAL".equals(c.getCar()) || "EVAL".equals(c.getCar())) {
            // cadr = car of cdr
            return Unify.car(c.getCdr());
        }
        // (t `(cons ,(quotize (car pattern)) ,(quotize (cdr pattern))))
        return Unify.list("CONS",
                          quotize(c.getCar()),
                          quotize(c.getCdr()));
    }

    // ---------------------------------------------------------------
    // pattern-free-variables
    // ---------------------------------------------------------------

    /**
     * Return the list of free variables in {@code pattern}.
     * A variable is free if it is not in the previously-collected
     * list and not in the current {@link #getBoundVars()}.
     *
     * <p>Corresponds to:
     * <pre>
     * (defun pattern-free-variables (pattern) ...)
     * </pre>
     */
    public static List<String> patternFreeVariables(Object pattern) {
        return patternFreeVars1(pattern, new ArrayList<>());
    }

    /**
     * Recursive helper for {@link #patternFreeVariables}.
     *
     * <p>Corresponds to:
     * <pre>
     * (defun pattern-free-vars1 (pattern vars) ...)
     * </pre>
     */
    public static List<String> patternFreeVars1(Object pattern, List<String> vars) {
        if (pattern == null) {
            return vars;
        }
        if (Unify.isVariable(pattern)) {
            String v = (String) pattern;
            if (vars.contains(v) || getBoundVars().contains(v)) {
                return vars;
            }
            List<String> extended = new ArrayList<>(vars);
            extended.add(v);
            return extended;
        }
        if (!(pattern instanceof Cons)) {
            return vars;
        }
        Cons c = (Cons) pattern;
        return patternFreeVars1(c.getCdr(),
                                patternFreeVars1(c.getCar(), vars));
    }

    // ---------------------------------------------------------------
    // generate-match-body
    // ---------------------------------------------------------------

    /**
     * Generate an explicit match body consisting of tests and
     * binding specifications for the given pattern.
     *
     * <p>Corresponds to:
     * <pre>
     * (defun generate-match-body (pattern vars extra-test ...) ...)
     * </pre>
     *
     * @param pattern   the s-expression pattern
     * @param vars      the list of known pattern variables
     * @param extraTest an optional extra test expression (may be null)
     * @return a {@link MatchBody} containing tests and binding specs
     */
    public static MatchBody generateMatchBody(Object pattern,
                                              List<String> vars,
                                              Object extraTest) {
        List<Object> structureTests = new ArrayList<>();
        List<Object> equalTests = new ArrayList<>();
        List<Object> bindingSpecs = new ArrayList<>();
        // var-alist maps variable names to their last path occurrence.
        List<Unify.Binding> varAlist = new ArrayList<>();

        List<Object> rawTests = generateUnifyTests(pattern, vars,
                                                   new ArrayList<>(), "P");
        for (Object testObj : rawTests) {
            if (!(testObj instanceof Cons)) {
                structureTests.add(testObj);
                continue;
            }
            Cons test = (Cons) testObj;
            Object head = test.getCar();
            if (Unify.isVariable(head)) {
                // test looks like (?x path1 path2 ...)
                List<Object> paths = consToList(test.getCdr());
                equalTests.addAll(generatePairwiseTests(paths));
                if (extraTest != null && !paths.isEmpty()) {
                    varAlist.add(new Unify.Binding((String) head,
                                                   paths.get(paths.size() - 1)));
                }
                if (!paths.isEmpty()) {
                    bindingSpecs.add(paths.get(paths.size() - 1));
                }
            } else {
                structureTests.add(testObj);
            }
        }

        // (setq extra-test (sublis var-alist extra-test))
        extraTest = sublis(varAlist, extraTest);

        // (when (pattern-free-variables extra-test) (error ...))
        if (extraTest != null && !patternFreeVariables(extraTest).isEmpty()) {
            throw new RuntimeException(
                "Rule test includes free variable: " + extraTest);
        }

        List<Object> allTests = new ArrayList<>(structureTests);
        allTests.addAll(equalTests);
        if (extraTest != null) {
            allTests.add(extraTest);
        }

        return new MatchBody(allTests, bindingSpecs);
    }

    // ---------------------------------------------------------------
    // generate-pairwise-tests
    // ---------------------------------------------------------------

    /**
     * Given a list of path expressions, generate pairwise EQUAL
     * tests between consecutive elements.
     *
     * <p>Corresponds to:
     * <pre>
     * (defun generate-pairwise-tests (tests) ...)
     * </pre>
     */
    public static List<Object> generatePairwiseTests(List<Object> tests) {
        List<Object> result = new ArrayList<>();
        for (int i = 0; i + 1 < tests.size(); i++) {
            result.add(Unify.list("EQUAL", tests.get(i), tests.get(i + 1)));
        }
        return result;
    }

    // ---------------------------------------------------------------
    // generate-unify-tests
    // ---------------------------------------------------------------

    /**
     * Generate a list of explicit tests for matching the given
     * pattern.  Assumes the pattern to be tested is in variable
     * {@code "P"}.  Tests are returned in backward order.
     *
     * <p>Example (Lisp notation):
     * <pre>
     * (generate-unify-tests '(foo ?x) nil nil 'P)
     *  => ((NULL (CDR (CDR P)))
     *      (EQUAL ?X (CAR (CDR P)))
     *      (CONSP (CDR P))
     *      (EQUAL (QUOTE FOO) (CAR P))
     *      (CONSP P))
     * </pre>
     *
     * <p>Corresponds to:
     * <pre>
     * (defun generate-unify-tests (pattern vars tests path) ...)
     * </pre>
     *
     * @param pattern the s-expression pattern
     * @param vars    known pattern variables (for multi-occurrence tracking)
     * @param tests   accumulator of tests so far
     * @param path    current accessor path expression
     * @return extended list of tests
     */
    public static List<Object> generateUnifyTests(Object pattern,
                                                  List<String> vars,
                                                  List<Object> tests,
                                                  Object path) {
        // (null pattern) => (cons `(null ,path) tests)
        if (pattern == null) {
            List<Object> result = new ArrayList<>(tests);
            result.add(Unify.list("NULL", path));
            return result;
        }

        // (member pattern vars)
        if (pattern instanceof String && vars.contains(pattern)) {
            // Look for a previous entry for this variable in tests.
            for (Object t : tests) {
                if (t instanceof Cons) {
                    Cons tc = (Cons) t;
                    if (Objects.equals(tc.getCar(), pattern)) {
                        // push path onto (cdr previous)
                        // We need to mutate the list in-place like the Lisp version.
                        // Since Cons is immutable, we rebuild the tests list.
                        List<Object> result = new ArrayList<>();
                        for (Object t2 : tests) {
                            if (t2 == t) {
                                // Append path to this entry's cdr list.
                                result.add(appendToCons((Cons) t2, path));
                            } else {
                                result.add(t2);
                            }
                        }
                        return result;
                    }
                }
            }
            // Not found — add a new entry.
            List<Object> result = new ArrayList<>(tests);
            result.add(Unify.list(pattern, path));
            return result;
        }

        // (variable? pattern) => (cons `(equal ,pattern ,path) tests)
        if (Unify.isVariable(pattern)) {
            List<Object> result = new ArrayList<>(tests);
            result.add(Unify.list("EQUAL", pattern, path));
            return result;
        }

        // (numberp pattern) => (cons `(and (numberp ,path) (= ,pattern ,path)) tests)
        if (pattern instanceof Number) {
            List<Object> result = new ArrayList<>(tests);
            result.add(Unify.list("AND",
                                  Unify.list("NUMBERP", path),
                                  Unify.list("=", pattern, path)));
            return result;
        }

        // (atom pattern) => (cons `(equal ',pattern ,path) tests)
        if (!(pattern instanceof Cons)) {
            List<Object> result = new ArrayList<>(tests);
            result.add(Unify.list("EQUAL",
                                  Unify.list("QUOTE", pattern),
                                  path));
            return result;
        }

        // Recurse on a list.
        Cons c = (Cons) pattern;
        // First add (consp path) test, then recurse on car, then cdr.
        List<Object> withConsp = new ArrayList<>(tests);
        withConsp.add(Unify.list("CONSP", path));

        List<Object> afterCar = generateUnifyTests(
            c.getCar(), vars, withConsp,
            Unify.list("CAR", path));

        return generateUnifyTests(
            c.getCdr(), vars, afterCar,
            Unify.list("CDR", path));
    }

    // ---------------------------------------------------------------
    // Helpers
    // ---------------------------------------------------------------

    /**
     * Substitute variable bindings throughout an s-expression.
     * Corresponds to the Lisp {@code sublis} function.
     *
     * @param alist    list of bindings (variable -> replacement)
     * @param expr     the expression to transform
     * @return the expression with substitutions applied
     */
    public static Object sublis(List<Unify.Binding> alist, Object expr) {
        if (expr == null || alist == null || alist.isEmpty()) {
            return expr;
        }
        // Check if expr itself is a key in the alist.
        for (Unify.Binding b : alist) {
            if (Objects.equals(b.getVar(), expr)) {
                return b.getVal();
            }
        }
        if (expr instanceof Cons) {
            Cons c = (Cons) expr;
            Object newCar = sublis(alist, c.getCar());
            Object newCdr = sublis(alist, c.getCdr());
            if (newCar == c.getCar() && newCdr == c.getCdr()) {
                return expr; // No change — preserve identity.
            }
            return new Cons(newCar, newCdr);
        }
        return expr;
    }

    /**
     * Convert a Cons-based proper list to a {@code List<Object>}.
     */
    public static List<Object> consToList(Object sexpr) {
        List<Object> result = new ArrayList<>();
        Object cur = sexpr;
        while (cur instanceof Cons) {
            result.add(((Cons) cur).getCar());
            cur = ((Cons) cur).getCdr();
        }
        // If cur is non-null here it was a dotted pair; ignore tail.
        return result;
    }

    /**
     * Append an element to the end of a Cons-based proper list,
     * returning a new Cons list.  Used to simulate the Lisp
     * {@code (push path (cdr previous))} which mutably extends
     * the cdr-chain of a test entry.
     */
    private static Cons appendToCons(Cons list, Object element) {
        List<Object> items = consToList(list);
        items.add(element);
        return (Cons) Unify.list(items.toArray());
    }
}
