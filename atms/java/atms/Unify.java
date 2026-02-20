// -*- Mode: Java -*-
//
// Variables and unification.
// Translated from unify.lisp, last edited 1/29/93 by KDF.
//
// Copyright (c) 1988-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package atms;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Variables and unification.
 *
 * <p>Provides basic unification of s-expressions represented as
 * nested {@link Cons} cells and Java objects.  A <em>variable</em> is
 * any {@code String} whose first character is {@code '?'}.
 *
 * <p>Bindings are represented as a {@code List<Binding>} (an
 * association list).  A {@code null} return from {@link #unify}
 * with {@code success == false} signals failure (the Lisp
 * {@code :FAIL} sentinel).  See {@link UnifyResult} for details.
 */
public class Unify {

    // ---------------------------------------------------------------
    // Cons — a classic car/cdr pair for s-expression representation.
    // ---------------------------------------------------------------

    /**
     * A Lisp-style cons cell holding a car and a cdr.
     * Used to represent s-expressions (nested list structure).
     */
    public static class Cons {
        private final Object car;
        private final Object cdr;

        public Cons(Object car, Object cdr) {
            this.car = car;
            this.cdr = cdr;
        }

        public Object getCar() { return car; }
        public Object getCdr() { return cdr; }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof Cons)) return false;
            Cons other = (Cons) o;
            return Objects.equals(car, other.car)
                && Objects.equals(cdr, other.cdr);
        }

        @Override
        public int hashCode() {
            return Objects.hash(car, cdr);
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("(");
            sb.append(car);
            Object rest = cdr;
            while (rest instanceof Cons) {
                sb.append(' ');
                sb.append(((Cons) rest).car);
                rest = ((Cons) rest).cdr;
            }
            if (rest != null) {
                sb.append(" . ");
                sb.append(rest);
            }
            sb.append(')');
            return sb.toString();
        }
    }

    // ---------------------------------------------------------------
    // Binding — a single var → value pair.
    // ---------------------------------------------------------------

    /**
     * A binding of a variable name to a value.
     * Corresponds to a {@code (cons var exp)} entry in the Lisp
     * association-list of bindings.
     */
    public static class Binding {
        private final String var;
        private final Object val;

        public Binding(String var, Object val) {
            this.var = var;
            this.val = val;
        }

        public String getVar() { return var; }
        public Object getVal() { return val; }

        @Override
        public String toString() {
            return "(" + var + " . " + val + ")";
        }
    }

    // ---------------------------------------------------------------
    // UnifyResult — wrapper distinguishing success from failure.
    // ---------------------------------------------------------------

    /**
     * Result of a unification attempt.
     *
     * <p>In the original Lisp code, success with no bindings is
     * represented by the empty list {@code NIL}, while failure is
     * the keyword {@code :FAIL}.  Both would map to {@code null} in
     * Java, so this wrapper carries a boolean {@link #success} flag
     * to distinguish the two cases.
     */
    public static class UnifyResult {
        private final boolean success;
        private final List<Binding> bindings;

        private UnifyResult(boolean success, List<Binding> bindings) {
            this.success = success;
            this.bindings = bindings;
        }

        /** Successful unification with the given bindings. */
        public static UnifyResult success(List<Binding> bindings) {
            return new UnifyResult(true, bindings == null ? new ArrayList<>() : bindings);
        }

        /** Failed unification (corresponds to Lisp {@code :FAIL}). */
        public static UnifyResult fail() {
            return new UnifyResult(false, null);
        }

        public boolean isSuccess() { return success; }

        /**
         * Returns the list of bindings, or {@code null} if the
         * unification failed.
         */
        public List<Binding> getBindings() { return bindings; }

        @Override
        public String toString() {
            return success ? "Success" + (bindings == null ? "" : bindings.toString())
                           : ":FAIL";
        }
    }

    // ---------------------------------------------------------------
    // Public API
    // ---------------------------------------------------------------

    /**
     * Tests whether {@code x} is a variable — a {@code String}
     * whose first character is {@code '?'}.
     */
    public static boolean isVariable(Object x) {
        return (x instanceof String)
            && ((String) x).length() > 0
            && ((String) x).charAt(0) == '?';
    }

    /**
     * Unify two s-expressions {@code a} and {@code b} with no
     * initial bindings.
     */
    public static UnifyResult unify(Object a, Object b) {
        return unify(a, b, new ArrayList<>());
    }

    /**
     * Unify {@code a} and {@code b} under the given {@code bindings}.
     *
     * <p>Returns a {@link UnifyResult} indicating success (with
     * possibly extended bindings) or failure.
     *
     * <p>Corresponds to the Lisp function:
     * <pre>
     * (defun unify (a b &amp;optional (bindings nil)) ...)
     * </pre>
     */
    public static UnifyResult unify(Object a, Object b, List<Binding> bindings) {
        // (equal a b) => bindings
        if (Objects.equals(a, b)) {
            return UnifyResult.success(bindings);
        }
        // (variable? a) => unify-variable
        if (isVariable(a)) {
            return unifyVariable((String) a, b, bindings);
        }
        // (variable? b) => unify-variable
        if (isVariable(b)) {
            return unifyVariable((String) b, a, bindings);
        }
        // (or (not (listp a)) (not (listp b))) => :FAIL
        if (!(a instanceof Cons) || !(b instanceof Cons)) {
            return UnifyResult.fail();
        }
        // Recurse on car, then cdr
        Cons ca = (Cons) a;
        Cons cb = (Cons) b;
        UnifyResult carResult = unify(ca.car, cb.car, bindings);
        if (!carResult.isSuccess()) {
            return UnifyResult.fail();
        }
        return unify(ca.cdr, cb.cdr, carResult.getBindings());
    }

    /**
     * Attempt to bind {@code var} to {@code exp} under
     * {@code bindings}.
     *
     * <p>Corresponds to the Lisp function:
     * <pre>
     * (defun unify-variable (var exp bindings) ...)
     * </pre>
     */
    public static UnifyResult unifyVariable(String var, Object exp, List<Binding> bindings) {
        Binding val = assoc(var, bindings);
        if (val != null) {
            // Variable already bound — unify bound value with exp.
            return unify(val.val, exp, bindings);
        }
        // If safe (no occurs-check violation), bind var to exp.
        if (freeIn(var, exp, bindings)) {
            List<Binding> extended = new ArrayList<>(bindings);
            extended.add(new Binding(var, exp));
            return UnifyResult.success(extended);
        }
        return UnifyResult.fail();
    }

    /**
     * Returns {@code true} if {@code var} does <em>not</em> occur in
     * {@code exp} (the "occurs check").
     *
     * <p>Corresponds to the Lisp function:
     * <pre>
     * (defun free-in? (var exp bindings) ...)
     * </pre>
     */
    public static boolean freeIn(Object var, Object exp, List<Binding> bindings) {
        if (exp == null) {
            return true;
        }
        if (Objects.equals(var, exp)) {
            return false;
        }
        if (isVariable(exp)) {
            Binding val = assoc((String) exp, bindings);
            if (val != null) {
                return freeIn(var, val.val, bindings);
            }
            return true;
        }
        if (!(exp instanceof Cons)) {
            return true;
        }
        Cons c = (Cons) exp;
        return freeIn(var, c.car, bindings)
            && freeIn(var, c.cdr, bindings);
    }

    // ---------------------------------------------------------------
    // Helpers
    // ---------------------------------------------------------------

    /**
     * Look up {@code var} in the binding list (association list).
     * Returns the first {@link Binding} whose var matches, or
     * {@code null} if not found.
     */
    public static Binding assoc(String var, List<Binding> bindings) {
        if (bindings == null) return null;
        for (Binding b : bindings) {
            if (b.var.equals(var)) {
                return b;
            }
        }
        return null;
    }

    // ---------------------------------------------------------------
    // Convenience builders for Cons lists
    // ---------------------------------------------------------------

    /** Build a proper list from the given elements. */
    public static Cons list(Object... elements) {
        Cons result = null;
        for (int i = elements.length - 1; i >= 0; i--) {
            result = new Cons(elements[i], result);
        }
        return result;
    }

    /** Build a cons cell. */
    public static Cons cons(Object car, Object cdr) {
        return new Cons(car, cdr);
    }

    /** Return the car of a Cons, or null. */
    public static Object car(Object x) {
        return (x instanceof Cons) ? ((Cons) x).car : null;
    }

    /** Return the cdr of a Cons, or null. */
    public static Object cdr(Object x) {
        return (x instanceof Cons) ? ((Cons) x).cdr : null;
    }
}
