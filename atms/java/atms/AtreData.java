// -*- Mode: Java -*-

// ATRE database
// Translated from adata.lisp

// Copyright (c) 1992, Kenneth D. Forbus, Northwestern
// University, and Johan de Kleer, the Xerox Corporation
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;

/**
 * ATRE database operations: assertion, assumption, querying, and display.
 */
public class AtreData {

    /**
     * Assert a fact with a justification.
     */
    public static Datum assertFact(Object fact, Object just, Atre atre) {
        Datum datum = referent(fact, true, atre);
        TmsNode node = datum.getTmsNode();

        List<Object> justList;
        if (just instanceof List) {
            justList = new ArrayList<>((List<?>) just);
        } else {
            justList = new ArrayList<>();
            justList.add(just);
        }

        if (atre.isDebugging()) {
            System.out.printf("%n    Asserting %s via %s.%n", fact, justList);
        }

        List<TmsNode> antecedents = new ArrayList<>();
        for (int i = 1; i < justList.size(); i++) {
            antecedents.add(referent(justList.get(i), true, atre).getTmsNode());
        }

        atre.getAtms().justifyNode(justList.get(0), node, antecedents);
        return datum;
    }

    /**
     * Assume a fact with a reason.
     */
    public static Datum assumeFact(Object fact, Object reason, Atre atre) {
        Datum datum = referent(fact, true, atre);
        TmsNode node = datum.getTmsNode();

        if (datum.getAssumption() == null) {
            datum.setAssumption(reason);
            if (atre.isDebugging()) {
                System.out.printf("%n    Assuming %s via %s.%n", fact, reason);
            }
            atre.getAtms().assumeNode(node);
        } else if (!reason.equals(datum.getAssumption())) {
            throw new RuntimeException(String.format(
                "Fact %s assumed because of %s assumed again because of %s",
                showDatum(datum), datum.getAssumption(), reason));
        }
        return datum;
    }

    /**
     * Check if a fact is already assumed.
     */
    public static boolean alreadyAssumed(Object fact, Atre atre) {
        TmsNode node = getTmsNode(fact, atre);
        return node.isAssumption();
    }

    /**
     * Assume a fact if it hasn't been assumed yet.
     */
    public static void assumeIfNeeded(Object fact, Object reason, Atre atre) {
        if (!alreadyAssumed(fact, atre)) {
            assumeFact(fact, reason, atre);
        }
    }

    /**
     * Mark a fact as contradictory.
     */
    public static void contradiction(Object fact, Atre atre) {
        atre.getAtms().makeContradiction(referent(fact, true, atre).getTmsNode());
    }

    /**
     * Get the dbclass for a fact pattern.
     */
    public static Dbclass getDbclass(Object fact, Atre atre) {
        if (fact == null) {
            throw new RuntimeException("NIL can't be a dbclass.");
        }
        if (fact instanceof List) {
            List<?> list = (List<?>) fact;
            if (list.isEmpty()) throw new RuntimeException("Empty list can't be a dbclass.");
            return getDbclass(list.get(0), atre);
        }
        if (fact instanceof String) {
            String name = (String) fact;
            if (Unify.isVariable(name)) {
                throw new RuntimeException("Dbclass unbound: " + name);
            }
            Dbclass dbclass = atre.getDbclassTable().get(name);
            if (dbclass != null) return dbclass;

            dbclass = new Dbclass(name, atre);
            atre.getDbclassTable().put(name, dbclass);
            atre.getDbclasses().add(dbclass);
            return dbclass;
        }
        throw new RuntimeException("Bad dbclass type: " + fact);
    }

    /**
     * Find the datum for a fact, optionally creating it.
     */
    public static Datum referent(Object fact, boolean virtual, Atre atre) {
        if (virtual) {
            return insert(fact, atre);
        }
        return referent1(fact, atre);
    }

    /**
     * Find an existing datum for a fact.
     */
    public static Datum referent1(Object fact, Atre atre) {
        Dbclass dbclass = getDbclass(fact, atre);
        for (Datum candidate : dbclass.getFacts()) {
            if (deepEquals(candidate.getLispForm(), fact)) {
                return candidate;
            }
        }
        return null;
    }

    /**
     * Insert a fact, creating a new datum if needed.
     */
    public static Datum insert(Object fact, Atre atre) {
        Datum datum = referent1(fact, atre);
        if (datum != null) return datum;

        atre.incrementDatumCounter();
        datum = new Datum(atre.getDatumCounter(), atre, fact, getDbclass(fact, atre));
        datum.setTmsNode(atre.getAtms().createNode(datum, false, false));
        datum.getDbclass().getFacts().add(0, datum);
        AtreRules.tryRules(datum);
        return datum;
    }

    /**
     * Fetch all facts matching a pattern.
     */
    public static List<Object> fetch(Object pattern, Atre atre) {
        List<Object> unifiers = new ArrayList<>();
        List<Datum> candidates = getCandidates(pattern, atre);
        for (Datum candidate : candidates) {
            Unify.UnifyResult result = Unify.unify(pattern, candidate.getLispForm());
            if (result.isSuccess()) {
                unifiers.add(Funify.sublis(result.getBindings(), pattern));
            }
        }
        return unifiers;
    }

    /**
     * Get candidate datums matching a pattern's dbclass.
     */
    public static List<Datum> getCandidates(Object pattern, Atre atre) {
        return getDbclass(pattern, atre).getFacts();
    }

    /**
     * Check if a fact is true (has an empty-env label).
     */
    public static boolean isTrue(Object fact, Atre atre) {
        Datum r = referent(fact, false, atre);
        return r != null && atre.getAtms().isTrueNode(r.getTmsNode());
    }

    /**
     * Check if a fact is in a given environment.
     */
    public static boolean isIn(Object fact, Env env, Atre atre) {
        Datum r = referent(fact, false, atre);
        return r != null && atre.getAtms().isInNode(r.getTmsNode(), env);
    }

    /**
     * Check if a fact is out in a given environment.
     */
    public static boolean isOut(Object fact, Env env, Atre atre) {
        Datum r = referent(fact, false, atre);
        return r != null && atre.getAtms().isOutNode(r.getTmsNode(), env);
    }

    /**
     * Check if a fact is consistent with an environment.
     */
    public static boolean isConsistentWith(Object fact, Env env, Atre atre) {
        Datum r = referent(fact, false, atre);
        return r != null && atre.getAtms().isNodeConsistentWith(r.getTmsNode(), env);
    }

    /**
     * Get the environment for a set of assumed facts.
     */
    public static Env environmentOf(List<Object> facts, Atre atre) {
        Env env = atre.getAtms().getEmptyEnv();
        for (Object fact : facts) {
            TmsNode node = getTmsNode(fact, atre);
            if (!node.isAssumption()) {
                throw new RuntimeException("Non-assumption in ENVIRONMENT-OF: " + fact);
            }
            env = atre.getAtms().consEnv(node, env);
            if (env.isNogood()) {
                return null;
            }
        }
        return env;
    }

    /**
     * Get the TMS node for a fact.
     */
    public static TmsNode getTmsNode(Object fact, Atre atre) {
        return referent(fact, true, atre).getTmsNode();
    }

    /**
     * View a node's datum as its lisp form.
     */
    public static Object viewNode(TmsNode node) {
        if (node.getDatum() instanceof Datum) {
            return ((Datum) node.getDatum()).getLispForm();
        }
        return node.getDatum();
    }

    /**
     * Stringify a node for display.
     */
    public static String stringifyNode(TmsNode node) {
        return String.valueOf(viewNode(node));
    }

    /**
     * Get all assumption environments for a fact.
     */
    public static List<Env> assumptionsOf(Object fact, Atre atre) {
        return getTmsNode(fact, atre).getLabel();
    }

    /**
     * Show a datum as a string.
     */
    public static String showDatum(Datum datum) {
        return String.valueOf(datum.getLispForm());
    }

    /**
     * Show all data in an ATRE.
     */
    public static int showData(Atre atre, PrintWriter stream) {
        int counter = 0;
        stream.printf("%n%d facts total.", atre.getDatumCounter());
        for (Dbclass dbclass : atre.getDbclasses()) {
            for (Datum datum : dbclass.getFacts()) {
                counter++;
                stream.printf("%n%s: %s", showDatum(datum),
                    assumptionsOf(datum.getLispForm(), atre));
            }
        }
        stream.flush();
        return counter;
    }

    /**
     * Show all data in an environment context.
     */
    public static int showContext(Env env, Atre atre, PrintWriter stream) {
        int counter = 0;
        for (Dbclass dbclass : atre.getDbclasses()) {
            for (Datum datum : dbclass.getFacts()) {
                if (atre.getAtms().isInNode(datum.getTmsNode(), env)) {
                    counter++;
                    stream.printf("%n%s", showDatum(datum));
                }
            }
        }
        stream.printf("%n%d facts total.", counter);
        stream.flush();
        return counter;
    }

    /**
     * Show dbclass information.
     */
    public static int showDbclasses(Atre atre, PrintWriter stream) {
        int counter = 0;
        for (Dbclass dbclass : atre.getDbclasses()) {
            counter++;
            stream.printf("%n %s: %d facts, %d rules",
                dbclass.getName(),
                dbclass.getFacts().size(),
                dbclass.getRules().size());
        }
        stream.flush();
        return counter;
    }

    /**
     * Deep equality for s-expressions.
     */
    @SuppressWarnings("unchecked")
    private static boolean deepEquals(Object a, Object b) {
        if (a == b) return true;
        if (a == null || b == null) return false;
        if (a.equals(b)) return true;
        if (a instanceof List && b instanceof List) {
            List<Object> la = (List<Object>) a;
            List<Object> lb = (List<Object>) b;
            if (la.size() != lb.size()) return false;
            for (int i = 0; i < la.size(); i++) {
                if (!deepEquals(la.get(i), lb.get(i))) return false;
            }
            return true;
        }
        return false;
    }
}
