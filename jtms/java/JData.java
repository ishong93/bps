// JTRE Database module.
// Translated from jdata.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * JData provides database operations for the JTRE system:
 * assert, assume, retract, fetch, and query functions.
 */
public class JData {

    /** Assert a fact with justification. Corresponds to assert!. */
    public static Datum jassert(Object fact, Object just, Jtre jtre) {
        Datum datum = referent(fact, true, jtre);
        JTMS.TmsNode node = datum.getTmsNode();
        List<Object> justList;
        if (just instanceof List) {
            justList = new ArrayList<>((List<?>) just);
        } else {
            justList = new ArrayList<>();
            justList.add(just);
        }
        jtre.debug("%n    Asserting %s via %s.", fact, justList);
        List<JTMS.TmsNode> antecedents = new ArrayList<>();
        for (int i = 1; i < justList.size(); i++) {
            antecedents.add(referent(justList.get(i), true, jtre).getTmsNode());
        }
        node.getJtms().justifyNode(justList.get(0), node, antecedents);
        return datum;
    }

    /** Assume a fact. Corresponds to assume!. */
    public static Datum jassume(Object fact, Object reason, Jtre jtre) {
        Datum datum = referent(fact, true, jtre);
        JTMS.TmsNode node = datum.getTmsNode();
        if (!datum.isAssumption()) {
            datum.setAssumption(reason);
            jtre.debug("%n    Assuming %s via %s.", fact, reason);
            node.getJtms().assumeNode(node);
            node.getJtms().enableAssumption(node);
        } else if (Objects.equals(reason, datum.getAssumption())) {
            // Same reason, do nothing
        } else {
            throw new RuntimeException(String.format(
                "Fact %s assumed because of %s assumed again because of %s",
                showDatum(datum), datum.getAssumption(), reason));
        }
        return datum;
    }

    /** Retract a fact. Corresponds to retract!. */
    public static JTMS.TmsNode jretract(Object fact, Object just, boolean quiet, Jtre jtre) {
        Datum datum = referent(fact, true, jtre);
        JTMS.TmsNode node = datum.getTmsNode();
        if (!node.isAssumption()) {
            if (!quiet) System.out.printf("%n%s isn't an assumption.", showDatum(datum));
        } else if (!JTMS.inNode(node)) {
            if (!quiet) System.out.printf("%nThe assumption %s is not currently in.", fact);
        } else if (Objects.equals(just, datum.getAssumption())) {
            jtre.debug("%n    Retracting %s via %s.", fact, just);
            datum.setAssumption(null);
            node.getJtms().retractAssumption(node);
        } else if (!quiet) {
            System.out.printf("%n%s not source of assumption for %s", just, fact);
        }
        return node;
    }

    /** Mark a fact as contradictory. Corresponds to contradiction. */
    public static void contradiction(Object fact, Jtre jtre) {
        referent(fact, true, jtre).getTmsNode().getJtms().makeContradiction(
            referent(fact, true, jtre).getTmsNode());
    }

    /** Check if a fact is IN. Corresponds to in?. */
    public static boolean isIn(Object fact, Jtre jtre) {
        Datum r = referent(fact, false, jtre);
        return r != null && JTMS.inNode(r.getTmsNode());
    }

    /** Check if a fact is OUT. Corresponds to out?. */
    public static boolean isOut(Object fact, Jtre jtre) {
        Datum r = referent(fact, false, jtre);
        return r != null && JTMS.outNode(r.getTmsNode());
    }

    /** Print why a fact is believed. Corresponds to why?. */
    public static void why(Object fact, Jtre jtre) {
        Datum r = referent(fact, false, jtre);
        if (r != null) r.getTmsNode().getJtms().whyNode(r.getTmsNode());
    }

    /** Fetch all facts matching a pattern (exact match). Corresponds to fetch. */
    public static List<Object> fetch(Object pattern, Jtre jtre) {
        List<Object> results = new ArrayList<>();
        List<Datum> candidates = getCandidates(pattern, jtre);
        for (Datum candidate : candidates) {
            if (deepEqual(pattern, candidate.getLispForm())) {
                results.add(candidate.getLispForm());
            }
        }
        return results;
    }

    /** Get or create the dbclass for a fact. Corresponds to get-dbclass. */
    public static Dbclass getDbclass(Object fact, Jtre jtre) {
        if (fact == null) throw new RuntimeException("null can't be a dbclass.");
        String key;
        if (fact instanceof List) {
            List<?> list = (List<?>) fact;
            if (list.isEmpty()) throw new RuntimeException("empty list can't be a dbclass");
            return getDbclass(list.get(0), jtre);
        } else {
            key = String.valueOf(fact);
        }
        Dbclass dbclass = jtre.getDbclassTable().get(key);
        if (dbclass != null) return dbclass;
        dbclass = new Dbclass(key, jtre);
        jtre.getDbclassTable().put(key, dbclass);
        return dbclass;
    }

    /** Find or create datum for a fact. Corresponds to referent. */
    public static Datum referent(Object fact, boolean virtual, Jtre jtre) {
        if (virtual) return insert(fact, jtre);
        return referent1(fact, jtre);
    }

    /** Look up existing datum. Corresponds to referent1. */
    private static Datum referent1(Object fact, Jtre jtre) {
        Dbclass dbclass = getDbclass(fact, jtre);
        for (Datum candidate : dbclass.getFacts()) {
            if (deepEqual(candidate.getLispForm(), fact)) return candidate;
        }
        return null;
    }

    /** Insert a fact, creating datum and TMS node. Corresponds to insert. */
    public static Datum insert(Object fact, Jtre jtre) {
        Datum existing = referent1(fact, jtre);
        if (existing != null) return existing;
        jtre.setDatumCounter(jtre.getDatumCounter() + 1);
        Datum datum = new Datum(jtre.getDatumCounter(), fact, getDbclass(fact, jtre));
        datum.setTmsNode(jtre.getJtms().createNode(datum, false, false));
        datum.getDbclass().getFacts().add(0, datum);
        JRules.tryRules(datum);
        return datum;
    }

    /** Get candidate facts for a pattern. */
    public static List<Datum> getCandidates(Object pattern, Jtre jtre) {
        return getDbclass(pattern, jtre).getFacts();
    }

    /** Apply a function to each dbclass. Corresponds to map-dbclass. */
    public static void mapDbclass(java.util.function.Consumer<Dbclass> proc, Jtre jtre) {
        for (Dbclass dbclass : jtre.getDbclassTable().values()) {
            proc.accept(dbclass);
        }
    }

    /** Get the TMS node for a fact. Corresponds to get-tms-node. */
    public static JTMS.TmsNode getTmsNode(Object fact, Jtre jtre) {
        return referent(fact, true, jtre).getTmsNode();
    }

    /** Get the lisp-form from a TMS node's datum. Corresponds to view-node. */
    public static String viewNode(JTMS.TmsNode node) {
        if (node.getDatum() instanceof Datum) {
            return String.valueOf(((Datum) node.getDatum()).getLispForm());
        }
        return String.valueOf(node.getDatum());
    }

    /** String representation of a datum. Corresponds to show-datum. */
    public static String showDatum(Datum datum) {
        return String.valueOf(datum.getLispForm());
    }

    /** Find a datum by ID. Corresponds to get-datum. */
    public static Datum getDatum(int num, Jtre jtre) {
        final Datum[] result = {null};
        mapDbclass(dbclass -> {
            for (Datum d : dbclass.getFacts()) {
                if (d.getId() == num) { result[0] = d; return; }
            }
        }, jtre);
        return result[0];
    }

    /** Display all data. Corresponds to show-data. */
    public static void showData(Jtre jtre, PrintStream out) {
        if (out == null) out = System.out;
        PrintStream finalOut = out;
        finalOut.printf("%n%d facts total.", jtre.getDatumCounter());
        mapDbclass(dbclass -> {
            for (Datum datum : dbclass.getFacts()) {
                String status = JTMS.inNode(datum.getTmsNode()) ? "IN" : "OUT";
                finalOut.printf("%n%s: %s", showDatum(datum), status);
            }
        }, jtre);
    }

    /** Deep structural equality. */
    @SuppressWarnings("unchecked")
    public static boolean deepEqual(Object a, Object b) {
        if (Objects.equals(a, b)) return true;
        if (a == null || b == null) return false;
        if (a instanceof List && b instanceof List) {
            List<Object> la = (List<Object>) a;
            List<Object> lb = (List<Object>) b;
            if (la.size() != lb.size()) return false;
            for (int i = 0; i < la.size(); i++) {
                if (!deepEqual(la.get(i), lb.get(i))) return false;
            }
            return true;
        }
        return String.valueOf(a).equals(String.valueOf(b));
    }
}
