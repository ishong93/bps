package ltms;

import java.util.*;

/**
 * Logic-based Truth maintenance Reasoning Engine.
 * Provides a database and reasoning layer on top of the LTMS.
 * Converted from linter.lisp and ldata.lisp in "Building Problem Solvers".
 */
public class LTRE {
    public String title;
    public LTMS ltms;
    public Map<String, Dbclass> dbclassTable = new HashMap<>();
    public int datumCounter;
    public int ruleCounter;
    public boolean debugging;
    public List<Object[]> queue = new ArrayList<>(); // rule invocations
    public int rulesRun;

    public static LTRE currentLTRE;

    // Connectives list for formula parsing
    private static final Set<String> CONNECTIVES =
        Set.of(":IMPLIES", ":AND", ":OR", ":IFF", ":NOT", ":TAXONOMY");

    // --- Creation ---

    public LTRE(String title, boolean debugging) {
        this.title = title;
        this.debugging = debugging;
    }

    public static LTRE createLTRE(String title, boolean debugging) {
        LTRE l = new LTRE(title, debugging);
        l.ltms = LTMS.createLTMS(
            "(:LTMS-OF " + title + ")",
            node -> {
                if (node.datum instanceof Datum) {
                    return ((Datum) node.datum).showDatum();
                }
                return String.valueOf(node.datum);
            },
            false,
            true,
            null,
            null,
            false,
            LTMS.CompleteMode.NONE,
            true
        );
        l.ltms.enqueueProcedure = pair -> l.enqueue(pair);
        currentLTRE = l;
        return l;
    }

    public static void inLTRE(LTRE l) {
        currentLTRE = l;
    }

    // --- Proposition classification ---

    public static boolean isSimpleProposition(Object x) {
        if (!(x instanceof List)) return true;
        List<?> lst = (List<?>) x;
        if (lst.isEmpty()) return true;
        if (lst.get(0) instanceof String && CONNECTIVES.contains((String) lst.get(0))) {
            return false;
        }
        return true;
    }

    public static boolean isNegatedProposition(Object form) {
        if (!(form instanceof List)) return false;
        List<?> lst = (List<?>) form;
        return lst.size() == 2
            && ":NOT".equals(lst.get(0))
            && isSimpleProposition(lst.get(1));
    }

    // --- Assertion operations ---

    public static void assertFact(Object fact, Object just, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Object formula = buildTmsFormula(fact, ltre);
        LTMS.addFormula(ltre.ltms, formula, just);
    }

    public static Datum assumeFact(Object fact, Object reason, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Datum datum;
        boolean isNeg;
        if (isNegatedProposition(fact)) {
            List<?> lst = (List<?>) fact;
            datum = referent(lst.get(1), true, ltre);
            isNeg = true;
        } else {
            datum = referent(fact, true, ltre);
            isNeg = false;
        }
        TmsNode node = datum.tmsNode;

        if (!isNeg && !isSimpleProposition(fact)) {
            List<Object> formula = Arrays.asList(
                ":IMPLIES", node, buildTmsFormula(fact, ltre)
            );
            LTMS.addFormula(ltre.ltms, formula, reason);
        }

        if (datum.assumption == null) {
            datum.assumption = reason;
            LTMS.convertToAssumption(node);
            LTMS.enableAssumption(node, isNeg ? NodeLabel.FALSE : NodeLabel.TRUE);
        } else if (!datum.assumption.equals(reason)) {
            throw new LtmsError(
                "Fact assumed with different reasons: " + datum.showDatum()
            );
        }
        return datum;
    }

    public static TmsNode retractFact(Object fact, Object just, LTRE ltre, boolean quiet) {
        if (ltre == null) ltre = currentLTRE;
        Datum datum = referent(fact, true, ltre);
        TmsNode node = datum.tmsNode;
        if (!node.isAssumption) {
            if (!quiet) {
                System.out.println("Can't retract non-assumption: " + datum.showDatum());
            }
            return node;
        }
        if (!node.isKnown()) {
            if (!quiet) {
                System.out.println("Node not believed: " + datum.showDatum());
            }
            return node;
        }
        if (just != null && just.equals(datum.assumption)) {
            datum.assumption = null;
            LTMS.retractAssumption(node);
        } else if (!quiet) {
            System.out.println(
                "Wrong informant for retraction of " + datum.showDatum()
            );
        }
        return node;
    }

    public static Object alreadyAssumed(Object fact, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Datum d = referent(fact, false, ltre);
        return d != null ? d.assumption : null;
    }

    // --- Formula building ---

    @SuppressWarnings("unchecked")
    public static Object buildTmsFormula(Object formula, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        if (formula instanceof List) {
            List<Object> lst = (List<Object>) formula;
            if (!lst.isEmpty()
                && lst.get(0) instanceof String
                && CONNECTIVES.contains((String) lst.get(0))) {
                List<Object> result = new ArrayList<>();
                result.add(lst.get(0));
                for (int i = 1; i < lst.size(); i++) {
                    result.add(buildTmsFormula(lst.get(i), ltre));
                }
                return result;
            }
        }
        return referent(formula, true, ltre).tmsNode;
    }

    // --- Database class operations ---

    public static Dbclass getDbclass(Object fact, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        String name = dbclassName(fact);
        Dbclass db = ltre.dbclassTable.get(name);
        if (db != null) return db;
        db = new Dbclass(name, ltre);
        ltre.dbclassTable.put(name, db);
        return db;
    }

    @SuppressWarnings("unchecked")
    static String dbclassName(Object fact) {
        if (fact instanceof List) {
            List<Object> lst = (List<Object>) fact;
            if (!lst.isEmpty()) {
                if (isNegatedProposition(fact)) return dbclassName(lst.get(1));
                return String.valueOf(lst.get(0));
            }
        }
        return String.valueOf(fact);
    }

    // --- Datum referencing and insertion ---

    public static Datum referent(Object fact, boolean virtual, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Object form = isNegatedProposition(fact) ? ((List<?>) fact).get(1) : fact;
        Dbclass db = getDbclass(fact, ltre);
        for (Datum d : db.facts) {
            if (equalForms(d.lispForm, form)) return d;
        }
        if (!virtual) return null;
        return insert(fact, ltre);
    }

    public static Datum insert(Object fact, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Object form = isNegatedProposition(fact) ? ((List<?>) fact).get(1) : fact;
        Dbclass db = getDbclass(form, ltre);
        for (Datum d : db.facts) {
            if (equalForms(d.lispForm, form)) return d;
        }
        ltre.datumCounter++;
        Datum datum = new Datum(ltre.datumCounter, ltre, form, db);
        datum.tmsNode = ltre.ltms.tmsCreateNode(datum, false);
        db.facts.add(datum);
        LtreRules.tryRules(datum);
        return datum;
    }

    // --- Query operations ---

    @SuppressWarnings("unchecked")
    public static List<Object> fetch(Object pattern, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Object form = isNegatedProposition(pattern)
            ? ((List<?>) pattern).get(1) : pattern;
        List<Object> results = new ArrayList<>();
        Dbclass db = getDbclass(form, ltre);
        for (Datum d : db.facts) {
            Object bindings = Unifier.unify(form, d.lispForm);
            if (bindings != Unifier.FAIL) {
                results.add(Unifier.sublis(bindings, pattern));
            }
        }
        return results;
    }

    public static boolean isTrue(Object fact, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Datum r = referent(fact, false, ltre);
        if (r == null) return false;
        if (isNegatedProposition(fact)) return r.tmsNode.isFalse();
        return r.tmsNode.isTrue();
    }

    public static boolean isFalse(Object fact, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Datum r = referent(fact, false, ltre);
        if (r == null) return false;
        if (isNegatedProposition(fact)) return r.tmsNode.isTrue();
        return r.tmsNode.isFalse();
    }

    public static boolean isKnown(Object fact, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Datum r = referent(fact, false, ltre);
        return r != null && r.tmsNode.isKnown();
    }

    public static boolean isUnknown(Object fact, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Datum r = referent(fact, false, ltre);
        return r == null || r.tmsNode.isUnknown();
    }

    // --- Node access ---

    public static TmsNode getTmsNode(Object fact, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        return referent(fact, true, ltre).tmsNode;
    }

    public static Object viewNode(TmsNode node) {
        if (node.datum instanceof Datum) return ((Datum) node.datum).lispForm;
        return node.datum;
    }

    public static Object signedViewNode(TmsNode node) {
        Object v = viewNode(node);
        if (node.isTrue()) return v;
        return Arrays.asList(":NOT", v);
    }

    public static List<TmsNode> assumptionsOf(Object fact, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        Datum d = referent(fact, false, ltre);
        if (d == null) return Collections.emptyList();
        return LTMS.assumptionsOfNode(d.tmsNode);
    }

    // --- Higher-level operations ---

    public static void uassert(Object fact, Object just, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        if (just == null) just = "user";
        assertFact(fact, just, ltre);
        LtreRules.runRules(ltre);
    }

    public static void uassume(Object fact, Object reason, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        assumeFact(fact, reason, ltre);
        LtreRules.runRules(ltre);
    }

    // --- Queue management ---

    public void enqueue(Object pair) {
        queue.add(0, new Object[]{pair});
    }

    public Object dequeue() {
        if (queue.isEmpty()) return null;
        return queue.remove(queue.size() - 1)[0];
    }

    // --- Contradiction helper ---

    public static void contradiction(List<Object> losers, LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        List<TmsNode> trues = new ArrayList<>();
        List<TmsNode> falses = new ArrayList<>();
        for (Object fact : losers) {
            Datum datum = referent(fact, true, ltre);
            if (isNegatedProposition(fact)) {
                falses.add(datum.tmsNode);
            } else {
                trues.add(datum.tmsNode);
            }
        }
        LTMS.addClause(trues, falses, "DECLARED-CONTRADICTION");
    }

    // --- Temporary assumption context ---

    public static void assuming(List<Object> factsToAssume, LTRE ltre, Runnable body) {
        if (ltre == null) ltre = currentLTRE;
        List<TmsNode> nodes = new ArrayList<>();
        List<NodeLabel> labels = new ArrayList<>();
        for (Object fact : factsToAssume) {
            Datum datum;
            if (isNegatedProposition(fact)) {
                datum = referent(((List<?>) fact).get(1), true, ltre);
                LTMS.convertToAssumption(datum.tmsNode);
                nodes.add(datum.tmsNode);
                labels.add(NodeLabel.FALSE);
            } else {
                datum = referent(fact, true, ltre);
                LTMS.convertToAssumption(datum.tmsNode);
                nodes.add(datum.tmsNode);
                labels.add(NodeLabel.TRUE);
            }
        }
        LTMS.withAssumptions(nodes, labels, body);
    }

    // --- Display ---

    public static void showData(LTRE ltre) {
        if (ltre == null) ltre = currentLTRE;
        int count = 0;
        for (Dbclass db : ltre.dbclassTable.values()) {
            for (Datum d : db.facts) {
                count++;
                String status;
                if (d.tmsNode.isTrue()) status = "TRUE";
                else if (d.tmsNode.isFalse()) status = "FALSE";
                else status = "UNKNOWN";
                System.out.printf("\n%s: %s", d.showDatum(), status);
            }
        }
        System.out.printf("\n%d facts total.", count);
    }

    // --- Structural comparison ---

    @SuppressWarnings("unchecked")
    public static boolean equalForms(Object a, Object b) {
        if (a == b) return true;
        if (a == null || b == null) return false;
        if (a instanceof List && b instanceof List) {
            List<Object> la = (List<Object>) a;
            List<Object> lb = (List<Object>) b;
            if (la.size() != lb.size()) return false;
            for (int i = 0; i < la.size(); i++) {
                if (!equalForms(la.get(i), lb.get(i))) return false;
            }
            return true;
        }
        return a.equals(b);
    }

    @Override
    public String toString() {
        return "<LTRE: " + title + ">";
    }
}
