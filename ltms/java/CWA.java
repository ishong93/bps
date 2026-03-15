package ltms;

import java.util.*;

public class CWA {

    // Returns [members, cwa] or null
    public static Object[] setMembers(Object setName, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        List<Object> pattern = Arrays.asList(setName, "MEMBERS", "?elements");
        List<Object> matches = LTRE.fetch(pattern, ltre);
        for (Object mform : matches) {
            if (LTRE.isTrue(mform, ltre)) {
                if (mform instanceof List) {
                    List<?> lst = (List<?>) mform;
                    if (lst.size() >= 3) {
                        Object cwa = findCWAForSet(mform, ltre);
                        return new Object[]{lst.get(2), cwa};
                    }
                }
            }
        }
        return null;
    }

    // Returns [members, cwa, alreadyClosed]
    public static Object[] closeSetIfNeeded(Object setName, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        Object[] result = setMembers(setName, ltre);
        if (result != null && result[1] != null) {
            return new Object[]{result[0], result[1], false};
        }
        return closeSet(setName, ltre);
    }

    public static Object[] closeSet(Object setName, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        boolean nonContradictory = false;
        try {
            Object[][] setInfo = getSetInformation(setName, ltre);
            List<Object> knownMembers = Arrays.asList((Object[]) setInfo[0]);
            List<Object> knownNot = Arrays.asList((Object[]) setInfo[1]);

            Object cwaForm = makeCWAForm(setName, knownMembers);
            List<Object> membersForm = Arrays.asList(setName, "MEMBERS", knownMembers);

            retractCWAs(setName, ltre);
            assumeCWAIfNeeded(cwaForm, ltre);
            justifyCWAIfNeeded(setName, knownMembers, knownNot, cwaForm, membersForm, ltre);
            nonContradictory = true;
            return new Object[]{knownMembers, cwaForm, true};
        } finally {
            if (!nonContradictory) retractCWAs(setName, ltre);
        }
    }

    public static Object findCWAForSet(Object ms, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        if (!(ms instanceof List)) return null;
        List<?> msLst = (List<?>) ms;
        if (msLst.isEmpty()) return null;
        Object setName = msLst.get(0);
        List<TmsNode> asns = LTRE.assumptionsOf(ms, ltre);
        for (TmsNode asn : asns) {
            Object v = LTRE.viewNode(asn);
            if (isCWAForm(v)) {
                if (v instanceof List) {
                    List<?> lst = (List<?>) v;
                    if (!lst.isEmpty() && LTRE.equalForms(lst.get(0), setName)) {
                        return v;
                    }
                }
            }
        }
        return null;
    }

    public static boolean isCWAForm(Object form) {
        if (!(form instanceof List)) return false;
        List<?> lst = (List<?>) form;
        return lst.size() >= 2 && "CWA".equals(lst.get(1));
    }

    public static Object makeCWAForm(Object setName, Object members) {
        return Arrays.asList(setName, "CWA", members);
    }

    public static Object[] parseCWAForm(Object cwaForm) {
        if (cwaForm instanceof List) {
            List<?> lst = (List<?>) cwaForm;
            if (lst.size() >= 3) return new Object[]{lst.get(0), lst.get(2)};
        }
        return new Object[]{null, null};
    }

    @SuppressWarnings("unchecked")
    public static Object[][] getSetInformation(Object setName, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        List<Object> knownIn = new ArrayList<>();
        List<Object> knownOut = new ArrayList<>();
        List<Object> pattern = Arrays.asList(setName, "HAS-MEMBER", "?member");
        List<Object> matches = LTRE.fetch(pattern, ltre);
        for (Object possible : matches) {
            if (LTRE.isTrue(possible, ltre)) {
                if (possible instanceof List && ((List<?>)possible).size() >= 3)
                    knownIn.add(((List<?>)possible).get(2));
            } else if (LTRE.isFalse(possible, ltre)) {
                if (possible instanceof List && ((List<?>)possible).size() >= 3)
                    knownOut.add(((List<?>)possible).get(2));
            }
        }
        return new Object[][]{knownIn.toArray(), knownOut.toArray()};
    }

    public static void assumeCWAIfNeeded(Object cwaForm, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        if (LTRE.isFalse(cwaForm, ltre)) {
            TmsNode node = LTRE.getTmsNode(cwaForm, ltre);
            LTMS.propagateUnknownness(node);
        }
        LTRE.assumeFact(cwaForm, ":CWA", ltre);
    }

    public static void justifyCWAIfNeeded(Object name, List<Object> members, List<Object> notMembers,
            Object cwaForm, Object membersForm, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        List<Object> anteParts = new ArrayList<>();
        anteParts.add(":AND");
        anteParts.add(Arrays.asList("SET", name));
        for (Object el : members) {
            anteParts.add(Arrays.asList(name, "HAS-MEMBER", el));
        }
        for (Object el : notMembers) {
            anteParts.add(Arrays.asList(":NOT", Arrays.asList(name, "HAS-MEMBER", el)));
        }
        anteParts.add(cwaForm);
        LTRE.assertFact(Arrays.asList("CWA-JUSTIFICATION", anteParts, membersForm), ":SET-CWA-CLOSURE", ltre);
    }

    public static Object fetchCWAFor(Object name, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        List<Object> pattern = Arrays.asList(name, "CWA", "?x");
        List<Object> matches = LTRE.fetch(pattern, ltre);
        for (Object cwa : matches) {
            if (LTRE.isTrue(cwa, ltre)) return cwa;
        }
        return null;
    }

    public static boolean setCWAHandler(List<Clause> clauses, LTMS l, Object setName, Object cwa, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        TmsNode cwaNode = LTRE.getTmsNode(cwa, ltre);
        for (Clause cl : clauses) {
            List<TmsNode> asns = LTMS.assumptionsOfClause(cl);
            boolean hasCWA = asns.contains(cwaNode);
            if (hasCWA && cwaInvalid(cwa, ltre)) {
                retractCWA(cwa, ltre);
                return true;
            }
        }
        return false;
    }

    public static void retractCWA(Object cwa, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        Object assumed = LTRE.alreadyAssumed(cwa, ltre);
        if (assumed != null) LTRE.retractFact(cwa, assumed, ltre, true);
    }

    @SuppressWarnings("unchecked")
    public static boolean cwaInvalid(Object cwa, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        Object[] parsed = parseCWAForm(cwa);
        Object setName = parsed[0];
        Object presumedEls = parsed[1];
        if (presumedEls instanceof List) {
            for (Object el : (List<Object>) presumedEls) {
                if (!LTRE.isTrue(Arrays.asList(setName, "HAS-MEMBER", el), ltre)) return true;
            }
        }
        List<Object> pattern = Arrays.asList(setName, "HAS-MEMBER", "?el");
        List<Object> matches = LTRE.fetch(pattern, ltre);
        for (Object hmForm : matches) {
            if (LTRE.isTrue(hmForm, ltre)) {
                if (hmForm instanceof List && ((List<?>)hmForm).size() >= 3) {
                    Object member = ((List<?>)hmForm).get(2);
                    if (presumedEls instanceof List) {
                        boolean found = false;
                        for (Object el : (List<Object>) presumedEls) {
                            if (LTRE.equalForms(member, el)) { found = true; break; }
                        }
                        if (!found) return true;
                    }
                }
            }
        }
        return false;
    }

    public static void retractCWAs(Object set, LTRE ltre) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        List<Object> pattern = Arrays.asList(set, "CWA", "?members");
        List<Object> matches = LTRE.fetch(pattern, ltre);
        for (Object cwa : matches) {
            if (LTRE.isKnown(cwa, ltre)) {
                Object assumed = LTRE.alreadyAssumed(cwa, ltre);
                if (assumed != null) LTRE.retractFact(cwa, assumed, ltre, true);
            }
        }
    }

    // withClosedSet - returns [success, members]
    public static Object[] withClosedSet(Object setName, LTRE ltre, Runnable body) {
        if (ltre == null) ltre = LTRE.currentLTRE;
        Object[] closed = closeSetIfNeeded(setName, ltre);
        Object cwa = closed != null ? closed[1] : null;
        boolean[] caught = {false};
        final LTRE finalLtre = ltre;

        LTMS.withContradictionHandler(ltre.ltms,
            (clauses, ltms) -> {
                if (setCWAHandler(clauses, ltms, setName, cwa, finalLtre)) {
                    caught[0] = true;
                    return true;
                }
                return false;
            },
            () -> {
                try {
                    body.run();
                } catch (CWALostException e) {
                    caught[0] = true;
                }
            });

        if (caught[0]) return new Object[]{false, null};
        return new Object[]{true, closed != null ? closed[0] : null};
    }

    public static class CWALostException extends RuntimeException {
        public CWALostException() { super("Lost CWA"); }
    }
}
