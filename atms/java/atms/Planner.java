// -*- Mode: Java -*-

// ATMS-based planner using ATRE + ATMS
// Translated from aplanr.lisp

// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

import java.io.PrintWriter;
import java.util.*;

/**
 * Planning problem structure and utilities.
 */
public class Planner {

    private String title;
    private Atre atre;
    private List<List<List<Object>>> basisSet; // list of choice sets
    private Map<String, Operator> operators;
    private Map<String, Object> plist;

    public Planner(String title, Atre atre, List<List<List<Object>>> basisSet) {
        this.title = title;
        this.atre = atre;
        this.basisSet = basisSet != null ? basisSet : new ArrayList<>();
        this.operators = new LinkedHashMap<>();
        this.plist = new HashMap<>();
    }

    @Override
    public String toString() {
        return String.format("<PlnPr: %s>", title);
    }

    // Getters and setters
    public String getTitle() { return title; }
    public Atre getAtre() { return atre; }
    public List<List<List<Object>>> getBasisSet() { return basisSet; }
    public Map<String, Operator> getOperators() { return operators; }
    public Map<String, Object> getPlist() { return plist; }

    /**
     * Operator definition.
     */
    public static class Operator {
        private List<Object> form;
        private List<List<Object>> preconditions;
        private List<List<Object>> addList;
        private List<List<Object>> deleteList;

        public Operator(List<Object> form,
                         List<List<Object>> preconditions,
                         List<List<Object>> addList,
                         List<List<Object>> deleteList) {
            this.form = form;
            this.preconditions = preconditions;
            this.addList = addList;
            this.deleteList = deleteList;
        }

        @Override
        public String toString() {
            return String.format("<Operator %s>", form);
        }

        public List<Object> getForm() { return form; }
        public List<List<Object>> getPreconditions() { return preconditions; }
        public List<List<Object>> getAddList() { return addList; }
        public List<List<Object>> getDeleteList() { return deleteList; }
    }

    /**
     * Create a new planning problem.
     */
    public static Planner createPlanningProblem(String title,
                                                  List<List<List<Object>>> basisSet) {
        Atre atre = Atre.createAtre(String.format("ATRE(%s)", title), false);
        return new Planner(title, atre, basisSet);
    }

    /**
     * Set up choice sets as assumptions.
     */
    public void setupChoiceSets() {
        String informant = String.format("BASIS SET(%s)", title);
        for (List<List<Object>> choiceSet : basisSet) {
            for (List<Object> choice : choiceSet) {
                AtreData.assumeIfNeeded(choice, informant, atre);
            }
        }
        AtreRules.runRules(atre);
    }

    /**
     * Set debugging state.
     */
    public void setDebug(boolean state) {
        plist.put("DEBUGGING", state);
    }

    /**
     * Check if debugging is enabled.
     */
    public boolean isDebugging() {
        Object val = plist.get("DEBUGGING");
        return val instanceof Boolean && (Boolean) val;
    }

    /**
     * Define an operator.
     */
    public void defineOperator(List<Object> form,
                                List<List<Object>> preconditions,
                                List<List<Object>> addList,
                                List<List<Object>> deleteList) {
        String opName = String.valueOf(form.get(0));
        operators.put(opName, new Operator(form, preconditions, addList, deleteList));
    }

    /**
     * Find applicable operator instances in the given state.
     */
    @SuppressWarnings("unchecked")
    public List<List<Object>> findApplicableOperators(Env state) {
        List<List<Object>> result = new ArrayList<>();
        List<Object> pattern = new ArrayList<>();
        pattern.add("applicable");
        pattern.add("?x");

        List<Object> candidates = AtreData.fetch(pattern, atre);
        for (Object candidate : candidates) {
            if (candidate instanceof List) {
                List<Object> candList = (List<Object>) candidate;
                if (AtreData.isIn(candidate, state, atre) && candList.size() > 1) {
                    if (candList.get(1) instanceof List) {
                        result.add((List<Object>) candList.get(1));
                    }
                }
            }
        }
        return result;
    }

    /**
     * Fetch an operator by name.
     */
    public Operator fetchOperator(String opName) {
        return operators.get(opName);
    }

    /**
     * Apply an operator instance to a state, producing a new state.
     */
    @SuppressWarnings("unchecked")
    public Env applyOperator(Env state, List<Object> opInst) {
        String opName = String.valueOf(opInst.get(0));
        Operator operator = fetchOperator(opName);
        if (operator == null) return null;

        List<Object> vals = opInst.subList(1, opInst.size());
        List<TmsNode> assumptions = new ArrayList<>(state.getAssumptions());

        // Create bindings
        Map<String, Object> bindings = new HashMap<>();
        List<Object> formVars = operator.getForm().subList(1, operator.getForm().size());
        for (int i = 0; i < formVars.size() && i < vals.size(); i++) {
            bindings.put(String.valueOf(formVars.get(i)), vals.get(i));
        }

        // Substitute in add and delete lists
        List<List<Object>> addList = substituteBindings(operator.getAddList(), bindings);
        List<List<Object>> deleteList = substituteBindings(operator.getDeleteList(), bindings);

        // Remove delete-list items
        assumptions.removeIf(a -> {
            Object datum = a.getDatum();
            if (datum instanceof Datum) {
                Object form = ((Datum) datum).getLispForm();
                for (List<Object> del : deleteList) {
                    if (deepEquals(form, del)) return true;
                }
            }
            return false;
        });

        // Add add-list items
        for (List<Object> add : addList) {
            TmsNode node = AtreData.getTmsNode(add, atre);
            // Insert in order
            int pos = 0;
            while (pos < assumptions.size() &&
                   assumptions.get(pos).getIndex() < node.getIndex()) {
                pos++;
            }
            if (pos >= assumptions.size() || assumptions.get(pos) != node) {
                assumptions.add(pos, node);
            }
        }

        return atre.getAtms().findOrMakeEnv(assumptions);
    }

    private List<List<Object>> substituteBindings(List<List<Object>> patterns,
                                                    Map<String, Object> bindings) {
        List<List<Object>> result = new ArrayList<>();
        for (List<Object> p : patterns) {
            result.add(substitutePattern(p, bindings));
        }
        return result;
    }

    @SuppressWarnings("unchecked")
    private List<Object> substitutePattern(List<Object> pattern,
                                            Map<String, Object> bindings) {
        List<Object> result = new ArrayList<>();
        for (Object elem : pattern) {
            if (elem instanceof String && bindings.containsKey((String) elem)) {
                result.add(bindings.get((String) elem));
            } else if (elem instanceof List) {
                result.add(substitutePattern((List<Object>) elem, bindings));
            } else {
                result.add(elem);
            }
        }
        return result;
    }

    /**
     * Fetch states consistent with facts.
     */
    @SuppressWarnings("unchecked")
    public List<Env> fetchStates(List<Object> facts) {
        List<List<TmsNode>> choiceSets = new ArrayList<>();
        for (Object fact : facts) {
            List<TmsNode> single = new ArrayList<>();
            single.add(AtreData.getTmsNode(fact, atre));
            choiceSets.add(single);
        }
        for (List<List<Object>> bs : basisSet) {
            List<TmsNode> nodes = new ArrayList<>();
            for (List<Object> choice : bs) {
                nodes.add(AtreData.getTmsNode(choice, atre));
            }
            choiceSets.add(nodes);
        }
        return atre.getAtms().interpretations(choiceSets, null);
    }

    /**
     * Check if a state satisfies goals.
     */
    public boolean satisfiesGoal(Env state, List<Object> goals) {
        return checkGoals(goals, state, null);
    }

    private boolean checkGoals(List<Object> goals, Env state,
                                List<Unify.Binding> bindings) {
        if (goals == null || goals.isEmpty()) return true;

        Object firstGoal = goals.get(0);
        List<Object> restGoals = goals.subList(1, goals.size());

        List<Object> candidates = AtreData.fetch(firstGoal, atre);
        for (Object candidate : candidates) {
            if (AtreData.isIn(candidate, state, atre)) {
                Unify.UnifyResult result = Unify.unify(firstGoal, candidate,
                        bindings);
                if (result.isSuccess()) {
                    if (checkGoals(restGoals, state, result.getBindings())) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /**
     * Display a plan.
     */
    public static void showPlan(List<Object> plan, PrintWriter stream) {
        List<Object> reversed = new ArrayList<>(plan);
        Collections.reverse(reversed);
        for (int i = 0; i < reversed.size(); i += 2) {
            if (reversed.get(i) instanceof Env) {
                // Print the environment
                stream.printf("%n%s", reversed.get(i));
            }
            if (i + 1 < reversed.size() && reversed.get(i + 1) != null) {
                stream.printf("%n  then, by %s, ", reversed.get(i + 1));
            }
        }
        stream.flush();
    }

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
