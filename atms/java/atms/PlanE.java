// -*- Mode: Java -*-

// ATMS-based Envisioner for planning problems
// Translated from plan-e.lisp

// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

import java.io.PrintWriter;
import java.util.*;

/**
 * Envisioner: generates all states and transitions, and searches for plans.
 */
public class PlanE {

    /**
     * A state transition via an operator.
     */
    public static class Transition {
        public final List<Object> opInst;
        public final Env newState;

        public Transition(List<Object> opInst, Env newState) {
            this.opInst = opInst;
            this.newState = newState;
        }
    }

    /**
     * A state and its possible transitions.
     */
    public static class StateEntry {
        public final Env state;
        public final List<Transition> transitions;

        public StateEntry(Env state, List<Transition> transitions) {
            this.state = state;
            this.transitions = transitions;
        }
    }

    /**
     * Generate all states and transitions for the planning problem.
     */
    @SuppressWarnings("unchecked")
    public static List<StateEntry> envision(Planner planner) {
        List<List<TmsNode>> choiceSets = new ArrayList<>();
        for (List<List<Object>> bs : planner.getBasisSet()) {
            List<TmsNode> nodes = new ArrayList<>();
            for (List<Object> choice : bs) {
                nodes.add(AtreData.getTmsNode(choice, planner.getAtre()));
            }
            choiceSets.add(nodes);
        }

        List<Env> states = planner.getAtre().getAtms()
                .interpretations(choiceSets, null);
        planner.getPlist().put("STATES", states);

        List<StateEntry> transTable = applyAllOperators(states, planner);
        planner.getPlist().put("TRANSITIONS", transTable);
        return transTable;
    }

    private static List<StateEntry> applyAllOperators(List<Env> states,
                                                        Planner planner) {
        List<StateEntry> result = new ArrayList<>();
        for (Env state : states) {
            List<Transition> transitions = new ArrayList<>();
            for (List<Object> opInst : planner.findApplicableOperators(state)) {
                Env newState = planner.applyOperator(state, opInst);
                if (newState != null) {
                    transitions.add(new Transition(opInst, newState));
                }
            }
            result.add(new StateEntry(state, transitions));
        }
        return result;
    }

    /**
     * Display the envisionment.
     */
    @SuppressWarnings("unchecked")
    public static void showEnvisionment(Planner planner, PrintWriter stream) {
        Object statesVal = planner.getPlist().get("STATES");
        List<Env> states = statesVal instanceof List ? (List<Env>) statesVal : null;

        if (states == null || states.isEmpty()) {
            stream.println("\nThe state space is empty.");
            stream.flush();
            return;
        }

        stream.printf("%n %d states have been generated:", states.size());
        for (Env state : states) {
            stream.printf("%n%s", state);
        }

        stream.printf("%nTransition Table:");
        Object transVal = planner.getPlist().get("TRANSITIONS");
        List<StateEntry> transTable = transVal instanceof List ?
                (List<StateEntry>) transVal : null;

        if (transTable == null || transTable.isEmpty()) {
            stream.print(" empty.");
        } else {
            for (StateEntry entry : transTable) {
                stream.printf("%n  %s: ", entry.state);
                for (Transition t : entry.transitions) {
                    stream.printf("%n   %s -> %s", t.opInst, t.newState);
                }
            }
        }
        stream.flush();
    }

    /**
     * Find a plan by searching the envisionment.
     */
    @SuppressWarnings("unchecked")
    public static List<Object> findPlan(List<Object> start, List<Object> goals,
                                          Planner planner) {
        List<Env> goalStates = planner.fetchStates(goals);
        List<Env> startStates = planner.fetchStates(start);

        if (planner.isDebugging()) {
            System.out.printf("%nInitial states are %s.", startStates);
            System.out.printf("%nGoal states are %s.", goalStates);
        }

        Object transVal = planner.getPlist().get("TRANSITIONS");
        List<StateEntry> transTable = transVal instanceof List ?
                (List<StateEntry>) transVal : new ArrayList<>();

        // Initialize queue
        LinkedList<List<Object>> queue = new LinkedList<>();
        for (Env state : startStates) {
            List<Object> path = new ArrayList<>();
            path.add(state);
            queue.add(path);
        }

        while (!queue.isEmpty()) {
            List<Object> current = queue.removeFirst();
            Env currentState = (Env) current.get(0);

            // Check if we've reached a goal state
            boolean isGoal = goalStates.contains(currentState);
            if (isGoal) {
                planner.getPlist().put("PLAN", current);
                return current;
            }

            // Find transitions for the current state
            for (StateEntry entry : transTable) {
                if (entry.state != currentState) continue;
                for (Transition trans : entry.transitions) {
                    // Avoid loops
                    boolean visited = false;
                    for (int i = 2; i < current.size(); i++) {
                        if (current.get(i) instanceof Env &&
                            current.get(i) == trans.newState) {
                            visited = true;
                            break;
                        }
                    }

                    if (!visited) {
                        if (planner.isDebugging()) {
                            System.out.printf("%n Can reach %s via %s from %s.",
                                    trans.newState, trans.opInst, currentState);
                        }
                        List<Object> newPath = new ArrayList<>();
                        newPath.add(trans.newState);
                        newPath.add(trans.opInst);
                        newPath.addAll(current);
                        queue.addLast(newPath);
                    }
                }
            }
        }

        planner.getPlist().put("PLAN", null);
        return null;
    }
}
