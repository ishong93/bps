// -*- Mode: Java -*-

// Antecedent Planner (a.k.a. Plan-A)
// Translated from plan-a.lisp

// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * Antecedent planner using breadth-first search through the state space.
 */
public class PlanA {

    /**
     * Result of a planning search.
     */
    public static class PlanResult {
        public final List<Object> plan;
        public final int numberExamined;

        public PlanResult(List<Object> plan, int numberExamined) {
            this.plan = plan;
            this.numberExamined = numberExamined;
        }
    }

    /**
     * Perform antecedent planning from a start state to a goal.
     * Uses BFS through the state space applying operators.
     *
     * @param start  the starting environment/state
     * @param goals  list of goal conditions
     * @param planner the planning problem
     * @return the plan and number of states examined
     */
    public static PlanResult planA(Env start, List<Object> goals, Planner planner) {
        LinkedList<List<Object>> queue = new LinkedList<>();
        List<Object> initial = new ArrayList<>();
        initial.add(start);
        queue.add(initial);

        int numberExamined = 1;

        while (!queue.isEmpty()) {
            List<Object> current = queue.removeFirst();
            Env currentState = (Env) current.get(0);

            // Check if current state satisfies goals
            if (planner.satisfiesGoal(currentState, goals)) {
                planner.getPlist().put("PLAN", current);
                return new PlanResult(current, numberExamined);
            }

            // Try all applicable operators
            for (List<Object> opInst : planner.findApplicableOperators(currentState)) {
                Env result = planner.applyOperator(currentState, opInst);
                if (result == null) continue;

                // Check for loops
                boolean alreadyVisited = false;
                for (Object item : current) {
                    if (item instanceof Env && item == result) {
                        alreadyVisited = true;
                        break;
                    }
                }

                if (!alreadyVisited) {
                    if (planner.isDebugging()) {
                        System.out.printf("%n  Reaching %s via %s on %s..",
                                result, opInst, currentState);
                    }

                    List<Object> newPath = new ArrayList<>();
                    newPath.add(result);
                    newPath.add(opInst);
                    newPath.addAll(current);
                    queue.addLast(newPath);
                }
            }

            numberExamined++;
        }

        planner.getPlist().put("PLAN", null);
        return new PlanResult(null, numberExamined);
    }
}
