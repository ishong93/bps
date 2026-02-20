// -*- Mode: Java -*-
//
// Assumption-based truth maintenance system, version 61 of 7/21/92.
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty. The above copyright notice and that
// paragraph must be included in any separate copy of this file.

package atms;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents an environment (a set of assumptions) in the ATMS.
 * Corresponds to the Lisp defstruct env.
 */
public class Env {

    private int index;
    private int count; // Number of assumptions
    private List<TmsNode> assumptions;
    private List<TmsNode> nodes;
    // nogood? can hold a Just (the justification that made it nogood)
    // or an Env (a subset env that is nogood), or null if not nogood.
    private Object nogood;
    private List<Object> rules; // Call these if becomes nogood

    public Env(int index, int count, List<TmsNode> assumptions) {
        this.index = index;
        this.count = count;
        this.assumptions = (assumptions != null)
            ? new ArrayList<>(assumptions)
            : new ArrayList<>();
        this.nodes = new ArrayList<>();
        this.nogood = null;
        this.rules = new ArrayList<>();
    }

    // --- Accessors ---

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        this.index = index;
    }

    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }

    public List<TmsNode> getAssumptions() {
        return assumptions;
    }

    public void setAssumptions(List<TmsNode> assumptions) {
        this.assumptions = assumptions;
    }

    public List<TmsNode> getNodes() {
        return nodes;
    }

    public void setNodes(List<TmsNode> nodes) {
        this.nodes = nodes;
    }

    /**
     * Returns the nogood marker. null means not nogood.
     * Can be a Just or an Env.
     */
    public Object getNogood() {
        return nogood;
    }

    /**
     * Returns true if this environment is nogood (contradictory).
     */
    public boolean isNogood() {
        return nogood != null;
    }

    public void setNogood(Object nogood) {
        this.nogood = nogood;
    }

    public List<Object> getRules() {
        return rules;
    }

    public void setRules(List<Object> rules) {
        this.rules = rules;
    }

    @Override
    public String toString() {
        return "E-" + index;
    }
}
