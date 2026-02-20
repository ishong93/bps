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
 * Represents a node in the ATMS.
 * Corresponds to the Lisp defstruct tms-node.
 */
public class TmsNode {

    private int index;          // Unique name
    private Object datum;       // Pointer to IE data structures
    private List<Env> label;    // Minimal envs believed under
    private List<Just> justs;   // Providers of support
    private List<Just> consequences; // Provides support for
    private boolean contradictory;   // Flag marking it as contradictory
    private boolean assumption;      // Flag marking it as an assumption
    private List<Object> rules;      // Run when label non-empty
    private ATMS atms;

    public TmsNode(int index, Object datum, boolean assumption,
                   boolean contradictory, ATMS atms) {
        this.index = index;
        this.datum = datum;
        this.assumption = assumption;
        this.contradictory = contradictory;
        this.atms = atms;
        this.label = new ArrayList<>();
        this.justs = new ArrayList<>();
        this.consequences = new ArrayList<>();
        this.rules = new ArrayList<>();
    }

    // --- Accessors ---

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        this.index = index;
    }

    public Object getDatum() {
        return datum;
    }

    public void setDatum(Object datum) {
        this.datum = datum;
    }

    public List<Env> getLabel() {
        return label;
    }

    public void setLabel(List<Env> label) {
        this.label = label;
    }

    public List<Just> getJusts() {
        return justs;
    }

    public void setJusts(List<Just> justs) {
        this.justs = justs;
    }

    public List<Just> getConsequences() {
        return consequences;
    }

    public void setConsequences(List<Just> consequences) {
        this.consequences = consequences;
    }

    public boolean isContradictory() {
        return contradictory;
    }

    public void setContradictory(boolean contradictory) {
        this.contradictory = contradictory;
    }

    public boolean isAssumption() {
        return assumption;
    }

    public void setAssumption(boolean assumption) {
        this.assumption = assumption;
    }

    public List<Object> getRules() {
        return rules;
    }

    public void setRules(List<Object> rules) {
        this.rules = rules;
    }

    public ATMS getAtms() {
        return atms;
    }

    public void setAtms(ATMS atms) {
        this.atms = atms;
    }

    /**
     * Returns the node-string for this node using the ATMS's node-string function.
     */
    public String nodeString() {
        return atms.getNodeString().apply(this);
    }

    @Override
    public String toString() {
        if (assumption) {
            return "A-" + index;
        } else {
            return "<NODE: " + nodeString() + ">";
        }
    }
}
