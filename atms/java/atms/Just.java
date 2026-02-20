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
 * Represents a justification in the ATMS.
 * Corresponds to the Lisp defstruct just.
 */
public class Just {

    private int index;
    private Object informant;
    private TmsNode consequence;
    private List<TmsNode> antecedents;

    public Just(int index, Object informant, TmsNode consequence,
                List<TmsNode> antecedents) {
        this.index = index;
        this.informant = informant;
        this.consequence = consequence;
        this.antecedents = (antecedents != null)
            ? new ArrayList<>(antecedents)
            : new ArrayList<>();
    }

    // --- Accessors ---

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        this.index = index;
    }

    public Object getInformant() {
        return informant;
    }

    public void setInformant(Object informant) {
        this.informant = informant;
    }

    public TmsNode getConsequence() {
        return consequence;
    }

    public void setConsequence(TmsNode consequence) {
        this.consequence = consequence;
    }

    public List<TmsNode> getAntecedents() {
        return antecedents;
    }

    public void setAntecedents(List<TmsNode> antecedents) {
        this.antecedents = antecedents;
    }

    @Override
    public String toString() {
        return "<" + informant + " " + index + ">";
    }
}
