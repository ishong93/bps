// -*- Mode: Java -*-
//
// ATRE definitions and interface.
// Translated from ainter.lisp, last edited 1/29/93 by KDF.
//
// Copyright (c) 1990-1993, Kenneth D. Forbus, Northwestern University,
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
 * Represents a rule in the ATRE rule engine.
 * Rules are triggered by patterns matching facts in the database.
 * Corresponds to the Lisp defstruct rule.
 */
public class Rule {

    private int counter;             // Unique ID for easy lookup
    private Atre atre;               // The ATRE it is part of
    private Dbclass dbclass;         // Dbclass of associated pattern
    private Object matcher;          // Procedure that performs the match
    private Object body;             // Procedure that does the rule's work
    private List<TmsNode> inNodes;   // Must have a jointly non-empty label
    private List<TmsNode> impNodes;  // Must be implied by the focus

    public Rule(int counter, Atre atre, Dbclass dbclass,
                Object matcher, Object body) {
        this.counter = counter;
        this.atre = atre;
        this.dbclass = dbclass;
        this.matcher = matcher;
        this.body = body;
        this.inNodes = new ArrayList<>();
        this.impNodes = new ArrayList<>();
    }

    // --- Accessors ---

    public int getCounter() {
        return counter;
    }

    public void setCounter(int counter) {
        this.counter = counter;
    }

    public Atre getAtre() {
        return atre;
    }

    public void setAtre(Atre atre) {
        this.atre = atre;
    }

    public Dbclass getDbclass() {
        return dbclass;
    }

    public void setDbclass(Dbclass dbclass) {
        this.dbclass = dbclass;
    }

    public Object getMatcher() {
        return matcher;
    }

    public void setMatcher(Object matcher) {
        this.matcher = matcher;
    }

    public Object getBody() {
        return body;
    }

    public void setBody(Object body) {
        this.body = body;
    }

    public List<TmsNode> getInNodes() {
        return inNodes;
    }

    public void setInNodes(List<TmsNode> inNodes) {
        this.inNodes = inNodes;
    }

    public List<TmsNode> getImpNodes() {
        return impNodes;
    }

    public void setImpNodes(List<TmsNode> impNodes) {
        this.impNodes = impNodes;
    }

    @Override
    public String toString() {
        return "<Rule " + counter + ">";
    }
}
