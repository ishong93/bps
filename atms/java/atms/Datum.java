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

import java.util.HashMap;
import java.util.Map;

/**
 * Represents a datum (fact) stored in the ATRE database.
 * Each datum wraps a lisp-form (pattern) and is linked to a TMS node.
 * Corresponds to the Lisp defstruct datum.
 */
public class Datum {

    private int counter;           // Unique ID for easy lookup
    private Atre atre;             // The ATRE it is part of
    private Object lispForm;       // Expression for pattern-matching
    private TmsNode tmsNode;       // Pointer into TMS
    private Dbclass dbclass;       // Dbclass of the corresponding pattern
    private Object assumption;     // If non-null, indicates informant
    private Map<String, Object> plist; // Local property list

    public Datum(int counter, Atre atre, Object lispForm, Dbclass dbclass) {
        this.counter = counter;
        this.atre = atre;
        this.lispForm = lispForm;
        this.dbclass = dbclass;
        this.tmsNode = null;
        this.assumption = null;
        this.plist = new HashMap<>();
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

    public Object getLispForm() {
        return lispForm;
    }

    public void setLispForm(Object lispForm) {
        this.lispForm = lispForm;
    }

    public TmsNode getTmsNode() {
        return tmsNode;
    }

    public void setTmsNode(TmsNode tmsNode) {
        this.tmsNode = tmsNode;
    }

    public Dbclass getDbclass() {
        return dbclass;
    }

    public void setDbclass(Dbclass dbclass) {
        this.dbclass = dbclass;
    }

    /**
     * Returns the assumption informant, or null if this datum
     * is not an assumption.
     */
    public Object getAssumption() {
        return assumption;
    }

    /**
     * Returns true if this datum has been assumed.
     */
    public boolean isAssumption() {
        return assumption != null;
    }

    public void setAssumption(Object assumption) {
        this.assumption = assumption;
    }

    public Map<String, Object> getPlist() {
        return plist;
    }

    public void setPlist(Map<String, Object> plist) {
        this.plist = plist;
    }

    @Override
    public String toString() {
        return "<Datum " + counter + ">";
    }
}
