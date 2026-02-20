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
 * Represents a database class in ATRE.
 * Groups facts and rules that share a common leading symbol.
 * Corresponds to the Lisp defstruct dbclass.
 */
public class Dbclass {

    private String name;       // Corresponding symbol
    private Atre atre;         // ATRE it is part of
    private List<Datum> facts; // Associated facts
    private List<Rule> rules;  // Associated rules

    public Dbclass(String name, Atre atre) {
        this.name = name;
        this.atre = atre;
        this.facts = new ArrayList<>();
        this.rules = new ArrayList<>();
    }

    // --- Accessors ---

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Atre getAtre() {
        return atre;
    }

    public void setAtre(Atre atre) {
        this.atre = atre;
    }

    public List<Datum> getFacts() {
        return facts;
    }

    public void setFacts(List<Datum> facts) {
        this.facts = facts;
    }

    public List<Rule> getRules() {
        return rules;
    }

    public void setRules(List<Rule> rules) {
        this.rules = rules;
    }

    @Override
    public String toString() {
        return "<Dbclass " + name + ">";
    }
}
