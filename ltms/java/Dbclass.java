package ltms;

import java.util.*;

/**
 * Database class for organizing facts and rules in the LTRE system.
 * Converted from ldata.lisp in "Building Problem Solvers".
 */
public class Dbclass {
    public String name;
    public LTRE ltre;
    public List<Datum> facts = new ArrayList<>();
    public List<LtreRules.Rule> rules = new ArrayList<>();

    public Dbclass(String name, LTRE ltre) {
        this.name = name;
        this.ltre = ltre;
    }

    @Override
    public String toString() {
        return "<Dbclass " + name + ">";
    }
}
