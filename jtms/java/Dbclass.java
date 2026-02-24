// Dbclass: groups facts and rules by predicate name.
// Translated from jdata.lisp in "Building Problem Solvers".
package jtms;

import java.util.ArrayList;
import java.util.List;

/**
 * Dbclass groups facts and rules that share a common predicate name.
 */
public class Dbclass {
    private String name;
    private Jtre jtre;
    private List<Datum> facts = new ArrayList<>();
    private List<Rule> rules = new ArrayList<>();

    public Dbclass(String name, Jtre jtre) {
        this.name = name;
        this.jtre = jtre;
    }

    public String getName() { return name; }
    public Jtre getJtre() { return jtre; }
    public List<Datum> getFacts() { return facts; }
    public List<Rule> getRules() { return rules; }

    @Override
    public String toString() {
        return String.format("<Dbclass %s>", name);
    }
}
