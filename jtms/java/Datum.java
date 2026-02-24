// Datum: represents a single fact in the JTRE database.
// Translated from jdata.lisp in "Building Problem Solvers".
package jtms;

import java.util.HashMap;
import java.util.Map;

/**
 * Datum represents a single fact in the JTRE database.
 */
public class Datum {
    private int id;
    private Object lispForm;
    private JTMS.TmsNode tmsNode;
    private Dbclass dbclass;
    private Object assumption;  // non-null indicates informant
    private Map<String, Object> plist = new HashMap<>();

    public Datum(int id, Object lispForm, Dbclass dbclass) {
        this.id = id;
        this.lispForm = lispForm;
        this.dbclass = dbclass;
    }

    public int getId() { return id; }
    public Object getLispForm() { return lispForm; }
    public JTMS.TmsNode getTmsNode() { return tmsNode; }
    public void setTmsNode(JTMS.TmsNode node) { this.tmsNode = node; }
    public Dbclass getDbclass() { return dbclass; }
    public Object getAssumption() { return assumption; }
    public void setAssumption(Object a) { this.assumption = a; }
    public boolean isAssumption() { return assumption != null; }
    public Map<String, Object> getPlist() { return plist; }

    @Override
    public String toString() {
        return String.format("<Datum %d>", id);
    }
}
