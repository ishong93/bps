package ltms;

/**
 * A datum represents a fact stored in the LTRE database.
 * Each datum is linked to a TMS node for truth maintenance.
 * Converted from ldata.lisp in "Building Problem Solvers".
 */
public class Datum {
    public int counter;
    public LTRE ltre;
    public Object lispForm;
    public TmsNode tmsNode;
    public Dbclass dbclass;
    public Object assumption; // null or informant string/object

    public Datum(int counter, LTRE ltre, Object lispForm, Dbclass dbclass) {
        this.counter = counter;
        this.ltre = ltre;
        this.lispForm = lispForm;
        this.dbclass = dbclass;
    }

    public String showDatum() {
        return String.valueOf(lispForm);
    }

    @Override
    public String toString() {
        return "<Datum " + counter + ">";
    }
}
