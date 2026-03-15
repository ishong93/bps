package ltms;

import java.util.ArrayList;
import java.util.List;

/**
 * A node in the LTMS.
 */
public class TmsNode {
    public int index;
    public Object datum;
    public NodeLabel label = NodeLabel.UNKNOWN;
    public Object support;  // Clause, ENABLED_ASSUMPTION string, or null
    public List<Clause> trueClauses = new ArrayList<>();
    public List<Clause> falseClauses = new ArrayList<>();
    public Object mark;
    public boolean isAssumption;
    public List<Object> trueRules = new ArrayList<>();
    public List<Object> falseRules = new ArrayList<>();
    public LTMS ltms;
    public Literal trueLiteral;
    public Literal falseLiteral;

    public TmsNode(int index, Object datum, boolean isAssumption, LTMS ltms) {
        this.index = index;
        this.datum = datum;
        this.isAssumption = isAssumption;
        this.ltms = ltms;
        this.trueLiteral = new Literal(this, NodeLabel.TRUE);
        this.falseLiteral = new Literal(this, NodeLabel.FALSE);
    }

    public boolean isUnknown() { return label == NodeLabel.UNKNOWN; }
    public boolean isKnown() { return label != NodeLabel.UNKNOWN; }
    public boolean isTrue() { return label == NodeLabel.TRUE; }
    public boolean isFalse() { return label == NodeLabel.FALSE; }

    public String nodeString() {
        if (ltms != null && ltms.nodeString != null) {
            return ltms.nodeString.apply(this);
        }
        return String.valueOf(datum);
    }

    @Override
    public String toString() {
        return "#<NODE: " + nodeString() + ">";
    }
}
