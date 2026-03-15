package ltms;

import java.util.List;

/**
 * A disjunctive clause in the LTMS.
 */
public class Clause {
    public int index;
    public Object informant;
    public List<Literal> literals;
    public int pvs;      // potentially violating terms
    public int length;
    public int sats;     // satisfying terms
    public ClauseStatus status = ClauseStatus.NONE;

    public Clause(int index, List<Literal> literals, Object informant, int length) {
        this.index = index;
        this.literals = literals;
        this.informant = informant;
        this.length = length;
    }

    public boolean isSatisfied() {
        return sats > 0;
    }

    public boolean isViolated() {
        return pvs == 0;
    }

    @Override
    public String toString() {
        return "#<Clause " + index + ">";
    }
}
