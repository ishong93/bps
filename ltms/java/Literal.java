package ltms;

/**
 * A literal is a (node, sign) pair in a clause.
 */
public class Literal {
    public TmsNode node;
    public NodeLabel sign;

    public Literal(TmsNode node, NodeLabel sign) {
        this.node = node;
        this.sign = sign;
    }

    @Override
    public String toString() {
        if (sign == NodeLabel.TRUE) {
            return node.nodeString();
        }
        return "(:NOT " + node.nodeString() + ")";
    }
}
