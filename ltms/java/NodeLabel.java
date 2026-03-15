package ltms;

/**
 * Three-valued label for TMS nodes.
 */
public enum NodeLabel {
    UNKNOWN(":UNKNOWN"),
    TRUE(":TRUE"),
    FALSE(":FALSE");

    private final String display;

    NodeLabel(String display) {
        this.display = display;
    }

    @Override
    public String toString() {
        return display;
    }
}
