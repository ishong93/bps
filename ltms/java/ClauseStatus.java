package ltms;

/**
 * Status of a clause in the LTMS.
 */
public enum ClauseStatus {
    NONE,
    SUBSUMED,
    QUEUED,
    DIRTY,
    NOT_INDEXED
}
