// -*- Mode: Java -*-
//
// Assumption-based truth maintenance system, version 61 of 7/21/92.
//
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
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
 * An entry in an environment table, associating an assumption count
 * with a list of environments having that many assumptions.
 *
 * The Lisp code uses an association list of (count . envs).
 * This class represents one such entry.
 */
public class EnvTable {

    /**
     * Represents a single (count, envs) entry in the table.
     */
    public static class Entry {
        private int count;
        private List<Env> envs;

        public Entry(int count, List<Env> envs) {
            this.count = count;
            this.envs = (envs != null) ? envs : new ArrayList<>();
        }

        public Entry(int count) {
            this(count, new ArrayList<>());
        }

        public int getCount() {
            return count;
        }

        public void setCount(int count) {
            this.count = count;
        }

        public List<Env> getEnvs() {
            return envs;
        }

        public void setEnvs(List<Env> envs) {
            this.envs = envs;
        }

        @Override
        public String toString() {
            return "(count=" + count + ", envs=" + envs.size() + ")";
        }
    }

    /**
     * Look up the entry with the given count in the table.
     * Returns null if not found.
     */
    public static Entry lookup(List<Entry> table, int count) {
        for (Entry entry : table) {
            if (entry.count == count) {
                return entry;
            }
        }
        return null;
    }

    /**
     * Insert an environment into the table, maintaining order by count.
     * If an entry for the env's count already exists, the env is prepended
     * to its list. Otherwise a new entry is created in sorted position.
     *
     * Corresponds to insert-in-table in the Lisp code.
     *
     * @param table the current table (may be null)
     * @param env   the environment to insert
     * @return the updated table
     */
    public static List<Entry> insertInTable(List<Entry> table, Env env) {
        if (table == null) {
            table = new ArrayList<>();
        }
        int count = env.getCount();
        Entry entry = lookup(table, count);
        if (entry != null) {
            entry.getEnvs().add(0, env);
            return table;
        }
        // Need to insert a new entry in sorted order by count.
        Entry newEntry = new Entry(count);
        newEntry.getEnvs().add(env);
        // Find insertion point
        int i = 0;
        while (i < table.size() && table.get(i).getCount() < count) {
            i++;
        }
        table.add(i, newEntry);
        return table;
    }
}
