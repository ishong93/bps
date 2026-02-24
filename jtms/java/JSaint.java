// JSAINT: A rational reconstruction of Slagel's SAINT program.
// Symbolic integration using JTRE for dependency-directed search.
// Translated from jsaint.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/**
 * JSaint implements a symbolic integration solver using JTRE.
 */
public class JSaint {
    private String title;
    private Jtre jtre;
    private List<AgendaEntry> agenda = new ArrayList<>();
    private Object problem;
    private Object solution;
    private int nSubproblems = 0;
    private int maxTasks = 20;
    private boolean debugging = false;

    /** Agenda entry with difficulty estimate. */
    public static class AgendaEntry {
        public int difficulty;
        public Object problem;
        public AgendaEntry(int difficulty, Object problem) {
            this.difficulty = difficulty;
            this.problem = problem;
        }
    }

    private static JSaint current;

    public JSaint(String title, Object problem, boolean debugging, int maxTasks) {
        this.title = title;
        this.problem = problem;
        this.debugging = debugging;
        this.maxTasks = maxTasks > 0 ? maxTasks : 20;
        this.jtre = new Jtre("JTRE of " + title, false);
        this.jtre.getJtms().setContradictionHandler(JTMS::askUserHandler);
        current = this;
    }

    // Getters
    public String getTitle() { return title; }
    public Jtre getJtre() { return jtre; }
    public Object getProblem() { return problem; }
    public Object getSolution() { return solution; }
    public void setSolution(Object s) { this.solution = s; }
    public List<AgendaEntry> getAgenda() { return agenda; }
    public int getNSubproblems() { return nSubproblems; }
    public int getMaxTasks() { return maxTasks; }
    public boolean isDebugging() { return debugging; }
    public static JSaint getCurrent() { return current; }
    public static void setCurrent(JSaint js) { current = js; }

    @Override
    public String toString() {
        return String.format("<Agenda %s>", title);
    }

    /** Debug output. */
    public void debug(String fmt, Object... args) {
        if (debugging) System.out.printf(fmt, args);
    }

    /** Run the solver. Corresponds to run-jsaint. */
    public Object run() {
        if (solution != null) return solution;
        if (nSubproblems > maxTasks) return "TIME-OUT";

        while (true) {
            Object sol = fetchSolution(problem);
            List<Object> failureSignal = Arrays.asList("Failed",
                Arrays.asList("Integrate", problem));

            if (sol != null) {
                solution = sol;
                debug("%n %s: Solved original problem.", title);
                return solution;
            }
            if (JData.isIn(failureSignal, jtre)) {
                debug("%n %s: Failed on original problem.", title);
                solution = "FAILED-PROBLEM";
                return solution;
            }
            if (agenda.isEmpty()) {
                debug("%n %s: Agenda empty.", title);
                solution = "FAILED-EMPTY";
                return solution;
            }
            AgendaEntry entry = agenda.remove(0);
            processSubproblem(entry.problem);
        }
    }

    /** Process a subproblem. Corresponds to process-subproblem. */
    @SuppressWarnings("unchecked")
    public boolean processSubproblem(Object item) {
        debug("%n  Trying to solve %s.", item);
        openSubproblem(item);
        if (fetchSolution(item) != null) {
            debug("%n    ..already solved.");
            return true;
        }
        List<Object> andSubgoals = JData.fetch(
            Arrays.asList("AND-SUBGOALS", item, "?subproblems"), jtre);
        for (Object f : andSubgoals) {
            if (JData.isIn(f, jtre)) {
                debug("%n   ..already expanded.");
                return true;
            }
        }
        List<Object> suggestions = new ArrayList<>();
        List<Object> suggestFacts = JData.fetch(
            Arrays.asList("SUGGEST-FOR", item, "?operator"), jtre);
        for (Object suggestion : suggestFacts) {
            if (JData.isIn(suggestion, jtre)) {
                List<Object> s = (List<Object>) suggestion;
                if (s.size() >= 3) {
                    List<Object> tryProblem = Arrays.asList("try", s.get(2));
                    queueProblem(tryProblem, item);
                    suggestions.add(tryProblem);
                }
            }
        }
        JData.jassert(Arrays.asList("OR-SUBGOALS", item, suggestions),
            "OR-SUBGOALS", jtre);
        JRules.runRules(jtre);
        return false;
    }

    /** Open a subproblem. Corresponds to open-subproblem. */
    public void openSubproblem(Object item) {
        JData.jassert(Arrays.asList("expanded", item), "EXPAND-AGENDA-ITEM", jtre);
        JData.jassume(Arrays.asList("open", item), "EXPAND-AGENDA-ITEM", jtre);
        JRules.runRules(jtre);
    }

    /** Queue a problem sorted by difficulty. Corresponds to queue-problem. */
    public void queueProblem(Object problem, Object parent) {
        int difficulty = estimateDifficulty(problem);
        debug("%n   Queueing %s, difficulty = %d", problem, difficulty);
        agenda.add(new AgendaEntry(difficulty, problem));
        agenda.sort(Comparator.comparingInt(a -> a.difficulty));
    }

    /** Estimate difficulty. Corresponds to estimate-difficulty. */
    public static int estimateDifficulty(Object problem) {
        return maxDepth(problem) + countSymbols(problem);
    }

    /** Count symbols. Corresponds to count-symbols. */
    @SuppressWarnings("unchecked")
    public static int countSymbols(Object pr) {
        if (pr == null) return 0;
        if (pr instanceof List) {
            int sum = 0;
            for (Object e : (List<Object>) pr) sum += countSymbols(e);
            return sum;
        }
        return 1;
    }

    /** Max depth. Corresponds to max-depth. */
    @SuppressWarnings("unchecked")
    public static int maxDepth(Object pr) {
        if (!(pr instanceof List)) return 1;
        int max = 0;
        for (Object e : (List<Object>) pr) {
            int d = maxDepth(e);
            if (d > max) max = d;
        }
        return 1 + max;
    }

    /** Fetch solution for a problem. Corresponds to fetch-solution. */
    @SuppressWarnings("unchecked")
    public Object fetchSolution(Object prob) {
        List<Object> solutions = JData.fetch(
            Arrays.asList("SOLUTION-OF", prob, "?answer"), jtre);
        for (Object solution : solutions) {
            if (JData.isIn(solution, jtre)) {
                List<Object> s = (List<Object>) solution;
                if (s.size() >= 3) return s.get(2);
            }
        }
        return null;
    }

    /** Explain result. Corresponds to explain-result. */
    public void explainResult() {
        if (solution == null) {
            System.out.println("\n Problem not solved yet.");
        } else if ("FAILED-PROBLEM".equals(solution)) {
            System.out.println("\n Failed to find a solution.");
        } else if ("FAILED-EMPTY".equals(solution)) {
            System.out.println("\n Ran out of things to do.");
        } else {
            System.out.printf("%n Solved the problem: %s%n", solution);
        }
    }

    /** Convenience test function. Corresponds to try-jsaint. */
    public static Object tryJSaint(Object problem, String title) {
        if (title == null) title = "JSAINT Test";
        JSaint js = new JSaint(title, problem, true, 20);
        return js.run();
    }

    // Sample problems
    public static final Object PROBLEM1 = Arrays.asList("Integrate",
        Arrays.asList("Integral", 1, "x"));
    public static final Object PROBLEM2 = Arrays.asList("Integrate",
        Arrays.asList("integral", Arrays.asList("+", "x", 5), "x"));
    public static final Object PROBLEM3 = Arrays.asList("Integrate",
        Arrays.asList("integral", Arrays.asList("*", 46, Arrays.asList("log", "x", "%e")), "x"));
}
