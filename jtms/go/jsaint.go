// JSAINT: A rational reconstruction of Slagel's SAINT program.
// Symbolic integration using JTRE for dependency-directed search.
// Translated from jsaint.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

import (
	"fmt"
	"sort"
)

// JSaint represents the JSAINT integration solver.
// Corresponds to the Lisp defstruct jsaint.
type JSaint struct {
	Title        string
	Jtre         *Jtre
	Agenda       []*AgendaEntry
	Problem      interface{}
	Solution     interface{}
	NSubproblems int
	MaxTasks     int
	Debugging    bool
}

// AgendaEntry represents a queued subproblem with difficulty estimate.
type AgendaEntry struct {
	Difficulty int
	Problem    interface{}
}

// String implements fmt.Stringer for JSaint.
func (js *JSaint) String() string {
	return fmt.Sprintf("<Agenda %s>", js.Title)
}

// CurrentJSaint is the current active JSAINT instance.
var CurrentJSaint *JSaint

// CreateJSaint creates a new JSAINT instance.
// Corresponds to the Lisp function create-jsaint.
func CreateJSaint(title string, problem interface{}, debugging bool, maxTasks int) *JSaint {
	if maxTasks <= 0 {
		maxTasks = 20
	}
	js := &JSaint{
		Title:    title,
		Problem:  problem,
		Jtre:     CreateJtre("JTRE of "+title, false),
		MaxTasks: maxTasks,
		Debugging: debugging,
	}
	// Install contradiction handler
	ChangeJTMS(js.Jtre.JTMS, WithContradictionHandler(func(jtms *JTMS, contras []*TmsNode) {
		AskUserHandler(jtms, contras)
	}))
	CurrentJSaint = js
	return js
}

// ChangeJSaint updates optional fields of the JSAINT.
// Corresponds to the Lisp function change-jsaint.
func ChangeJSaint(js *JSaint, debugging *bool, problem interface{}, maxTasks *int) {
	if debugging != nil {
		js.Debugging = *debugging
	}
	if problem != nil {
		js.Problem = problem
	}
	if maxTasks != nil {
		js.MaxTasks = *maxTasks
	}
}

// UseJSaint sets the current JSAINT instance.
func UseJSaint(js *JSaint) {
	CurrentJSaint = js
}

// debuggingJSaint prints debug output if debugging is enabled.
func debuggingJSaint(js *JSaint, msg string, args ...interface{}) {
	if js.Debugging {
		fmt.Printf(msg, args...)
	}
}

// RunJSaint runs the JSAINT solver.
// Corresponds to the Lisp function run-jsaint.
func RunJSaint(js *JSaint) (interface{}, *JSaint) {
	if js.Solution != nil {
		return js.Solution, js
	}
	if js.NSubproblems > js.MaxTasks {
		return "TIME-OUT", js
	}
	for {
		solution := FetchJSaintSolution(js.Problem, js)
		failureSignal := []interface{}{"Failed", []interface{}{"Integrate", js.Problem}}

		if solution != nil {
			js.Solution = solution
			debuggingJSaint(js, "\n %s: Solved original problem.", js.Title)
			return js.Solution, js
		}
		if JIn(failureSignal, js.Jtre) {
			debuggingJSaint(js, "\n %s: Failed on original problem.", js.Title)
			js.Solution = "FAILED-PROBLEM"
			return js.Solution, js
		}
		if len(js.Agenda) == 0 {
			debuggingJSaint(js, "\n %s: Agenda empty.", js.Title)
			js.Solution = "FAILED-EMPTY"
			return js.Solution, js
		}
		entry := js.Agenda[0]
		js.Agenda = js.Agenda[1:]
		ProcessSubproblem(entry.Problem, js)
	}
}

// ProcessSubproblem processes a single subproblem from the agenda.
// Corresponds to the Lisp function process-subproblem.
func ProcessSubproblem(item interface{}, js *JSaint) bool {
	debuggingJSaint(js, "\n  Trying to solve %v.", item)
	OpenSubproblem(item, js)
	if FetchJSaintSolution(item, js) != nil {
		debuggingJSaint(js, "\n    ..already solved.")
		return true
	}
	// Check if already expanded
	andSubgoals := JFetch([]interface{}{"AND-SUBGOALS", item, "?subproblems"}, js.Jtre)
	for _, f := range andSubgoals {
		if JIn(f, js.Jtre) {
			debuggingJSaint(js, "\n   ..already expanded.")
			return true
		}
	}
	// Look for suggestions
	var suggestions []interface{}
	suggestFacts := JFetch([]interface{}{"SUGGEST-FOR", item, "?operator"}, js.Jtre)
	for _, suggestion := range suggestFacts {
		if JIn(suggestion, js.Jtre) {
			s, ok := suggestion.([]interface{})
			if ok && len(s) >= 3 {
				tryProblem := []interface{}{"try", s[2]}
				QueueJSaintProblem(tryProblem, item, js)
				suggestions = append(suggestions, tryProblem)
			}
		}
	}
	JAssert([]interface{}{"OR-SUBGOALS", item, suggestions}, "OR-SUBGOALS", js.Jtre)
	RunJRules(js.Jtre)
	return false
}

// OpenSubproblem opens a subproblem for exploration.
// Corresponds to the Lisp function open-subproblem.
func OpenSubproblem(item interface{}, js *JSaint) {
	JAssert([]interface{}{"expanded", item}, "EXPAND-AGENDA-ITEM", js.Jtre)
	JAssume([]interface{}{"open", item}, "EXPAND-AGENDA-ITEM", js.Jtre)
	RunJRules(js.Jtre)
}

// QueueJSaintProblem adds a problem to the agenda sorted by difficulty.
// Corresponds to the Lisp function queue-problem.
func QueueJSaintProblem(problem interface{}, parent interface{}, js *JSaint) {
	difficulty := EstimateDifficulty(problem)
	entry := &AgendaEntry{Difficulty: difficulty, Problem: problem}
	debuggingJSaint(js, "\n   Queueing %v, difficulty = %d", problem, difficulty)
	js.Agenda = append(js.Agenda, entry)
	sort.SliceStable(js.Agenda, func(i, j int) bool {
		return js.Agenda[i].Difficulty < js.Agenda[j].Difficulty
	})
}

// EstimateDifficulty estimates the difficulty of a problem.
// Corresponds to the Lisp function estimate-difficulty.
func EstimateDifficulty(problem interface{}) int {
	return MaxDepth(problem) + CountSymbols(problem)
}

// CountSymbols counts the number of symbols in an expression.
// Corresponds to the Lisp function count-symbols.
func CountSymbols(pr interface{}) int {
	if pr == nil {
		return 0
	}
	if s, ok := pr.([]interface{}); ok {
		sum := 0
		for _, e := range s {
			sum += CountSymbols(e)
		}
		return sum
	}
	return 1
}

// MaxDepth returns the maximum nesting depth of an expression.
// Corresponds to the Lisp function max-depth.
func MaxDepth(pr interface{}) int {
	s, ok := pr.([]interface{})
	if !ok {
		return 1
	}
	maxD := 0
	for _, e := range s {
		d := MaxDepth(e)
		if d > maxD {
			maxD = d
		}
	}
	return 1 + maxD
}

// FetchJSaintSolution looks for a solution to a problem.
// Corresponds to the Lisp function fetch-solution.
func FetchJSaintSolution(problem interface{}, js *JSaint) interface{} {
	solutions := JFetch([]interface{}{"SOLUTION-OF", problem, "?answer"}, js.Jtre)
	for _, solution := range solutions {
		if JIn(solution, js.Jtre) {
			s, ok := solution.([]interface{})
			if ok && len(s) >= 3 {
				return s[2]
			}
		}
	}
	return nil
}

// ExplainResult explains the result of the JSAINT solver.
// Corresponds to the Lisp function explain-result.
func ExplainResult(js *JSaint) {
	if js == nil {
		js = CurrentJSaint
	}
	if js.Solution == nil {
		fmt.Println("\n Problem not solved yet.")
	} else if js.Solution == "FAILED-PROBLEM" {
		fmt.Println("\n Failed to find a solution.")
	} else if js.Solution == "FAILED-EMPTY" {
		fmt.Println("\n Ran out of things to do.")
	} else {
		fmt.Printf("\n Solved the problem: %v\n", js.Solution)
	}
}

// TryJSaint is a convenience function for testing integration.
// Corresponds to the Lisp function try-jsaint.
func TryJSaint(problem interface{}, title string) (interface{}, *JSaint) {
	if title == "" {
		title = "JSAINT Test"
	}
	js := CreateJSaint(title, problem, true, 20)
	return RunJSaint(js)
}

// Sample problems
var (
	JSaintProblem1 = []interface{}{"Integrate", []interface{}{"Integral", 1, "x"}}
	JSaintProblem2 = []interface{}{"Integrate", []interface{}{"integral", []interface{}{"+", "x", 5}, "x"}}
	JSaintProblem3 = []interface{}{"Integrate", []interface{}{"integral", []interface{}{"*", 46, []interface{}{"log", "x", "%e"}}, "x"}}
)
