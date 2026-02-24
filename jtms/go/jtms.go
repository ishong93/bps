// Package jtms implements a Justification-based Truth Maintenance System (JTMS).
// Translated from the Common Lisp version in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
package jtms

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// Label constants corresponding to :IN and :OUT in the Lisp version.
type Label int

const (
	LabelOut Label = iota
	LabelIn
)

func (l Label) String() string {
	if l == LabelIn {
		return "IN"
	}
	return "OUT"
}

// EnabledAssumption is a sentinel value used as a support marker
// (corresponds to :ENABLED-ASSUMPTION in the Lisp version).
var EnabledAssumption = &Just{Index: -1, Informant: ":ENABLED-ASSUMPTION"}

// NodeStringFunc computes a printable string for a TmsNode.
type NodeStringFunc func(node *TmsNode) string

// EnqueueFunc is called when a rule needs to be enqueued.
type EnqueueFunc func(rule interface{})

// ContradictionHandlerFunc handles contradictions found in the JTMS.
type ContradictionHandlerFunc func(jtms *JTMS, contradictions []*TmsNode)

// JTMS is the main truth maintenance system structure.
// Corresponds to the (defstruct jtms ...) in the Lisp version.
type JTMS struct {
	Title                  string
	NodeCounter            int
	JustCounter            int
	Nodes                  []*TmsNode
	Justs                  []*Just
	Debugging              bool
	Contradictions         []*TmsNode
	Assumptions            []*TmsNode
	CheckingContradictions bool
	NodeString             NodeStringFunc
	ContradictionHandler   ContradictionHandlerFunc
	EnqueueProcedure       EnqueueFunc
}

// String implements fmt.Stringer for JTMS.
func (j *JTMS) String() string {
	return fmt.Sprintf("#<JTMS: %s>", j.Title)
}

// TmsNode represents a node in the JTMS.
// Corresponds to the (defstruct tms-node ...) in the Lisp version.
type TmsNode struct {
	Index         int
	Datum         interface{}
	Label         Label
	Support       interface{} // nil, *Just, or EnabledAssumption sentinel
	Justs         []*Just
	Consequences  []*Just
	Mark          interface{}
	Contradictory bool
	Assumption    bool // true or the symbol :DEFAULT
	IsDefault     bool // true when assumption? was :DEFAULT in Lisp
	InRules       []interface{}
	OutRules      []interface{}
	Jtms          *JTMS
}

// String implements fmt.Stringer for TmsNode.
func (n *TmsNode) String() string {
	return fmt.Sprintf("#<Node: %s>", NodeString(n))
}

// Just represents a justification in the JTMS.
// Corresponds to the (defstruct just ...) in the Lisp version.
type Just struct {
	Index       int
	Informant   interface{}
	Consequence *TmsNode
	Antecedents []*TmsNode
}

// String implements fmt.Stringer for Just.
func (j *Just) String() string {
	return fmt.Sprintf("#<Just %d>", j.Index)
}

// ---------------------------------------------------------------------------
// Utility / query functions
// ---------------------------------------------------------------------------

// IsTmsNodePremise returns true if the node is a premise
// (has support that is a Just with no antecedents and is not an enabled assumption).
func IsTmsNodePremise(node *TmsNode) bool {
	if node.Support == nil {
		return false
	}
	if node.Support == EnabledAssumption {
		return false
	}
	just, ok := node.Support.(*Just)
	if !ok {
		return false
	}
	return len(just.Antecedents) == 0
}

// NodeString returns the printable string for a node using
// the JTMS's NodeString function.
func NodeString(node *TmsNode) string {
	return node.Jtms.NodeString(node)
}

// debuggingJtms prints a debugging message if debugging is enabled.
func debuggingJtms(jtms *JTMS, msg string, args ...interface{}) {
	if jtms.Debugging {
		fmt.Fprintf(os.Stderr, msg, args...)
	}
}

// TmsError panics with a formatted error message about a node.
func TmsError(msg string, node *TmsNode) {
	panic(fmt.Sprintf(msg, NodeString(node)))
}

// DefaultNodeString is the default function for converting a node to a string.
func DefaultNodeString(n *TmsNode) string {
	return fmt.Sprintf("%v", n.Datum)
}

// ---------------------------------------------------------------------------
// JTMS creation and configuration
// ---------------------------------------------------------------------------

// CreateJTMS creates a new JTMS with the given title and optional settings.
func CreateJTMS(title string, opts ...func(*JTMS)) *JTMS {
	jtms := &JTMS{
		Title:                  title,
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
	}
	for _, opt := range opts {
		opt(jtms)
	}
	return jtms
}

// Option functions for CreateJTMS

func WithNodeString(fn NodeStringFunc) func(*JTMS) {
	return func(j *JTMS) { j.NodeString = fn }
}

func WithDebugging(d bool) func(*JTMS) {
	return func(j *JTMS) { j.Debugging = d }
}

func WithCheckingContradictions(c bool) func(*JTMS) {
	return func(j *JTMS) { j.CheckingContradictions = c }
}

func WithContradictionHandler(fn ContradictionHandlerFunc) func(*JTMS) {
	return func(j *JTMS) { j.ContradictionHandler = fn }
}

func WithEnqueueProcedure(fn EnqueueFunc) func(*JTMS) {
	return func(j *JTMS) { j.EnqueueProcedure = fn }
}

// ChangeJTMS modifies an existing JTMS with the given options.
func ChangeJTMS(jtms *JTMS, opts ...func(*JTMS)) {
	for _, opt := range opts {
		opt(jtms)
	}
}

// ---------------------------------------------------------------------------
// Node status queries
// ---------------------------------------------------------------------------

// InNode returns true if the node's label is :IN.
func InNode(node *TmsNode) bool {
	return node.Label == LabelIn
}

// OutNode returns true if the node's label is :OUT.
func OutNode(node *TmsNode) bool {
	return node.Label == LabelOut
}

// ---------------------------------------------------------------------------
// Node creation
// ---------------------------------------------------------------------------

// TmsCreateNode creates a new TMS node in the given JTMS.
func TmsCreateNode(jtms *JTMS, datum interface{}, assumptionp bool, contradictoryp bool) *TmsNode {
	jtms.NodeCounter++
	node := &TmsNode{
		Index:         jtms.NodeCounter,
		Datum:         datum,
		Label:         LabelOut,
		Assumption:    assumptionp,
		Contradictory: contradictoryp,
		Jtms:          jtms,
	}
	if assumptionp {
		jtms.Assumptions = prepend(jtms.Assumptions, node)
	}
	if contradictoryp {
		jtms.Contradictions = prepend(jtms.Contradictions, node)
	}
	jtms.Nodes = prepend(jtms.Nodes, node)
	return node
}

// TmsCreateNodeDefault creates a node with assumption mode :DEFAULT.
func TmsCreateNodeDefault(jtms *JTMS, datum interface{}, contradictoryp bool) *TmsNode {
	node := TmsCreateNode(jtms, datum, true, contradictoryp)
	node.IsDefault = true
	return node
}

// prepend inserts a node at the front of a slice (like CL push).
func prepend(slice []*TmsNode, node *TmsNode) []*TmsNode {
	return append([]*TmsNode{node}, slice...)
}

// prependJust inserts a Just at the front of a slice.
func prependJust(slice []*Just, j *Just) []*Just {
	return append([]*Just{j}, slice...)
}

// ---------------------------------------------------------------------------
// Assumption management
// ---------------------------------------------------------------------------

// AssumeNode converts a node into an assumption and enables it.
func AssumeNode(node *TmsNode) {
	jtms := node.Jtms
	if !node.Assumption {
		debuggingJtms(jtms, "\nConverting %s into an assumption", NodeString(node))
		node.Assumption = true
	}
	EnableAssumption(node)
}

// MakeContradiction marks a node as contradictory.
func MakeContradiction(node *TmsNode) {
	jtms := node.Jtms
	if !node.Contradictory {
		node.Contradictory = true
		jtms.Contradictions = prepend(jtms.Contradictions, node)
		CheckForContradictions(jtms)
	}
}

// ---------------------------------------------------------------------------
// Justification
// ---------------------------------------------------------------------------

// JustifyNode adds a justification for the consequence node.
func JustifyNode(informant interface{}, consequence *TmsNode, antecedents []*TmsNode) {
	jtms := consequence.Jtms
	jtms.JustCounter++
	just := &Just{
		Index:       jtms.JustCounter,
		Informant:   informant,
		Consequence: consequence,
		Antecedents: antecedents,
	}
	consequence.Justs = prependJust(consequence.Justs, just)
	for _, node := range antecedents {
		node.Consequences = prependJust(node.Consequences, just)
	}
	jtms.Justs = prependJust(jtms.Justs, just)

	// Build antecedent name list for debugging.
	if jtms.Debugging {
		names := make([]string, len(antecedents))
		for i, a := range antecedents {
			names[i] = NodeString(a)
		}
		debuggingJtms(jtms, "\nJustifying %s by %v using %v.",
			NodeString(consequence), informant, names)
	}

	if len(antecedents) > 0 || OutNode(consequence) {
		if CheckJustification(just) {
			installSupport(consequence, just)
		}
	} else {
		consequence.Support = just
	}
	CheckForContradictions(jtms)
}

// CheckJustification checks whether a justification can fire.
func CheckJustification(just *Just) bool {
	return OutNode(just.Consequence) && JustificationSatisfied(just)
}

// JustificationSatisfied returns true if all antecedents are IN.
func JustificationSatisfied(just *Just) bool {
	for _, ant := range just.Antecedents {
		if !InNode(ant) {
			return false
		}
	}
	return true
}

// installSupport makes the consequence node IN and propagates.
func installSupport(conseq *TmsNode, just *Just) {
	makeNodeIn(conseq, just)
	propagateInness(conseq)
}

// propagateInness propagates belief from a newly-IN node.
func propagateInness(node *TmsNode) {
	jtms := node.Jtms
	q := []*TmsNode{node}
	for len(q) > 0 {
		current := q[0]
		q = q[1:]
		debuggingJtms(jtms, "\n   Propagating belief in %s.", NodeString(current))
		for _, justification := range current.Consequences {
			if CheckJustification(justification) {
				makeNodeIn(justification.Consequence, justification)
				q = append(q, justification.Consequence)
			}
		}
	}
}

// makeNodeIn marks a node as IN with the given reason.
func makeNodeIn(conseq *TmsNode, reason interface{}) {
	jtms := conseq.Jtms
	enqueuef := jtms.EnqueueProcedure

	if jtms.Debugging {
		var reasonStr interface{}
		if just, ok := reason.(*Just); ok && reason != EnabledAssumption {
			antNames := make([]string, len(just.Antecedents))
			for i, a := range just.Antecedents {
				antNames[i] = jtms.NodeString(a)
			}
			reasonStr = fmt.Sprintf("(%v %s)", just.Informant, strings.Join(antNames, " "))
		} else {
			reasonStr = reason
		}
		debuggingJtms(jtms, "\n     Making %s in via %v.",
			NodeString(conseq), reasonStr)
	}

	conseq.Label = LabelIn
	conseq.Support = reason
	if enqueuef != nil {
		for _, inRule := range conseq.InRules {
			enqueuef(inRule)
		}
		conseq.InRules = nil
	}
}

// ---------------------------------------------------------------------------
// Retraction
// ---------------------------------------------------------------------------

// RetractAssumption retracts a previously enabled assumption.
func RetractAssumption(node *TmsNode) {
	if node.Support != EnabledAssumption {
		return
	}
	jtms := node.Jtms
	debuggingJtms(jtms, "\n  Retracting assumption %s.", NodeString(node))
	makeNodeOut(node)
	outQueue := propagateOutness(node, jtms)
	outQueue = append([]*TmsNode{node}, outQueue...)
	findAlternativeSupport(jtms, outQueue)
}

// EnableAssumption enables an assumption node.
func EnableAssumption(node *TmsNode) {
	jtms := node.Jtms
	if !node.Assumption {
		TmsError("Can't enable the non-assumption %s", node)
	}
	debuggingJtms(jtms, "\n  Enabling assumption %s.", NodeString(node))
	if OutNode(node) {
		makeNodeIn(node, EnabledAssumption)
		propagateInness(node)
	} else if node.Support == EnabledAssumption {
		// Already an enabled assumption, do nothing.
	} else if just, ok := node.Support.(*Just); ok && len(just.Antecedents) == 0 {
		// Supported by a premise justification, do nothing.
	} else {
		node.Support = EnabledAssumption
	}
	CheckForContradictions(jtms)
}

// makeNodeOut marks a node as OUT.
func makeNodeOut(node *TmsNode) {
	jtms := node.Jtms
	enqueuef := jtms.EnqueueProcedure
	debuggingJtms(jtms, "\n     Retracting belief in %s.", NodeString(node))
	node.Support = nil
	node.Label = LabelOut
	if enqueuef != nil {
		for _, outRule := range node.OutRules {
			enqueuef(outRule)
		}
	}
	node.OutRules = nil
}

// propagateOutness propagates disbelief outward from a retracted node.
func propagateOutness(node *TmsNode, jtms *JTMS) []*TmsNode {
	debuggingJtms(jtms, "\n   Propagating disbelief in %s.", NodeString(node))
	var outQueue []*TmsNode
	js := make([]*Just, len(node.Consequences))
	copy(js, node.Consequences)
	for len(js) > 0 {
		j := js[0]
		js = js[1:]
		conseq := j.Consequence
		if conseq.Support == j {
			makeNodeOut(conseq)
			outQueue = append(outQueue, conseq)
			js = append(js, conseq.Consequences...)
		}
	}
	return outQueue
}

// findAlternativeSupport looks for alternative justifications for nodes that went OUT.
func findAlternativeSupport(jtms *JTMS, outQueue []*TmsNode) {
	debuggingJtms(jtms, "\n   Looking for alternative supports.")
	for _, node := range outQueue {
		if !InNode(node) {
			for _, just := range node.Justs {
				if CheckJustification(just) {
					installSupport(just.Consequence, just)
					break
				}
			}
		}
	}
}

// ---------------------------------------------------------------------------
// Contradiction handling
// ---------------------------------------------------------------------------

// CheckForContradictions checks whether any contradiction nodes are IN.
func CheckForContradictions(jtms *JTMS) {
	if !jtms.CheckingContradictions {
		return
	}
	var contradictions []*TmsNode
	for _, cnode := range jtms.Contradictions {
		if InNode(cnode) {
			contradictions = append(contradictions, cnode)
		}
	}
	if len(contradictions) > 0 && jtms.ContradictionHandler != nil {
		jtms.ContradictionHandler(jtms, contradictions)
	}
}

// WithoutContradictionCheck runs body with contradiction checking disabled,
// then restores the previous setting.
func WithoutContradictionCheck(jtms *JTMS, body func()) {
	oldValue := jtms.CheckingContradictions
	jtms.CheckingContradictions = false
	defer func() { jtms.CheckingContradictions = oldValue }()
	body()
}

// WithContradictionCheck runs body with contradiction checking enabled,
// then restores the previous setting.
func WithContradictionCheck(jtms *JTMS, body func()) {
	oldValue := jtms.CheckingContradictions
	jtms.CheckingContradictions = true
	defer func() { jtms.CheckingContradictions = oldValue }()
	body()
}

// WithContradictionHandler runs body with a temporary contradiction handler,
// then restores the previous handler.
func WithContradictionHandler(jtms *JTMS, handler ContradictionHandlerFunc, body func()) {
	oldHandler := jtms.ContradictionHandler
	jtms.ContradictionHandler = handler
	defer func() { jtms.ContradictionHandler = oldHandler }()
	body()
}

// contradictionSignal is used to simulate CL's throw/catch for contradiction handling.
type contradictionSignal struct{}

// DefaultAssumptions enables default assumptions, retracting any that cause contradictions.
func DefaultAssumptions(jtms *JTMS) {
	WithContradictionCheck(jtms, func() {
		WithContradictionHandler(jtms, func(_ *JTMS, _ []*TmsNode) {
			panic(contradictionSignal{})
		}, func() {
			for _, assumption := range jtms.Assumptions {
				if assumption.Support == EnabledAssumption {
					continue
				}
				if !assumption.IsDefault {
					continue
				}
				caught := func() (caught bool) {
					defer func() {
						if r := recover(); r != nil {
							if _, ok := r.(contradictionSignal); ok {
								caught = true
							} else {
								panic(r) // re-panic
							}
						}
						return
					}()
					EnableAssumption(assumption)
					return false
				}()
				if caught {
					RetractAssumption(assumption)
				}
			}
		})
	})
}

// ---------------------------------------------------------------------------
// Query / inspection functions
// ---------------------------------------------------------------------------

// SupportingJustificationForNode returns the support for a node.
func SupportingJustificationForNode(node *TmsNode) interface{} {
	return node.Support
}

// AssumptionsOfNode traces back through justifications to find
// the enabled assumptions supporting a node.
func AssumptionsOfNode(node *TmsNode) []*TmsNode {
	var assumptions []*TmsNode
	marker := &struct{}{} // unique marker
	nodes := []*TmsNode{node}
	for len(nodes) > 0 {
		current := nodes[0]
		nodes = nodes[1:]
		if current.Mark == marker {
			continue
		}
		current.Mark = marker
		if current.Support == EnabledAssumption {
			assumptions = append(assumptions, current)
		} else if InNode(current) {
			if just, ok := current.Support.(*Just); ok {
				nodes = append(nodes, just.Antecedents...)
			}
		}
	}
	return assumptions
}

// EnabledAssumptions returns all currently enabled assumptions.
func EnabledAssumptions(jtms *JTMS) []*TmsNode {
	var result []*TmsNode
	for _, assumption := range jtms.Assumptions {
		if assumption.Support == EnabledAssumption {
			result = append(result, assumption)
		}
	}
	return result
}

// WhyNode prints the reason a node is IN or OUT.
func WhyNode(node *TmsNode) *TmsNode {
	justification := node.Support
	switch {
	case justification == EnabledAssumption:
		fmt.Printf("\n%s is an enabled assumption", NodeString(node))
	case justification != nil:
		just := justification.(*Just)
		fmt.Printf("\n%s is IN via %v on", NodeString(node), just.Informant)
		for _, anode := range just.Antecedents {
			fmt.Printf("\n  %s", NodeString(anode))
		}
	default:
		fmt.Printf("\n%s is OUT.", NodeString(node))
	}
	return node
}

// WhyNodes prints the reason for every node in the JTMS.
func WhyNodes(jtms *JTMS) {
	for _, node := range jtms.Nodes {
		WhyNode(node)
	}
}

// ---------------------------------------------------------------------------
// Contradiction handler (interactive)
// ---------------------------------------------------------------------------

// contraAssumptions is the package-level equivalent of *contra-assumptions*.
var contraAssumptions []*TmsNode

// AskUserHandler is the default contradiction handler.
func AskUserHandler(jtms *JTMS, contradictions []*TmsNode) {
	HandleOneContradiction(contradictions[0])
	CheckForContradictions(jtms)
}

// HandleOneContradiction presents the user with the conflicting assumptions
// and asks which to retract.
func HandleOneContradiction(contraNode *TmsNode) {
	contraAssumptions = AssumptionsOfNode(contraNode)
	if len(contraAssumptions) == 0 {
		TmsError("\nThere is a flaw in the universe...%s", contraNode)
	}
	fmt.Printf("\nContradiction found: %s", NodeString(contraNode))
	PrintContraList(contraAssumptions)
	fmt.Printf("\nCall TmsAnswer(<number>) to retract assumption.")

	scanner := bufio.NewScanner(os.Stdin)
	fmt.Print("\nJTMS contradiction break> ")
	if scanner.Scan() {
		text := strings.TrimSpace(scanner.Text())
		num, err := strconv.Atoi(text)
		if err == nil {
			TmsAnswer(num)
		} else {
			fmt.Printf("\nIgnoring answer, must be an integer.")
		}
	}
}

// PrintContraList prints a numbered list of nodes.
func PrintContraList(nodes []*TmsNode) {
	for i, node := range nodes {
		fmt.Printf("\n%d %s", i+1, NodeString(node))
	}
}

// TmsAnswer processes a user's answer to a contradiction.
func TmsAnswer(num int) {
	if num <= 0 {
		fmt.Printf("\nIgnoring answer, too small")
		return
	}
	if num > len(contraAssumptions) {
		fmt.Printf("\nIgnoring answer, too big.")
		return
	}
	RetractAssumption(contraAssumptions[num-1])
}

// ---------------------------------------------------------------------------
// Interactive network exploration
// ---------------------------------------------------------------------------

// ExploreNetwork allows interactive exploration of the support network.
func ExploreNetwork(node *TmsNode) *TmsNode {
	if !InNode(node) {
		fmt.Printf("\n Sorry, %s not believed.", NodeString(node))
		return node
	}
	scanner := bufio.NewScanner(os.Stdin)
	var stack []*TmsNode
	current := node
	for {
		WhyNode(current)
		var options []*TmsNode
		if just, ok := current.Support.(*Just); ok && current.Support != EnabledAssumption {
			options = just.Antecedents
		}
		olen := len(options)

		for {
			fmt.Print("\n>>>")
			if !scanner.Scan() {
				return current
			}
			text := strings.TrimSpace(scanner.Text())
			if text == "q" {
				return current
			}
			choice, err := strconv.Atoi(text)
			if err != nil || choice < 0 || choice > olen {
				fmt.Printf("\n Must be q or an integer from 0 to %d.", olen)
				continue
			}
			if choice == 0 {
				if len(stack) > 0 {
					current = stack[len(stack)-1]
					stack = stack[:len(stack)-1]
				} else {
					return current
				}
			} else {
				stack = append(stack, current)
				current = options[choice-1]
			}
			break
		}
	}
}
