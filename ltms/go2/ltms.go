// Package ltms implements a Logic-based Truth Maintenance System.
// Translated from Common Lisp LTMS version 43, 7/5/93.
package ltms

import (
	"fmt"
	"sort"
	"strings"
)

// NodeLabel represents the three-valued logic labels for TMS nodes.
type NodeLabel int

const (
	LabelUnknown NodeLabel = iota
	LabelTrue
	LabelFalse
)

func (l NodeLabel) String() string {
	switch l {
	case LabelTrue:
		return "TRUE"
	case LabelFalse:
		return "FALSE"
	default:
		return "UNKNOWN"
	}
}

// Opposite returns the opposite label (TRUE<->FALSE). UNKNOWN returns UNKNOWN.
func (l NodeLabel) Opposite() NodeLabel {
	switch l {
	case LabelTrue:
		return LabelFalse
	case LabelFalse:
		return LabelTrue
	default:
		return LabelUnknown
	}
}

// ClauseStatus represents the status of a clause.
type ClauseStatus int

const (
	StatusNone ClauseStatus = iota
	StatusSubsumed
	StatusQueued
	StatusDirty
	StatusNotIndexed
)

// EnabledAssumption is a sentinel string used as a support value to indicate
// that a node is an enabled assumption.
const EnabledAssumption = ":ENABLED-ASSUMPTION"

// Literal represents a pair of (node, sign) used in clauses.
type Literal struct {
	Node *TmsNode
	Sign NodeLabel
}

// AssumptionValue pairs a node with a desired label for WithAssumptions.
type AssumptionValue struct {
	Node  *TmsNode
	Label NodeLabel
}

// LTMS is the main Logic-based Truth Maintenance System structure.
type LTMS struct {
	Title                    string
	NodeCounter              int
	ClauseCounter            int
	Nodes                    map[interface{}]*TmsNode // nil if caching disabled
	Clauses                  []*Clause
	Debugging                bool
	CheckingContradictions   bool
	NodeString               func(node *TmsNode) string
	ContradictionHandlers    []func(violatedClauses []*Clause, ltms *LTMS) bool
	PendingContradictions    []*Clause
	EnqueueProcedure         func(rule interface{})
	Complete                 interface{} // nil or true (bool) or trie
	ViolatedClauses          []*Clause
	Queue                    interface{}
	Conses                   []*Literal
	DelaySat                 bool
	ConsSize                 int

	// Function fields for forward references to cltms.go functions.
	IpiaFn                    func(ltms *LTMS)
	PropagateMoreUnknownnessFn func(oldValue NodeLabel, node *TmsNode, ltms *LTMS)
	FullAddClauseFn           func(ltms *LTMS, literals []*Literal, informant interface{})
	WalkTrieFn                func(f func(*Clause), trie interface{})
}

// TmsNode represents a node in the LTMS.
type TmsNode struct {
	Index        int
	Datum        interface{}
	Label        NodeLabel
	Support      interface{} // can be *Clause, string (EnabledAssumption), or nil
	TrueClauses  []*Clause
	FalseClauses []*Clause
	Mark         interface{}
	IsAssumption bool
	TrueRules    []interface{}
	FalseRules   []interface{}
	LTMS         *LTMS
	TrueLiteral  *Literal
	FalseLiteral *Literal
}

// Clause represents a clause in the LTMS.
type Clause struct {
	Index    int
	Informant interface{}
	Literals []*Literal
	Pvs      int
	Length   int
	Sats     int
	Status   ClauseStatus
}

// --- Print functions ---

func (l *LTMS) String() string {
	return fmt.Sprintf("#<LTMS: %s>", l.Title)
}

func (n *TmsNode) String() string {
	return fmt.Sprintf("#<NODE: %s>", NodeString(n))
}

func (c *Clause) String() string {
	return fmt.Sprintf("#<Clause %d>", c.Index)
}

// NodeString returns the string representation of a node using the LTMS's
// node-string function.
func NodeString(node *TmsNode) string {
	return node.LTMS.NodeString(node)
}

// DebuggingLtms prints debugging output if debugging is enabled.
func DebuggingLtms(ltms *LTMS, msg string, args ...interface{}) {
	if ltms.Debugging {
		fmt.Printf(msg, args...)
	}
}

// LtmsError panics with an LTMS error message.
func LtmsError(message string, thing interface{}) {
	if thing != nil {
		panic(fmt.Sprintf("%s %v", message, thing))
	}
	panic(message)
}

// DefaultNodeString returns the default string representation of a node.
func DefaultNodeString(n *TmsNode) string {
	return fmt.Sprintf("%v", n.Datum)
}

// SatisfiedClause returns true if the clause has at least one satisfied literal.
func SatisfiedClause(clause *Clause) bool {
	return clause.Sats > 0
}

// ViolatedClause returns true if the clause has no possible values left.
func ViolatedClause(clause *Clause) bool {
	return clause.Pvs == 0
}

// WalkClauses applies f to every clause in the LTMS.
func WalkClauses(ltms *LTMS, f func(*Clause)) {
	if ltms.Complete != nil && ltms.WalkTrieFn != nil {
		ltms.WalkTrieFn(f, ltms.Complete)
	} else {
		for _, c := range ltms.Clauses {
			f(c)
		}
	}
}

// --- Basic inference-engine interface ---

// CreateLTMS creates a new LTMS with the given options.
func CreateLTMS(title string, opts ...func(*LTMS)) *LTMS {
	ltms := &LTMS{
		Title:                  title,
		NodeString:             DefaultNodeString,
		Debugging:              false,
		CheckingContradictions: true,
		DelaySat:               true,
		Nodes:                  make(map[interface{}]*TmsNode), // cache-datums default true
	}
	ltms.ContradictionHandlers = []func([]*Clause, *LTMS) bool{AskUserHandler}
	for _, opt := range opts {
		opt(ltms)
	}
	return ltms
}

// Option functions for CreateLTMS.

func OptionNodeString(f func(*TmsNode) string) func(*LTMS) {
	return func(l *LTMS) { l.NodeString = f }
}

func OptionDebugging(d bool) func(*LTMS) {
	return func(l *LTMS) { l.Debugging = d }
}

func OptionCheckingContradictions(c bool) func(*LTMS) {
	return func(l *LTMS) { l.CheckingContradictions = c }
}

func OptionContradictionHandler(h func([]*Clause, *LTMS) bool) func(*LTMS) {
	return func(l *LTMS) { l.ContradictionHandlers = []func([]*Clause, *LTMS) bool{h} }
}

func OptionEnqueueProcedure(f func(interface{})) func(*LTMS) {
	return func(l *LTMS) { l.EnqueueProcedure = f }
}

func OptionCacheDatums(cache bool) func(*LTMS) {
	return func(l *LTMS) {
		if !cache {
			l.Nodes = nil
		}
	}
}

func OptionComplete(c interface{}) func(*LTMS) {
	return func(l *LTMS) { l.Complete = c }
}

func OptionDelaySat(d bool) func(*LTMS) {
	return func(l *LTMS) { l.DelaySat = d }
}

// ChangeLTMS modifies an existing LTMS. Pass option functions to change fields.
func ChangeLTMS(ltms *LTMS, opts ...func(*LTMS)) {
	for _, opt := range opts {
		opt(ltms)
	}
}

// --- Node query functions ---

func UnknownNode(node *TmsNode) bool {
	return node.Label == LabelUnknown
}

func KnownNode(node *TmsNode) bool {
	return node.Label != LabelUnknown
}

func TrueNode(node *TmsNode) bool {
	return node.Label == LabelTrue
}

func FalseNode(node *TmsNode) bool {
	return node.Label == LabelFalse
}

// --- Node creation ---

// TmsCreateNode creates a new TMS node with the given datum.
func TmsCreateNode(ltms *LTMS, datum interface{}, assumptionp bool) *TmsNode {
	if ltms.Nodes != nil {
		if _, ok := ltms.Nodes[datum]; ok {
			LtmsError("Two nodes with same datum:", datum)
		}
	}
	ltms.NodeCounter++
	node := &TmsNode{
		Index:        ltms.NodeCounter,
		Datum:        datum,
		Label:        LabelUnknown,
		IsAssumption: assumptionp,
		LTMS:         ltms,
	}
	node.TrueLiteral = &Literal{Node: node, Sign: LabelTrue}
	node.FalseLiteral = &Literal{Node: node, Sign: LabelFalse}
	if ltms.Nodes != nil {
		ltms.Nodes[datum] = node
	}
	if ltms.Complete != nil && ltms.NodeCounter > ltms.ConsSize {
		ltms.Conses = nil
		ltms.ConsSize += 50
		for i := 0; i < ltms.ConsSize; i++ {
			ltms.Conses = append(ltms.Conses, &Literal{})
		}
	}
	return node
}

// --- Assumption management ---

// EnableAssumption enables a node as an assumption with the given label.
func EnableAssumption(node *TmsNode, label NodeLabel) {
	if !node.IsAssumption {
		LtmsError("Can't enable the non-assumption", node)
	}
	if node.Label == label {
		node.Support = EnabledAssumption
	} else if node.Label == LabelUnknown {
		TopSetTruth(node, label, EnabledAssumption)
	} else {
		LtmsError("Can't set an already set node", node)
	}
}

// ConvertToAssumption converts a node into an assumption.
func ConvertToAssumption(node *TmsNode) {
	if !node.IsAssumption {
		DebuggingLtms(node.LTMS, "\nConverting %s into an assumption", NodeString(node))
		node.IsAssumption = true
	}
}

// RetractAssumption retracts a node's assumption status.
func RetractAssumption(node *TmsNode) {
	if KnownNode(node) && node.Support == EnabledAssumption {
		FindAlternativeSupport(node.LTMS, PropagateUnknownness(node))
	}
}

// --- Adding formulas ---

// Formula expression types for normalized boolean formulas.
// Expressions can be:
//   - A string or interface{} datum (leaf node name)
//   - []interface{} where [0] is a keyword string like ":AND", ":OR", etc.

// AddFormula adds a formula to the LTMS.
func AddFormula(ltms *LTMS, formula interface{}, informant interface{}) {
	inf := []interface{}{":IMPLIED-BY", formula, informant}
	clauses := Normalize(ltms, formula)
	for _, clause := range clauses {
		simplified := SimplifyClause(clause)
		if simplified != nil { // nil means :TRUE (tautology)
			AddClauseInternal(simplified, inf, true)
		}
	}
	CheckForContradictions(ltms)
}

// SimplifyClause simplifies a clause by removing duplicate literals.
// Returns nil if the clause is a tautology (complementary literals found).
func SimplifyClause(literals []*Literal) []*Literal {
	if len(literals) == 0 {
		return literals
	}
	literals = SortClause(literals)
	i := 0
	for j := 1; j < len(literals); j++ {
		if literals[i].Node != literals[j].Node {
			i++
			literals[i] = literals[j]
		} else if literals[i].Sign != literals[j].Sign {
			// Complementary literals: tautology
			return nil
		}
		// else duplicate, skip
	}
	return literals[:i+1]
}

// SortClause sorts literals by node index.
func SortClause(literals []*Literal) []*Literal {
	result := make([]*Literal, len(literals))
	copy(result, literals)
	sort.Slice(result, func(i, j int) bool {
		return result[i].Node.Index < result[j].Node.Index
	})
	return result
}

// Normalize converts a formula expression into conjunctive normal form.
// Returns a slice of clauses, where each clause is a slice of *Literal.
func Normalize(ltms *LTMS, exp interface{}) [][]*Literal {
	return normalize1(ltms, exp, false)
}

func normalize1(ltms *LTMS, exp interface{}, negate bool) [][]*Literal {
	// Check if exp is a list expression (represented as []interface{})
	if list, ok := exp.([]interface{}); ok && len(list) > 0 {
		if op, ok := list[0].(string); ok {
			switch op {
			case ":IMPLIES":
				if negate {
					r1 := normalize1(ltms, list[1], false)
					r2 := normalize1(ltms, list[2], true)
					return append(r1, r2...)
				}
				return disjoin(
					normalize1(ltms, list[1], true),
					normalize1(ltms, list[2], false),
				)
			case ":IFF":
				return normalizeIff(ltms, list, negate)
			case ":OR":
				if negate {
					return normalizeConjunction(ltms, list, true)
				}
				return normalizeDisjunction(ltms, list, false)
			case ":AND":
				if negate {
					return normalizeDisjunction(ltms, list, true)
				}
				return normalizeConjunction(ltms, list, false)
			case ":NOT":
				return normalize1(ltms, list[1], !negate)
			case ":TAXONOMY":
				return normalizeTax(ltms, list, negate)
			}
		}
	}
	// Leaf node
	node := FindNode(ltms, exp)
	if negate {
		return [][]*Literal{{node.FalseLiteral}}
	}
	return [][]*Literal{{node.TrueLiteral}}
}

func normalizeTax(ltms *LTMS, exp []interface{}, negate bool) [][]*Literal {
	// Build (:AND (:OR items...) (:NOT (:AND a b)) ...)
	items := exp[1:]
	andParts := []interface{}{":AND"}
	orPart := []interface{}{":OR"}
	orPart = append(orPart, items...)
	andParts = append(andParts, orPart)

	for i := 0; i < len(items); i++ {
		for j := i + 1; j < len(items); j++ {
			andParts = append(andParts,
				[]interface{}{":NOT", []interface{}{":AND", items[i], items[j]}})
		}
	}
	return normalize1(ltms, andParts, negate)
}

func normalizeConjunction(ltms *LTMS, exp []interface{}, negate bool) [][]*Literal {
	var result [][]*Literal
	for _, sub := range exp[1:] {
		result = append(result, normalize1(ltms, sub, negate)...)
	}
	return result
}

func normalizeIff(ltms *LTMS, exp []interface{}, negate bool) [][]*Literal {
	r1 := normalize1(ltms, []interface{}{":IMPLIES", exp[1], exp[2]}, negate)
	r2 := normalize1(ltms, []interface{}{":IMPLIES", exp[2], exp[1]}, negate)
	return append(r1, r2...)
}

func normalizeDisjunction(ltms *LTMS, exp []interface{}, negate bool) [][]*Literal {
	if len(exp) < 2 {
		return [][]*Literal{nil}
	}
	result := normalize1(ltms, exp[1], negate)
	for _, sub := range exp[2:] {
		result = disjoin(normalize1(ltms, sub, negate), result)
	}
	return result
}

func disjoin(conj1, conj2 [][]*Literal) [][]*Literal {
	if len(conj1) == 0 || len(conj2) == 0 {
		return nil
	}
	var result [][]*Literal
	for _, disj1 := range conj1 {
		for _, disj2 := range conj2 {
			combined := make([]*Literal, 0, len(disj1)+len(disj2))
			combined = append(combined, disj1...)
			combined = append(combined, disj2...)
			result = append(result, combined)
		}
	}
	return result
}

// FindNode finds or creates a node by datum.
func FindNode(ltms *LTMS, name interface{}) *TmsNode {
	if node, ok := name.(*TmsNode); ok {
		return node
	}
	if ltms.Nodes != nil {
		if node, ok := ltms.Nodes[name]; ok {
			return node
		}
	}
	return TmsCreateNode(ltms, name, false)
}

// --- Adding clauses ---

// AddClause adds a clause from lists of true-nodes and false-nodes.
func AddClause(trueNodes []*TmsNode, falseNodes []*TmsNode, informant interface{}) {
	var literals []*Literal
	for _, n := range trueNodes {
		literals = append(literals, n.TrueLiteral)
	}
	for _, n := range falseNodes {
		literals = append(literals, n.FalseLiteral)
	}
	AddClauseInternal(literals, informant, false)
}

// AddClauseInternal adds a clause to the LTMS.
func AddClauseInternal(literals []*Literal, informant interface{}, internal bool) {
	if len(literals) == 0 || literals[0].Node == nil {
		LtmsError("Total contradiction: Null clause", informant)
	}
	ltms := literals[0].Node.LTMS
	if ltms.Complete != nil && ltms.FullAddClauseFn != nil {
		ltms.FullAddClauseFn(ltms, literals, informant)
	} else {
		cl := BcpAddClause(ltms, literals, informant, true)
		ltms.Clauses = append(ltms.Clauses, cl)
	}
	if !internal {
		CheckForContradictions(ltms)
	}
}

// BcpAddClause creates a clause and performs initial BCP setup.
func BcpAddClause(ltms *LTMS, literals []*Literal, informant interface{}, index bool) *Clause {
	ltms.ClauseCounter++
	cl := &Clause{
		Index:    ltms.ClauseCounter,
		Literals: literals,
		Informant: informant,
		Length:   len(literals),
	}
	for _, term := range literals {
		label := term.Node.Label
		if label == LabelUnknown {
			cl.Pvs++
		}
		switch term.Sign {
		case LabelTrue:
			if index {
				InsertTrueClause(cl, term.Node)
			}
			if label == LabelTrue {
				cl.Sats++
				cl.Pvs++
			}
		case LabelFalse:
			if index {
				InsertFalseClause(cl, term.Node)
			}
			if label == LabelFalse {
				cl.Sats++
				cl.Pvs++
			}
		}
	}
	if index {
		CheckClauses(ltms, []*Clause{cl})
	}
	return cl
}

// InsertTrueClause adds a clause to a node's true-clauses list.
func InsertTrueClause(cl *Clause, node *TmsNode) {
	node.TrueClauses = append(node.TrueClauses, cl)
}

// InsertFalseClause adds a clause to a node's false-clauses list.
func InsertFalseClause(cl *Clause, node *TmsNode) {
	node.FalseClauses = append(node.FalseClauses, cl)
}

// AddNogood adds a nogood clause based on culprit assumptions.
func AddNogood(culprit *TmsNode, sign NodeLabel, assumptions []*TmsNode) {
	var trues, falses []*TmsNode
	for _, a := range assumptions {
		var lbl NodeLabel
		if a == culprit {
			lbl = sign
		} else {
			lbl = a.Label
		}
		switch lbl {
		case LabelTrue:
			falses = append(falses, a)
		case LabelFalse:
			trues = append(trues, a)
		}
	}
	AddClause(trues, falses, "NOGOOD")
}

// --- Boolean Constraint Propagation ---

// CheckClauses performs BCP on the given clauses.
func CheckClauses(ltms *LTMS, clausesToCheck []*Clause) {
	DebuggingLtms(ltms, "\n Beginning propagation...")
	for len(clausesToCheck) > 0 {
		cl := clausesToCheck[0]
		clausesToCheck = clausesToCheck[1:]
		CheckClause(ltms, cl, &clausesToCheck)
	}
}

// CheckClause checks a single clause and propagates if possible.
func CheckClause(ltms *LTMS, clause *Clause, clausesToCheck *[]*Clause) {
	if ViolatedClause(clause) {
		// pushnew
		found := false
		for _, vc := range ltms.ViolatedClauses {
			if vc == clause {
				found = true
				break
			}
		}
		if !found {
			ltms.ViolatedClauses = append(ltms.ViolatedClauses, clause)
		}
	} else if clause.Pvs == 1 {
		unknownPair := FindUnknownPair(clause)
		if unknownPair != nil {
			SetTruth(unknownPair.Node, unknownPair.Sign, clause, clausesToCheck)
		}
	}
}

// FindUnknownPair finds the first unknown literal in a clause.
func FindUnknownPair(clause *Clause) *Literal {
	for _, term := range clause.Literals {
		if UnknownNode(term.Node) {
			return term
		}
	}
	return nil
}

// TopSetTruth sets the truth of a node and propagates.
func TopSetTruth(node *TmsNode, value NodeLabel, reason interface{}) {
	var clausesToCheck []*Clause
	SetTruth(node, value, reason, &clausesToCheck)
	CheckClauses(node.LTMS, clausesToCheck)
	CheckForContradictions(node.LTMS)
}

// SetTruth sets the truth value of a node and updates clause counters.
func SetTruth(node *TmsNode, value NodeLabel, reason interface{}, clausesToCheck *[]*Clause) {
	ltms := node.LTMS
	DebuggingLtms(ltms, "\n  Setting %s to %s, via %v.", NodeString(node), value, reason)
	node.Support = reason
	node.Label = value
	switch value {
	case LabelTrue:
		if ltms.EnqueueProcedure != nil {
			for _, rule := range node.TrueRules {
				ltms.EnqueueProcedure(rule)
			}
			node.TrueRules = nil
		}
		for _, clause := range node.TrueClauses {
			clause.Sats++
		}
		for _, clause := range node.FalseClauses {
			clause.Pvs--
			if clause.Pvs < 2 {
				*clausesToCheck = append(*clausesToCheck, clause)
			}
		}
	case LabelFalse:
		if ltms.EnqueueProcedure != nil {
			for _, rule := range node.FalseRules {
				ltms.EnqueueProcedure(rule)
			}
			node.FalseRules = nil
		}
		for _, clause := range node.FalseClauses {
			clause.Sats++
		}
		for _, clause := range node.TrueClauses {
			clause.Pvs--
			if clause.Pvs < 2 {
				*clausesToCheck = append(*clausesToCheck, clause)
			}
		}
	}
}

// --- Retracting an assumption ---

// PropagateUnknownness propagates unknownness from a retracted node.
// Returns the list of nodes that became unknown.
func PropagateUnknownness(inNode *TmsNode) []*TmsNode {
	ltms := inNode.LTMS
	forgetQueue := []*TmsNode{inNode}
	var unknownQueue []*TmsNode

	for len(forgetQueue) > 0 {
		node := forgetQueue[0]
		forgetQueue = forgetQueue[1:]
		unknownQueue = append(unknownQueue, node)

		DebuggingLtms(ltms, "\n Retracting %s.", NodeString(node))
		oldValue := node.Label
		node.Label = LabelUnknown
		node.Support = nil

		var clauses []*Clause
		switch oldValue {
		case LabelTrue:
			clauses = node.FalseClauses
		case LabelFalse:
			clauses = node.TrueClauses
		}
		for _, clause := range clauses {
			clause.Pvs++
			if clause.Pvs == 2 {
				node2 := ClauseConsequent(clause)
				if node2 != nil {
					forgetQueue = append(forgetQueue, node2)
				}
			}
		}

		if ltms.Complete != nil && ltms.PropagateMoreUnknownnessFn != nil {
			ltms.PropagateMoreUnknownnessFn(oldValue, node, ltms)
		}
	}
	return unknownQueue
}

// ClauseConsequent finds the node in a clause that was set by this clause.
func ClauseConsequent(clause *Clause) *TmsNode {
	for _, term := range clause.Literals {
		if term.Node.Label == term.Sign {
			if clause == term.Node.Support {
				return term.Node
			}
			return nil
		}
	}
	return nil
}

// FindAlternativeSupport tries to find alternative support for nodes
// that became unknown.
func FindAlternativeSupport(ltms *LTMS, nodes []*TmsNode) {
	for _, node := range nodes {
		if UnknownNode(node) {
			CheckClauses(ltms, copyClauseSlice(node.TrueClauses))
			CheckClauses(ltms, copyClauseSlice(node.FalseClauses))
		}
	}
	if completeBool, ok := ltms.Complete.(bool); ok && completeBool && ltms.IpiaFn != nil {
		ltms.IpiaFn(ltms)
	}
}

func copyClauseSlice(s []*Clause) []*Clause {
	if s == nil {
		return nil
	}
	c := make([]*Clause, len(s))
	copy(c, s)
	return c
}

// --- Contradiction handling ---

// CheckForContradictions checks for and handles contradictions.
func CheckForContradictions(ltms *LTMS) {
	var violatedClauses []*Clause
	for _, c := range ltms.ViolatedClauses {
		if ViolatedClause(c) {
			violatedClauses = append(violatedClauses, c)
		}
	}
	ltms.ViolatedClauses = violatedClauses
	if len(violatedClauses) > 0 {
		ContradictionHandler(ltms, violatedClauses)
	}
}

// ContradictionHandler dispatches contradiction handling.
func ContradictionHandler(ltms *LTMS, violatedClauses []*Clause) {
	if !ltms.CheckingContradictions {
		var pending []*Clause
		for _, c := range ltms.PendingContradictions {
			if ViolatedClause(c) {
				pending = append(pending, c)
			}
		}
		for _, vc := range violatedClauses {
			if ViolatedClause(vc) {
				found := false
				for _, p := range pending {
					if p == vc {
						found = true
						break
					}
				}
				if !found {
					pending = append(pending, vc)
				}
			}
		}
		ltms.PendingContradictions = pending
	} else {
		for _, handler := range ltms.ContradictionHandlers {
			if handler(violatedClauses, ltms) {
				return
			}
		}
	}
}

// WithContradictionCheck temporarily enables contradiction checking,
// executes body, then restores the previous state.
func WithContradictionCheck(ltms *LTMS, body func()) {
	oldCC := ltms.CheckingContradictions
	ltms.CheckingContradictions = true
	defer func() {
		ltms.CheckingContradictions = oldCC
	}()
	body()
	CheckForContradictions(ltms)
}

// WithoutContradictionCheck temporarily disables contradiction checking,
// executes body, then restores the previous state.
func WithoutContradictionCheck(ltms *LTMS, body func()) {
	oldCC := ltms.CheckingContradictions
	ltms.CheckingContradictions = false
	defer func() {
		ltms.CheckingContradictions = oldCC
	}()
	body()
}

// WithContradictionHandler temporarily adds a contradiction handler,
// executes body, then restores the previous handlers.
func WithContradictionHandler(ltms *LTMS, handler func([]*Clause, *LTMS) bool, body func()) {
	oldHandlers := ltms.ContradictionHandlers
	ltms.ContradictionHandlers = []func([]*Clause, *LTMS) bool{handler}
	defer func() {
		ltms.ContradictionHandlers = oldHandlers
	}()
	body()
}

// WithAssumptions temporarily enables the given assumptions, executes body,
// then retracts them.
func WithAssumptions(assumptionValues []*AssumptionValue, body func()) {
	for _, av := range assumptionValues {
		EnableAssumption(av.Node, av.Label)
	}
	defer func() {
		for i := len(assumptionValues) - 1; i >= 0; i-- {
			RetractAssumption(assumptionValues[i].Node)
		}
	}()
	body()
}

// --- Inquiring about well-founded support ---

// SupportForNode returns the supporting nodes and informant for a node.
func SupportForNode(node *TmsNode) (supportNodes []*TmsNode, informant interface{}) {
	support := node.Support
	if support == nil {
		return nil, nil
	}
	if support == EnabledAssumption {
		return nil, EnabledAssumption
	}
	clause, ok := support.(*Clause)
	if !ok {
		return nil, nil
	}
	var result []*TmsNode
	for _, pair := range clause.Literals {
		if pair.Node != node {
			result = append(result, pair.Node)
		}
	}
	return result, clause.Informant
}

// AssumptionsOfNode returns the assumptions supporting a node.
func AssumptionsOfNode(node *TmsNode) []*TmsNode {
	if node.Support == EnabledAssumption {
		return []*TmsNode{node}
	}
	if KnownNode(node) {
		if clause, ok := node.Support.(*Clause); ok {
			return AssumptionsOfClause(clause)
		}
	}
	return nil
}

// AssumptionsOfClause returns all assumptions supporting a clause.
func AssumptionsOfClause(inClause *Clause) []*TmsNode {
	clauseQueue := []*Clause{inClause}
	mark := new(struct{}) // unique marker
	var assumptions []*TmsNode

	for len(clauseQueue) > 0 {
		clause := clauseQueue[0]
		clauseQueue = clauseQueue[1:]
		for _, termPair := range clause.Literals {
			node := termPair.Node
			if node.Mark == mark {
				continue
			}
			if node.Label != termPair.Sign {
				if node.Support == EnabledAssumption {
					assumptions = append(assumptions, node)
				} else if node.Support == nil {
					LtmsError("Node is unknown", node)
				} else if supportClause, ok := node.Support.(*Clause); ok {
					clauseQueue = append(clauseQueue, supportClause)
				}
			}
			node.Mark = mark
		}
	}
	return assumptions
}

// --- Simple user interface ---

// AskUserHandler is a default contradiction handler that prints info.
func AskUserHandler(contradictions []*Clause, ltms *LTMS) bool {
	fmt.Println("\nContradiction detected!")
	HandleOneContradiction(contradictions[0])
	return true
}

// HandleOneContradiction prints details about a single violated clause.
func HandleOneContradiction(violatedClause *Clause) {
	assumptions := AssumptionsOfClause(violatedClause)
	if len(assumptions) == 0 {
		LtmsError("Total contradiction", violatedClause)
	}
	fmt.Println("Violated clause:")
	fmt.Printf("  %s\n", violatedClause)
	PrintContraList(assumptions)
}

// PrintContraList prints a list of contradictory assumption nodes.
func PrintContraList(nodes []*TmsNode) {
	fmt.Println("Assumptions:")
	for i, node := range nodes {
		fmt.Printf("  %d: %s (%s)\n", i+1, NodeString(node), node.Label)
	}
}

// TmsAnswer returns the node at the given 1-based index in a list.
func TmsAnswer(num int, nodes []*TmsNode) *TmsNode {
	if num < 1 || num > len(nodes) {
		return nil
	}
	return nodes[num-1]
}

// AvoidAll retracts assumptions to resolve all contradictions.
func AvoidAll(contradictions []*Clause, _ *LTMS) bool {
	for _, contradiction := range contradictions {
		if ViolatedClause(contradiction) {
			culprits := AssumptionsOfClause(contradiction)
			if len(culprits) == 0 {
				LtmsError("Total contradiction", contradiction)
			}
			culprit := culprits[0]
			sign := culprit.Label
			RetractAssumption(culprit)
			AddNogood(culprit, sign, culprits)
		}
	}
	return true
}

// --- Display and debugging functions ---

// ClauseAntecedents returns the antecedent nodes of a clause.
func ClauseAntecedents(clause *Clause) []*TmsNode {
	var result []*TmsNode
	for _, lit := range clause.Literals {
		if lit.Node.Label != lit.Sign {
			result = append(result, lit.Node)
		}
	}
	return result
}

// SignedNodeString returns the node string with its sign.
func SignedNodeString(node *TmsNode) string {
	switch node.Label {
	case LabelTrue:
		return NodeString(node)
	case LabelFalse:
		return fmt.Sprintf("~%s", NodeString(node))
	default:
		return fmt.Sprintf("?%s", NodeString(node))
	}
}

// NodeConsequences returns the consequences of a node.
func NodeConsequences(node *TmsNode) []*TmsNode {
	var conseqs []*TmsNode
	addConseq := func(clauses []*Clause) {
		for _, clause := range clauses {
			for _, lit := range clause.Literals {
				if lit.Node != node && lit.Node.Label == lit.Sign {
					if supportClause, ok := lit.Node.Support.(*Clause); ok {
						if supportClause == clause {
							conseqs = append(conseqs, lit.Node)
						}
					}
				}
			}
		}
	}
	addConseq(node.TrueClauses)
	addConseq(node.FalseClauses)
	return conseqs
}

// WhyNode prints the reason for a node's current label.
func WhyNode(node *TmsNode) {
	if UnknownNode(node) {
		fmt.Printf("%s is unknown.\n", NodeString(node))
		return
	}
	support := node.Support
	if support == EnabledAssumption {
		fmt.Printf("%s is %s via enabled assumption.\n",
			NodeString(node), node.Label)
		return
	}
	if clause, ok := support.(*Clause); ok {
		fmt.Printf("%s is %s via clause %s:\n",
			NodeString(node), node.Label, clause)
		antecedents := ClauseAntecedents(clause)
		for _, ant := range antecedents {
			fmt.Printf("  %s\n", SignedNodeString(ant))
		}
		if clause.Informant != nil {
			fmt.Printf("  Informant: %v\n", clause.Informant)
		}
	}
}

// WhyNodes prints the reasons for all nodes in the LTMS.
func WhyNodes(ltms *LTMS) {
	if ltms.Nodes != nil {
		for _, node := range ltms.Nodes {
			WhyNode(node)
		}
	}
}

// ExplainNode prints a full explanation for a node's belief.
func ExplainNode(node *TmsNode) {
	explain1(node, make(map[*TmsNode]bool))
}

func explain1(node *TmsNode, visited map[*TmsNode]bool) {
	if visited[node] {
		return
	}
	visited[node] = true
	WhyNode(node)
	if node.Support == EnabledAssumption || node.Support == nil {
		return
	}
	if clause, ok := node.Support.(*Clause); ok {
		for _, lit := range clause.Literals {
			if lit.Node != node {
				explain1(lit.Node, visited)
			}
		}
	}
}

// PrettyPrintClauses prints all clauses in the LTMS.
func PrettyPrintClauses(ltms *LTMS) {
	WalkClauses(ltms, func(c *Clause) {
		PrettyPrintClause(c)
	})
}

// PrettyPrintClause prints a single clause in a readable format.
func PrettyPrintClause(clause *Clause) {
	var parts []string
	for _, lit := range clause.Literals {
		switch lit.Sign {
		case LabelTrue:
			parts = append(parts, NodeString(lit.Node))
		case LabelFalse:
			parts = append(parts, fmt.Sprintf("~%s", NodeString(lit.Node)))
		}
	}
	status := ""
	if SatisfiedClause(clause) {
		status = " [satisfied]"
	} else if ViolatedClause(clause) {
		status = " [violated]"
	}
	fmt.Printf("Clause %d: %s%s\n", clause.Index,
		strings.Join(parts, " v "), status)
	if clause.Informant != nil {
		fmt.Printf("  Informant: %v\n", clause.Informant)
	}
}

// ShowNodeConsequences prints the consequences of a node.
func ShowNodeConsequences(node *TmsNode) {
	fmt.Printf("Consequences of %s:\n", SignedNodeString(node))
	conseqs := NodeConsequences(node)
	if len(conseqs) == 0 {
		fmt.Println("  None.")
	} else {
		for _, c := range conseqs {
			fmt.Printf("  %s\n", SignedNodeString(c))
		}
	}
}

// ExploreNetwork interactively explores the support network from a node.
// Corresponds to the Lisp function explore-network.
func ExploreNetwork(node *TmsNode) {
	fmt.Printf("\nExploring network from %s...\n", NodeString(node))
	WhyNode(node)
	conseqs := NodeConsequences(node)
	if len(conseqs) > 0 {
		fmt.Println("Consequences:")
		for _, c := range conseqs {
			fmt.Printf("  %s\n", SignedNodeString(c))
		}
	}
}

// NodeShowClauses prints all clauses involving a node.
func NodeShowClauses(node *TmsNode) {
	fmt.Printf("Clauses for %s:\n", NodeString(node))
	fmt.Println("  True clauses:")
	for _, c := range node.TrueClauses {
		fmt.Printf("    ")
		PrettyPrintClause(c)
	}
	fmt.Println("  False clauses:")
	for _, c := range node.FalseClauses {
		fmt.Printf("    ")
		PrettyPrintClause(c)
	}
}
