// Package ltms implements a Logic-based Truth Maintenance System.
// Converted from Common Lisp (BPS by Forbus & de Kleer).
package ltms

import (
	"fmt"
	"sort"
)

// NodeLabel represents the three-valued label of a TMS node.
type NodeLabel int

const (
	LabelUnknown NodeLabel = iota
	LabelTrue
	LabelFalse
)

func (l NodeLabel) String() string {
	switch l {
	case LabelTrue:
		return ":TRUE"
	case LabelFalse:
		return ":FALSE"
	default:
		return ":UNKNOWN"
	}
}

// ClauseStatus represents the status of a clause.
type ClauseStatus int

const (
	StatusNone       ClauseStatus = iota
	StatusSubsumed                // :SUBSUMED
	StatusQueued                  // :QUEUED
	StatusDirty                   // :DIRTY
	StatusNotIndexed              // :NOT-INDEXED
)

// EnabledAssumption is a sentinel value for support.
const EnabledAssumption = ":ENABLED-ASSUMPTION"

// CompleteMode represents the complete flag for LTMS.
type CompleteMode int

const (
	CompleteNone    CompleteMode = iota
	CompleteTrue                // T
	CompleteDelay               // :DELAY
	CompleteComplete            // :COMPLETE
)

// Literal represents a (node, sign) pair in a clause.
type Literal struct {
	Node *TmsNode
	Sign NodeLabel
}

// LTMS is the Logic-based Truth Maintenance System.
type LTMS struct {
	Title                    string
	NodeCounter              int
	ClauseCounter            int
	Nodes                    map[interface{}]*TmsNode // nil if not caching
	Clauses                  interface{}              // []*Clause or trie
	Debugging                bool
	CheckingContradictions   bool
	NodeString               func(*TmsNode) string
	ContradictionHandlers    []func([]*Clause, *LTMS) bool
	PendingContradictions    []*Clause
	EnqueueProcedure         func(interface{})
	Complete                 CompleteMode
	ViolatedClauses          []*Clause
	Queue                    []*QueueSlot // for IPIA
	Conses                   []*Literal   // reuse pool
	DelaySat                 bool
	ConsSize                 int
	// Function fields for cross-module calls
	IpiaFn                     func(*LTMS)
	PropagateMoreUnknownnessFn func(NodeLabel, *TmsNode, *LTMS)
	FullAddClauseFn            func(*LTMS, []*Literal, interface{})
	WalkTrieFn                 func(func(*Clause), interface{})
}

// QueueSlot is a slot in the IPIA priority queue.
type QueueSlot struct {
	Length  int
	Clauses []*Clause
}

// TmsNode represents a node in the LTMS.
type TmsNode struct {
	Index        int
	Datum        interface{}
	Label        NodeLabel
	Support      interface{} // *Clause, string (EnabledAssumption), or nil
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

// Clause represents a disjunctive clause.
type Clause struct {
	Index    int
	Informant interface{}
	Literals  []*Literal
	Pvs       int // potentially violating terms
	Length    int
	Sats      int // satisfying terms
	Status    ClauseStatus
}

// --- String methods ---

func (l *LTMS) String() string {
	return fmt.Sprintf("#<LTMS: %v>", l.Title)
}

func (n *TmsNode) String() string {
	return fmt.Sprintf("#<NODE: %v>", NodeString(n))
}

func (c *Clause) String() string {
	return fmt.Sprintf("#<Clause %d>", c.Index)
}

func NodeString(node *TmsNode) string {
	if node.LTMS != nil && node.LTMS.NodeString != nil {
		return node.LTMS.NodeString(node)
	}
	return DefaultNodeString(node)
}

func DefaultNodeString(n *TmsNode) string {
	return fmt.Sprintf("%v", n.Datum)
}

func debuggingLtms(l *LTMS, msg string, args ...interface{}) {
	if l.Debugging {
		fmt.Printf(msg, args...)
	}
}

func LtmsError(msg string, thing interface{}) {
	panic(fmt.Sprintf("%s %v", msg, thing))
}

func SatisfiedClause(clause *Clause) bool {
	return clause.Sats > 0
}

func ViolatedClause(clause *Clause) bool {
	return clause.Pvs == 0
}

// WalkClauses iterates over all clauses.
func (l *LTMS) WalkClauses(f func(*Clause)) {
	if l.Complete != CompleteNone && l.WalkTrieFn != nil {
		l.WalkTrieFn(f, l.Clauses)
	} else {
		if clauses, ok := l.Clauses.([]*Clause); ok {
			for _, cl := range clauses {
				f(cl)
			}
		}
	}
}

// --- Creation ---

type LTMSOptions struct {
	NodeString              func(*TmsNode) string
	Debugging               bool
	CheckingContradictions  bool
	ContradictionHandler    func([]*Clause, *LTMS) bool
	EnqueueProcedure        func(interface{})
	CacheDatums             bool
	Complete                CompleteMode
	DelaySat                bool
}

func DefaultLTMSOptions() LTMSOptions {
	return LTMSOptions{
		NodeString:             DefaultNodeString,
		Debugging:              false,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		Complete:               CompleteNone,
		DelaySat:               true,
	}
}

func CreateLTMS(title string, opts ...LTMSOptions) *LTMS {
	opt := DefaultLTMSOptions()
	if len(opts) > 0 {
		opt = opts[0]
	}
	l := &LTMS{
		Title:                  title,
		NodeString:             opt.NodeString,
		Debugging:              opt.Debugging,
		CheckingContradictions: opt.CheckingContradictions,
		EnqueueProcedure:       opt.EnqueueProcedure,
		DelaySat:               opt.DelaySat,
		Complete:               opt.Complete,
	}
	if opt.CacheDatums {
		l.Nodes = make(map[interface{}]*TmsNode)
	}
	if opt.ContradictionHandler != nil {
		l.ContradictionHandlers = []func([]*Clause, *LTMS) bool{opt.ContradictionHandler}
	}
	l.Clauses = []*Clause{}
	return l
}

func ChangeLTMS(l *LTMS, opts LTMSOptions) {
	if opts.NodeString != nil {
		l.NodeString = opts.NodeString
	}
	l.Debugging = opts.Debugging
	l.CheckingContradictions = opts.CheckingContradictions
	if opts.ContradictionHandler != nil {
		l.ContradictionHandlers = []func([]*Clause, *LTMS) bool{opts.ContradictionHandler}
	}
	if opts.EnqueueProcedure != nil {
		l.EnqueueProcedure = opts.EnqueueProcedure
	}
	l.Complete = opts.Complete
	l.DelaySat = opts.DelaySat
}

// SetEnqueueProcedure sets the enqueue procedure.
func (l *LTMS) SetEnqueueProcedure(fn func(interface{})) {
	l.EnqueueProcedure = fn
}

// --- Node queries ---

func UnknownNode(node *TmsNode) bool { return node.Label == LabelUnknown }
func KnownNode(node *TmsNode) bool   { return node.Label != LabelUnknown }
func TrueNode(node *TmsNode) bool    { return node.Label == LabelTrue }
func FalseNode(node *TmsNode) bool   { return node.Label == LabelFalse }

// --- Node creation ---

func TmsCreateNode(l *LTMS, datum interface{}, assumptionp bool) *TmsNode {
	if l.Nodes != nil {
		if _, exists := l.Nodes[datum]; exists {
			LtmsError("Two nodes with same datum:", datum)
		}
	}
	l.NodeCounter++
	node := &TmsNode{
		Index:        l.NodeCounter,
		Datum:        datum,
		Label:        LabelUnknown,
		IsAssumption: assumptionp,
		LTMS:         l,
	}
	node.TrueLiteral = &Literal{Node: node, Sign: LabelTrue}
	node.FalseLiteral = &Literal{Node: node, Sign: LabelFalse}
	if l.Nodes != nil {
		l.Nodes[datum] = node
	}
	if l.Complete != CompleteNone && l.NodeCounter > l.ConsSize {
		l.Conses = nil
		l.ConsSize += 50
		for i := 0; i < l.ConsSize; i++ {
			l.Conses = append(l.Conses, &Literal{})
		}
	}
	return node
}

// --- Assumption operations ---

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

func ConvertToAssumption(node *TmsNode) {
	if !node.IsAssumption {
		debuggingLtms(node.LTMS, "\nConverting %s into an assumption", NodeString(node))
		node.IsAssumption = true
	}
}

func RetractAssumption(node *TmsNode) {
	if KnownNode(node) && node.Support == EnabledAssumption {
		unknowns := PropagateUnknownness(node)
		FindAlternativeSupport(node.LTMS, unknowns)
	}
}

// --- Formula normalization ---

// NormLTMS is the package-level LTMS used during normalization.
var NormLTMS *LTMS

func AddFormula(l *LTMS, formula interface{}, informant interface{}) {
	if informant == nil {
		informant = []interface{}{":IMPLIED-BY", formula}
	} else {
		informant = []interface{}{":IMPLIED-BY", formula, informant}
	}
	clauses := Normalize(l, formula)
	for _, clause := range clauses {
		simplified := SimplifyClause(clause)
		if simplified != nil { // nil means tautology
			AddClauseInternal(simplified, informant, true)
		}
	}
	CheckForContradictions(l)
}

// SimplifyClause simplifies a clause, returning nil if it's a tautology.
func SimplifyClause(literals []*Literal) []*Literal {
	if len(literals) == 0 {
		return literals
	}
	literals = SortClause(literals)
	result := []*Literal{literals[0]}
	for i := 1; i < len(literals); i++ {
		prev := result[len(result)-1]
		curr := literals[i]
		if prev.Node != curr.Node {
			result = append(result, curr)
		} else if prev.Sign != curr.Sign {
			return nil // tautology
		}
		// else duplicate, skip
	}
	return result
}

func SortClause(literals []*Literal) []*Literal {
	cp := make([]*Literal, len(literals))
	copy(cp, literals)
	sort.Slice(cp, func(i, j int) bool {
		return cp[i].Node.Index < cp[j].Node.Index
	})
	return cp
}

func Normalize(l *LTMS, exp interface{}) [][]*Literal {
	NormLTMS = l
	return normalize1(exp, false)
}

func normalize1(exp interface{}, negate bool) [][]*Literal {
	if lst, ok := exp.([]interface{}); ok && len(lst) > 0 {
		op, _ := lst[0].(string)
		switch op {
		case ":IMPLIES":
			if negate {
				r1 := normalize1(lst[1], false)
				r2 := normalize1(lst[2], true)
				return append(r1, r2...)
			}
			return disjoin(normalize1(lst[1], true), normalize1(lst[2], false))
		case ":IFF":
			return normalizeIff(lst, negate)
		case ":OR":
			if negate {
				return normalizeConjunction(lst, true)
			}
			return normalizeDisjunction(lst, false)
		case ":AND":
			if negate {
				return normalizeDisjunction(lst, true)
			}
			return normalizeConjunction(lst, false)
		case ":NOT":
			return normalize1(lst[1], !negate)
		case ":TAXONOMY":
			return normalizeTax(lst, negate)
		}
	}
	node := FindNode(NormLTMS, exp)
	if negate {
		return [][]*Literal{{node.FalseLiteral}}
	}
	return [][]*Literal{{node.TrueLiteral}}
}

func normalizeTax(exp []interface{}, negate bool) [][]*Literal {
	items := exp[1:]
	// Build: (:AND (:OR items...) pairwise-not-ands...)
	orClause := make([]interface{}, 0, len(items)+1)
	orClause = append(orClause, ":OR")
	orClause = append(orClause, items...)
	andParts := []interface{}{":AND", orClause}
	for i := 0; i < len(items); i++ {
		for j := i + 1; j < len(items); j++ {
			andParts = append(andParts,
				[]interface{}{":NOT", []interface{}{":AND", items[i], items[j]}})
		}
	}
	return normalize1(andParts, negate)
}

func normalizeConjunction(exp []interface{}, negate bool) [][]*Literal {
	var result [][]*Literal
	for _, sub := range exp[1:] {
		result = append(result, normalize1(sub, negate)...)
	}
	return result
}

func normalizeIff(exp []interface{}, negate bool) [][]*Literal {
	r1 := normalize1([]interface{}{":IMPLIES", exp[1], exp[2]}, negate)
	r2 := normalize1([]interface{}{":IMPLIES", exp[2], exp[1]}, negate)
	return append(r1, r2...)
}

func normalizeDisjunction(exp []interface{}, negate bool) [][]*Literal {
	if len(exp) < 2 {
		return [][]*Literal{nil}
	}
	result := normalize1(exp[1], negate)
	for _, sub := range exp[2:] {
		result = disjoin(normalize1(sub, negate), result)
	}
	return result
}

func disjoin(conj1, conj2 [][]*Literal) [][]*Literal {
	if len(conj1) == 0 && len(conj2) == 0 {
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

func FindNode(l *LTMS, name interface{}) *TmsNode {
	if node, ok := name.(*TmsNode); ok {
		return node
	}
	if l.Nodes != nil {
		if node, exists := l.Nodes[name]; exists {
			return node
		}
	}
	return TmsCreateNode(l, name, false)
}

// --- Adding clauses ---

func AddClause(trueNodes, falseNodes []*TmsNode, informant interface{}) {
	var literals []*Literal
	for _, n := range trueNodes {
		literals = append(literals, n.TrueLiteral)
	}
	for _, n := range falseNodes {
		literals = append(literals, n.FalseLiteral)
	}
	AddClauseInternal(literals, informant, false)
}

func AddClauseInternal(literals []*Literal, informant interface{}, internal bool) {
	if len(literals) == 0 {
		LtmsError("Total contradiction: Null clause", informant)
	}
	l := literals[0].Node.LTMS
	if l.Complete != CompleteNone && l.FullAddClauseFn != nil {
		l.FullAddClauseFn(l, literals, informant)
	} else {
		cl := BcpAddClause(l, literals, informant, true)
		if clauses, ok := l.Clauses.([]*Clause); ok {
			l.Clauses = append([]*Clause{cl}, clauses...)
		}
	}
	if !internal {
		CheckForContradictions(l)
	}
}

func BcpAddClause(l *LTMS, literals []*Literal, informant interface{}, index bool) *Clause {
	l.ClauseCounter++
	cl := &Clause{
		Index:    l.ClauseCounter,
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
		CheckClauses(l, []*Clause{cl})
	}
	return cl
}

func InsertTrueClause(cl *Clause, node *TmsNode) {
	node.TrueClauses = append([]*Clause{cl}, node.TrueClauses...)
}

func InsertFalseClause(cl *Clause, node *TmsNode) {
	node.FalseClauses = append([]*Clause{cl}, node.FalseClauses...)
}

func AddNogood(culprit *TmsNode, sign NodeLabel, assumptions []*TmsNode) {
	var trues, falses []*TmsNode
	for _, a := range assumptions {
		var label NodeLabel
		if a == culprit {
			label = sign
		} else {
			label = a.Label
		}
		switch label {
		case LabelTrue:
			falses = append(falses, a)
		case LabelFalse:
			trues = append(trues, a)
		}
	}
	AddClause(trues, falses, "NOGOOD")
}

// --- Boolean Constraint Propagation ---

func CheckClauses(l *LTMS, clausesToCheck []*Clause) {
	debuggingLtms(l, "\n Beginning propagation...")
	for len(clausesToCheck) > 0 {
		cl := clausesToCheck[0]
		clausesToCheck = clausesToCheck[1:]
		newClauses := checkClause(l, cl)
		clausesToCheck = append(clausesToCheck, newClauses...)
	}
}

func checkClause(l *LTMS, clause *Clause) []*Clause {
	if ViolatedClause(clause) {
		found := false
		for _, vc := range l.ViolatedClauses {
			if vc == clause {
				found = true
				break
			}
		}
		if !found {
			l.ViolatedClauses = append(l.ViolatedClauses, clause)
		}
		return nil
	}
	if clause.Pvs == 1 {
		unknownPair := FindUnknownPair(clause)
		if unknownPair != nil {
			return SetTruth(unknownPair.Node, unknownPair.Sign, clause)
		}
	}
	return nil
}

func FindUnknownPair(clause *Clause) *Literal {
	for _, term := range clause.Literals {
		if UnknownNode(term.Node) {
			return term
		}
	}
	return nil
}

func TopSetTruth(node *TmsNode, value NodeLabel, reason interface{}) {
	clausesToCheck := SetTruth(node, value, reason)
	CheckClauses(node.LTMS, clausesToCheck)
	CheckForContradictions(node.LTMS)
}

func SetTruth(node *TmsNode, value NodeLabel, reason interface{}) []*Clause {
	l := node.LTMS
	enqueuef := l.EnqueueProcedure
	debuggingLtms(l, "\n  Setting %s to %s, via %v.", NodeString(node), value, reason)
	node.Support = reason
	node.Label = value
	var clausesToCheck []*Clause
	switch value {
	case LabelTrue:
		if enqueuef != nil {
			for _, rule := range node.TrueRules {
				enqueuef(rule)
			}
			node.TrueRules = nil
		}
		for _, clause := range node.TrueClauses {
			clause.Sats++
		}
		for _, clause := range node.FalseClauses {
			clause.Pvs--
			if clause.Pvs < 2 {
				clausesToCheck = append(clausesToCheck, clause)
			}
		}
	case LabelFalse:
		if enqueuef != nil {
			for _, rule := range node.FalseRules {
				enqueuef(rule)
			}
			node.FalseRules = nil
		}
		for _, clause := range node.FalseClauses {
			clause.Sats++
		}
		for _, clause := range node.TrueClauses {
			clause.Pvs--
			if clause.Pvs < 2 {
				clausesToCheck = append(clausesToCheck, clause)
			}
		}
	}
	return clausesToCheck
}

// --- Retracting assumptions ---

func PropagateUnknownness(inNode *TmsNode) []*TmsNode {
	l := inNode.LTMS
	var unknownQueue []*TmsNode
	forgetQueue := []*TmsNode{inNode}

	for len(forgetQueue) > 0 {
		node := forgetQueue[0]
		forgetQueue = forgetQueue[1:]
		unknownQueue = append(unknownQueue, node)

		debuggingLtms(l, "\n Retracting %s.", NodeString(node))
		oldValue := node.Label
		node.Label = LabelUnknown
		node.Support = nil

		var clauseList []*Clause
		switch oldValue {
		case LabelTrue:
			clauseList = node.FalseClauses
		case LabelFalse:
			clauseList = node.TrueClauses
		}
		for _, clause := range clauseList {
			clause.Pvs++
			if clause.Pvs == 2 {
				if node2 := ClauseConsequent(clause); node2 != nil {
					forgetQueue = append(forgetQueue, node2)
				}
			}
		}
		if l.Complete != CompleteNone && l.PropagateMoreUnknownnessFn != nil {
			l.PropagateMoreUnknownnessFn(oldValue, node, l)
		}
	}
	return unknownQueue
}

func ClauseConsequent(clause *Clause) *TmsNode {
	for _, termPair := range clause.Literals {
		if termPair.Node.Label == termPair.Sign {
			if clause == termPair.Node.Support {
				return termPair.Node
			}
			return nil
		}
	}
	return nil
}

func FindAlternativeSupport(l *LTMS, nodes []*TmsNode) {
	for _, node := range nodes {
		if UnknownNode(node) {
			CheckClauses(l, copyClauseSlice(node.TrueClauses))
			CheckClauses(l, copyClauseSlice(node.FalseClauses))
		}
	}
	if l.Complete == CompleteTrue && l.IpiaFn != nil {
		l.IpiaFn(l)
	}
}

func copyClauseSlice(src []*Clause) []*Clause {
	cp := make([]*Clause, len(src))
	copy(cp, src)
	return cp
}

// --- Contradiction handling ---

func CheckForContradictions(l *LTMS) {
	var stillViolated []*Clause
	for _, c := range l.ViolatedClauses {
		if ViolatedClause(c) {
			stillViolated = append(stillViolated, c)
		}
	}
	l.ViolatedClauses = stillViolated
	if len(stillViolated) > 0 {
		handleContradiction(l, stillViolated)
	}
}

func handleContradiction(l *LTMS, violatedClauses []*Clause) {
	if !l.CheckingContradictions {
		var stillPending []*Clause
		for _, c := range l.PendingContradictions {
			if ViolatedClause(c) {
				stillPending = append(stillPending, c)
			}
		}
		l.PendingContradictions = stillPending
		for _, vc := range violatedClauses {
			if ViolatedClause(vc) {
				found := false
				for _, pc := range l.PendingContradictions {
					if pc == vc {
						found = true
						break
					}
				}
				if !found {
					l.PendingContradictions = append(l.PendingContradictions, vc)
				}
			}
		}
	} else {
		for _, handler := range l.ContradictionHandlers {
			if handler(violatedClauses, l) {
				return
			}
		}
	}
}

// WithContradictionCheck runs body with contradiction checking enabled.
func WithContradictionCheck(l *LTMS, body func()) {
	oldValue := l.CheckingContradictions
	l.CheckingContradictions = true
	defer func() { l.CheckingContradictions = oldValue }()
	body()
}

// WithoutContradictionCheck runs body with contradiction checking disabled.
func WithoutContradictionCheck(l *LTMS, body func()) {
	oldValue := l.CheckingContradictions
	l.CheckingContradictions = false
	defer func() { l.CheckingContradictions = oldValue }()
	body()
}

// WithContradictionHandler runs body with an additional contradiction handler.
func WithContradictionHandler(l *LTMS, handler func([]*Clause, *LTMS) bool, body func()) {
	l.ContradictionHandlers = append([]func([]*Clause, *LTMS) bool{handler}, l.ContradictionHandlers...)
	defer func() {
		if len(l.ContradictionHandlers) > 0 {
			l.ContradictionHandlers = l.ContradictionHandlers[1:]
		}
	}()
	body()
}

// AssumptionValue pairs a node with a label for WithAssumptions.
type AssumptionValue struct {
	Node  *TmsNode
	Label NodeLabel
}

// WithAssumptions enables assumptions and retracts them after body runs.
func WithAssumptions(avs []*AssumptionValue, body func()) {
	for _, av := range avs {
		EnableAssumption(av.Node, av.Label)
	}
	defer func() {
		for _, av := range avs {
			RetractAssumption(av.Node)
		}
	}()
	body()
}

// --- Support queries ---

func SupportForNode(node *TmsNode) ([]*TmsNode, interface{}) {
	if node.Support == nil {
		return nil, nil
	}
	if node.Support == EnabledAssumption {
		return nil, EnabledAssumption
	}
	clause, ok := node.Support.(*Clause)
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

func AssumptionsOfClause(inClause *Clause) []*TmsNode {
	mark := &struct{}{} // unique marker
	clauseQueue := []*Clause{inClause}
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
				} else if cl, ok := node.Support.(*Clause); ok {
					clauseQueue = append(clauseQueue, cl)
				}
			}
			node.Mark = mark
		}
	}
	return assumptions
}

// --- User interface ---

func AskUserHandler(contradictions []*Clause, l *LTMS) bool {
	for _, contradiction := range contradictions {
		if ViolatedClause(contradiction) {
			HandleOneContradiction(contradiction)
		}
	}
	return true
}

func HandleOneContradiction(violatedClause *Clause) {
	contraAssumptions := AssumptionsOfClause(violatedClause)
	if len(contraAssumptions) == 0 {
		LtmsError("Global contradiction", violatedClause)
	}
	fmt.Println("\nContradiction found:")
	PrintContraList(contraAssumptions)
	fmt.Println("\nRetracting first assumption to resolve.")
	RetractAssumption(contraAssumptions[0])
}

func PrintContraList(nodes []*TmsNode) {
	for i, node := range nodes {
		fmt.Printf("\n%d %s", i+1, NodeString(node))
	}
}

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

// --- Display functions ---

func ClauseAntecedents(clause *Clause) []*TmsNode {
	var result []*TmsNode
	for _, pair := range clause.Literals {
		if pair.Node.Support != clause {
			result = append(result, pair.Node)
		}
	}
	return result
}

func SignedNodeString(node *TmsNode) string {
	if TrueNode(node) {
		return NodeString(node)
	}
	if FalseNode(node) {
		return fmt.Sprintf("Not[%s]", NodeString(node))
	}
	return fmt.Sprintf("Unknown[%s]", NodeString(node))
}

func NodeConsequences(node *TmsNode) []*TmsNode {
	var clauses []*Clause
	switch node.Label {
	case LabelTrue:
		clauses = node.FalseClauses
	case LabelFalse:
		clauses = node.TrueClauses
	}
	var conseqs []*TmsNode
	for _, cl := range clauses {
		if cl != node.Support {
			if conseq := ClauseConsequent(cl); conseq != nil {
				conseqs = append(conseqs, conseq)
			}
		}
	}
	return conseqs
}

func WhyNode(node *TmsNode) {
	if UnknownNode(node) {
		fmt.Printf("\n%s is unknown.", NodeString(node))
		return
	}
	if node.Support == EnabledAssumption {
		fmt.Printf("\n%s is %s <%s>", NodeString(node), node.Label, EnabledAssumption)
		return
	}
	clause, ok := node.Support.(*Clause)
	if !ok {
		fmt.Printf("\n%s is %s (unknown support)", NodeString(node), node.Label)
		return
	}
	fmt.Printf("\n%s is %s via %v on", NodeString(node), node.Label, clause.Informant)
	for _, termPair := range clause.Literals {
		if termPair.Node.Label != termPair.Sign {
			fmt.Printf("\n   %s is %s", NodeString(termPair.Node), termPair.Node.Label)
		}
	}
}

func WhyNodes(l *LTMS) {
	if l.Nodes != nil {
		for _, n := range l.Nodes {
			WhyNode(n)
		}
	}
}

func ExplainNode(node *TmsNode) {
	if node.Label == LabelUnknown {
		return
	}
	lineCount := 0
	if node.LTMS.Nodes != nil {
		for _, n := range node.LTMS.Nodes {
			n.Mark = nil
		}
	}
	explain1(node, &lineCount)
}

func explain1(node *TmsNode, lineCount *int) interface{} {
	if node.Mark != nil {
		return node.Mark
	}
	if node.Support == EnabledAssumption {
		*lineCount++
		label := NodeString(node)
		if !TrueNode(node) {
			label = fmt.Sprintf("(:NOT %s)", label)
		}
		fmt.Printf("\n%3d %-15s %-15s   Assumption", *lineCount, label, "()")
		node.Mark = *lineCount
		return *lineCount
	}
	clause, ok := node.Support.(*Clause)
	if !ok {
		return nil
	}
	antecedents := ClauseAntecedents(clause)
	var anteResults []interface{}
	for _, a := range antecedents {
		anteResults = append(anteResults, explain1(a, lineCount))
	}
	*lineCount++
	label := NodeString(node)
	if !TrueNode(node) {
		label = fmt.Sprintf("(:NOT %s)", label)
	}
	fmt.Printf("\n%3d %-15s %-15v  ", *lineCount, label, anteResults)
	PrettyPrintClause(clause)
	node.Mark = *lineCount
	return *lineCount
}

func PrettyPrintClauses(l *LTMS) {
	l.WalkClauses(func(cl *Clause) {
		fmt.Print("\n ")
		PrettyPrintClause(cl)
	})
}

func PrettyPrintClause(clause *Clause) {
	fmt.Print("(:OR")
	for _, lit := range clause.Literals {
		if lit.Sign == LabelTrue {
			fmt.Printf(" %s", NodeString(lit.Node))
		} else {
			fmt.Printf(" (:NOT %s)", NodeString(lit.Node))
		}
	}
	fmt.Print(")")
}

func ShowNodeConsequences(node *TmsNode) {
	conseqs := NodeConsequences(node)
	if len(conseqs) > 0 {
		fmt.Printf("\n Consequences of %s:", SignedNodeString(node))
		for _, conseq := range conseqs {
			fmt.Printf("\n  %s", SignedNodeString(conseq))
		}
	} else {
		fmt.Printf("\n %s has no consequences.", NodeString(node))
	}
}

func NodeShowClauses(node *TmsNode) {
	fmt.Printf("For %s:", NodeString(node))
	for _, cl := range node.TrueClauses {
		fmt.Println()
		PrettyPrintClause(cl)
	}
	for _, cl := range node.FalseClauses {
		fmt.Println()
		PrettyPrintClause(cl)
	}
}
