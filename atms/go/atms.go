// Assumption-based truth maintenance system, version 61 of 7/21/92.
// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.
//
// Translated from Common Lisp to Go.

package atms

import (
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
)

// EnvCompareResult represents the result of comparing two environments.
type EnvCompareResult int

const (
	CompareEQ  EnvCompareResult = iota // environments are equal
	CompareS12                         // e1 is a subset of e2
	CompareS21                         // e2 is a subset of e1
	CompareNone                        // no subset relationship
)

// EnvTableEntry represents one bucket in an env-table or nogood-table,
// keyed by the count of assumptions.
type EnvTableEntry struct {
	Count int
	Envs  []*Env
}

// ATMS is the main Assumption-based Truth Maintenance System structure.
type ATMS struct {
	Title            string
	NodeCounter      int
	JustCounter      int
	EnvCounter       int
	Nodes            []*TmsNode
	Justs            []*Just
	Contradictions   []*TmsNode
	Assumptions      []*TmsNode
	Debugging        bool
	NogoodTable      []EnvTableEntry
	ContraNode       *TmsNode
	EnvTable         []EnvTableEntry
	EmptyEnv         *Env
	NodeString       func(*TmsNode) string
	EnqueueProcedure func(interface{})
}

// String implements the Stringer interface for ATMS.
func (a *ATMS) String() string {
	return fmt.Sprintf("#<ATMS: %s>", a.Title)
}

// TmsNode represents a node in the ATMS.
type TmsNode struct {
	Index          int
	Datum          interface{}
	Label          []*Env
	Justs          []*Just
	Consequences   []*Just
	Contradictory  bool
	IsAssumption   bool
	Rules          []interface{}
	ATMS           *ATMS
}

// String implements the Stringer interface for TmsNode.
func (n *TmsNode) String() string {
	if n.IsAssumption {
		return fmt.Sprintf("A-%d", n.Index)
	}
	return fmt.Sprintf("#<NODE: %s>", NodeString(n))
}

// Just represents a justification in the ATMS.
type Just struct {
	Index       int
	Informant   interface{}
	Consequence *TmsNode
	Antecedents []*TmsNode
}

// String implements the Stringer interface for Just.
func (j *Just) String() string {
	return fmt.Sprintf("<%v %d>", j.Informant, j.Index)
}

// Env represents an environment (a set of assumptions).
type Env struct {
	Index       int
	Count       int
	Assumptions []*TmsNode
	Nodes       []*TmsNode
	Nogood      interface{} // Can be nil, *Just, or *Env
	Rules       []interface{}
}

// IsNogood returns true if the environment is marked as nogood.
func (e *Env) IsNogood() bool {
	return e.Nogood != nil
}

// String implements the Stringer interface for Env.
func (e *Env) String() string {
	return fmt.Sprintf("E-%d", e.Index)
}

// NodeString returns the string representation of a node using the ATMS's
// node-string function.
func NodeString(node *TmsNode) string {
	return node.ATMS.NodeString(node)
}

// debugging prints a debug message if debugging is enabled on the ATMS.
func debugging(atms *ATMS, msg string, args ...interface{}) {
	if atms.Debugging {
		fmt.Fprintf(os.Stderr, msg, args...)
	}
}

// DefaultNodeString is the default function for converting a node to a string.
func DefaultNodeString(n *TmsNode) string {
	return fmt.Sprintf("%v", n.Datum)
}

// OrderedInsert inserts item into a sorted slice, maintaining order according
// to the less function. If item is already present (by pointer equality),
// the original slice is returned.
func OrderedInsert(item *TmsNode, list []*TmsNode, less func(*TmsNode, *TmsNode) bool) []*TmsNode {
	if len(list) == 0 {
		return []*TmsNode{item}
	}
	if less(item, list[0]) {
		return append([]*TmsNode{item}, list...)
	}
	if item == list[0] {
		return list
	}
	rest := OrderedInsert(item, list[1:], less)
	// If rest is the same slice as list[1:], return the original list
	if len(rest) == len(list[1:]) && len(rest) > 0 && &rest[0] == &list[1] {
		return list
	}
	result := make([]*TmsNode, 0, 1+len(rest))
	result = append(result, list[0])
	result = append(result, rest...)
	return result
}

// AssumptionOrder returns true if a1 should come before a2 (by index).
func AssumptionOrder(a1, a2 *TmsNode) bool {
	return a1.Index < a2.Index
}

// EnvOrder returns true if e1 should come before e2 (by index).
func EnvOrder(e1, e2 *Env) bool {
	return e1.Index < e2.Index
}

// CreateATMS creates a new ATMS with the given title and options.
func CreateATMS(title string, opts ...func(*ATMS)) *ATMS {
	atms := &ATMS{
		Title:      title,
		NodeString: DefaultNodeString,
	}
	for _, opt := range opts {
		opt(atms)
	}
	if atms.NodeString == nil {
		atms.NodeString = DefaultNodeString
	}
	atms.ContraNode = TmsCreateNode(atms, "The contradiction", TMSNodeOpts{Contradictory: true})
	atms.EmptyEnv = CreateEnv(atms, nil)
	return atms
}

// TMSNodeOpts holds optional parameters for TmsCreateNode.
type TMSNodeOpts struct {
	IsAssumption  bool
	Contradictory bool
}

// ChangeATMS updates optional fields of the ATMS.
func ChangeATMS(atms *ATMS, nodeString func(*TmsNode) string, enqueueProcedure func(interface{}), debugging *bool) {
	if nodeString != nil {
		atms.NodeString = nodeString
	}
	if debugging != nil {
		atms.Debugging = *debugging
	}
	if enqueueProcedure != nil {
		atms.EnqueueProcedure = enqueueProcedure
	}
}

// TrueNode returns true if the node's label starts with the empty environment.
func TrueNode(node *TmsNode) bool {
	if len(node.Label) == 0 {
		return false
	}
	return node.Label[0] == node.ATMS.EmptyEnv
}

// InNode returns true if the node is "in" — either in any environment (if env
// is nil), or in the specific environment given.
func InNode(n *TmsNode, env *Env) bool {
	if env != nil {
		for _, le := range n.Label {
			if SubsetEnv(le, env) {
				return true
			}
		}
		return false
	}
	return len(n.Label) > 0
}

// OutNode returns true if the node is not in the given environment.
func OutNode(n *TmsNode, env *Env) bool {
	return !InNode(n, env)
}

// NodeConsistentWith returns true if the node is consistent with the given
// environment (i.e., at least one label environment union with env is not nogood).
func NodeConsistentWith(n *TmsNode, env *Env) bool {
	for _, le := range n.Label {
		u := UnionEnv(le, env)
		if u != nil && !u.IsNogood() {
			return true
		}
	}
	return false
}

// TmsCreateNode creates a new node in the ATMS.
func TmsCreateNode(atms *ATMS, datum interface{}, opts TMSNodeOpts) *TmsNode {
	atms.NodeCounter++
	node := &TmsNode{
		Index:         atms.NodeCounter,
		Datum:         datum,
		IsAssumption:  opts.IsAssumption,
		Contradictory: opts.Contradictory,
		ATMS:          atms,
	}
	atms.Nodes = append([]*TmsNode{node}, atms.Nodes...)
	if opts.Contradictory {
		atms.Contradictions = append([]*TmsNode{node}, atms.Contradictions...)
	}
	if opts.IsAssumption {
		atms.Assumptions = append([]*TmsNode{node}, atms.Assumptions...)
		env := CreateEnv(atms, []*TmsNode{node})
		node.Label = append([]*Env{env}, node.Label...)
	}
	return node
}

// AssumeNode converts a node into an assumption.
func AssumeNode(node *TmsNode) {
	if node.IsAssumption {
		return
	}
	atms := node.ATMS
	debugging(atms, "\nConverting %s into an assumption", NodeString(node))
	node.IsAssumption = true
	atms.Assumptions = append([]*TmsNode{node}, atms.Assumptions...)
	env := CreateEnv(atms, []*TmsNode{node})
	Update([]*Env{env}, node, "ASSUME-NODE")
}

// MakeContradiction marks a node as contradictory.
func MakeContradiction(node *TmsNode) {
	atms := node.ATMS
	if node.Contradictory {
		return
	}
	node.Contradictory = true
	atms.Contradictions = append([]*TmsNode{node}, atms.Contradictions...)
	for {
		if len(node.Label) == 0 {
			break
		}
		nogood := node.Label[0]
		if nogood == nil {
			break
		}
		NewNogood(atms, nogood, "MAKE-CONTRADICTION")
	}
}

// JustifyNode adds a justification that consequence is supported by antecedents.
func JustifyNode(informant interface{}, consequence *TmsNode, antecedents []*TmsNode) *Just {
	atms := consequence.ATMS
	atms.JustCounter++
	just := &Just{
		Index:       atms.JustCounter,
		Informant:   informant,
		Consequence: consequence,
		Antecedents: antecedents,
	}
	consequence.Justs = append([]*Just{just}, consequence.Justs...)
	for _, node := range antecedents {
		node.Consequences = append([]*Just{just}, node.Consequences...)
	}
	atms.Justs = append([]*Just{just}, atms.Justs...)

	if atms.Debugging {
		antStrs := make([]string, len(antecedents))
		for i, a := range antecedents {
			antStrs[i] = NodeString(a)
		}
		debugging(atms, "\nJustifying %s in terms of %v on %v",
			NodeString(consequence), informant, antStrs)
	}

	Propagate(just, nil, []*Env{atms.EmptyEnv})
	return just
}

// NogoodNodes declares the given nodes as mutually inconsistent.
func NogoodNodes(informant interface{}, nodes []*TmsNode) *Just {
	return JustifyNode(informant, nodes[0].ATMS.ContraNode, nodes)
}

/// Label updating

// Propagate propagates a justification with the given environments.
func Propagate(just *Just, antecedent *TmsNode, envs []*Env) {
	newEnvs := Weave(antecedent, envs, just.Antecedents)
	if newEnvs != nil {
		Update(newEnvs, just.Consequence, just)
	}
}

// Update updates the label of consequence with new environments.
// The just parameter can be a *Just or a string (for AssumeNode).
func Update(newEnvs []*Env, consequence *TmsNode, just interface{}) {
	atms := consequence.ATMS

	// If the consequence is contradictory, mark all new envs as nogood.
	if consequence.Contradictory {
		for _, env := range newEnvs {
			NewNogood(atms, env, just)
		}
		return
	}

	newEnvs = UpdateLabel(consequence, newEnvs)
	if len(newEnvs) == 0 {
		return
	}

	// Fire enqueue procedure for rules.
	if atms.EnqueueProcedure != nil {
		for _, rule := range consequence.Rules {
			atms.EnqueueProcedure(rule)
		}
		consequence.Rules = nil
	}

	// Propagate to supported justifications.
	for _, supportedJust := range consequence.Consequences {
		Propagate(supportedJust, consequence, newEnvs)

		// Remove from newEnvs any env no longer in consequence's label.
		for i := range newEnvs {
			if newEnvs[i] != nil {
				found := false
				for _, le := range consequence.Label {
					if newEnvs[i] == le {
						found = true
						break
					}
				}
				if !found {
					newEnvs[i] = nil
				}
			}
		}
		newEnvs = deleteNilEnvs(newEnvs)
		if len(newEnvs) == 0 {
			return
		}
	}
}

// UpdateLabel updates a node's label with new environments, removing
// subsumed environments. Returns the truly new environments that were added.
func UpdateLabel(node *TmsNode, newEnvs []*Env) []*Env {
	envs := node.Label

	for i := range newEnvs {
		if newEnvs[i] == nil {
			continue
		}
		found := false
		for j := range envs {
			if envs[j] == nil || newEnvs[i] == nil {
				continue
			}
			cmp := CompareEnv(newEnvs[i], envs[j])
			switch cmp {
			case CompareEQ, CompareS21:
				newEnvs[i] = nil
				found = true
			case CompareS12:
				// Remove node from old env's node list.
				envs[j].Nodes = deleteNodeOnce(envs[j].Nodes, node)
				envs[j] = nil
			}
			if found {
				break
			}
		}
		if !found && newEnvs[i] != nil {
			envs = append(envs, newEnvs[i])
		}
	}

	newEnvs = deleteNilEnvs(newEnvs)
	for _, newEnv := range newEnvs {
		newEnv.Nodes = append([]*TmsNode{node}, newEnv.Nodes...)
	}
	node.Label = deleteNilEnvsFromSlice(envs)
	return newEnvs
}

// Weave combines environments across the antecedent labels.
func Weave(antecedent *TmsNode, envs []*Env, antecedents []*TmsNode) []*Env {
	// Copy envs
	envs = copyEnvSlice(envs)

	for _, node := range antecedents {
		if node == antecedent {
			continue
		}
		var newEnvs []*Env
		for _, env := range envs {
			if env == nil {
				continue
			}
			for _, nodeEnv := range node.Label {
				newEnv := UnionEnv(env, nodeEnv)
				if newEnv == nil || newEnv.IsNogood() {
					continue
				}
				// Check if new-env is subsumed by or subsumes existing new-envs.
				subsumed := false
				for k := range newEnvs {
					if newEnvs[k] == nil {
						continue
					}
					cmp := CompareEnv(newEnv, newEnvs[k])
					switch cmp {
					case CompareEQ, CompareS21:
						subsumed = true
					case CompareS12:
						newEnvs[k] = nil
					}
					if subsumed {
						break
					}
				}
				if !subsumed {
					newEnvs = append(newEnvs, newEnv)
				}
			}
		}
		envs = deleteNilEnvs(newEnvs)
		if len(envs) == 0 {
			return nil
		}
	}
	return envs
}

// InAntecedent returns true if the given nodes can all be simultaneously true.
func InAntecedent(nodes []*TmsNode) bool {
	if len(nodes) == 0 {
		return true
	}
	return WeaveQ(nodes[0].ATMS.EmptyEnv, nodes)
}

// WeaveQ returns true if the given environment can be extended to include
// all nodes.
func WeaveQ(env *Env, nodes []*TmsNode) bool {
	if len(nodes) == 0 {
		return true
	}
	for _, e := range nodes[0].Label {
		newEnv := UnionEnv(e, env)
		if newEnv == nil || newEnv.IsNogood() {
			continue
		}
		if WeaveQ(newEnv, nodes[1:]) {
			return true
		}
	}
	return false
}

// SupportingAntecedent returns true if all nodes are "in" the given environment.
func SupportingAntecedent(nodes []*TmsNode, env *Env) bool {
	for _, node := range nodes {
		if !InNode(node, env) {
			return false
		}
	}
	return true
}

// RemoveNode removes a node from the ATMS. The node must not have consequences.
func RemoveNode(node *TmsNode) error {
	if len(node.Consequences) > 0 {
		return fmt.Errorf("can't remove node with consequences")
	}
	atms := node.ATMS
	atms.Nodes = deleteNodeOnce(atms.Nodes, node)
	for _, just := range node.Justs {
		for _, ant := range just.Antecedents {
			ant.Consequences = deleteJustOnce(ant.Consequences, just)
		}
	}
	for _, env := range node.Label {
		env.Nodes = deleteNodeOnce(env.Nodes, node)
	}
	return nil
}

/// Creating and extending environments.

// CreateEnv creates a new environment with the given assumptions.
func CreateEnv(atms *ATMS, assumptions []*TmsNode) *Env {
	atms.EnvCounter++
	e := &Env{
		Index:       atms.EnvCounter,
		Assumptions: assumptions,
		Count:       len(assumptions),
	}
	atms.EnvTable = InsertInTable(atms.EnvTable, e)
	SetEnvContradictory(atms, e)
	return e
}

// UnionEnv returns the union of two environments. Returns nil if the result
// would be nogood (detected during construction).
func UnionEnv(e1, e2 *Env) *Env {
	if e1.Count > e2.Count {
		e1, e2 = e2, e1
	}
	for _, assume := range e1.Assumptions {
		e2 = ConsEnv(assume, e2)
		if e2.IsNogood() {
			return nil
		}
	}
	return e2
}

// ConsEnv adds an assumption to an environment, returning an existing
// environment if one with the same assumptions already exists.
func ConsEnv(assumption *TmsNode, env *Env) *Env {
	nassumes := OrderedInsert(assumption, env.Assumptions, AssumptionOrder)
	existing := LookupEnv(nassumes)
	if existing != nil {
		return existing
	}
	return CreateEnv(assumption.ATMS, nassumes)
}

// FindOrMakeEnv finds an existing environment with the given assumptions,
// or creates a new one.
func FindOrMakeEnv(assumptions []*TmsNode, atms *ATMS) *Env {
	if len(assumptions) == 0 {
		return atms.EmptyEnv
	}
	existing := LookupEnv(assumptions)
	if existing != nil {
		return existing
	}
	return CreateEnv(atms, assumptions)
}

/// Env tables.

// InsertInTable inserts an environment into an env-table (ordered list of
// buckets keyed by count).
func InsertInTable(table []EnvTableEntry, env *Env) []EnvTableEntry {
	count := env.Count
	for i := range table {
		if table[i].Count == count {
			table[i].Envs = append(table[i].Envs, env)
			return table
		}
	}
	// Insert new entry in order.
	newEntry := EnvTableEntry{Count: count, Envs: []*Env{env}}
	if len(table) == 0 {
		return []EnvTableEntry{newEntry}
	}
	// Find insertion point.
	idx := sort.Search(len(table), func(i int) bool {
		return table[i].Count >= count
	})
	// Insert at idx.
	table = append(table, EnvTableEntry{})
	copy(table[idx+1:], table[idx:])
	table[idx] = newEntry
	return table
}

// LookupEnv looks up an environment by its assumptions list.
func LookupEnv(assumes []*TmsNode) *Env {
	if len(assumes) == 0 {
		return nil
	}
	atms := assumes[0].ATMS
	count := len(assumes)
	for _, entry := range atms.EnvTable {
		if entry.Count == count {
			for _, env := range entry.Envs {
				if assumptionsEqual(env.Assumptions, assumes) {
					return env
				}
			}
			return nil
		}
	}
	return nil
}

// SubsetEnv returns true if e1's assumptions are a subset of e2's assumptions.
func SubsetEnv(e1, e2 *Env) bool {
	if e1 == e2 {
		return true
	}
	if e1.Count > e2.Count {
		return false
	}
	return isSubset(e1.Assumptions, e2.Assumptions)
}

// CompareEnv compares two environments and returns the relationship.
func CompareEnv(e1, e2 *Env) EnvCompareResult {
	if e1 == e2 {
		return CompareEQ
	}
	if e1.Count < e2.Count {
		if isSubset(e1.Assumptions, e2.Assumptions) {
			return CompareS12
		}
		return CompareNone
	}
	if isSubset(e2.Assumptions, e1.Assumptions) {
		return CompareS21
	}
	return CompareNone
}

/// Processing nogoods

// NewNogood marks an environment as nogood and propagates the consequences.
func NewNogood(atms *ATMS, cenv *Env, just interface{}) {
	if atms.Debugging {
		debugging(atms, "\n  %s new minimal nogood.", cenv)
	}
	cenv.Nogood = just
	RemoveEnvFromLabels(cenv, atms)
	atms.NogoodTable = InsertInTable(atms.NogoodTable, cenv)
	count := cenv.Count
	for i := range atms.NogoodTable {
		if atms.NogoodTable[i].Count > count {
			filtered := make([]*Env, 0, len(atms.NogoodTable[i].Envs))
			for _, old := range atms.NogoodTable[i].Envs {
				if !SubsetEnv(cenv, old) {
					filtered = append(filtered, old)
				}
			}
			atms.NogoodTable[i].Envs = filtered
		}
	}
	for i := range atms.EnvTable {
		if atms.EnvTable[i].Count > count {
			for _, old := range atms.EnvTable[i].Envs {
				if !old.IsNogood() && SubsetEnv(cenv, old) {
					old.Nogood = cenv
					RemoveEnvFromLabels(old, atms)
				}
			}
		}
	}
}

// SetEnvContradictory checks if a new environment is already nogood based
// on the nogood table.
func SetEnvContradictory(atms *ATMS, env *Env) bool {
	if env.IsNogood() {
		return true
	}
	count := env.Count
	for _, entry := range atms.NogoodTable {
		if entry.Count > count {
			return false
		}
		for _, cenv := range entry.Envs {
			if SubsetEnv(cenv, env) {
				env.Nogood = cenv
				return true
			}
		}
	}
	return false
}

// RemoveEnvFromLabels removes an environment from all node labels.
func RemoveEnvFromLabels(env *Env, atms *ATMS) {
	if atms.EnqueueProcedure != nil {
		for _, rule := range env.Rules {
			atms.EnqueueProcedure(rule)
		}
		env.Rules = nil
	}
	for _, node := range env.Nodes {
		node.Label = deleteEnvOnce(node.Label, env)
	}
}

/// Interpretation construction

// Interpretations constructs interpretations from the given choice sets.
func Interpretations(atms *ATMS, choiceSets [][]*TmsNode, defaults []*TmsNode) []*Env {
	if atms.Debugging {
		fmt.Fprintf(os.Stderr, "\n Constructing interpretations depth-first...")
	}

	var solutions []*Env

	// Expand choice sets: for each alternative set, collect all label envs.
	expandedChoiceSets := make([][]*Env, len(choiceSets))
	for i, altSet := range choiceSets {
		var envs []*Env
		for _, alt := range altSet {
			envs = append(envs, alt.Label...)
		}
		expandedChoiceSets[i] = envs
	}

	if len(expandedChoiceSets) > 0 {
		for _, choice := range expandedChoiceSets[0] {
			getDepthSolutions1(choice, expandedChoiceSets[1:], &solutions)
		}
	}

	solutions = deleteNilEnvs(solutions)

	if len(solutions) == 0 {
		if len(choiceSets) > 0 {
			return nil
		}
		solutions = []*Env{atms.EmptyEnv}
	}

	if len(defaults) > 0 {
		oldSolutions := solutions
		solutions = nil
		for _, solution := range oldSolutions {
			extendViaDefaults(solution, defaults, defaults, &solutions)
		}
	}

	return deleteNilEnvs(solutions)
}

// getDepthSolutions1 is the recursive depth-first search for interpretations.
func getDepthSolutions1(solution *Env, choiceSets [][]*Env, solutions *[]*Env) {
	if len(choiceSets) == 0 {
		// Check if solution is subsumed by an existing solution.
		subsumed := false
		for i, old := range *solutions {
			if old == nil {
				continue
			}
			cmp := CompareEnv(old, solution)
			switch cmp {
			case CompareEQ, CompareS12:
				subsumed = true
			case CompareS21:
				(*solutions)[i] = nil
			}
			if subsumed {
				break
			}
		}
		if !subsumed {
			*solutions = append(*solutions, solution)
		}
		return
	}

	if solution.IsNogood() {
		return
	}

	for _, choice := range choiceSets[0] {
		newSolution := UnionEnv(solution, choice)
		if newSolution != nil && !newSolution.IsNogood() {
			getDepthSolutions1(newSolution, choiceSets[1:], solutions)
		}
	}
}

// extendViaDefaults extends a solution with default assumptions.
func extendViaDefaults(solution *Env, remaining []*TmsNode, original []*TmsNode, solutions *[]*Env) {
	if len(remaining) == 0 {
		// Check if solution is already in solutions (by pointer).
		for _, s := range *solutions {
			if s == solution {
				return
			}
		}
		// Check if any default is neither assumed nor would cause nogood.
		for _, def := range original {
			inAssumptions := false
			for _, a := range solution.Assumptions {
				if a == def {
					inAssumptions = true
					break
				}
			}
			if inAssumptions {
				continue
			}
			ce := ConsEnv(def, solution)
			if ce.IsNogood() {
				continue
			}
			// A default can still be added, so this is not maximal.
			return
		}
		*solutions = append(*solutions, solution)
		return
	}

	newSolution := ConsEnv(remaining[0], solution)
	if !newSolution.IsNogood() {
		extendViaDefaults(newSolution, remaining[1:], original, solutions)
	}
	// Also try without this default (continue with remaining defaults).
	extendViaDefaults(solution, remaining[1:], original, solutions)
}

/// Generating explanations

// ExplainNode returns an explanation (list of justifications and assumptions)
// for why node is believed in env.
func ExplainNode(node *TmsNode, env *Env) []interface{} {
	return explainNode1(env, node, nil, nil)
}

// assumeExplanation represents an assumption in an explanation.
type assumeExplanation struct {
	Node *TmsNode
}

// explainNode1 is the recursive helper for ExplainNode.
func explainNode1(env *Env, node *TmsNode, queuedNodes []*TmsNode, explanation []interface{}) []interface{} {
	// Check if node is already queued.
	for _, q := range queuedNodes {
		if q == node {
			return nil
		}
	}

	// If node is an assumption in env, add it to the explanation.
	if node.IsAssumption {
		for _, a := range env.Assumptions {
			if a == node {
				return append([]interface{}{&assumeExplanation{Node: node}}, explanation...)
			}
		}
	}

	// Check if node is already explained.
	for _, entry := range explanation {
		switch e := entry.(type) {
		case *assumeExplanation:
			if e.Node == node {
				return explanation
			}
		case *Just:
			if e.Consequence == node {
				return explanation
			}
		}
	}

	queuedNodes = append([]*TmsNode{node}, queuedNodes...)

	for _, just := range node.Justs {
		// Check if all antecedents are in env.
		allIn := true
		for _, a := range just.Antecedents {
			if !InNode(a, env) {
				allIn = false
				break
			}
		}
		if !allIn {
			continue
		}

		newExplanation := explanation
		success := true
		for _, a := range just.Antecedents {
			newExplanation = explainNode1(env, a, queuedNodes, newExplanation)
			if newExplanation == nil {
				success = false
				break
			}
		}
		if success {
			return append([]interface{}{just}, newExplanation...)
		}
	}
	return nil
}

/// Printing

// WhyNode prints a description of a node and its label.
func WhyNode(node *TmsNode, w io.Writer, prefix string) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n<%s%v,{", prefix, node.Datum)
	for _, e := range node.Label {
		EnvString(e, w)
	}
	fmt.Fprintf(w, "}>")
}

// WhyNodes prints all nodes in the ATMS.
func WhyNodes(atms *ATMS, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	// Reverse order (original Lisp uses reverse of the nodes list).
	for i := len(atms.Nodes) - 1; i >= 0; i-- {
		WhyNode(atms.Nodes[i], w, "")
	}
}

// NodeJustifications prints the justifications for a node.
func NodeJustifications(node *TmsNode, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n For %s:", NodeString(node))
	for _, j := range node.Justs {
		PrintJustification(j, w)
	}
}

// PrintJustification prints a single justification.
func PrintJustification(j *Just, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	fmt.Fprintf(w, "\n  %v, ", j.Informant)
	for _, a := range j.Antecedents {
		WhyNode(a, w, "     ")
	}
}

// E finds an environment by index in the ATMS.
func E(atms *ATMS, n int) *Env {
	for _, bucket := range atms.EnvTable {
		for _, env := range bucket.Envs {
			if env.Index == n {
				return env
			}
		}
	}
	return nil
}

// PrintEnv prints an environment.
func PrintEnv(e *Env, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	nogoodMark := " "
	if e.IsNogood() {
		nogoodMark = "* "
	}
	fmt.Fprintf(w, "\n%s:%s", e, nogoodMark)
	EnvString(e, w)
}

// EnvString prints the assumptions in an environment as a formatted string.
func EnvString(e *Env, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	assumptions := e.Assumptions
	var strs []string
	var printer func(*TmsNode) string
	if len(assumptions) > 0 {
		printer = assumptions[0].ATMS.NodeString
	}
	for _, a := range assumptions {
		strs = append(strs, printer(a))
	}
	sort.Strings(strs)
	fmt.Fprintf(w, "{%s}", strings.Join(strs, ","))
}

/// Printing global data

// PrintNogoods prints the nogood table of the ATMS.
func PrintNogoods(atms *ATMS, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	PrintEnvTable(atms.NogoodTable, w)
}

// PrintEnvs prints the environment table of the ATMS.
func PrintEnvs(atms *ATMS, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	PrintEnvTable(atms.EnvTable, w)
}

// PrintEnvTable prints all environments in a table.
func PrintEnvTable(table []EnvTableEntry, w io.Writer) {
	if w == nil {
		w = os.Stdout
	}
	for _, bucket := range table {
		for _, env := range bucket.Envs {
			PrintEnv(env, w)
		}
	}
}

// PrintATMSStatistics prints statistics about the ATMS tables.
func PrintATMSStatistics(atms *ATMS) {
	PrintTable("\n For env table:", atms.EnvTable)
	PrintTable("\n For nogood table:", atms.NogoodTable)
}

// PrintTable prints a summary of a table.
func PrintTable(msg string, table []EnvTableEntry) {
	fmt.Print(msg)
	for _, entry := range table {
		fmt.Printf("\n   Length %d, %d", entry.Count, len(entry.Envs))
	}
}

/// Helper functions

// deleteNilEnvs removes nil entries from an Env slice.
func deleteNilEnvs(envs []*Env) []*Env {
	result := make([]*Env, 0, len(envs))
	for _, e := range envs {
		if e != nil {
			result = append(result, e)
		}
	}
	return result
}

// deleteNilEnvsFromSlice is the same as deleteNilEnvs (used where the
// original Lisp code calls (delete nil envs :test #'eq)).
func deleteNilEnvsFromSlice(envs []*Env) []*Env {
	return deleteNilEnvs(envs)
}

// deleteNodeOnce removes the first occurrence of node from a slice.
func deleteNodeOnce(nodes []*TmsNode, node *TmsNode) []*TmsNode {
	for i, n := range nodes {
		if n == node {
			return append(nodes[:i], nodes[i+1:]...)
		}
	}
	return nodes
}

// deleteJustOnce removes the first occurrence of just from a slice.
func deleteJustOnce(justs []*Just, just *Just) []*Just {
	for i, j := range justs {
		if j == just {
			return append(justs[:i], justs[i+1:]...)
		}
	}
	return justs
}

// deleteEnvOnce removes the first occurrence of env from a slice.
func deleteEnvOnce(envs []*Env, env *Env) []*Env {
	for i, e := range envs {
		if e == env {
			return append(envs[:i], envs[i+1:]...)
		}
	}
	return envs
}

// copyEnvSlice returns a shallow copy of an Env slice.
func copyEnvSlice(envs []*Env) []*Env {
	if envs == nil {
		return nil
	}
	result := make([]*Env, len(envs))
	copy(result, envs)
	return result
}

// assumptionsEqual checks if two assumption slices are equal (same nodes
// in same order).
func assumptionsEqual(a, b []*TmsNode) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// isSubset checks if all elements of sub appear in super.
// Assumes both slices are sorted by index.
func isSubset(sub, super []*TmsNode) bool {
	j := 0
	for _, s := range sub {
		for j < len(super) && super[j].Index < s.Index {
			j++
		}
		if j >= len(super) || super[j] != s {
			return false
		}
		j++
	}
	return true
}
