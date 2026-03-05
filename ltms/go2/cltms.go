// Package ltms implements the Complete LTMS (cltms).
// This file provides trie-based clause storage and resolution (IPIA algorithm)
// on top of the basic LTMS.
//
// Translated from cltms.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.
//
// Copyright (c) 1988-1992, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.

package ltms

import "fmt"

// TrieEntry represents a node in the trie used for clause storage.
// The trie maps literal sequences to clauses. Each entry associates
// a Literal key with a sub-trie (either []*TrieEntry or *Clause at leaves).
type TrieEntry struct {
	Literal *Literal
	SubTrie interface{} // []*TrieEntry or *Clause
}

// QueueSlot represents a bucket in the IPIA priority queue.
// Clauses are grouped by length for processing shortest-first.
type QueueSlot struct {
	Length  int
	Clauses []*Clause
}

// ConsPair is used during consensus computation to hold intermediate results.
type ConsPair struct {
	Lit  *Literal
	Next *ConsPair
}

// --- Trie operations ---

// WalkTrie applies fn to every clause stored in the trie.
func WalkTrie(fn func(*Clause), trie interface{}) {
	if trie == nil {
		return
	}
	switch t := trie.(type) {
	case *Clause:
		fn(t)
	case []*TrieEntry:
		for _, entry := range t {
			WalkTrie(fn, entry.SubTrie)
		}
	}
}

// Collect returns all clauses stored in the LTMS's trie.
func Collect(ltms *LTMS) []*Clause {
	var result []*Clause
	WalkTrie(func(cl *Clause) {
		result = append(result, cl)
	}, ltms.Complete)
	return result
}

// BuildTrie builds a trie from a sorted list of literals terminating at cl.
func BuildTrie(lits []*Literal, cl *Clause) []*TrieEntry {
	if len(lits) == 0 {
		return nil
	}
	entry := &TrieEntry{Literal: lits[0]}
	if len(lits) == 1 {
		entry.SubTrie = cl
	} else {
		entry.SubTrie = BuildTrie(lits[1:], cl)
	}
	return []*TrieEntry{entry}
}

// Subsumed checks whether lits is subsumed by any clause in the trie.
// Returns the subsuming *Clause or nil.
func Subsumed(lits []*Literal, trie interface{}) *Clause {
	entries, ok := trie.([]*TrieEntry)
	if !ok || entries == nil {
		return nil
	}
	for _, entry := range entries {
		if len(lits) == 0 {
			return nil
		}
		slot := findLiteralInSorted(entry.Literal, lits)
		if slot >= 0 {
			switch sub := entry.SubTrie.(type) {
			case *Clause:
				return sub
			case []*TrieEntry:
				if result := Subsumed(lits[slot+1:], sub); result != nil {
					return result
				}
			}
		}
	}
	return nil
}

// findLiteralInSorted finds the index of lit in a sorted literal slice.
// Returns -1 if not found.
func findLiteralInSorted(lit *Literal, lits []*Literal) int {
	for i, l := range lits {
		if l == lit {
			return i
		}
	}
	return -1
}

// AddToTrie adds a clause (or literal list) to the LTMS's trie.
func AddToTrie(cl interface{}, ltms *LTMS) {
	var lits []*Literal
	var clause *Clause
	switch c := cl.(type) {
	case *Clause:
		lits = c.Literals
		clause = c
	default:
		return
	}

	if ltms.Complete == nil {
		ltms.Complete = BuildTrie(lits, clause)
		return
	}

	trie, ok := ltms.Complete.([]*TrieEntry)
	if !ok {
		return
	}

	addToTrieHelper(lits, clause, &trie)
	ltms.Complete = trie
}

// addToTrieHelper inserts lits/clause into the given trie (passed by pointer
// so the caller's slice can be modified).
func addToTrieHelper(lits []*Literal, clause *Clause, trie *[]*TrieEntry) {
	for litIdx := 0; litIdx < len(lits); litIdx++ {
		lit := lits[litIdx]
		index := lit.Node.Index

		// Search for a matching entry in the current trie level.
		var slot *TrieEntry
		insertPos := -1
		for i, entry := range *trie {
			if entry.Literal == lit {
				slot = entry
				break
			}
			if entry.Literal.Node.Index > index {
				insertPos = i
				break
			}
		}

		if slot == nil {
			// No matching entry found; build the rest of the trie.
			newEntries := BuildTrie(lits[litIdx:], clause)
			if insertPos >= 0 {
				// Insert before the entry with a larger index.
				result := make([]*TrieEntry, 0, len(*trie)+len(newEntries))
				result = append(result, (*trie)[:insertPos]...)
				result = append(result, newEntries...)
				result = append(result, (*trie)[insertPos:]...)
				*trie = result
			} else {
				// Append at the end.
				*trie = append(*trie, newEntries...)
			}
			return
		}

		// Found a matching entry; descend into the sub-trie.
		if litIdx == len(lits)-1 {
			// This is the last literal; store the clause.
			slot.SubTrie = clause
			return
		}

		switch sub := slot.SubTrie.(type) {
		case []*TrieEntry:
			subCopy := make([]*TrieEntry, len(sub))
			copy(subCopy, sub)
			addToTrieHelper(lits[litIdx+1:], clause, &subCopy)
			slot.SubTrie = subCopy
			return
		case *Clause:
			// Leaf: this shouldn't normally happen during insertion of
			// a non-subsumed clause, but handle gracefully.
			slot.SubTrie = clause
			return
		default:
			newSub := BuildTrie(lits[litIdx+1:], clause)
			slot.SubTrie = newSub
			return
		}
	}
}

// RemoveSubsumed removes from the trie all clauses that are subsumed by lits.
// fn is called on each removed clause.
func RemoveSubsumed(fn func(*Clause), lits []*Literal, ltms *LTMS) {
	trie, ok := ltms.Complete.([]*TrieEntry)
	if !ok {
		return
	}
	if removeSubsumed1(fn, lits, &trie) {
		ltms.Complete = nil
	} else {
		ltms.Complete = trie
	}
}

// removeSubsumed1 recursively removes subsumed clauses from the trie.
// Returns true if the entire sub-trie was removed.
func removeSubsumed1(fn func(*Clause), lits []*Literal, trie *[]*TrieEntry) bool {
	if len(lits) == 0 {
		// All literals matched; everything below is subsumed.
		walkTrieEntries(fn, *trie)
		return true
	}
	if trie == nil || len(*trie) == 0 {
		return false
	}

	au := lits[0].Node.Index
	i := 0
	for i < len(*trie) {
		entry := (*trie)[i]
		entryIdx := entry.Literal.Node.Index
		removed := false

		if entryIdx >= au {
			if entry.Literal == lits[0] {
				// Same literal: recurse with remaining lits.
				removed = removeFromEntry(fn, lits[1:], entry, trie, i)
			} else if entryIdx > au {
				// Past the point where our literal could appear.
				break
			}
		} else {
			// Entry index < au: check if lits is subsumed in sub-trie.
			removed = removeFromEntry(fn, lits, entry, trie, i)
		}

		if removed {
			if len(*trie) == 0 {
				return true
			}
			// Don't increment i; the slice shifted.
		} else {
			i++
		}
	}
	return false
}

// removeFromEntry handles removal from a single trie entry.
// Returns true if the entry was removed from the trie.
func removeFromEntry(fn func(*Clause), lits []*Literal, entry *TrieEntry, trie *[]*TrieEntry, i int) bool {
	switch sub := entry.SubTrie.(type) {
	case *Clause:
		if len(lits) == 0 {
			fn(sub)
			*trie = append((*trie)[:i], (*trie)[i+1:]...)
			return true
		}
		return false
	case []*TrieEntry:
		subCopy := make([]*TrieEntry, len(sub))
		copy(subCopy, sub)
		if removeSubsumed1(fn, lits, &subCopy) {
			// Entire sub-trie removed.
			*trie = append((*trie)[:i], (*trie)[i+1:]...)
			return true
		}
		entry.SubTrie = subCopy
		return false
	}
	return false
}

// walkTrieEntries applies fn to all clauses in a trie entry slice.
func walkTrieEntries(fn func(*Clause), entries []*TrieEntry) {
	for _, entry := range entries {
		WalkTrie(fn, entry.SubTrie)
	}
}

// --- Queue operations for IPIA ---

// InsertQueue adds a clause to the LTMS's priority queue, grouped by length.
func InsertQueue(cl *Clause, ltms *LTMS) {
	cl.Status = StatusQueued
	queue := ltmsQueue(ltms)
	insertList2(cl, &queue)
	setLtmsQueue(ltms, queue)
}

// ltmsQueue extracts the queue from ltms.Queue as []*QueueSlot.
func ltmsQueue(ltms *LTMS) []*QueueSlot {
	if ltms.Queue == nil {
		return nil
	}
	if q, ok := ltms.Queue.([]*QueueSlot); ok {
		return q
	}
	return nil
}

// setLtmsQueue stores the queue back into ltms.Queue.
func setLtmsQueue(ltms *LTMS, queue []*QueueSlot) {
	if len(queue) == 0 {
		ltms.Queue = nil
	} else {
		ltms.Queue = queue
	}
}

// insertList2 inserts clause cl into a sorted queue of (length, clauses) slots.
func insertList2(cl *Clause, queue *[]*QueueSlot) {
	clCount := cl.Length
	for i, slot := range *queue {
		if clCount == slot.Length {
			slot.Clauses = append(slot.Clauses, cl)
			return
		}
		if clCount < slot.Length {
			// Insert before this slot.
			newSlot := &QueueSlot{Length: clCount, Clauses: []*Clause{cl}}
			result := make([]*QueueSlot, 0, len(*queue)+1)
			result = append(result, (*queue)[:i]...)
			result = append(result, newSlot)
			result = append(result, (*queue)[i:]...)
			*queue = result
			return
		}
	}
	// Append at the end.
	*queue = append(*queue, &QueueSlot{Length: clCount, Clauses: []*Clause{cl}})
}

// insertList inserts a clause into a queue and returns the updated queue.
func insertList(cl *Clause, queue []*QueueSlot) []*QueueSlot {
	insertList2(cl, &queue)
	return queue
}

// --- Clause insertion for complete LTMS (sorted by clause length) ---

// InsertTrueClauseComplete inserts a clause into a node's true-clauses list,
// sorted by clause length (shorter clauses first).
func InsertTrueClauseComplete(cl *Clause, node *TmsNode) {
	node.TrueClauses = insertClauseSorted(cl, node.TrueClauses)
}

// InsertFalseClauseComplete inserts a clause into a node's false-clauses list,
// sorted by clause length (shorter clauses first).
func InsertFalseClauseComplete(cl *Clause, node *TmsNode) {
	node.FalseClauses = insertClauseSorted(cl, node.FalseClauses)
}

// insertClauseSorted inserts cl into list sorted by clause length (ascending).
func insertClauseSorted(cl *Clause, list []*Clause) []*Clause {
	clCount := cl.Length
	for i, c := range list {
		if clCount >= c.Length {
			continue
		}
		// Insert before position i.
		result := make([]*Clause, 0, len(list)+1)
		result = append(result, list[:i]...)
		result = append(result, cl)
		result = append(result, list[i:]...)
		return result
	}
	return append(list, cl)
}

// --- Index clause ---

// IndexClause indexes a clause by adding it to each of its literals'
// node clause lists, then performs BCP on it.
func IndexClause(cl *Clause, ltms *LTMS) {
	for _, term := range cl.Literals {
		switch term.Sign {
		case LabelTrue:
			InsertTrueClauseComplete(cl, term.Node)
		case LabelFalse:
			InsertFalseClauseComplete(cl, term.Node)
		}
	}
	CheckClauses(ltms, []*Clause{cl})
}

// --- Literal connections ---

// LiteralConnections returns the clauses connected to the complement of
// a literal. For a TRUE literal on node N, it returns N's false-clauses
// (which contain ~N and thus could resolve with the literal).
func LiteralConnections(literal *Literal) []*Clause {
	if literal.Sign == LabelTrue {
		return literal.Node.FalseClauses
	}
	return literal.Node.TrueClauses
}

// --- Propagation ---

// PropagateMoreUnknownness handles additional unknownness propagation
// for the complete LTMS. When a node becomes unknown, previously
// satisfied clauses may need to be re-queued.
func PropagateMoreUnknownness(oldValue NodeLabel, node *TmsNode, ltms *LTMS) {
	var clauses []*Clause
	switch oldValue {
	case LabelTrue:
		clauses = node.TrueClauses
	case LabelFalse:
		clauses = node.FalseClauses
	}
	for _, clause := range clauses {
		clause.Sats--
		if clause.Sats == 0 && clause.Status == StatusDirty {
			InsertQueue(clause, ltms)
		}
	}
}

// --- Consensus algorithm ---

// SimplifyConsensus computes the resolvent of cl1 and cl2 on the literal term1.
// Returns the merged literal list, or nil if no valid consensus exists.
// This implements the consensus operation with subsumption-aware checks.
func SimplifyConsensus(cl1, cl2 *Clause, term1 *Literal) []*Literal {
	// Don't resolve two clauses with the same non-nil informant.
	if cl1.Informant != nil && cl1.Informant == cl2.Informant {
		return nil
	}

	var result []*Literal
	i, j := 0, 0
	terms1 := cl1.Literals
	terms2 := cl2.Literals

	for {
		if i >= len(terms1) {
			// Append remaining terms2.
			result = append(result, terms2[j:]...)
			reverseInPlace(result)
			return result
		}
		if j >= len(terms2) {
			// Append remaining terms1.
			result = append(result, terms1[i:]...)
			reverseInPlace(result)
			return result
		}

		t1 := terms1[i]
		t2 := terms2[j]

		if t1 == t2 {
			// Same literal in both clauses.
			result = append(result, t1)
			i++
			j++
		} else if t1.Node.Index == t2.Node.Index {
			// Same node, different sign: complementary pair.
			if t1 != term1 {
				// A second complementary pair means no consensus.
				return nil
			}
			i++
			j++
		} else if t1.Node.Index < t2.Node.Index {
			result = append(result, t1)
			i++
		} else {
			result = append(result, t2)
			j++
		}
	}
}

// reverseInPlace reverses a slice of literals in place.
func reverseInPlace(lits []*Literal) {
	for i, j := 0, len(lits)-1; i < j; i, j = i+1, j-1 {
		lits[i], lits[j] = lits[j], lits[i]
	}
}

// SimplifySubsumeConsensus computes the consensus of cl1 and cl2 on literal p.
// If the result is not subsumed, it is processed as a new clause.
// Returns the new clause, or nil if no valid consensus was found.
func SimplifySubsumeConsensus(ltms *LTMS, cl1, cl2 *Clause, p *Literal) *Clause {
	if cl1.Informant != nil && cl1.Informant == cl2.Informant {
		return nil
	}
	literals := SimplifyConsensus(cl1, cl2, p)
	if literals == nil {
		return nil
	}
	if Subsumed(literals, ltms.Complete) != nil {
		return nil
	}
	informant := []interface{}{"RESOLVE", cl1, cl2, p}
	return ProcessClause(ltms, copyLiterals(literals), informant, true)
}

// copyLiterals returns a copy of a literal slice.
func copyLiterals(lits []*Literal) []*Literal {
	result := make([]*Literal, len(lits))
	copy(result, lits)
	return result
}

// --- Full add clause ---

// FullAddClause is the complete LTMS's entry point for adding a clause.
// It installs the clause and, if the LTMS is not in delay mode, runs IPIA.
func FullAddClause(ltms *LTMS, literals []*Literal, informant interface{}) {
	if InstallClause(ltms, literals, informant) != nil {
		complete := ltms.Complete
		if _, ok := complete.(string); ok && complete == "DELAY" {
			return
		}
		CheckForContradictions(ltms)
		IPIA(ltms)
	}
}

// --- Delay-sat check ---

// delaySat checks if a clause is satisfied and should be delayed.
// Returns true if the clause was marked dirty (delayed).
func delaySat(clause *Clause, ltms *LTMS) bool {
	if ltms.DelaySat && SatisfiedClause(clause) {
		clause.Status = StatusDirty
		return true
	}
	return false
}

// --- IPIA: Iterative Prime Implicate Algorithm ---

// CompleteLTMS runs the IPIA algorithm on the LTMS to compute all prime
// implicates, then checks for contradictions.
func CompleteLTMS(ltms *LTMS) {
	old := ltms.Complete
	defer func() {
		ltms.Complete = old
		CheckForContradictions(ltms)
	}()
	ltms.Complete = true
	IPIA(ltms)
}

// IPIA implements the Iterative Prime Implicate Algorithm (Tison's method).
// It processes the clause queue, resolving clauses and adding prime implicates.
func IPIA(ltms *LTMS) {
	for {
		queue := ltmsQueue(ltms)
		if len(queue) == 0 {
			break
		}
		slot := queue[0]
		if len(slot.Clauses) == 0 {
			// Remove empty slot.
			setLtmsQueue(ltms, queue[1:])
			continue
		}

		// Pop the first clause from the first slot.
		c := slot.Clauses[0]
		if len(slot.Clauses) > 1 {
			slot.Clauses = slot.Clauses[1:]
		} else {
			// Last clause in this slot; remove the slot.
			setLtmsQueue(ltms, queue[1:])
		}

		if c.Status != StatusQueued {
			continue
		}
		if delaySat(c, ltms) {
			continue
		}

		// Build sigma (sorted queue of clauses by length) and pxs (connection lists).
		var sigma []*QueueSlot
		sigma = insertList(c, sigma)

		var pxs [][]*Clause
		for _, lit := range c.Literals {
			var px []*Clause
			for _, p := range LiteralConnections(lit) {
				if p.Status == StatusQueued || p.Status == StatusSubsumed {
					continue
				}
				if p.Status == StatusDirty && delaySat(p, ltms) {
					continue
				}
				if SimplifyConsensus(c, p, lit) == nil {
					continue
				}
				if delaySat(p, ltms) {
					continue
				}
				px = append(px, p)
			}
			pxs = append(pxs, px)
		}

		// Resolve sigma with pxs.
		if len(pxs) > 0 {
			pxIdx := 0
			for _, lit := range c.Literals {
				px := pxs[pxIdx]
				pxIdx++
				if len(px) == 0 {
					continue
				}
				// Iterate over all clauses in sigma.
				for _, l := range sigma {
					for _, s := range l.Clauses {
						if s.Status == StatusSubsumed {
							continue
						}
						if !literalInClause(lit, s) {
							continue
						}
						if delaySat(s, ltms) {
							continue
						}
						sigma = IPIAInner(ltms, sigma, px, s, lit)
					}
				}
			}
		}

		if c.Status == StatusQueued {
			c.Status = StatusNone
		}

		// Index all NOT-INDEXED clauses in sigma.
		for _, l := range sigma {
			for _, s := range l.Clauses {
				if s.Status == StatusNotIndexed {
					IndexClause(s, ltms)
					s.Status = StatusNone
				}
			}
		}

		CheckForContradictions(ltms)
	}
}

// literalInClause checks whether a specific literal pointer appears in a clause.
func literalInClause(lit *Literal, clause *Clause) bool {
	for _, l := range clause.Literals {
		if l == lit {
			return true
		}
	}
	return false
}

// IPIAInner is the inner loop of the IPIA algorithm. For each clause p in px,
// it tries to compute the consensus with s on lit, and if successful, adds the
// result to sigma.
func IPIAInner(ltms *LTMS, sigma []*QueueSlot, px []*Clause, s *Clause, lit *Literal) []*QueueSlot {
	var sChildren []*Clause

	for _, p := range px {
		if delaySat(p, ltms) {
			continue
		}
		if p.Status == StatusSubsumed {
			continue
		}
		consensus := SimplifySubsumeConsensus(ltms, s, p, lit)
		if consensus == nil {
			continue
		}
		consensus.Status = StatusNotIndexed
		if s.Status == StatusSubsumed {
			sigma = insertList(consensus, sigma)
			sChildren = nil
			break
		}
		sChildren = append(sChildren, consensus)
	}

	for _, child := range sChildren {
		if child.Status != StatusSubsumed {
			sigma = insertList(child, sigma)
		}
	}

	return sigma
}

// --- Clause processing ---

// RemoveClause marks oldClause as subsumed and removes it from node indices.
// If oldClause was supporting a node that is not in newClause, it propagates
// unknownness and finds alternative support.
func RemoveClause(oldClause, newClause *Clause) {
	if oldClause.Status != StatusNotIndexed {
		for _, term := range oldClause.Literals {
			switch term.Sign {
			case LabelTrue:
				term.Node.TrueClauses = removeClauseFromList(oldClause, term.Node.TrueClauses)
			case LabelFalse:
				term.Node.FalseClauses = removeClauseFromList(oldClause, term.Node.FalseClauses)
			}
		}
	}
	oldClause.Status = StatusSubsumed

	node := ClauseConsequent(oldClause)
	if node == nil {
		return
	}
	// Check if node appears in newClause's literals.
	if nodeInClauseLiterals(node, newClause) {
		node.Support = newClause
	} else {
		// A contradiction is being introduced.
		FindAlternativeSupport(node.LTMS, PropagateUnknownness(node))
	}
}

// removeClauseFromList removes cl from a clause list.
func removeClauseFromList(cl *Clause, list []*Clause) []*Clause {
	for i, c := range list {
		if c == cl {
			return append(list[:i], list[i+1:]...)
		}
	}
	return list
}

// nodeInClauseLiterals checks if a node appears in a clause's literals
// (equivalent to Lisp's (assoc node (clause-literals new-clause))).
func nodeInClauseLiterals(node *TmsNode, clause *Clause) bool {
	for _, lit := range clause.Literals {
		if lit.Node == node {
			return true
		}
	}
	return false
}

// InstallClause installs a clause if it is not already subsumed.
func InstallClause(ltms *LTMS, literals []*Literal, informant interface{}) *Clause {
	if Subsumed(literals, ltms.Complete) != nil {
		return nil
	}
	return ProcessClause(ltms, literals, informant, false)
}

// ProcessClause creates a clause via BCP, removes subsumed clauses,
// adds it to the trie, indexes it, and optionally queues it for IPIA.
func ProcessClause(ltms *LTMS, literals []*Literal, informant interface{}, internal bool) *Clause {
	cl := BcpAddClause(ltms, literals, informant, false)

	RemoveSubsumed(func(oldClause *Clause) {
		RemoveClause(oldClause, cl)
	}, literals, ltms)

	AddToTrie(cl, ltms)

	if internal {
		// Internal clause: don't index or queue yet.
		return cl
	}

	if delaySat(cl, ltms) {
		IndexClause(cl, ltms)
	} else {
		IndexClause(cl, ltms)
		InsertQueue(cl, ltms)
	}
	return cl
}

// --- Disjoin clauses ---

// DisjoinClauses merges two sorted literal lists. Returns nil if complementary
// literals (same node, different sign) are found (the :FAIL case).
// The second return value is false when the result is :FAIL.
func DisjoinClauses(terms1, terms2 []*Literal) ([]*Literal, bool) {
	var result []*Literal
	i, j := 0, 0

	for {
		if i >= len(terms1) {
			result = append(result, terms2[j:]...)
			reverseInPlace(result)
			return result, true
		}
		if j >= len(terms2) {
			result = append(result, terms1[i:]...)
			reverseInPlace(result)
			return result, true
		}

		t1 := terms1[i]
		t2 := terms2[j]

		if t1 == t2 {
			result = append(result, t1)
			i++
			j++
		} else if t1.Node.Index == t2.Node.Index {
			// Same node but different sign: tautology -> FAIL.
			return nil, false
		} else if t1.Node.Index < t2.Node.Index {
			result = append(result, t1)
			i++
		} else {
			result = append(result, t2)
			j++
		}
	}
}

// --- Complete LTMS normalize ---

// NormalizeDisjunctionComplete normalizes a disjunction using trie-based
// subsumption. This is the complete LTMS version of normalizeDisjunction.
func NormalizeDisjunctionComplete(ltms *LTMS, exp []interface{}, negate bool) [][]*Literal {
	if len(exp) < 2 {
		return [][]*Literal{nil}
	}

	result := normalize1(ltms, exp[1], negate)
	tempLtms := CreateLTMS("Normalize disjunction")
	initCompleteTrie(tempLtms)

	for _, c := range result {
		cl := &Clause{Literals: c, Length: len(c)}
		AddToTrie(cl, tempLtms)
	}

	for _, subExp := range exp[2:] {
		nltms := CreateLTMS("Normalize disjunction")
		initCompleteTrie(nltms)

		for _, disj1 := range normalize1(ltms, subExp, negate) {
			WalkTrie(func(disj2 *Clause) {
				merged, ok := DisjoinClauses(disj1, disj2.Literals)
				if !ok {
					return
				}
				if Subsumed(merged, nltms.Complete) != nil {
					return
				}
				RemoveSubsumed(func(_ *Clause) {}, merged, nltms)
				cl := &Clause{Literals: merged, Length: len(merged)}
				AddToTrie(cl, nltms)
			}, tempLtms.Complete)
		}
		tempLtms = nltms
	}

	var clauses [][]*Literal
	for _, cl := range Collect(tempLtms) {
		clauses = append(clauses, cl.Literals)
	}
	return clauses
}

// NormalizeConjunctionComplete normalizes a conjunction using trie-based
// subsumption. This is the complete LTMS version of normalizeConjunction.
func NormalizeConjunctionComplete(ltms *LTMS, exp []interface{}, negate bool) [][]*Literal {
	tempLtms := CreateLTMS("Normalize conjunction")
	initCompleteTrie(tempLtms)

	for _, subExp := range exp[1:] {
		for _, disjunct := range normalize1(ltms, subExp, negate) {
			if Subsumed(disjunct, tempLtms.Complete) != nil {
				continue
			}
			RemoveSubsumed(func(_ *Clause) {}, disjunct, tempLtms)
			cl := &Clause{Literals: disjunct, Length: len(disjunct)}
			AddToTrie(cl, tempLtms)
		}
	}

	var clauses [][]*Literal
	for _, cl := range Collect(tempLtms) {
		clauses = append(clauses, cl.Literals)
	}
	return clauses
}

// initCompleteTrie initializes an LTMS for trie-based clause storage.
func initCompleteTrie(ltms *LTMS) {
	// Set Complete to a non-nil value so trie functions are used.
	// We use an empty trie entry slice initially (will be set by AddToTrie).
	ltms.Complete = nil // AddToTrie handles nil -> builds initial trie
}

// --- AddFormula for complete LTMS ---

// AddFormulaComplete adds a formula to a complete LTMS. It normalizes the
// formula using a temporary LTMS, computes prime implicates via IPIA, and
// installs the results in the main LTMS.
func AddFormulaComplete(ltms *LTMS, formula, informant interface{}) {
	// Create a temporary complete LTMS for normalization.
	tltms := CreateLTMS("Temporary for add-formula",
		OptionComplete("DELAY"),
		OptionDelaySat(false),
	)
	RegisterCompleteLTMSFunctions(tltms)

	clauses := Normalize(ltms, formula)
	if informant == nil {
		informant = []interface{}{":IMPLIED-BY", formula}
	}

	// Clear marks on all nodes.
	if ltms.Nodes != nil {
		for _, node := range ltms.Nodes {
			node.Mark = 0
		}
	}

	// Collect literals used in clauses and count occurrences.
	var literals []*TmsNode
	for _, clause := range clauses {
		for _, literal := range clause {
			markVal, _ := literal.Node.Mark.(int)
			if markVal == 0 {
				literals = append(literals, literal.Node)
			}
			literal.Node.Mark = markVal + 1
		}
	}

	// Sort literals by occurrence count (ascending) and create temp nodes.
	sortNodesByMark(literals)
	for _, literal := range literals {
		literal.Mark = TmsCreateNode(tltms, literal, false)
	}

	// Map clauses to the temp LTMS and add them.
	for _, clause := range clauses {
		mappedLits := mapOverLiterals(clause, func(node *TmsNode) *TmsNode {
			return node.Mark.(*TmsNode)
		})
		sorted := SortClause(mappedLits)
		AddClauseInternalForCLTMS(tltms, sorted, nil, true)
	}

	// Run IPIA on the temp LTMS.
	CompleteLTMS(tltms)

	// Walk the resulting trie and install clauses in the main LTMS.
	WalkTrie(func(clause *Clause) {
		mappedLits := mapOverLiterals(clause.Literals, func(node *TmsNode) *TmsNode {
			return node.Datum.(*TmsNode)
		})
		sorted := SortClause(mappedLits)
		AddClauseInternalForCLTMS(ltms, sorted, informant, true)
	}, tltms.Complete)

	CheckForContradictions(ltms)
	if completeBool, ok := ltms.Complete.(bool); ok && completeBool {
		IPIA(ltms)
	}
}

// AddClauseInternalForCLTMS is a variant of AddClauseInternal used by the
// complete LTMS to add clauses with internal=true (no contradiction check).
func AddClauseInternalForCLTMS(ltms *LTMS, literals []*Literal, informant interface{}, internal bool) {
	if len(literals) == 0 {
		LtmsError("Total contradiction: Null clause", informant)
		return
	}
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

// mapOverLiterals maps clause literals through a node transformation function,
// preserving the sign of each literal.
func mapOverLiterals(lits []*Literal, mapf func(*TmsNode) *TmsNode) []*Literal {
	result := make([]*Literal, len(lits))
	for i, lit := range lits {
		mappedNode := mapf(lit.Node)
		if lit.Sign == LabelTrue {
			result[i] = mappedNode.TrueLiteral
		} else {
			result[i] = mappedNode.FalseLiteral
		}
	}
	return result
}

// sortNodesByMark sorts nodes by their Mark value (expected to be int), ascending.
func sortNodesByMark(nodes []*TmsNode) {
	for i := 1; i < len(nodes); i++ {
		key := nodes[i]
		keyMark, _ := key.Mark.(int)
		j := i - 1
		for j >= 0 {
			jMark, _ := nodes[j].Mark.(int)
			if jMark <= keyMark {
				break
			}
			nodes[j+1] = nodes[j]
			j--
		}
		nodes[j+1] = key
	}
}

// --- TmsEnv ---

// TmsEnv computes the environments (sets of assumptions) that support
// a node having the given sign.
func TmsEnv(node *TmsNode, sign NodeLabel) [][]*Literal {
	var label [][]*Literal

	if node.IsAssumption {
		var lit *Literal
		if sign == LabelTrue {
			lit = node.TrueLiteral
		} else {
			lit = node.FalseLiteral
		}
		label = append(label, []*Literal{lit})
	}

	var clauses []*Clause
	if sign == LabelTrue {
		clauses = node.TrueClauses
	} else {
		clauses = node.FalseClauses
	}

	for _, p := range clauses {
		hasNonAssumption := false
		for _, lit := range p.Literals {
			if lit.Node != node && !lit.Node.IsAssumption {
				hasNonAssumption = true
				break
			}
		}
		if hasNonAssumption {
			continue
		}

		var env []*Literal
		for _, lit := range p.Literals {
			if lit.Node != node {
				if lit.Sign == LabelTrue {
					env = append(env, lit.Node.FalseLiteral)
				} else {
					env = append(env, lit.Node.TrueLiteral)
				}
			}
		}
		label = append(label, env)
	}

	return label
}

// --- PI (Prime Implicates) ---

// PI computes the prime implicates of a formula. Returns the complete LTMS
// containing the prime implicates.
func PI(formula interface{}) *LTMS {
	ltms := CreateLTMS("Prime Implicates")
	tltms := CreateLTMS("Prime Implicates",
		OptionComplete("DELAY"),
		OptionDelaySat(false),
	)
	RegisterCompleteLTMSFunctions(tltms)

	clauses := Normalize(ltms, formula)

	// Collect all nodes and count occurrences.
	var literals []*TmsNode
	if ltms.Nodes != nil {
		for _, node := range ltms.Nodes {
			node.Mark = 0
			literals = append(literals, node)
		}
	}

	for _, clause := range clauses {
		for _, literal := range clause {
			markVal, _ := literal.Node.Mark.(int)
			literal.Node.Mark = markVal + 1
		}
	}

	sortNodesByMark(literals)
	for _, literal := range literals {
		literal.Mark = TmsCreateNode(tltms, literal.Datum, false)
	}

	for _, clause := range clauses {
		mappedLits := mapOverLiterals(clause, func(node *TmsNode) *TmsNode {
			return node.Mark.(*TmsNode)
		})
		sorted := SortClause(mappedLits)
		AddClauseInternalForCLTMS(tltms, sorted, nil, true)
	}

	CompleteLTMS(tltms)
	fmt.Printf("\n There now are %d clauses", len(Collect(tltms)))
	return tltms
}

// --- Registration ---

// RegisterCompleteLTMSFunctions registers the complete LTMS function
// implementations on an LTMS instance. This must be called to enable
// the complete LTMS behavior (trie storage, IPIA, etc.).
func RegisterCompleteLTMSFunctions(ltms *LTMS) {
	ltms.FullAddClauseFn = FullAddClause
	ltms.PropagateMoreUnknownnessFn = PropagateMoreUnknownness
	ltms.IpiaFn = IPIA
	ltms.WalkTrieFn = WalkTrie
	// Initialize the queue field to the right type.
	ltms.Queue = []*QueueSlot{}
}
