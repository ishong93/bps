// Complete LTMS with trie-based clause storage and resolution.
// Converted from cltms.lisp.
package ltms

import (
	"fmt"
	"sort"
)

// TrieEntry represents an entry in the clause trie.
type TrieEntry struct {
	Literal *Literal
	SubTrie interface{} // []*TrieEntry or *Clause
}

// --- Initialization ---

// InitCompleteLTMS hooks the complete LTMS functions into an LTMS.
func InitCompleteLTMS(l *LTMS) {
	l.IpiaFn = IPIA
	l.PropagateMoreUnknownnessFn = PropagateMoreUnknownness
	l.FullAddClauseFn = FullAddClause
	l.WalkTrieFn = WalkTrie
}

// --- Trie operations ---

// WalkTrie walks a trie, calling fn on each clause leaf.
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

// Collect returns all clauses in the trie.
func Collect(l *LTMS) []*Clause {
	var result []*Clause
	WalkTrie(func(cl *Clause) {
		result = append(result, cl)
	}, l.Clauses)
	return result
}

// Subsumed checks if lits is subsumed by any clause in trie.
func Subsumed(lits []*Literal, trie interface{}) *Clause {
	if trie == nil || len(lits) == 0 {
		return nil
	}
	entries, ok := trie.([]*TrieEntry)
	if !ok {
		return nil
	}
	for _, entry := range entries {
		idx := -1
		for i, lit := range lits {
			if lit == entry.Literal {
				idx = i
				break
			}
		}
		if idx < 0 {
			continue
		}
		switch sub := entry.SubTrie.(type) {
		case *Clause:
			return sub
		case []*TrieEntry:
			if result := Subsumed(lits[idx+1:], sub); result != nil {
				return result
			}
		}
	}
	return nil
}

// BuildTrie builds a trie from literals leading to clause.
func BuildTrie(lits []*Literal, cl *Clause) []*TrieEntry {
	if len(lits) == 0 {
		return nil
	}
	var subTrie interface{}
	if len(lits) == 1 {
		subTrie = cl
	} else {
		subTrie = BuildTrie(lits[1:], cl)
	}
	return []*TrieEntry{{Literal: lits[0], SubTrie: subTrie}}
}

// AddToTrie adds a clause to the trie.
func AddToTrie(cl interface{}, l *LTMS) {
	var lits []*Literal
	var clause *Clause
	switch c := cl.(type) {
	case *Clause:
		lits = c.Literals
		clause = c
	default:
		return
	}
	if l.Clauses == nil {
		l.Clauses = BuildTrie(lits, clause)
		return
	}
	entries, ok := l.Clauses.([]*TrieEntry)
	if !ok {
		l.Clauses = BuildTrie(lits, clause)
		return
	}
	l.Clauses = insertIntoTrie(entries, lits, clause)
}

func insertIntoTrie(trie []*TrieEntry, lits []*Literal, cl *Clause) []*TrieEntry {
	if len(lits) == 0 {
		return trie
	}
	lit := lits[0]
	for i, entry := range trie {
		if entry.Literal == lit {
			if len(lits) == 1 {
				trie[i].SubTrie = cl
			} else if sub, ok := entry.SubTrie.([]*TrieEntry); ok {
				trie[i].SubTrie = insertIntoTrie(sub, lits[1:], cl)
			} else {
				trie[i].SubTrie = BuildTrie(lits[1:], cl)
			}
			return trie
		}
		if entry.Literal.Node.Index > lit.Node.Index {
			newEntry := &TrieEntry{Literal: lit}
			if len(lits) == 1 {
				newEntry.SubTrie = cl
			} else {
				newEntry.SubTrie = BuildTrie(lits[1:], cl)
			}
			result := make([]*TrieEntry, 0, len(trie)+1)
			result = append(result, trie[:i]...)
			result = append(result, newEntry)
			result = append(result, trie[i:]...)
			return result
		}
	}
	newEntry := &TrieEntry{Literal: lit}
	if len(lits) == 1 {
		newEntry.SubTrie = cl
	} else {
		newEntry.SubTrie = BuildTrie(lits[1:], cl)
	}
	return append(trie, newEntry)
}

// RemoveSubsumed removes clauses subsumed by lits.
func RemoveSubsumed(fn func(*Clause), lits []*Literal, l *LTMS) {
	if removeSubsumed1(fn, lits, l.Clauses) {
		l.Clauses = nil
	}
}

func removeSubsumed1(fn func(*Clause), lits []*Literal, trie interface{}) bool {
	if len(lits) == 0 {
		WalkTrie(fn, trie)
		return true
	}
	entries, ok := trie.([]*TrieEntry)
	if !ok {
		return false
	}
	litIndex := lits[0].Node.Index
	i := 0
	for i < len(entries) {
		entry := entries[i]
		entryIndex := entry.Literal.Node.Index
		if entryIndex >= litIndex {
			if lits[0] == entry.Literal {
				if removeSubsumed1(fn, lits[1:], entry.SubTrie) {
					entries = append(entries[:i], entries[i+1:]...)
					continue
				}
			} else if entryIndex > litIndex {
				break
			}
		} else {
			if removeSubsumed1(fn, lits, entry.SubTrie) {
				entries = append(entries[:i], entries[i+1:]...)
				continue
			}
		}
		i++
	}
	return len(entries) == 0
}

// --- Complete LTMS operations ---

// InsertTrueClauseComplete inserts with ordering by length.
func InsertTrueClauseComplete(cl *Clause, node *TmsNode) {
	node.TrueClauses = insertClauseOrdered(cl, node.TrueClauses)
}

// InsertFalseClauseComplete inserts with ordering by length.
func InsertFalseClauseComplete(cl *Clause, node *TmsNode) {
	node.FalseClauses = insertClauseOrdered(cl, node.FalseClauses)
}

func insertClauseOrdered(cl *Clause, list []*Clause) []*Clause {
	for i, existing := range list {
		if cl.Length >= existing.Length {
			result := make([]*Clause, 0, len(list)+1)
			result = append(result, list[:i]...)
			result = append(result, cl)
			result = append(result, list[i:]...)
			return result
		}
	}
	return append(list, cl)
}

// IndexClause indexes a clause and runs BCP.
func IndexClause(cl *Clause, l *LTMS) {
	for _, term := range cl.Literals {
		switch term.Sign {
		case LabelTrue:
			InsertTrueClauseComplete(cl, term.Node)
		case LabelFalse:
			InsertFalseClauseComplete(cl, term.Node)
		}
	}
	CheckClauses(l, []*Clause{cl})
}

// LiteralConnections returns clauses connected to a literal's complement.
func LiteralConnections(literal *Literal) []*Clause {
	if literal.Sign == LabelTrue {
		return literal.Node.FalseClauses
	}
	return literal.Node.TrueClauses
}

// PropagateMoreUnknownness handles unknownness in complete LTMS.
func PropagateMoreUnknownness(oldValue NodeLabel, node *TmsNode, l *LTMS) {
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
			InsertQueueSlot(clause, l)
		}
	}
}

// FullAddClause adds a clause to a complete LTMS.
func FullAddClause(l *LTMS, literals []*Literal, informant interface{}) {
	cl := InstallClause(l, literals, informant)
	if cl != nil && l.Complete != CompleteDelay {
		CheckForContradictions(l)
		IPIA(l)
	}
}

// CompleteLTMS runs resolution to completion.
func CompleteLTMS(l *LTMS) {
	old := l.Complete
	l.Complete = CompleteTrue
	defer func() {
		l.Complete = old
		CheckForContradictions(l)
	}()
	IPIA(l)
}

// InsertQueueSlot inserts a clause into the IPIA queue ordered by length.
func InsertQueueSlot(cl *Clause, l *LTMS) {
	cl.Status = StatusQueued
	for i, slot := range l.Queue {
		if cl.Length == slot.Length {
			l.Queue[i].Clauses = append(slot.Clauses, cl)
			return
		}
		if cl.Length < slot.Length {
			newSlot := &QueueSlot{Length: cl.Length, Clauses: []*Clause{cl}}
			result := make([]*QueueSlot, 0, len(l.Queue)+1)
			result = append(result, l.Queue[:i]...)
			result = append(result, newSlot)
			result = append(result, l.Queue[i:]...)
			l.Queue = result
			return
		}
	}
	l.Queue = append(l.Queue, &QueueSlot{Length: cl.Length, Clauses: []*Clause{cl}})
}

func delaySat(clause *Clause, l *LTMS) bool {
	if l.DelaySat && SatisfiedClause(clause) {
		clause.Status = StatusDirty
		return true
	}
	return false
}

// IPIA implements the Iterative Prime Implicate Algorithm.
func IPIA(l *LTMS) {
	for len(l.Queue) > 0 {
		slot := l.Queue[0]
		if len(slot.Clauses) == 0 {
			l.Queue = l.Queue[1:]
			continue
		}
		c := slot.Clauses[0]
		slot.Clauses = slot.Clauses[1:]
		if len(slot.Clauses) == 0 {
			l.Queue = l.Queue[1:]
		}

		if c.Status != StatusQueued || delaySat(c, l) {
			continue
		}

		// Build sigma (local sorted list)
		var sigma []*QueueSlot
		sigma = insertSigma(c, sigma)

		// For each literal, find resolvable clauses
		var pxs [][]*Clause
		for _, lit := range c.Literals {
			var px []*Clause
			for _, p := range LiteralConnections(lit) {
				if p.Status == StatusQueued || p.Status == StatusSubsumed {
					continue
				}
				if p.Status == StatusDirty && delaySat(p, l) {
					continue
				}
				if SimplifyConsensus(c, p, lit, l.Conses) == nil {
					continue
				}
				if delaySat(p, l) {
					continue
				}
				px = append(px, p)
			}
			pxs = append(pxs, px)
		}

		// Perform pairwise resolution
		if len(pxs) > 0 {
			for i, lit := range c.Literals {
				px := pxs[i]
				if len(px) == 0 {
					continue
				}
				for _, sl := range sigma {
					for _, s := range sl.Clauses {
						if s.Status == StatusSubsumed {
							continue
						}
						hasLit := false
						for _, slit := range s.Literals {
							if slit == lit {
								hasLit = true
								break
							}
						}
						if !hasLit {
							continue
						}
						if delaySat(s, l) {
							continue
						}
						sigma = ipiaInner(l, sigma, px, s, lit)
					}
				}
			}
		}

		if c.Status == StatusQueued {
			c.Status = StatusNone
		}
		for _, sl := range sigma {
			for _, s := range sl.Clauses {
				if s.Status == StatusNotIndexed {
					IndexClause(s, l)
					s.Status = StatusNone
				}
			}
		}
		CheckForContradictions(l)
	}
}

func ipiaInner(l *LTMS, sigma []*QueueSlot, px []*Clause, s *Clause, lit *Literal) []*QueueSlot {
	var sChildren []*Clause
	for _, p := range px {
		if delaySat(p, l) {
			continue
		}
		if p.Status == StatusSubsumed {
			continue
		}
		consensus := SimplifySubsumeConsensus(l, s, p, lit)
		if consensus == nil {
			continue
		}
		consensus.Status = StatusNotIndexed
		if s.Status == StatusSubsumed {
			sigma = insertSigma(consensus, sigma)
			sChildren = nil
			break
		}
		sChildren = append(sChildren, consensus)
	}
	for _, child := range sChildren {
		if child.Status != StatusSubsumed {
			sigma = insertSigma(child, sigma)
		}
	}
	return sigma
}

func insertSigma(cl *Clause, sigma []*QueueSlot) []*QueueSlot {
	for i, slot := range sigma {
		if cl.Length == slot.Length {
			sigma[i].Clauses = append(slot.Clauses, cl)
			return sigma
		}
		if cl.Length < slot.Length {
			newSlot := &QueueSlot{Length: cl.Length, Clauses: []*Clause{cl}}
			result := make([]*QueueSlot, 0, len(sigma)+1)
			result = append(result, sigma[:i]...)
			result = append(result, newSlot)
			result = append(result, sigma[i:]...)
			return result
		}
	}
	return append(sigma, &QueueSlot{Length: cl.Length, Clauses: []*Clause{cl}})
}

// SimplifyConsensus computes consensus of two clauses on a literal.
func SimplifyConsensus(cl1, cl2 *Clause, term1 *Literal, conses []*Literal) []*Literal {
	if cl1.Informant != nil && cl2.Informant != nil && cl1.Informant == cl2.Informant {
		return nil
	}
	var result []*Literal
	i1, i2 := 0, 0
	for i1 < len(cl1.Literals) && i2 < len(cl2.Literals) {
		t1 := cl1.Literals[i1]
		t2 := cl2.Literals[i2]
		if t1 == t2 {
			result = append(result, t1)
			i1++
			i2++
		} else if t1.Node.Index == t2.Node.Index {
			// Same node, different sign - this is the resolution variable
			if t1 != term1 {
				return nil // can't resolve on more than one variable
			}
			i1++
			i2++
		} else if t1.Node.Index < t2.Node.Index {
			result = append(result, t1)
			i1++
		} else {
			result = append(result, t2)
			i2++
		}
	}
	for ; i1 < len(cl1.Literals); i1++ {
		result = append(result, cl1.Literals[i1])
	}
	for ; i2 < len(cl2.Literals); i2++ {
		result = append(result, cl2.Literals[i2])
	}
	return result
}

// SimplifySubsumeConsensus performs consensus with subsumption checking.
func SimplifySubsumeConsensus(l *LTMS, cl1, cl2 *Clause, p *Literal) *Clause {
	if cl1.Informant != nil && cl2.Informant != nil && cl1.Informant == cl2.Informant {
		return nil
	}
	literals := SimplifyConsensus(cl1, cl2, p, l.Conses)
	if literals == nil {
		return nil
	}
	if Subsumed(literals, l.Clauses) != nil {
		return nil
	}
	informant := []interface{}{"RESOLVE", cl1, cl2, p}
	return ProcessClause(l, copyLiterals(literals), informant, true)
}

func copyLiterals(lits []*Literal) []*Literal {
	cp := make([]*Literal, len(lits))
	copy(cp, lits)
	return cp
}

// RemoveClause removes an old clause replaced by a new one.
func RemoveClause(oldClause, newClause *Clause) {
	if oldClause.Status != StatusNotIndexed {
		for _, term := range oldClause.Literals {
			switch term.Sign {
			case LabelTrue:
				term.Node.TrueClauses = removeFromClauses(oldClause, term.Node.TrueClauses)
			case LabelFalse:
				term.Node.FalseClauses = removeFromClauses(oldClause, term.Node.FalseClauses)
			}
		}
	}
	oldClause.Status = StatusSubsumed
	node := ClauseConsequent(oldClause)
	if node == nil {
		return
	}
	// Check if node appears in new clause
	for _, lit := range newClause.Literals {
		if lit.Node == node {
			node.Support = newClause
			return
		}
	}
	// Contradiction: need to find alternative support
	FindAlternativeSupport(node.LTMS, PropagateUnknownness(node))
}

func removeFromClauses(cl *Clause, list []*Clause) []*Clause {
	for i, c := range list {
		if c == cl {
			return append(list[:i], list[i+1:]...)
		}
	}
	return list
}

// InstallClause installs a clause if not subsumed.
func InstallClause(l *LTMS, literals []*Literal, informant interface{}) *Clause {
	if Subsumed(literals, l.Clauses) != nil {
		return nil
	}
	return ProcessClause(l, literals, informant, false)
}

// ProcessClause processes a new clause.
func ProcessClause(l *LTMS, literals []*Literal, informant interface{}, internal bool) *Clause {
	cl := BcpAddClause(l, literals, informant, false)
	RemoveSubsumed(func(oldClause *Clause) {
		RemoveClause(oldClause, cl)
	}, literals, l)
	AddToTrie(cl, l)
	if !internal {
		if delaySat(cl, l) {
			IndexClause(cl, l)
		} else {
			IndexClause(cl, l)
			InsertQueueSlot(cl, l)
		}
	}
	return cl
}

// --- Complete LTMS versions of normalize ---

// DisjoinClauses merges two sorted literal lists.
func DisjoinClauses(terms1, terms2 []*Literal) ([]*Literal, bool) {
	var result []*Literal
	i1, i2 := 0, 0
	for i1 < len(terms1) && i2 < len(terms2) {
		t1 := terms1[i1]
		t2 := terms2[i2]
		if t1 == t2 {
			result = append(result, t1)
			i1++
			i2++
		} else if t1.Node.Index == t2.Node.Index {
			return nil, false // complementary literals = FAIL
		} else if t1.Node.Index < t2.Node.Index {
			result = append(result, t1)
			i1++
		} else {
			result = append(result, t2)
			i2++
		}
	}
	for ; i1 < len(terms1); i1++ {
		result = append(result, terms1[i1])
	}
	for ; i2 < len(terms2); i2++ {
		result = append(result, terms2[i2])
	}
	return result, true
}

// AddFormulaComplete adds formula to a complete LTMS.
func AddFormulaComplete(l *LTMS, formula, informant interface{}) {
	tltms := CreateLTMS("Temporary for add-formula", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		Complete:               CompleteDelay,
		DelaySat:               false,
	})
	InitCompleteLTMS(tltms)

	clauses := Normalize(l, formula)
	if informant == nil {
		informant = []interface{}{":IMPLIED-BY", formula}
	}

	// Mark nodes with occurrence count
	if l.Nodes != nil {
		for _, node := range l.Nodes {
			node.Mark = 0
		}
	}

	var literals []*TmsNode
	for _, clause := range clauses {
		for _, literal := range clause {
			if count, ok := literal.Node.Mark.(int); ok && count == 0 {
				literals = append(literals, literal.Node)
			}
			if count, ok := literal.Node.Mark.(int); ok {
				literal.Node.Mark = count + 1
			}
		}
	}

	sort.Slice(literals, func(i, j int) bool {
		ci, _ := literals[i].Mark.(int)
		cj, _ := literals[j].Mark.(int)
		return ci < cj
	})

	// Create temporary nodes
	for _, lit := range literals {
		lit.Mark = TmsCreateNode(tltms, lit, false)
	}

	// Add clauses to temporary LTMS
	for _, clause := range clauses {
		var mapped []*Literal
		for _, lit := range clause {
			tNode := lit.Node.Mark.(*TmsNode)
			if lit.Sign == LabelTrue {
				mapped = append(mapped, tNode.TrueLiteral)
			} else {
				mapped = append(mapped, tNode.FalseLiteral)
			}
		}
		mapped = SortClause(mapped)
		AddClauseInternal(mapped, nil, true)
	}

	CompleteLTMS(tltms)

	// Copy results back
	WalkTrie(func(clause *Clause) {
		var mapped []*Literal
		for _, lit := range clause.Literals {
			origNode := lit.Node.Datum.(*TmsNode)
			if lit.Sign == LabelTrue {
				mapped = append(mapped, origNode.TrueLiteral)
			} else {
				mapped = append(mapped, origNode.FalseLiteral)
			}
		}
		mapped = SortClause(mapped)
		AddClauseInternal(mapped, informant, true)
	}, tltms.Clauses)

	CheckForContradictions(l)
	if l.Complete == CompleteComplete {
		IPIA(l)
	}
}

// PI computes prime implicates of a formula.
func PI(formula interface{}) *LTMS {
	l := CreateLTMS("Prime Implicates", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		DelaySat:               true,
	})
	tltms := CreateLTMS("Prime Implicates", LTMSOptions{
		NodeString:             DefaultNodeString,
		CheckingContradictions: true,
		ContradictionHandler:   AskUserHandler,
		CacheDatums:            true,
		Complete:               CompleteDelay,
		DelaySat:               false,
	})
	InitCompleteLTMS(tltms)

	clauses := Normalize(l, formula)
	var literals []*TmsNode
	if l.Nodes != nil {
		for _, node := range l.Nodes {
			node.Mark = 0
			literals = append(literals, node)
		}
	}
	for _, clause := range clauses {
		for _, literal := range clause {
			if count, ok := literal.Node.Mark.(int); ok {
				literal.Node.Mark = count + 1
			}
		}
	}
	sort.Slice(literals, func(i, j int) bool {
		ci, _ := literals[i].Mark.(int)
		cj, _ := literals[j].Mark.(int)
		return ci < cj
	})
	for _, lit := range literals {
		lit.Mark = TmsCreateNode(tltms, lit.Datum, false)
	}
	for _, clause := range clauses {
		var mapped []*Literal
		for _, lit := range clause {
			tNode := lit.Node.Mark.(*TmsNode)
			if lit.Sign == LabelTrue {
				mapped = append(mapped, tNode.TrueLiteral)
			} else {
				mapped = append(mapped, tNode.FalseLiteral)
			}
		}
		mapped = SortClause(mapped)
		AddClauseInternal(mapped, nil, true)
	}
	CompleteLTMS(tltms)
	collected := Collect(tltms)
	fmt.Printf("\n There now are %d clauses", len(collected))
	return tltms
}
