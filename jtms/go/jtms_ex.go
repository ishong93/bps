// Examples for Justification-based TMS.
// Translated from jtms-ex.lisp in "Building Problem Solvers"
// by Kenneth D. Forbus and Johan de Kleer.

package jtms

import "fmt"

// GetNode finds a node by its datum in the JTMS.
// Corresponds to the Lisp function get-node.
func GetNode(datum interface{}, jtms *JTMS) *TmsNode {
	for _, node := range jtms.Nodes {
		if deepEqual(datum, node.Datum) {
			return node
		}
	}
	return nil
}

// GetJustification finds a justification by its index.
// Corresponds to the Lisp function get-justification.
func GetJustification(num int, jtms *JTMS) *Just {
	for _, just := range jtms.Justs {
		if just.Index == num {
			return just
		}
	}
	return nil
}

// Ex1Result holds the nodes created by Ex1.
type Ex1Result struct {
	Jtms               *JTMS
	Na, Nb, Nc, Nd, Ne, Nf, Ng *TmsNode
}

// Ex1 creates a simple JTMS example with 7 assumption nodes
// and 4 justifications, then enables 4 assumptions.
// Corresponds to the Lisp function ex1.
func Ex1() *Ex1Result {
	j := CreateJTMS("Simple Example", WithDebugging(true))
	na := TmsCreateNode(j, "a", true, false)
	nb := TmsCreateNode(j, "b", true, false)
	nc := TmsCreateNode(j, "c", true, false)
	nd := TmsCreateNode(j, "d", true, false)
	ne := TmsCreateNode(j, "e", true, false)
	nf := TmsCreateNode(j, "f", true, false)
	ng := TmsCreateNode(j, "g", true, false)

	JustifyNode("j1", nf, []*TmsNode{na, nb})
	JustifyNode("j2", ne, []*TmsNode{nb, nc})
	JustifyNode("j3", ng, []*TmsNode{na, ne})
	JustifyNode("j4", ng, []*TmsNode{nd, ne})

	EnableAssumption(na)
	EnableAssumption(nb)
	EnableAssumption(nc)
	EnableAssumption(nd)

	return &Ex1Result{Jtms: j, Na: na, Nb: nb, Nc: nc, Nd: nd, Ne: ne, Nf: nf, Ng: ng}
}

// Ex2 adds a contradiction node to the JTMS from Ex1.
// Corresponds to the Lisp function ex2.
func Ex2(r *Ex1Result) *TmsNode {
	contra := TmsCreateNode(r.Jtms, "Loser", false, true)
	JustifyNode("j5", contra, []*TmsNode{r.Ne, r.Nf})
	return contra
}

// Ex3 demonstrates multiple support with contradictions.
// Corresponds to the Lisp function ex3.
func Ex3() {
	j := CreateJTMS("Multiple support example")
	assumptionA := TmsCreateNode(j, "A", true, false)
	assumptionC := TmsCreateNode(j, "C", true, false)
	assumptionE := TmsCreateNode(j, "E", true, false)
	nodeH := TmsCreateNode(j, "h", false, false)

	EnableAssumption(assumptionA)
	EnableAssumption(assumptionC)
	EnableAssumption(assumptionE)

	JustifyNode("R1", nodeH, []*TmsNode{assumptionC, assumptionE})

	nodeG := TmsCreateNode(j, "g", false, false)
	JustifyNode("R2", nodeG, []*TmsNode{assumptionA, assumptionC})

	contradiction := TmsCreateNode(j, "CONTRADICTION", false, true)
	JustifyNode("R3", contradiction, []*TmsNode{nodeG})

	fmt.Println("Ex3 completed. Check node states with WhyNodes.")
	WhyNodes(j)
}
