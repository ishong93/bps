// -*- Mode: Go -*-

// ATMS test code
// Translated from atest.lisp

// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
// and Johan de Kleer, the Xerox Corporation.
// All rights reserved.

// See the file legal.txt for a paragraph stating scope of permission
// and disclaimer of warranty.

package atms

import (
	"fmt"
	"os"
)

// AtmsTest1 creates a basic ATMS test with assumptions and justifications.
func AtmsTest1() (*ATMS, *TmsNode, *TmsNode, *TmsNode, *TmsNode, *TmsNode, *TmsNode) {
	atms := CreateATMS("atms-test1", WithDebugging(true))

	a := TmsCreateNode(atms, "A", false, false)
	b := TmsCreateNode(atms, "B", false, false)
	c := TmsCreateNode(atms, "C", false, false)
	d := TmsCreateNode(atms, "D", false, false)
	e := TmsCreateNode(atms, "E", false, false)
	f := TmsCreateNode(atms, "F", false, false)

	AssumeNode(a)
	AssumeNode(b)
	AssumeNode(c)

	JustifyNode("J1", d, []*TmsNode{a, b})
	JustifyNode("J2", e, []*TmsNode{b, c})
	JustifyNode("J3", f, []*TmsNode{d, e})

	return atms, a, b, c, d, e, f
}

// AtmsTest2 adds a simpler justification for d (must run after AtmsTest1).
func AtmsTest2(d, a *TmsNode) {
	JustifyNode("simpler", d, []*TmsNode{a})
}

// AtmsTest3 marks {a, b} as nogood (must run after AtmsTest1).
func AtmsTest3(a, b *TmsNode) {
	NogoodNodes("atms-test3", []*TmsNode{a, b})
}

// Step1 runs the example from de Kleer's paper by Gitchang.
func Step1() {
	atms := CreateATMS("Step-1")

	a := TmsCreateNode(atms, "A", false, false)
	b := TmsCreateNode(atms, "B", false, false)
	c := TmsCreateNode(atms, "C", false, false)
	x1 := TmsCreateNode(atms, "x=1", false, false)
	yx := TmsCreateNode(atms, "y=x", false, false)
	xz := TmsCreateNode(atms, "x=z", false, false)
	y1 := TmsCreateNode(atms, "y=1", false, false)
	z1 := TmsCreateNode(atms, "z=1", false, false)

	AssumeNode(a)
	AssumeNode(b)
	AssumeNode(c)

	JustifyNode("j1", x1, []*TmsNode{a})
	JustifyNode("j2", yx, []*TmsNode{b})
	JustifyNode("j3", xz, []*TmsNode{c})

	WhyNodes(atms, os.Stdout)
	PrintEnvs(atms, os.Stdout)

	fmt.Fprintf(os.Stdout, "\n\nNow register nogood{A,B}")
	NogoodNodes("NOGOOD", []*TmsNode{a, b})
	WhyNodes(atms, os.Stdout)
	PrintEnvs(atms, os.Stdout)

	fmt.Fprintf(os.Stdout, "\n\nx=1, y=x => y=1")
	JustifyNode("j4", y1, []*TmsNode{x1, yx})
	WhyNodes(atms, os.Stdout)
	PrintEnvs(atms, os.Stdout)

	fmt.Fprintf(os.Stdout, "\n\nWe have a premise z=1")
	JustifyNode("Premise", z1, nil)
	WhyNodes(atms, os.Stdout)
	PrintEnvs(atms, os.Stdout)

	fmt.Fprintf(os.Stdout, "\n\nz=1, x=z => x=1")
	JustifyNode("j5", x1, []*TmsNode{z1, xz})
	WhyNodes(atms, os.Stdout)
	PrintEnvs(atms, os.Stdout)
}
