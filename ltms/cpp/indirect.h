#pragma once

/// Indirect Proof Mechanism for LTRE
/// Converted from indirect.lisp
/// Copyright (c) 1986-1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ldata.h"
#include "linter.h"

bool try_indirect_proof(const std::any& fact, LTRE* ltre = nullptr);
void indirect_proof_example();
