#pragma once

/// Set Constraint Rules for LTRE
/// Converted from setrule.lisp
/// Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ldata.h"
#include "linter.h"

// Install set constraint rules on the given LTRE.
// In the Lisp version, these are pattern-triggered rules that enforce:
// 1. Members not in a construal are excluded
// 2. Construal uniqueness (different construals are exclusive)
// 3. CWA-JUSTIFICATION converts justification statements to clauses
void install_set_rules(LTRE* ltre = nullptr);
