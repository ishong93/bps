#pragma once

/// LTRE Module Loader
/// Converted from ltre.lisp
///
/// In Common Lisp, ltre.lisp loads the LTMS subsystem files in order.
/// In C++, this is handled by #include directives and the build system.
///
/// Module loading order:
///   1. ltms.h / ltms.cpp     -- Logic-based Truth Maintenance System
///   2. cltms.h / cltms.cpp   -- Complete LTMS extensions
///   3. linter.h / linter.cpp -- LTRE struct and interface (from linter.lisp)
///   4. ldata.h / ldata.cpp   -- Database: facts, datums, dbclasses (from ldata.lisp)
///   5. lrules.h / lrules.cpp -- Rule engine (from lrules.lisp) [not yet converted]
///
/// Copyright (c) 1986-1995, Kenneth D. Forbus, Northwestern University,
/// and Johan de Kleer, the Xerox Corporation. All rights reserved.

#include "ltms.h"
#include "linter.h"
#include "ldata.h"
