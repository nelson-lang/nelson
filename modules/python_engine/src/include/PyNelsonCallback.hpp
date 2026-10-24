//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "ArrayOf.hpp"
#include "nlsPython_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Wraps a Nelson function_handle into a Python callable so Python code (numpy,
// scikit-learn, ...) can call back into the same session. Returns a new Python reference
// as an opaque void* (this header stays free of <Python.h>), or nullptr on failure. The
// handle is kept in an internal registry keyed by an id carried in a PyCapsule; the entry
// is released when the Python proxy is garbage collected.
//
// The evaluator always runs on the interpreter thread: directly when Python calls on that
// thread, otherwise through the main-thread task queue. A callback from a foreign thread
// while the interpreter is not pumping fails with a Python exception.
NLSPYTHON_ENGINE_IMPEXP void*
createNelsonCallable(const ArrayOf& functionHandle);
//=============================================================================
// True while the interpreter thread runs a function handle invoked from Python. pyrun and
// pyrunfile use it to reject re-entrant calls instead of deadlocking.
NLSPYTHON_ENGINE_IMPEXP bool
isExecutingNelsonPythonCallback();
//=============================================================================
} // namespace Nelson
//=============================================================================
