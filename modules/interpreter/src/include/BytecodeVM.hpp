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
#include "BytecodeChunk.hpp"
#include "nlsInterpreter_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
class NLSINTERPRETER_IMPEXP BytecodeVM
{
public:
    // callerPushedScope=false runs the function without a context scope of its
    // own (scope-free fast call); the VM pushes one lazily if a runtime-resolved
    // callee turns out to need the caller context. It must be false whenever
    // the caller did not push a scope for this call (direct bytecode calls,
    // scope-free anonymous functions): the frame otherwise treats the current
    // scope, which belongs to the caller, as its own and empties it on exit.
    static ArrayOfVector
    executeFunction(Evaluator* eval, const BytecodeChunk& chunk, const ArrayOfVector& argIn,
        int nLhs, bool callerPushedScope = true);

    static void
    executeScript(Evaluator* eval, const BytecodeChunk& chunk);

    // True when calling funcDef requires the caller's context scope to exist
    // and be synced (refreshRequired=false) or refreshed after the call
    // (refreshRequired=true): scripts, nested functions, builtins flagged
    // mustSync/RefreshCallerContext, and macros transitively reaching one of
    // those. Results are cached on the callee chunks (callerContextSync* /
    // callerContextRefresh* flags).
    static bool
    functionNeedsCallerContext(Evaluator* eval, FunctionDef* funcDef, bool refreshRequired);
};
//=============================================================================
} // namespace Nelson
//=============================================================================
