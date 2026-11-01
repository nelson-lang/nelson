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
#include <string>
#include "nlsCore_exports.h"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// JSON views of the debugger shared by the webview RPC and the wasm bridge
//=============================================================================
// state: running, paused, currentFile, currentLine, reason, errorMessage, canStep, stack...
NLSCORE_IMPEXP std::string
debuggerStateJson(Evaluator* eval);
//=============================================================================
// {file, lines:[...], breakpoints:[{id,line,enabled,condition}]}
NLSCORE_IMPEXP std::string
debuggerBreakpointsJson(Evaluator* eval, const std::string& fileUtf8);
//=============================================================================
// breakpoints: [{id, file, line, function, enabled, condition}], errorBreakpoints, revision
NLSCORE_IMPEXP std::string
debuggerAllBreakpointsJson(Evaluator* eval);
//=============================================================================
// runs a debugger RPC method on the interpreter thread; unknown method gives {"error": ...}
NLSCORE_IMPEXP std::string
debuggerRpc(Evaluator* eval, const std::string& method, const std::string& paramsJson);
//=============================================================================
} // namespace Nelson
//=============================================================================
