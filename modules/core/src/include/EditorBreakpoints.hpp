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
// breakpoint operations shared by the editors: the line is adjusted as dbstop does
//=============================================================================
// name of the function declared above `line` in the file ("" for a script)
NLSCORE_IMPEXP std::string
breakpointFunctionNameAtLine(const std::wstring& filename, size_t line);
//=============================================================================
// adds a breakpoint from an editor click; returns the line used, 0 on failure
NLSCORE_IMPEXP size_t
addEditorBreakpoint(
    Evaluator* eval, const std::wstring& filename, size_t line, std::wstring& errorMessage);
//=============================================================================
// removes the breakpoint on the (adjusted) line when there is one, adds it otherwise
NLSCORE_IMPEXP size_t
toggleEditorBreakpoint(Evaluator* eval, const std::wstring& filename, size_t line, bool& added,
    std::wstring& errorMessage);
//=============================================================================
// one-shot Run to Here: arms the temporary breakpoint and queues the run or resume command
NLSCORE_IMPEXP bool
runToEditorLine(
    Evaluator* eval, const std::wstring& filename, size_t line, std::wstring& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
