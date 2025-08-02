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
#define _CRT_SECURE_NO_WARNINGS
//=============================================================================
#include "Context.hpp"
#include "Evaluator.hpp"
#include "nlsCore_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
NLSCORE_IMPEXP bool
EvaluateCommand(Evaluator* eval, const std::wstring& command, bool bCatch);
NLSCORE_IMPEXP bool
EvaluateCommand(Evaluator* eval, const std::string& command, bool bCatch);
NLSCORE_IMPEXP ArrayOfVector
EvaluateCommand(
    Evaluator* eval, int nLhs, const std::wstring& command, const std::wstring& catchCommand);
NLSCORE_IMPEXP ArrayOfVector
EvaluateInCommand(Evaluator* eval, int nLhs, SCOPE_LEVEL scope, const std::wstring& command);
NLSCORE_IMPEXP bool
EvaluateConsoleCommandToString(Evaluator* eval, const std::wstring& command, std::wstring& result);
NLSCORE_IMPEXP ArrayOfVector
EvaluateConsoleCommand(
    Evaluator* eval, int nLhs, const std::wstring& command, const std::wstring& catchCommand);
} // namespace Nelson
//=============================================================================
