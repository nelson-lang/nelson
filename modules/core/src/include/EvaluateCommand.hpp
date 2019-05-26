//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
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
NLSCORE_IMPEXP ArrayOfVector
EvaluateConsoleCommand(
    Evaluator* eval, int nLhs, const std::wstring& command, const std::wstring& catchCommand);
} // namespace Nelson
//=============================================================================
