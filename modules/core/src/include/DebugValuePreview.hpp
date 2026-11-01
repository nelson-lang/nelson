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
#include "ArrayOf.hpp"
#include "Evaluator.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// short text of a value for a data tip: scalars and small arrays, else "<dims> <class>"
NLSCORE_IMPEXP std::wstring
variableValueSummary(const ArrayOf& value, size_t maxLength = 120);
//=============================================================================
// data tip for a variable or dotted field path of the current scope; false when unknown
NLSCORE_IMPEXP bool
debugVariablePreview(Evaluator* eval, const std::wstring& name, std::wstring& preview);
//=============================================================================
// data tip for a selected expression, evaluated only when the interpreter waits at a prompt
NLSCORE_IMPEXP bool
debugExpressionPreview(Evaluator* eval, const std::wstring& expression, std::wstring& preview,
    std::wstring& errorMessage);
//=============================================================================
// data tip entry point of the editors: dotted names are looked up, other text is evaluated
NLSCORE_IMPEXP bool
debugValuePreview(
    Evaluator* eval, const std::wstring& text, std::wstring& preview, std::wstring& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
