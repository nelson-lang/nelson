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
#include "NelsonException.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
struct ErrorParseResult
{
    NelsonException exception;
    ArrayOfVector formatArguments;
    bool needsFormatting = false;
    bool useCurrentStack = true;
};
//=============================================================================
ErrorParseResult
ParseErrorFunctionArguments(const ArrayOfVector& argIn);
//=============================================================================
void
ThrowParsedError(Evaluator* eval, NelsonException exception, bool useCurrentStack);
//=============================================================================
} // namespace Nelson
//=============================================================================
