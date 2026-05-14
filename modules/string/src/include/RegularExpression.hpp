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
#include <vector>
#include "ArrayOf.hpp"
#include "nlsString_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class Evaluator;
//=============================================================================
enum class RegexOutputKind
{
    START,
    END,
    TOKEN_EXTENTS,
    MATCH,
    TOKENS,
    NAMES,
    SPLIT
};
//=============================================================================
struct RegexOptions
{
    bool ignoreCase = false;
    bool once = false;
    bool forceCellOutput = false;
    bool emptyMatch = false;
    bool dotExceptNewline = false;
    bool lineAnchors = false;
    bool warnings = false;
    bool preserveCase = false;
    int replaceOccurrence = 0;
};
//=============================================================================
NLSSTRING_IMPEXP ArrayOfVector
RegExpBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn, bool defaultIgnoreCase);
//=============================================================================
NLSSTRING_IMPEXP ArrayOfVector
RegexPrepBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn);
//=============================================================================
NLSSTRING_IMPEXP ArrayOfVector
RegexpTranslateBuiltin(int nLhs, const ArrayOfVector& argIn);
//=============================================================================
} // namespace Nelson
//=============================================================================
