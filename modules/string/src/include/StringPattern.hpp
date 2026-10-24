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
#include "ArrayOf.hpp"
#include "nlsString_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class PatternLogicalMode
{
    Contains,
    StartsWith,
    Matches,
    EndsWith
};
//=============================================================================
NLSSTRING_IMPEXP bool
IsPatternObject(const ArrayOf& value);
//=============================================================================
NLSSTRING_IMPEXP std::wstring
PatternExpression(const ArrayOf& value);
//=============================================================================
NLSSTRING_IMPEXP std::wstring
PatternRegexOf(const ArrayOf& value);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
PatternLogical(const ArrayOf& str, const std::wstring& expression, bool ignoreCase,
    PatternLogicalMode mode, bool& needToOverload);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
PatternCount(
    const ArrayOf& str, const std::wstring& expression, bool ignoreCase, bool& needToOverload);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
PatternReplace(const ArrayOf& str, const std::wstring& expression, const std::wstring& replacement,
    bool ignoreCase, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
