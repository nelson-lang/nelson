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
#include "nlsString_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// historic algo.
NLSSTRING_IMPEXP std::wstring
stringReplace(const std::wstring& searchStr, const std::wstring& pattern,
    const std::wstring& replacement, bool doOverlaps);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
StringReplace(const ArrayOf& STR, const ArrayOf& OLD, const ArrayOf& NEW, bool doOverlaps,
    bool& needToOverload);
//=============================================================================
// modern algo.
NLSSTRING_IMPEXP std::wstring
Replace(
    const std::wstring& searchStr, const std::wstring& pattern, const std::wstring& replacement);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
Replace(const ArrayOf& STR, const ArrayOf& OLD, const ArrayOf& NEW, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
