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
NLSSTRING_IMPEXP ArrayOf
StringErase(const ArrayOf& str, const ArrayOf& match, bool& needToOverload);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
StringReverse(const ArrayOf& str, bool& needToOverload);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
StringStrip(const ArrayOf& str, const std::wstring& side, const std::wstring& stripCharacters,
    bool& needToOverload);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
StringPad(const ArrayOf& str, size_t width, const std::wstring& side, wchar_t padCharacter,
    bool& needToOverload);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
StringExtractAfter(const ArrayOf& str, const ArrayOf& start, bool& needToOverload);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
StringExtractBefore(const ArrayOf& str, const ArrayOf& stop, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
