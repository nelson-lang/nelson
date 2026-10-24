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
// Whitespace as understood by strtrim, deblank and isspace: the ASCII
// controls 9-13 and 28-31, the space, and the Unicode space separators that
// are not no-break spaces.
inline bool
IsTextWhitespace(wchar_t c)
{
    return (c >= 9 && c <= 13) || (c >= 28 && c <= 32) || c == 0x1680
        || (c >= 0x2000 && c <= 0x200A && c != 0x2007) || c == 0x2028 || c == 0x2029 || c == 0x205F
        || c == 0x3000;
}
//=============================================================================
// Removes the leading (when trimLeft) and trailing columns of a 2-D character
// array whose characters are all whitespace (or NUL when nulIsBlank).
NLSSTRING_IMPEXP ArrayOf
TrimCharacterArray(const ArrayOf& A, bool trimLeft, bool nulIsBlank);
//=============================================================================
NLSSTRING_IMPEXP ArrayOf
StringTrim(const ArrayOf& A, bool& needToOverload);
//=============================================================================
} // namespace Nelson
//=============================================================================
