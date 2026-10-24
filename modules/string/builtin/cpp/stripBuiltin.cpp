//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "stripBuiltin.hpp"
#include "StringHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringModern.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::stripBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 3);
    // strip(str [, side] [, stripCharacter]): the side is 'left', 'right' or
    // 'both' (any case), the strip character a single character
    std::wstring side = L"both";
    std::wstring chars;
    bool haveSide = false;
    for (size_t k = 1; k < argIn.size(); ++k) {
        if (!argIn[k].isRowVectorCharacterArray()
            && !(argIn[k].isStringArray() && argIn[k].isScalar())) {
            Error(L"Nelson:string:strip:invalidArgument",
                L"Side must be 'left', 'right' or 'both' and the strip character a single "
                L"character.");
        }
        std::wstring text = argIn[k].asWideString();
        std::wstring lowered = StringHelpers::to_lower_copy(text);
        if (k == 1 && (lowered == L"left" || lowered == L"right" || lowered == L"both")) {
            side = lowered;
            haveSide = true;
        } else if (text.size() == 1 && chars.empty() && (k == 1 || haveSide)) {
            chars = text;
        } else {
            Error(L"Nelson:string:strip:invalidArgument",
                L"Side must be 'left', 'right' or 'both' and the strip character a single "
                L"character.");
        }
    }
    bool needToOverload = false;
    ArrayOf res = StringStrip(argIn[0], side, chars, needToOverload);
    if (needToOverload) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or cell");
    }
    ArrayOfVector retval;
    retval << res;
    return retval;
}
//=============================================================================
