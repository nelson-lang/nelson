//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "padBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "StringLength.hpp"
#include "StringModern.hpp"
#include "StringHelpers.hpp"
#include <algorithm>
#include <cmath>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::padBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 4);
    // pad(str [, width] [, side] [, padCharacter]): the side is 'left',
    // 'right' or 'both' (any case), the pad character a single character.
    std::wstring side = L"right";
    wchar_t padCharacter = L' ';
    size_t width = 0;
    size_t k = 1;
    if (k < argIn.size() && argIn[k].isNumeric()) {
        if (!argIn[k].isEmpty()) {
            double requested = argIn[k].asDoubleScalar(true, true);
            if (!std::isfinite(requested) || requested < 0) {
                Error(L"Nelson:string:pad:invalidWidth",
                    L"Width must be a non-negative integer scalar.");
            }
            width = static_cast<size_t>(requested);
        }
        ++k;
    }
    bool haveSide = false;
    for (; k < argIn.size(); ++k) {
        if (!argIn[k].isRowVectorCharacterArray()
            && !(argIn[k].isStringArray() && argIn[k].isScalar())) {
            Error(L"Nelson:string:pad:invalidArgument",
                L"Side must be 'left', 'right' or 'both' and the pad character a single "
                L"character.");
        }
        std::wstring text = argIn[k].asWideString();
        std::wstring lowered = StringHelpers::to_lower_copy(text);
        if (!haveSide && (lowered == L"left" || lowered == L"right" || lowered == L"both")) {
            side = lowered;
            haveSide = true;
        } else if (text.size() == 1 && k + 1 == argIn.size()) {
            padCharacter = text[0];
        } else {
            Error(L"Nelson:string:pad:invalidArgument",
                L"Side must be 'left', 'right' or 'both' and the pad character a single "
                L"character.");
        }
    }
    if (width == 0) {
        ArrayOf lengths = StringLength(argIn[0]);
        const double* ptr = static_cast<const double*>(lengths.dataPointer());
        for (indexType j = 0; j < lengths.elementCount(); j++) {
            if (std::isfinite(ptr[j])) { // a missing string has no length
                width = std::max(width, static_cast<size_t>(ptr[j]));
            }
        }
    }
    bool needToOverload = false;
    ArrayOf res = StringPad(argIn[0], width, side, padCharacter, needToOverload);
    if (needToOverload) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
            L"string or cell");
    }
    ArrayOfVector retval;
    retval << res;
    return retval;
}
//=============================================================================
