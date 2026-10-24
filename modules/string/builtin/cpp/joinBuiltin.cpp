//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "joinBuiltin.hpp"
#include "StringJoin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "OverloadRequired.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::joinBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 3);
    ArrayOf delimiters;
    ArrayOf A = argIn[0];
    if (A.isCell() && !A.isCellArrayOfCharacterVectors()) {
        Error(L"Nelson:string:join:invalidInput",
            L"First argument must be a string array or a cell array of character vectors.");
    }
    std::vector<indexType> vec = A.dimensions().asVector();
    indexType dim = 0;
    if (!A.isEmpty()) {
        for (int k = static_cast<int>(vec.size()) - 1; k >= 0; --k) {
            if (vec[k] != 0 && vec[k] != 1) {
                dim = k;
                break;
            }
        }
    }
    dim += 1;
    switch (argIn.size()) {
    case 1: {
        delimiters = ArrayOf::stringArray(L" ");
    } break;
    case 2: {
        if (argIn[1].isRowVectorCharacterArray()) {
            delimiters = ArrayOf::stringArray(argIn[1].asWideString());
        } else if (argIn[1].isStringArray()) {
            delimiters = argIn[1];
        } else if (argIn[1].isCellArrayOfCharacterVectors()) {
            delimiters
                = ArrayOf::stringArray(argIn[1].asWideStringVector(false), argIn[1].dimensions());
        } else {
            dim = argIn[1].asScalarIndex(false, true);
            delimiters = ArrayOf::stringArray(" ");
        }
    } break;
    case 3: {
        if (argIn[1].isRowVectorCharacterArray()) {
            delimiters = ArrayOf::stringArray(argIn[1].asWideString());
        } else if (argIn[1].isStringArray()) {
            delimiters = argIn[1];
        } else if (argIn[1].isCellArrayOfCharacterVectors()) {
            delimiters
                = ArrayOf::stringArray(argIn[1].asWideStringVector(false), argIn[1].dimensions());
        } else {
            Error(L"Nelson:string:invalidArgumentType",
                L"Wrong type for argument #%d: %s expected.", 3,
                L"string, characters or cell of characters");
        }
        dim = argIn[2].asScalarIndex(false, true);
    } break;
    default:
        break;
    }
    retval << StringJoin(A, delimiters, dim);
    return retval;
}
//=============================================================================
