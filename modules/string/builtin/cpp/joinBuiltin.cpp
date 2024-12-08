//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "joinBuiltin.hpp"
#include "StringJoin.hpp"
#include "Error.hpp"
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
    std::vector<indexType> vec = A.getDimensions().getAsVector();
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
        delimiters = ArrayOf::stringArrayConstructor(L" ");
    } break;
    case 2: {
        if (argIn[1].isRowVectorCharacterArray()) {
            delimiters = ArrayOf::stringArrayConstructor(argIn[1].getContentAsWideString());
        } else if (argIn[1].isStringArray()) {
            delimiters = argIn[1];
        } else if (argIn[1].isCellArrayOfCharacterVectors()) {
            delimiters = ArrayOf::stringArrayConstructor(
                argIn[1].getContentAsWideStringVector(false), argIn[1].getDimensions());
        } else {
            dim = argIn[1].getContentAsScalarIndex(false, true);
            delimiters = ArrayOf::stringArrayConstructor(" ");
        }
    } break;
    case 3: {
        if (argIn[1].isRowVectorCharacterArray()) {
            delimiters = ArrayOf::stringArrayConstructor(argIn[1].getContentAsWideString());
        } else if (argIn[1].isStringArray()) {
            delimiters = argIn[1];
        } else if (argIn[1].isCellArrayOfCharacterVectors()) {
            delimiters = ArrayOf::stringArrayConstructor(
                argIn[1].getContentAsWideStringVector(false), argIn[1].getDimensions());
        } else {
            Error(_W(
                "Wrong type for argument #3: string, characters or cell of characters expected."));
        }
        dim = argIn[2].getContentAsScalarIndex(false, true);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    retval << StringJoin(A, delimiters, dim);
    return retval;
}
//=============================================================================
