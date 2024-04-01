//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getenvBuiltin.hpp"
#include "Error.hpp"
#include "GetVariableEnvironment.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::getenvBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    NelsonType currentClass = argIn[0].getDataClass();
    switch (currentClass) {
    case NLS_CHAR: {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring varEnvName = argIn[0].getContentAsWideString();
            std::wstring ret = GetVariableEnvironment(varEnvName);
            retval << ArrayOf::characterArrayConstructor(ret);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } break;
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY: {
        if (argIn[0].isScalarStringArray()) {
            std::wstring varEnvName = argIn[0].getContentAsWideString();
            std::wstring ret = GetVariableEnvironment(varEnvName);
            retval << ArrayOf::characterArrayConstructor(ret);
            return retval;
        }
        ArrayOf* elements = (ArrayOf*)argIn[0].getDataPointer();
        ArrayOf* outputs
            = (ArrayOf*)ArrayOf::allocateArrayOf(currentClass, argIn[0].getElementCount());
        ArrayOf res = ArrayOf(currentClass, argIn[0].getDimensions(), outputs);
        for (size_t k = 0; k < argIn[0].getElementCount(); ++k) {
            if (elements[k].isRowVectorCharacterArray()) {
                std::wstring varEnvName = elements[k].getContentAsWideString();
                std::wstring ret = GetVariableEnvironment(varEnvName);
                outputs[k] = ArrayOf::characterArrayConstructor(ret);
            } else {
                Error(_W("Environment names must be a string array, cell array of character "
                         "vectors, or character array."));
            }
        }
        retval << res;
    } break;
    default: {
        Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
    } break;
    }
    return retval;
}
//=============================================================================
