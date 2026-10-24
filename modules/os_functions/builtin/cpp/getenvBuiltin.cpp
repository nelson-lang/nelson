//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "getenvBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "GetVariableEnvironment.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::getenvBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval;
    NelsonType currentClass = argIn[0].dataClass();
    switch (currentClass) {
    case NLS_CHAR: {
        if (argIn[0].isRowVectorCharacterArray()) {
            std::wstring varEnvName = argIn[0].asWideString();
            std::wstring ret = GetVariableEnvironment(varEnvName);
            retval << ArrayOf::characterArray(ret);
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
    } break;
    case NLS_CELL_ARRAY:
    case NLS_STRING_ARRAY: {
        if (argIn[0].isScalarStringArray()) {
            std::wstring varEnvName = argIn[0].asWideString();
            std::wstring ret = GetVariableEnvironment(varEnvName);
            retval << ArrayOf::characterArray(ret);
            return retval;
        }
        ArrayOf* elements = (ArrayOf*)argIn[0].dataPointer();
        ArrayOf* outputs
            = (ArrayOf*)ArrayOf::allocateArrayOf(currentClass, argIn[0].elementCount());
        ArrayOf res = ArrayOf(currentClass, argIn[0].dimensions(), outputs);
        for (size_t k = 0; k < argIn[0].elementCount(); ++k) {
            if (elements[k].isRowVectorCharacterArray()) {
                std::wstring varEnvName = elements[k].asWideString();
                std::wstring ret = GetVariableEnvironment(varEnvName);
                outputs[k] = ArrayOf::characterArray(ret);
            } else {
                Error(L"Nelson:os_functions:invalidName",
                    L"Environment names must be a string array, cell array of character vectors, "
                    L"or character array.");
            }
        }
        retval << res;
    } break;
    default: {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    } break;
    }
    return retval;
}
//=============================================================================
