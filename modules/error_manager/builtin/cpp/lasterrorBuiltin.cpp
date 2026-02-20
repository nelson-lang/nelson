//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "lasterrorBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "ErrorToStruct.hpp"
#include "IsErrorStruct.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::lasterrorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval(nLhs);
    nargincheck(argIn, 0, 1);
    if (argIn.size() == 1) {
        ArrayOf arg1 = argIn[0];
        if (arg1.isRowVectorCharacterArray()) {
            std::wstring str = arg1.getContentAsWideString();
            if (str == L"reset") {
                eval->setLastErrorException(Exception());
            } else {
                raiseError(
                    L"Nelson:error_manager:ERROR_WRONG_VALUE_FOR_2_INPUT_ARGUMENT_X_EXPECTED",
                    ERROR_WRONG_VALUE_FOR_2_INPUT_ARGUMENT_X_EXPECTED, "reset");
            }
        } else if (arg1.isStruct()) {
            Exception e;
            if (IsErrorStruct(arg1, e)) {
                eval->setLastErrorException(e);
            } else {
                raiseError2(_E("nelson:validators:invalidValueAtPosition"), 2);
            }
        } else {
            raiseError2(_E("nelson:validators:mustBeValidType"), 2);
        }
    }
    Exception lasterror = eval->getLastErrorException();
    retval << ErrorToStruct(lasterror);
    return retval;
}
//=============================================================================
