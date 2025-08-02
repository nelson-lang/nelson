//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "echoBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::DisplayFormatGateway::echoBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    ArrayOfVector retval(nLhs);
    if (argIn.empty()) {
        bool toggle = eval->getEchoMode();
        if (nLhs == 0) {
            eval->setEchoMode(!toggle);
        } else {
            if (toggle) {
                retval << ArrayOf::characterArrayConstructor("on");
            } else {
                retval << ArrayOf::characterArrayConstructor("off");
            }
        }
    } else if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()) {
            bool bMode = false;
            std::wstring arg = argIn[0].getContentAsWideString();
            if (arg == L"on") {
                bMode = true;
            } else if (arg == L"off") {
                bMode = false;
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_VALUE);
            }
            eval->setEchoMode(bMode);
        } else {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
