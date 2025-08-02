//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "quitBuiltin.hpp"
#include "Error.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "PredefinedErrorMessages.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static int
getExitCodeFromVariable(const ArrayOfVector& argIn);
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::quitBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 2);
    nargoutcheck(0, 0);
    ArrayOfVector retval;
    int iValue = 0;
    switch (argIn.size()) {
    case 0: {
        // quit
        eval->setExitCode(iValue);
        eval->setState(NLS_STATE_QUIT);
        return retval;
    } break;
    case 1: {
        if ((argIn[0].isStringArray() && argIn[0].isScalar())
            || argIn[0].isRowVectorCharacterArray()) {

            std::wstring quitCommand = argIn[0].getContentAsWideString();
            if (quitCommand == L"cancel") {
                // quit cancel
                iValue = 0;
                eval->setExitCode(iValue);
                eval->setState(NLS_STATE_CANCEL_QUIT);
                return retval;

            } else if (quitCommand == L"force") {
                // quit force
                iValue = 0;
                eval->setExitCode(iValue);
                eval->setState(NLS_STATE_FORCE_QUIT);
                return retval;
            } else {
                Error(_("Unknown option."));
            }
        }
        iValue = getExitCodeFromVariable(argIn);
        eval->setExitCode(iValue);
        eval->setState(NLS_STATE_QUIT);
    } break;
    case 2: {
        // quit(code, "force")
        bool isForce = ((argIn[1].isStringArray() && argIn[1].isScalar())
                           || argIn[1].isRowVectorCharacterArray())
            && (argIn[1].getContentAsWideString() == L"force");
        if (!isForce) {
            Error(_("Unknown option."));
        }
        iValue = getExitCodeFromVariable(argIn);
        eval->setExitCode(iValue);
        eval->setState(NLS_STATE_FORCE_QUIT);
    } break;
    default: {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } break;
    }
    return retval;
}
//=============================================================================
int
getExitCodeFromVariable(const ArrayOfVector& argIn)
{
    int iValue = 0;
    if ((argIn[0].isNumeric() || argIn[0].isLogical()) && argIn[0].isScalar()) {
        // quit code
        double dValue = argIn[0].getContentAsDoubleScalar();
        iValue = static_cast<int>(dValue);
#ifndef _MSC_VER
        if (iValue < 0 || iValue > 255) {
            Error(_("Value between 0 and 255 expected."));
        }
#endif
        if (static_cast<double>(iValue) != dValue) {
            Error(ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED);
        }
    } else {
        Error(_("Unknown option."));
    }
    return iValue;
}
//=============================================================================
