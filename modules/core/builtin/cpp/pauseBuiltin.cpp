//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "pauseBuiltin.hpp"
#include "Error.hpp"
#include "OverloadRequired.hpp"
#include "Pause.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static bool pauseOn = true;
//=============================================================================
ArrayOfVector
Nelson::CoreGateway::pauseBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 1);

    if (argIn.size() == 0) {
        if (nLhs == 1) {
            if (pauseOn) {
                retval << ArrayOf::characterArrayConstructor(L"on");
            } else {
                retval << ArrayOf::characterArrayConstructor(L"off");
            }
            return retval;
        }
        Interface* io = eval->getInterface();
        if (io != nullptr) {
            io->getInput(L"");
        }
    } else {
        ArrayOf param1 = argIn[0];
        if (param1.isRowVectorCharacterArray()) {
            std::wstring arg1Value = param1.getContentAsWideString();
            if (arg1Value == L"on" || arg1Value == L"off" || arg1Value == L"query") {
                bool previousValue = pauseOn;
                if (arg1Value == L"on") {
                    pauseOn = true;
                    if (nLhs == 1) {
                        if (previousValue) {
                            retval << ArrayOf::characterArrayConstructor(L"on");
                        } else {
                            retval << ArrayOf::characterArrayConstructor(L"off");
                        }
                        return retval;
                    }
                } else if (arg1Value == L"off") {
                    pauseOn = false;
                    if (nLhs == 1) {
                        if (previousValue) {
                            retval << ArrayOf::characterArrayConstructor(L"on");
                        } else {
                            retval << ArrayOf::characterArrayConstructor(L"off");
                        }
                        return retval;
                    }
                } else {
                    if (pauseOn) {
                        retval << ArrayOf::characterArrayConstructor(L"on");
                    } else {
                        retval << ArrayOf::characterArrayConstructor(L"off");
                    }
                    return retval;
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_1_VALUE);
            }
        } else if (param1.isNumeric()) {
            if (nLhs == 1) {
                if (pauseOn) {
                    retval << ArrayOf::characterArrayConstructor(L"on");
                } else {
                    retval << ArrayOf::characterArrayConstructor(L"off");
                }
                return retval;
            }
            double val = param1.getContentAsDoubleScalar();
            if (!pauseOn) {
                return retval;
            }
            Pause(eval, val);
        } else {
            OverloadRequired("pause");
        }
    }
    return retval;
}
//=============================================================================
