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
#include "ErrorID.hpp"
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
                retval << ArrayOf::characterArray("on");
            } else {
                retval << ArrayOf::characterArray("off");
            }
        }
    } else if (argIn.size() == 1) {
        if (argIn[0].isRowVectorCharacterArray()) {
            bool bMode = false;
            std::wstring arg = argIn[0].asWideString();
            if (arg == L"on") {
                bMode = true;
            } else if (arg == L"off") {
                bMode = false;
            } else {
                Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (1));
            }
            eval->setEchoMode(bMode);
        } else {
            Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
                L"string");
        }
    } else {
        Error(L"Nelson:tooManyInputs", L"Too many input arguments.");
    }
    return retval;
}
//=============================================================================
