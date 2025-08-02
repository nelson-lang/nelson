//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "beepBuiltin.hpp"
#include "Beep.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::beepBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 0, 1);
    if (argIn.empty()) {
        if (nLhs == 0) {
            beep();
        }
    } else {
        ArrayOf param1 = argIn[0];
        std::wstring str = param1.getContentAsWideString();
        if (str == L"on" || str == L"off") {
            if (str == L"on") {
                setBeepOn();
            } else {
                setBeepOff();
            }
        } else {
            Error(_W("Wrong value for #1 argument."));
        }
    }
    ArrayOfVector retval(nLhs);
    if (nLhs > 0) {
        bool beepState = getBeepState();
        if (beepState) {
            retval << ArrayOf::characterArrayConstructor(L"on");
        } else {
            retval << ArrayOf::characterArrayConstructor(L"off");
        }
    }
    return retval;
}
//=============================================================================
