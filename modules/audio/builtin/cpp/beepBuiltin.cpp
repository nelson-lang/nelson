//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// Alternatively, you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of
// the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this program. If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#include "beepBuiltin.hpp"
#include "Beep.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::AudioGateway::beepBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
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
