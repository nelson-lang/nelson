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
#include "computerBuiltin.hpp"
#include "Error.hpp"
#include "PlatformInfo.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::computerBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.empty()) {
        nargoutcheck(nLhs, 3);
        if (nLhs >= 0) {
            retval << ArrayOf::characterArrayConstructor(GetArchitectureType());
        }
        if (nLhs > 1) {
            retval << ArrayOf::doubleConstructor(GetMaxArrayOfSizeSupported());
        }
        if (nLhs > 2) {
            if (IsBigEndian()) {
                retval << ArrayOf::characterArrayConstructor(L"B");
            } else {
                retval << ArrayOf::characterArrayConstructor(L"L");
            }
        }
    } else if (argIn.size() == 1) {
        nargoutcheck(nLhs, 0, 1);
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring warg = argIn[0].getContentAsWideString();
        if (warg.compare(L"arch") != 0) {
            Error(_W("Unknown command option."));
        }
        retval << ArrayOf::characterArrayConstructor(GetArchitecture());
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
