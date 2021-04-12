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
#include "mustBeVectorBuiltin.hpp"
#include "ValidatorsInternal.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ValidatorsGateway::mustBeVectorBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 3);
    bool allowsAllEmpties = false;
    int argPos = -1;

    switch (argIn.size()) {
    case 2: {
        if (argIn[1].isNumeric() && argIn[1].isScalar()) {
            ArrayOf param2 = argIn[1];
            argPos = param2.getContentAsInteger32Scalar();
            if (argPos < 1) {
                Error(_W("The last argument must be a positive integer."));
            }
        } else if (argIn[1].isRowVectorCharacterArray()
            || (argIn[1].isStringArray() && argIn[1].isScalar())) {
            std::wstring flag = argIn[1].getContentAsWideString();
            if (flag != L"allows-all-empties") {
                Error(_W("The last argument must be a positive integer or 'allows-all-empties'."));
            }
            allowsAllEmpties = true;
        } else {
            Error(_W("The last argument must be a positive integer or 'allows-all-empties'."));
        }
    } break;
    case 3: {
        std::wstring flag = argIn[1].getContentAsWideString();
        if (flag != L"allows-all-empties") {
            Error(_W("Second argument must be a 'allows-all-empties'."));
        }
        ArrayOf param2 = argIn[1];
        argPos = param2.getContentAsInteger32Scalar();
        if (argPos < 1) {
            Error(_W("The last argument must be a positive integer."));
        }
    } break;
    }
    mustBeVector(argIn[0], allowsAllEmpties, argPos, true);
    return retval;
}
//=============================================================================
