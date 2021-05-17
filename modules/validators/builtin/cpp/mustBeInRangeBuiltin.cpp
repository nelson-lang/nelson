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
#include "mustBeInRangeBuiltin.hpp"
#include "ValidatorsInternal.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ValidatorsGateway::mustBeInRangeBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    // mustBeInRangeBuiltin(value, lower, upper)
    // mustBeInRangeBuiltin(value, lower, upper, pos)
    // mustBeInRangeBuiltin(value, lower, upper, flag1)
    // mustBeInRangeBuiltin(value, lower, upper, flag1, pos)
    // mustBeInRangeBuiltin(value, lower, upper, flag1, flag2)
    // mustBeInRangeBuiltin(value, lower, upper, flag1, flag2, pos)

    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 3, 6);
    int argPos = -1;
    std::wstring boundflag1 = L"";
    std::wstring boundflag2 = L"";
    switch (argIn.size()) {
    case 4: {
        ArrayOf param4 = argIn[3];
        if (param4.isCharacterArray()) {
            boundflag1 = param4.getContentAsWideString();
        } else {
            argPos = param4.getContentAsInteger32Scalar();
            if (argPos < 1) {
                Error(_W("The last argument must be a positive integer."));
            }
        }
    } break;
    case 5: {
        boundflag1 = argIn[3].getContentAsWideString();
        ArrayOf param5 = argIn[4];
        if (param5.isCharacterArray()) { 
          boundflag2 = param5.getContentAsWideString();
        } else {
            argPos = param5.getContentAsInteger32Scalar();
            if (argPos < 1) {
                Error(_W("The last argument must be a positive integer."));
            }
        }
    } break;
    case 6: {
        boundflag1 = argIn[3].getContentAsWideString();
        boundflag2 = argIn[4].getContentAsWideString();
        ArrayOf param6 = argIn[5];
        argPos = param6.getContentAsInteger32Scalar();
        if (argPos < 1) {
            Error(_W("The last argument must be a positive integer."));
        }
    } break;
    }
    mustBeInRange(argIn[0], argIn[1], argIn[2], boundflag1, boundflag2, argPos, true);
    return retval;
}
//=============================================================================
