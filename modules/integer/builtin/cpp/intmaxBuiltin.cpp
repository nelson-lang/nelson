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
#include "intmaxBuiltin.hpp"
#include "Error.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::IntegerGateway::intmaxBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 0, 1);
    nargoutcheck(nLhs, 0, 1);
    if (argIn.empty()) {
        retval << ArrayOf::int32Constructor(std::numeric_limits<int32>::max());
    } else {
        ArrayOf param1 = argIn[0];
        if (!param1.isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring classInt = param1.getContentAsWideString();
        if (classInt == L"int8") {
            retval << ArrayOf::int8Constructor(std::numeric_limits<int8>::max());
        } else if (classInt == L"uint8") {
            retval << ArrayOf::uint8Constructor(std::numeric_limits<uint8>::max());
        } else if (classInt == L"int16") {
            retval << ArrayOf::int16Constructor(std::numeric_limits<int16>::max());
        } else if (classInt == L"uint16") {
            retval << ArrayOf::uint16Constructor(std::numeric_limits<uint16>::max());
        } else if (classInt == L"int32") {
            retval << ArrayOf::int32Constructor(std::numeric_limits<int32>::max());
        } else if (classInt == L"uint32") {
            retval << ArrayOf::uint32Constructor(std::numeric_limits<uint32>::max());
        } else if (classInt == L"int64") {
            retval << ArrayOf::int64Constructor(std::numeric_limits<int64>::max());
        } else if (classInt == L"uint64") {
            retval << ArrayOf::uint64Constructor(std::numeric_limits<uint64>::max());
        } else {
            Error(_W("The name of an integer class expected."));
        }
    }
    return retval;
}
//=============================================================================
