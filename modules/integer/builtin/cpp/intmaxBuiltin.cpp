//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0) {
        retval.push_back(ArrayOf::int32Constructor(2147483647));
    } else {
        ArrayOf param1 = argIn[0];
        if (!param1.isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring classInt = param1.getContentAsWideString();
        if (classInt == L"int8") {
            retval.push_back(ArrayOf::int8Constructor(127));
        } else if (classInt == L"uint8") {
            retval.push_back(ArrayOf::uint8Constructor(255));
        } else if (classInt == L"int16") {
            retval.push_back(ArrayOf::int16Constructor(32767));
        } else if (classInt == L"uint16") {
            retval.push_back(ArrayOf::uint16Constructor(65535));
        } else if (classInt == L"int32") {
            retval.push_back(ArrayOf::int32Constructor(2147483647));
        } else if (classInt == L"uint32") {
            retval.push_back(ArrayOf::uint32Constructor(4294967295));
        } else if (classInt == L"int64") {
            retval.push_back(ArrayOf::int64Constructor(9223372036854775807));
        } else if (classInt == L"uint64") {
            retval.push_back(ArrayOf::uint64Constructor(std::numeric_limits<uint64>::max()));
        } else {
            Error(_W("The name of an integer class expected."));
        }
    }
    return retval;
}
//=============================================================================
