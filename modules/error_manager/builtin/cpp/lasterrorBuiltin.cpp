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
#include "lasterrorBuiltin.hpp"
#include "Error.hpp"
#include "ErrorToStruct.hpp"
#include "IsErrorStruct.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::ErrorManagerGateway::lasterrorBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() > 1) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    } else {
        if (argIn.size() == 1) {
            ArrayOf arg1 = argIn[0];
            if (arg1.isRowVectorCharacterArray()) {
                std::wstring str = arg1.getContentAsWideString();
                if (str == L"reset") {
                    eval->setLastErrorException(Exception());
                } else {
                    Error(_W("Wrong value for #2 input argument.") + _W("'reset' expected."));
                }
            } else if (arg1.isStruct()) {
                Exception e;
                if (IsErrorStruct(arg1, e)) {
                    eval->setLastErrorException(e);
                } else {
                    Error(ERROR_WRONG_ARGUMENT_2_VALUE);
                }
            } else {
                Error(ERROR_WRONG_ARGUMENT_2_TYPE
                    + _W("a structure or the string 'reset' expected."));
            }
        }
        Exception lasterror = eval->getLastErrorException();
        retval.push_back(ErrorToStruct(lasterror));
    }
    return retval;
}
//=============================================================================
