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
#include "computerBuiltin.hpp"
#include "Error.hpp"
#include "PlatformInfo.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::OsFunctionsGateway::computerBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (argIn.size() == 0) {
        if (nLhs > 3) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (nLhs >= 0) {
            retval.push_back(ArrayOf::characterArrayConstructor(GetArchitectureType()));
        }
        if (nLhs > 1) {
            retval.push_back(ArrayOf::doubleConstructor(GetMaxArrayOfSizeSupported()));
        }
        if (nLhs > 2) {
            if (IsBigEndian()) {
                retval.push_back(ArrayOf::characterArrayConstructor(L"B"));
            } else {
                retval.push_back(ArrayOf::characterArrayConstructor(L"L"));
            }
        }
    } else if (argIn.size() == 1) {
        if (nLhs > 1) {
            Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
        }
        if (!argIn[0].isRowVectorCharacterArray()) {
            Error(ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED);
        }
        std::wstring warg = argIn[0].getContentAsWideString();
        if (warg.compare(L"arch") != 0) {
            Error(_W("Unknown command option."));
        }
        retval.push_back(ArrayOf::characterArrayConstructor(GetArchitecture()));
    } else {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    return retval;
}
//=============================================================================
