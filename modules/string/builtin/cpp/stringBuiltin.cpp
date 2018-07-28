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
#include "stringBuiltin.hpp"
#include "ClassName.hpp"
#include "Error.hpp"
#include "IsCellOfStrings.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::stringBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(eval, ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 1) {
        Error(eval, ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->overloadOnBasicTypes) {
        retval = OverloadFunction(eval, nLhs, argIn, "string", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse()) {
            retval = OverloadFunction(eval, nLhs, argIn, "string", bSuccess);
            if (!bSuccess) {
                Error(eval, _W("Attempt to convert to unimplemented sparse type."));
            }
            return retval;
        }
        switch (argIn[0].getDataClass()) {
        case NLS_CHAR: {
            if (argIn[0].isColonVectorCharacterArray()) {
                std::wstring str = argIn[0].getContentAsWideString();
                retval.push_back(ArrayOf::stringArrayConstructor(str));
			}
		} break;
        default:
            Error(eval, _W("Cannot convert to string array."));
        }
    }
    return retval;
}
//=============================================================================
