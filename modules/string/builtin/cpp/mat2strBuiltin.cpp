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
#include "mat2strBuiltin.hpp"
#include "Error.hpp"
#include "MatrixToString.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::mat2strBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() == 0 || argIn.size() > 3) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "mat2str", bSuccess);
    }
    if (!bSuccess) {
        indexType defautPrecision = 15;
        bool withClass = false;
        if (argIn.size() == 3) {
            ArrayOf param3 = argIn[2];
            ArrayOf param2 = argIn[1];
            if (param3.isRowVectorCharacterArray()) {
                std::wstring str = param3.getContentAsWideString();
                if (str == L"class") {
                    withClass = true;
                } else {
                    Error(_W("'class' argument expected."));
                }
            }
            if (param2.isNumeric()) {
                defautPrecision = param2.getContentAsScalarIndex();
            } else {
                Error(_W("Second input argument must be a real positive integers."));
            }
        } else if (argIn.size() == 2) {
            ArrayOf param2 = argIn[1];
            if (param2.isRowVectorCharacterArray()) {
                std::wstring str = param2.getContentAsWideString();
                if (str == L"class") {
                    defautPrecision = 15;
                    withClass = true;
                } else {
                    Error(_W("'class' argument expected."));
                }
            } else if (param2.isNumeric()) {
                defautPrecision = param2.getContentAsScalarIndex();
                withClass = false;
            } else {
                Error(_W("Second input argument must be a real positive integers."));
            }
        } else // argIn.size() == 1
        {
            defautPrecision = 15;
            withClass = false;
        }
        ArrayOf A = argIn[0];
        bool canBeConvert = A.isNumeric() || A.isLogical() || A.isCharacterArray();
        if (!canBeConvert) {
            retval = OverloadFunction(eval, nLhs, argIn, "mat2str", bSuccess);
            if (!bSuccess) {
                Error(_W("An numeric matrix expected."));
            }
            return retval;
        }
        if (A.isSparse()) {
            retval = OverloadFunction(eval, nLhs, argIn, "mat2str", bSuccess);
            if (!bSuccess) {
                Error(ERROR_TYPE_NOT_SUPPORTED);
            }
            return retval;
        }
        std::wstring res = MatrixToString(A, defautPrecision, withClass);
        retval.push_back(ArrayOf::characterArrayConstructor(res));
    }
    return retval;
}
//=============================================================================
