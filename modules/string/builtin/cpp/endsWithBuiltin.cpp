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
#include "endsWithBuiltin.hpp"
#include "StringEndsWith.hpp"
#include "Error.hpp"
#include "OverloadFunction.hpp"
#include "StringFormat.hpp"
#include "IsCellOfStrings.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::StringGateway::endsWithBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 2 && argIn.size() != 4) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "endsWith", bSuccess);
    }
    if (!bSuccess) {
        bool bCaseSensitive = true;
        ArrayOf A = argIn[0];
        ArrayOf B = argIn[1];
        if (A.isCharacterArray() || A.isStringArray() || IsCellOfString(A)) {
            if (argIn.size() == 4) {
                ArrayOf param3 = argIn[2];
                std::wstring fieldname = param3.getContentAsWideString();
                if (fieldname != L"IgnoreCase") {
                    Error(StringFormat(ERROR_WRONG_ARGUMENT_X_VALUE.c_str(), 3));
                }
                ArrayOf param4 = argIn[3];
                logical fieldvalue = param4.getContentAsLogicalScalar();
                bCaseSensitive = (fieldvalue == 0);
            }
            retval.push_back(StringEndsWith(A, B, bCaseSensitive));
        } else {
            retval = OverloadFunction(eval, nLhs, argIn, "count", bSuccess);
            if (!bSuccess) {
                Error(_W("char vector or cell of strings expected."));
            }
        }
    }
    return retval;
}
//=============================================================================
