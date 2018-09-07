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
#include "jsonencodeBuiltin.hpp"
#include "Error.hpp"
#include "JsonEncode.hpp"
#include "OverloadFunction.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::JsonGateway::jsonencodeBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (!((argIn.size() == 1 || argIn.size() == 3))) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    // Call overload if it exists
    bool bSuccess = false;
    if (eval->mustOverloadBasicTypes()) {
        retval = OverloadFunction(eval, nLhs, argIn, "jsonencode", bSuccess);
    }
    if (!bSuccess) {
        if (argIn[0].isSparse() || argIn[0].isCell() || argIn[0].isHandle() || argIn[0].isStruct()
            || argIn[0].isClassStruct()) {
            retval = OverloadFunction(eval, nLhs, argIn, "jsonencode", bSuccess);
            if (bSuccess) {
                return retval;
            }
        }
        bool convertNanInf = false;
        ArrayOf param1 = argIn[0];
        if (argIn.size() == 3) {
            ArrayOf param2 = argIn[1];
            ArrayOf param3 = argIn[2];
            std::wstring fieldname = param2.getContentAsWideString();
            if (fieldname != L"ConvertInfAndNaN") {
                Error(_W("Wrong value for argument #2: 'ConvertInfAndNaN' expected."));
            }
            logical fieldvalue = param3.getContentAsLogicalScalar();
            convertNanInf = (fieldvalue != 0);
        }
        std::wstring errorMessage;
        ArrayOf res = jsonEncode(param1, convertNanInf, errorMessage);
        if (!errorMessage.empty()) {
            Error(errorMessage);
        }
        retval.push_back(res);
    }
    return retval;
}
//=============================================================================
