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
#include "webRESTBuiltin.hpp"
#include "Error.hpp"
#include "WebREST.hpp"
#include "ClassName.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
// webREST is an private function not documented used by
// webwrite
// filename = webREST(url, data, filename, struct(n1, v1, ..., nN, vN), options)
// websave, webread
// filename = webREST(url, [], filename, struct(n1, v1, ..., nN, vN), options)
//=============================================================================
ArrayOfVector
Nelson::WebtoolsGateway::webRESTBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    if (nLhs > 1) {
        Error(ERROR_WRONG_NUMBERS_OUTPUT_ARGS);
    }
    if (argIn.size() != 5) {
        Error(ERROR_WRONG_NUMBERS_INPUT_ARGS);
    }
    ArrayOf param1 = argIn[0];
    std::wstring url = param1.getContentAsWideString();
    ArrayOf param2 = argIn[1];
    std::wstring data;
    if (!param2.isEmpty()) {
        data = param2.getContentAsWideString();
    }
    ArrayOf param3 = argIn[2];
    std::wstring filename = param3.getContentAsWideString();
    ArrayOf param4 = argIn[3];
    if (!param4.isStruct()) {
        Error(_W("Wrong type for argument #4. struct expected."));
    }
    if (!param4.isScalar()) {
        Error(_W("Wrong size for argument #4. scalar struct expected."));
    }
    stringVector names = param4.getFieldNames();
    ArrayOfVector values;
    for (size_t k = 0; k < names.size(); k++) {
        ArrayOf paramValue = param4.getField(names[k]);
        if (paramValue.isSparse()) {
            Error(_W("Sparse not supported."));
        }
        if (paramValue.isCharacterArray() || paramValue.isStringArray()) {
            std::wstring val = paramValue.getContentAsWideString();
            values.push_back(ArrayOf::characterArrayConstructor(val));
        } else if (paramValue.isLogical() || paramValue.isNumeric()) {
            values.push_back(paramValue);
        } else {
            Error(_W("Type not supported. Only char, string, numeric or logical allowed."));
        }
    }
    ArrayOf param5 = argIn[4];
    if (ClassName(param5) != "weboptions") {
        Error(_W("Wrong type for argument #5. weboptions object expected."));
    }
    WebOptions options(param5);
    std::wstring fullFilename
        = WebREST(url, data, filename, names, values, options, eval->haveEventsLoop());
    retval.push_back(ArrayOf::characterArrayConstructor(fullFilename));
    return retval;
}
//=============================================================================
