//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "webRESTBuiltin.hpp"
#include "Error.hpp"
#include "i18n.hpp"
#include "WebREST.hpp"
#include "ClassName.hpp"
#include "InputOutputArgumentsCheckers.hpp"
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
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 5, 5);
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
    for (auto& name : names) {
        ArrayOf paramValue = param4.getField(name);
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
    bool haveEventsLoop = eval->haveEventsLoop();
    size_t evaluatorID = eval->getID();
    std::wstring fullFilename
        = WebREST(url, data, filename, names, values, options, haveEventsLoop, evaluatorID);
    retval << ArrayOf::characterArrayConstructor(fullFilename);
    return retval;
}
//=============================================================================
