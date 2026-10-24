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
#include "ErrorID.hpp"
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
// [filename, contentType] = webREST(url, [], filename, struct(n1, v1, ..., nN, vN), options)
// contentType is the Content-Type header of the response ('' when absent).
//=============================================================================
ArrayOfVector
Nelson::WebtoolsGateway::webRESTBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 2);
    nargincheck(argIn, 5, 5);
    ArrayOf param1 = argIn[0];
    std::wstring url = param1.asWideString();
    ArrayOf param2 = argIn[1];
    std::wstring data;
    if (!param2.isEmpty()) {
        data = param2.asWideString();
    }
    ArrayOf param3 = argIn[2];
    std::wstring filename = param3.asWideString();
    ArrayOf param4 = argIn[3];
    if (!param4.isStruct()) {
        Error(L"Nelson:webREST:invalidArgument", L"Wrong type for argument #%d: %s expected.", 4,
            L"struct");
    }
    if (!param4.isScalar()) {
        Error(L"Nelson:webREST:optionsMustBeScalarStruct",
            L"Wrong size for argument #4. scalar struct expected.");
    }
    stringVector names = param4.fieldNames();
    ArrayOfVector values;
    for (auto& name : names) {
        ArrayOf paramValue = param4.field(name);
        if (paramValue.isSparse()) {
            Error(L"Nelson:webREST:sparseNotSupported", L"Sparse not supported.");
        }
        if (paramValue.isCharacterArray() || paramValue.isStringArray()) {
            std::wstring val = paramValue.asWideString();
            values.push_back(ArrayOf::characterArray(val));
        } else if (paramValue.isLogical() || paramValue.isNumeric()) {
            values.push_back(paramValue);
        } else {
            Error(L"Nelson:webREST:unsupportedDataType",
                L"Type not supported. Only char, string, numeric or logical allowed.");
        }
    }
    ArrayOf param5 = argIn[4];
    if (ClassName(param5) != "weboptions") {
        Error(L"Nelson:webREST:invalidArgument", L"Wrong type for argument #%d: %s expected.", 5,
            L"weboptions object");
    }
    WebOptions options(param5);
    bool haveEventsLoop = eval->haveEventsLoop();
    size_t evaluatorID = eval->getID();
    std::wstring contentType;
    std::wstring fullFilename = WebREST(
        url, data, filename, names, values, options, haveEventsLoop, evaluatorID, contentType);
    retval << ArrayOf::characterArray(fullFilename);
    if (nLhs > 1) {
        retval << ArrayOf::characterArray(contentType);
    }
    return retval;
}
//=============================================================================
