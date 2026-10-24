//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xmlreadBuiltin.hpp"
#include "XmlBuiltinHelpers.hpp"
#include "XmlCore.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::XmlGateway::xmlreadBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 2);
    if (((argIn.size() - 1) % 2) != 0) {
        Error(L"Nelson:xml:nameValueNotPaired", L"Name-value arguments must be paired.");
    }
    XmlReadOptions options;
    for (size_t k = 1; k < argIn.size(); k += 2) {
        std::string name = normalizedXmlNameValueName(argIn[k]);
        if (name == "allowdoctype") {
            options.allowDoctype = argIn[k + 1].asLogicalScalar();
        } else if (name == "xmlengine") {
            options.xmlEngine = xmlOptionString(argIn[k + 1]);
            if (options.xmlEngine != "auto" && options.xmlEngine != "maxp"
                && options.xmlEngine != "jaxp") {
                Error(L"Nelson:xmlread:engineNotSupported", L"XMLEngine not supported.");
            }
        } else {
            Error(L"Nelson:xml:propertyNotSupported", L"Property name not supported.");
        }
    }
    std::wstring errorMessage;
    ArrayOf result = XmlReadDocument(argIn[0].asWideString(), options, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:xmlread:readFailed", errorMessage);
    }
    ArrayOfVector retval;
    retval << result;
    if (nLhs > 1) {
        retval << ArrayOf::empty();
    }
    return retval;
}
//=============================================================================
