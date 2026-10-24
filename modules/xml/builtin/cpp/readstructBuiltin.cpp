//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "readstructBuiltin.hpp"
#include "XmlBuiltinHelpers.hpp"
#include "XmlCore.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
static void
parseNamespaces(const ArrayOf& value, XmlStructReadOptions& options)
{
    stringVector values = value.asUtf8StringVector(false);
    if ((values.size() % 2) != 0) {
        Error(L"Nelson:readstruct:invalidRegisteredNamespaces",
            L"RegisteredNamespaces must contain prefix and URL pairs.");
    }
    for (size_t k = 0; k < values.size(); k += 2) {
        options.registeredNamespaces.push_back({ values[k], values[k + 1] });
    }
}
//=============================================================================
ArrayOfVector
Nelson::XmlGateway::readstructBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    if (((argIn.size() - 1) % 2) != 0) {
        Error(L"Nelson:xml:nameValueNotPaired", L"Name-value arguments must be paired.");
    }
    XmlStructReadOptions options;
    for (size_t k = 1; k < argIn.size(); k += 2) {
        std::string name = normalizedXmlNameValueName(argIn[k]);
        if (name == "filetype") {
            options.fileType = xmlOptionString(argIn[k + 1]);
        } else if (name == "structnodename") {
            options.structNodeName = argIn[k + 1].asWideString();
        } else if (name == "structselector") {
            options.structSelector = argIn[k + 1].asWideString();
        } else if (name == "importattributes") {
            options.importAttributes = argIn[k + 1].asLogicalScalar();
        } else if (name == "attributesuffix") {
            options.attributeSuffix = argIn[k + 1].asUtf8String();
        } else if (name == "registerednamespaces") {
            parseNamespaces(argIn[k + 1], options);
        } else if (name == "datelocale") {
            (void)argIn[k + 1].asWideString();
        } else {
            Error(L"Nelson:xml:propertyNotSupported", L"Property name not supported.");
        }
    }
    std::wstring errorMessage;
    ArrayOf result = XmlReadStruct(argIn[0].asWideString(), options, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:readstruct:readFailed", errorMessage);
    }
    ArrayOfVector retval;
    retval << result;
    return retval;
}
//=============================================================================
