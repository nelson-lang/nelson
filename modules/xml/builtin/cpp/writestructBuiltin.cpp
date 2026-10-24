//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "writestructBuiltin.hpp"
#include "XmlBuiltinHelpers.hpp"
#include "XmlCore.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::XmlGateway::writestructBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 0);
    if (((argIn.size() - 2) % 2) != 0) {
        Error(L"Nelson:xml:nameValueNotPaired", L"Name-value arguments must be paired.");
    }
    XmlStructWriteOptions options;
    for (size_t k = 2; k < argIn.size(); k += 2) {
        std::string name = normalizedXmlNameValueName(argIn[k]);
        if (name == "filetype") {
            options.fileType = xmlOptionString(argIn[k + 1]);
        } else if (name == "structnodename") {
            options.structNodeName = argIn[k + 1].asUtf8String();
        } else if (name == "attributesuffix") {
            options.attributeSuffix = argIn[k + 1].asUtf8String();
        } else if (name == "prettyprint") {
            options.prettyPrint = argIn[k + 1].asLogicalScalar();
        } else {
            Error(L"Nelson:xml:propertyNotSupported", L"Property name not supported.");
        }
    }
    std::wstring errorMessage;
    bool ok = XmlWriteStruct(argIn[0], argIn[1].asWideString(), options, errorMessage);
    if (!ok) {
        Error(L"Nelson:writestruct:writeFailed", errorMessage);
    }
    return {};
}
//=============================================================================
