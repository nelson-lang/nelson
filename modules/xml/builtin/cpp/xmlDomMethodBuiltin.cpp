//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xmlDomMethodBuiltin.hpp"
#include "XmlCore.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::XmlGateway::xmlDomMethodBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1);
    nargoutcheck(nLhs, 0, 1);
    std::string method = wstring_to_utf8(argIn[0].asWideString());
    ArrayOfVector args;
    for (size_t k = 1; k < argIn.size(); k++) {
        args << argIn[k];
    }
    std::wstring errorMessage;
    ArrayOfVector result = XmlDomMethod(method, args, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:xmldom:methodFailed", errorMessage);
    }
    return result;
}
//=============================================================================
