//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xmlwriteBuiltin.hpp"
#include "XmlCore.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::XmlGateway::xmlwriteBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::wstring errorMessage;
    ArrayOfVector retval;
    if (argIn.size() == 1) {
        std::wstring text;
        if (!XmlDocumentToText(argIn[0], text, errorMessage)) {
            Error(L"Nelson:xmlwrite:writeFailed", errorMessage);
        }
        retval << ArrayOf::characterArray(text);
        return retval;
    }
    bool ok = XmlWriteDocument(argIn[0].asWideString(), argIn[1], errorMessage);
    if (!ok) {
        Error(L"Nelson:xmlwrite:writeFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
