//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "htmltopdfBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "HtmlToPdf.hpp"
#include "HtmlToPdfProvider.hpp"
#include "FileSystemWrapper.hpp"
#include "InputOutputArgumentsCheckers.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::HelpToolsGateway::htmltopdfBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargincheck(argIn, 2, 2);
    nargoutcheck(nLhs, 0, 0);
    ArrayOf arg1 = argIn[0];
    ArrayOf arg2 = argIn[1];
    if (!arg1.isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }
    if (!arg2.isRowVectorCharacterArray()) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (2), L"string");
    }
    std::wstring param1 = arg1.asWideString();
    std::wstring param2 = arg2.asWideString();
    if (!FileSystemWrapper::Path::is_regular_file(param1)) {
        Error(L"Nelson:invalidValue", L"Wrong value for #%d argument.", (1));
    }
    // The conversion is served by the web stack (headless browser) through the
    // nelson_manager seam. Without a provider (e.g. nelson-cli, which does not
    // load the webview module) it is unavailable; otherwise report the failure.
    std::wstring errorMessage;
    if (!HtmlFileToPdfFile(param1, param2, errorMessage)) {
        if (!hasHtmlToPdfProvider()) {
            Error(L"Nelson:htmltopdf:notAvailable", L"HTML to PDF conversion is not available.");
        }
        if (errorMessage.empty()) {
            errorMessage = L"HTML to PDF conversion failed.";
        }
        Error(L"Nelson:htmltopdf:conversionFailed", errorMessage);
    }
    return retval;
}
//=============================================================================
