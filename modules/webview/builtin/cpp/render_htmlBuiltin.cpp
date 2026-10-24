//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "render_htmlBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "RenderHtml.hpp"
#include "i18n.hpp"
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::WebviewGateway::render_htmlBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    ArrayOf param1 = argIn[0];
    if (!(param1.isRowVectorCharacterArray() || param1.isScalarStringArray())) {
        Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1), L"string");
    }

    std::wstring renderedHtml;
    std::wstring error;
    if (!RenderHtmlWithWebview(param1.asWideString(), renderedHtml, error)) {
        if (error.empty()) {
            error = resolveMessageTemplate(
                L"Nelson:webview:htmlRenderingFails", L"HTML rendering fails.");
        }
        Error(L"Nelson:webview:renderFailed", error);
    }

    ArrayOfVector retval;
    retval << ArrayOf::characterArray(renderedHtml);
    return retval;
}
//=============================================================================
