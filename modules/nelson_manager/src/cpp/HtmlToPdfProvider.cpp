//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "HtmlToPdfProvider.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static HtmlToPdfProviderFn s_provider;
//=============================================================================
void
setHtmlToPdfProvider(HtmlToPdfProviderFn provider)
{
    s_provider = std::move(provider);
}
//=============================================================================
bool
hasHtmlToPdfProvider()
{
    return static_cast<bool>(s_provider);
}
//=============================================================================
bool
runHtmlToPdf(
    const std::wstring& htmlFilePath, const std::wstring& pdfFilePath, std::wstring& errorMessage)
{
    if (!s_provider) {
        return false;
    }
    return s_provider(htmlFilePath, pdfFilePath, errorMessage);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
