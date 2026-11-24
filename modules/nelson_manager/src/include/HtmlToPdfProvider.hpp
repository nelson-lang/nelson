//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include <functional>
#include <string>
#include "nlsNelson_manager_exports.h"
//=============================================================================
// HTML to PDF conversion is served by the web stack (nelson-webview), which
// drives a headless Chromium browser to print the page. The webview module
// registers a provider; help_tools calls it through this seam. The seam carries
// no toolkit dependency and lives in nelson_manager so help_tools does not link
// the desktop web stack.
//=============================================================================
namespace Nelson {
//=============================================================================
// Converts the HTML source file into a PDF file. Returns true on success; on
// failure it fills errorMessage with a localized description.
//=============================================================================
using HtmlToPdfProviderFn = std::function<bool(
    const std::wstring& htmlFilePath, const std::wstring& pdfFilePath, std::wstring& errorMessage)>;
//=============================================================================
NLSNELSON_MANAGER_IMPEXP void
setHtmlToPdfProvider(HtmlToPdfProviderFn provider);
NLSNELSON_MANAGER_IMPEXP bool
hasHtmlToPdfProvider();
// Returns true when the provider handled the conversion (PDF written).
NLSNELSON_MANAGER_IMPEXP bool
runHtmlToPdf(
    const std::wstring& htmlFilePath, const std::wstring& pdfFilePath, std::wstring& errorMessage);
//=============================================================================
} // namespace Nelson
//=============================================================================
