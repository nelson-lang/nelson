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
#include <string>
#include "nlsWeb_engine_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Converts an HTML file into a PDF file using a headless Chromium browser located
// by webui. Returns true on success; on failure it fills errorMessage.
//=============================================================================
NLSWEB_ENGINE_IMPEXP bool
HtmlFileToPdfWithWebview(
    const std::wstring& htmlFilePath, const std::wstring& pdfFilePath, std::wstring& errorMessage);
//=============================================================================
// Registers the HTML to PDF provider in nelson_manager so help_tools can reach
// the conversion without linking the web stack.
//=============================================================================
NLSWEB_ENGINE_IMPEXP void
RegisterHtmlToPdfProvider();
//=============================================================================
} // namespace Nelson
//=============================================================================
