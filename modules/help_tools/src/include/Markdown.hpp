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
#include "nlsHelp_tools_exports.h"
#include <string>
//=============================================================================
namespace Nelson {
NLSHELP_TOOLS_IMPEXP bool
MarkdownFile(const std::wstring& inputMarkdownString, const std::wstring& outputHtmlString);
NLSHELP_TOOLS_IMPEXP bool
MarkdownString(const std::wstring& inputMarkdownString, std::wstring& outputHtmlString);
NLSHELP_TOOLS_IMPEXP bool
MarkdownString(const std::string& inputMarkdownString, std::string& outputHtmlString);

} // namespace Nelson
//=============================================================================
