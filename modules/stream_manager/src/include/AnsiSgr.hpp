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
#include "Interface.hpp"
#include "nlsStream_manager_exports.h"
#include <string>
#include <vector>
//=============================================================================
namespace Nelson {
//=============================================================================
struct NLSSTREAM_MANAGER_IMPEXP AnsiStyledSpan
{
    size_t first = 0;
    size_t length = 0;
    TextStyle style;
};
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP bool
containsAnsiEscape(const std::wstring& text);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP size_t
ansiVisibleLength(const std::wstring& text);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP std::wstring
stripAnsiSgr(const std::wstring& text);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP std::string
buildAnsiStyledMessage(const std::string& text, const TextStyle& style);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP std::string
buildOsc8Hyperlink(const std::string& uri, const std::string& label);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP std::string
buildExpandableTextLink(long long id, const std::string& label);
//=============================================================================
NLSSTREAM_MANAGER_IMPEXP std::vector<AnsiStyledSpan>
parseAnsiStyledSpans(const std::wstring& text);
//=============================================================================
} // namespace Nelson
//=============================================================================
