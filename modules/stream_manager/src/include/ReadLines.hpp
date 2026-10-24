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
#include "ArrayOf.hpp"
#include "Types.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class ReadLinesEmptyLineRule
{
    READ,
    SKIP,
    ERROR_ON_EMPTY
};
//=============================================================================
enum class ReadLinesWhitespaceRule
{
    PRESERVE,
    TRIM,
    TRIM_LEADING,
    TRIM_TRAILING
};
//=============================================================================
struct ReadLinesOptions
{
    // Line terminators; at each position the longest matching one is used.
    wstringVector lineEndings = { L"\n", L"\r", L"\r\n" };
    // Characters removed by the whitespace rule and ignored to detect empty lines.
    std::wstring whitespace = L" \b\t";
    ReadLinesEmptyLineRule emptyLineRule = ReadLinesEmptyLineRule::READ;
    ReadLinesWhitespaceRule whitespaceRule = ReadLinesWhitespaceRule::PRESERVE;
    // "auto" (detected), "UTF-8" or a supported encoding name.
    std::wstring encoding = L"auto";
};
//=============================================================================
// Reads a text file and returns its lines as a N-by-1 string array.
NLSSTREAM_MANAGER_IMPEXP ArrayOf
ReadLines(const std::wstring& filename, const ReadLinesOptions& options);
//=============================================================================
// Splits a text into a N-by-1 string array of lines.
NLSSTREAM_MANAGER_IMPEXP ArrayOf
SplitTextLines(const std::wstring& text, const ReadLinesOptions& options);
//=============================================================================
} // namespace Nelson
//=============================================================================
