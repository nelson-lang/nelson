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
#include <vector>
#include "TextFileWriter.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class WriteLinesTrailingRule
{
    AUTO,
    ALWAYS,
    NEVER
};
//=============================================================================
// Platform line terminator: CR LF on Windows, LF elsewhere.
NLSSTREAM_MANAGER_IMPEXP std::wstring
nativeLineEnding();
//=============================================================================
struct WriteLinesOptions
{
    std::wstring lineEnding = nativeLineEnding();
    // "UTF-8" or a supported encoding name.
    std::string encoding = "UTF-8";
    bool append = false;
    WriteLinesTrailingRule trailingRule = WriteLinesTrailingRule::AUTO;
};
//=============================================================================
// Joins the lines with the line ending and writes them to a text file.
NLSSTREAM_MANAGER_IMPEXP void
WriteLines(const std::vector<TextSpan>& lines, const std::wstring& filename,
    const WriteLinesOptions& options);
//=============================================================================
} // namespace Nelson
//=============================================================================
