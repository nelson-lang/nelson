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
#include <cstdint>
#include <string>
#include <vector>
#include "ArrayOf.hpp"
#include "nlsStream_manager_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
// Read-only view on the characters of one text element.
struct TextSpan
{
    const wchar_t* data;
    size_t length;
};
//=============================================================================
enum class TextWriteStatus
{
    OK,
    OPEN_FAILED,
    ENCODING_FAILED,
    WRITE_FAILED
};
//=============================================================================
// Views a char row, a string array or a cell of char rows in column order (missing is empty).
NLSSTREAM_MANAGER_IMPEXP bool
textSpans(const ArrayOf& value, std::vector<TextSpan>& spans);
//=============================================================================
// Appends wide text as UTF-8 (invalid surrogates become U+FFFD).
NLSSTREAM_MANAGER_IMPEXP void
appendUtf8(std::string& out, const wchar_t* text, size_t length);
//=============================================================================
// Converts UTF-8 bytes to an encoding name ("UTF-8" keeps the bytes).
NLSSTREAM_MANAGER_IMPEXP bool
encodeUtf8Text(const std::string& utf8, const std::string& encoding, std::string& encoded);
//=============================================================================
// Encodes UTF-8 bytes and writes them in one call, replacing or appending to the file.
NLSSTREAM_MANAGER_IMPEXP TextWriteStatus
writeUtf8TextFile(const std::wstring& filename, const std::string& utf8,
    const std::string& encoding, bool append);
//=============================================================================
// Size of a file (0 if missing) and whether its last bytes are the given suffix.
NLSSTREAM_MANAGER_IMPEXP bool
fileEndsWith(const std::wstring& filename, const std::string& suffix, uint64_t& fileSize);
//=============================================================================
} // namespace Nelson
//=============================================================================
