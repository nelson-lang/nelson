//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS /* _wfopen */
#endif
//=============================================================================
#include <cstdio>
#include <climits>
#include "JsonFileReader.hpp"
#include "characters_encoding.hpp"
#include "ErrorID.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static bool
fileLength(std::FILE* fp, size_t& len);
//=============================================================================
bool
readJsonFile(
    const std::wstring& filename, simdjson::padded_string& content, std::string& errorMessage)
{
#ifdef _MSC_BUILD
    std::FILE* fp = _wfopen(filename.c_str(), L"rb");
#else
    std::FILE* fp = fopen(wstring_to_utf8(filename).c_str(), "rb");
#endif
    if (fp == nullptr) {
        errorMessage = resolveMessageTemplate(
            "Nelson:jsondecode:cannotOpenFile", "Impossible to open file.");
        return false;
    }
    size_t len = 0;
    if (!fileLength(fp, len)) {
        std::fclose(fp);
        errorMessage = resolveMessageTemplate(
            "Nelson:jsondecode:cannotOpenFile", "Impossible to open file.");
        return false;
    }
    simdjson::padded_string buffer(len);
    if (buffer.data() == nullptr) {
        std::fclose(fp);
        errorMessage = resolveMessageTemplate(
            "Nelson:jsondecode:fileTooBig", "File too big to load in memory.");
        return false;
    }
    std::rewind(fp);
    size_t bytes_read = std::fread(buffer.data(), 1, len, fp);
    if (std::fclose(fp) != 0 || bytes_read != len) {
        errorMessage = resolveMessageTemplate(
            "Nelson:jsondecode:cannotReadFile", "Impossible to read file.");
        return false;
    }
    content = std::move(buffer);
    return true;
}
//=============================================================================
static bool
fileLength(std::FILE* fp, size_t& len)
{
#if SIMDJSON_VISUAL_STUDIO && !SIMDJSON_IS_32BITS
    if (_fseeki64(fp, 0, SEEK_END) < 0) {
        return false;
    }
    __int64 llen = _ftelli64(fp);
    if (llen == -1L) {
        return false;
    }
#else
    if (std::fseek(fp, 0, SEEK_END) < 0) {
        return false;
    }
    long llen = std::ftell(fp);
    if ((llen < 0) || (llen == LONG_MAX)) {
        return false;
    }
#endif
    len = static_cast<size_t>(llen);
    return true;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
