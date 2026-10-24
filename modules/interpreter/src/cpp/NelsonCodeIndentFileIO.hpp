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
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>
//=============================================================================
namespace Nelson::codeindent_detail {
//=============================================================================
static bool
readTextFile(const std::filesystem::path& path, std::string& content)
{
    std::ifstream stream(path, std::ios::binary);
    if (!stream.good()) {
        return false;
    }
    std::ostringstream buffer;
    buffer << stream.rdbuf();
    content = buffer.str();
    return stream.good() || stream.eof();
}
//=============================================================================
static bool
writeTextFile(const std::filesystem::path& path, const std::string& content)
{
    std::ofstream stream(path, std::ios::binary | std::ios::trunc);
    if (!stream.good()) {
        return false;
    }
    stream << content;
    return stream.good();
}
//=============================================================================
static std::string
normalizeTextLineEndings(const std::string& content)
{
    std::string normalized;
    normalized.reserve(content.size());
    for (size_t k = 0; k < content.size(); ++k) {
        if (content[k] == '\r') {
            if (k + 1 < content.size() && content[k + 1] == '\n') {
                ++k;
            }
            normalized.push_back('\n');
        } else {
            normalized.push_back(content[k]);
        }
    }
    return normalized;
}
//=============================================================================
} // namespace Nelson::codeindent_detail
//=============================================================================
