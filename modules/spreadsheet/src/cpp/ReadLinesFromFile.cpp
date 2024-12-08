//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <algorithm>
#include "ReadLinesFromFile.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
std::stringstream
readLinesFromFile(const std::wstring& filename, const detectImportOptions& options)
{
    std::ifstream file;
#ifdef _MSC_VER
    file.open(filename);
#else
    file.open(wstring_to_utf8(filename));
#endif

    std::string line;
    int currentLine = 1;
    std::stringstream normalizedStream;

    while (currentLine < (int)options.DataLines[0] && std::getline(file, line)) {
        currentLine++;
    }

    auto normalizeLineEnding = [](const std::string& inputLine) -> std::string {
        std::string normalized = inputLine;
        normalized.erase(std::remove(normalized.begin(), normalized.end(), '\r'), normalized.end());
        return normalized;
    };

    if (std::isinf(options.DataLines[1])) {
        while (std::getline(file, line)) {
            normalizedStream << normalizeLineEnding(line) << '\n';
            currentLine++;
        }
    } else {
        while (currentLine <= (int)options.DataLines[1] && std::getline(file, line)) {
            normalizedStream << normalizeLineEnding(line) << '\n';
            currentLine++;
        }
    }
    return normalizedStream;
}
//=============================================================================
}
//=============================================================================
