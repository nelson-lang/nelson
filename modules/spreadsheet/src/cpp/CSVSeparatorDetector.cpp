//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <fstream>
#include <unordered_map>
#include "CSVSeparatorDetector.hpp"
#include "i18n.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
char
CSVSeparatorDetector(const std::wstring& filename, std::wstring& errorMessage)
{
#ifdef _MSC_VER
    std::ifstream file(filename);
#else
    std::ifstream file(wstring_to_utf8(filename));
#endif
    if (!file.is_open()) {
        errorMessage = _W("Invalid filename.");
        return '\0';
    }

    std::string line;
    if (std::getline(file, line)) {
        std::unordered_map<char, int> separatorCount;

        for (char sep : { '\t', ',', ';', ':', '|', ' ' }) {
            separatorCount[sep] = 0;
        }

        for (char ch : line) {
            if (separatorCount.find(ch) != separatorCount.end()) {
                separatorCount[ch]++;
            }
        }

        char likelySeparator = '\0';
        int maxCount = 0;
        for (const auto& entry : separatorCount) {
            if (entry.second > maxCount) {
                likelySeparator = entry.first;
                maxCount = entry.second;
            }
        }

        return likelySeparator;
    }

    return '\0';
}
//=============================================================================
} // namespace Nelson
//=============================================================================
