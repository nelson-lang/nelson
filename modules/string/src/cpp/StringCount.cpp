//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <vector>
#include "StringCount.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "ToLower.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static int
countPatterns(const std::wstring& str, const std::vector<std::wstring>& patterns);
static std::vector<std::wstring>
patternList(const ArrayOf& Pattern, bool caseSensitive);
//=============================================================================
static bool
isMissingStringElement(const ArrayOf& element)
{
    return !element.isCharacterArray();
}
//=============================================================================
ArrayOf
StringCount(const ArrayOf& A, const ArrayOf& Pattern, bool bCaseSensitive)
{
    bool patternIsText = Pattern.isCharacterArray() || Pattern.isStringArray()
        || Pattern.isCellArrayOfCharacterVectors();
    if (!patternIsText) {
        Error(L"Nelson:string:charVectorCellStringsExpected",
            L"character vector or cell of strings expected.");
    }
    std::vector<std::wstring> patterns = patternList(Pattern, bCaseSensitive);
    auto countIn = [&](const ArrayOf& text) -> double {
        if (isMissingStringElement(text)) {
            return 0;
        }
        std::wstring value = text.asWideString();
        return countPatterns(bCaseSensitive ? value : ToLower(value), patterns);
    };
    if (A.isCharacterArray()) {
        return ArrayOf::doubleScalar(countIn(A));
    }
    if (A.isStringArray() || A.isCellArrayOfCharacterVectors()) {
        Dimensions dimA = A.dimensions();
        size_t nbA = dimA.elementCount();
        double* result = static_cast<double*>(
            ArrayOf::allocateArrayOf(NLS_DOUBLE, nbA, stringVector(), false));
        auto* cellA = (ArrayOf*)(A.dataPointer());
        for (size_t k = 0; k < nbA; k++) {
            result[k] = countIn(cellA[k]);
        }
        return ArrayOf(NLS_DOUBLE, dimA, result);
    }
    Error(L"Nelson:string:charVectorCellStringsExpected",
        L"character vector or cell of strings expected.");
    return {};
}
//=============================================================================
// The patterns to look for, lowered when the search ignores case; a missing
// string is skipped.
static std::vector<std::wstring>
patternList(const ArrayOf& Pattern, bool caseSensitive)
{
    std::vector<std::wstring> patterns;
    if (Pattern.isCharacterArray()) {
        patterns.push_back(Pattern.asWideString());
    } else {
        auto* elements = (ArrayOf*)(Pattern.dataPointer());
        indexType n = Pattern.elementCount();
        patterns.reserve(n);
        for (indexType k = 0; k < n; k++) {
            if (!isMissingStringElement(elements[k])) {
                patterns.push_back(elements[k].asWideString());
            }
        }
    }
    if (!caseSensitive) {
        for (auto& pattern : patterns) {
            pattern = ToLower(pattern);
        }
    }
    return patterns;
}
//=============================================================================
// Occurrences of any pattern, scanning left to right without overlap: at a
// given position the first pattern of the list that matches is taken. An
// empty pattern matches before every character and at the end, but not right
// after a counted text.
static int
countPatterns(const std::wstring& str, const std::vector<std::wstring>& patterns)
{
    int count = 0;
    for (const auto& pattern : patterns) {
        if (!pattern.empty()) {
            continue;
        }
        size_t pos = 0;
        size_t previousEnd = std::wstring::npos;
        for (;;) {
            size_t match = patterns.size();
            for (size_t k = 0; k < patterns.size() && match == patterns.size(); ++k) {
                if (patterns[k].empty() || str.compare(pos, patterns[k].size(), patterns[k]) == 0) {
                    match = k;
                }
            }
            if (match < patterns.size() && !patterns[match].empty()) {
                ++count;
                pos += patterns[match].size();
                previousEnd = pos;
                continue;
            }
            if (match < patterns.size() && pos != previousEnd) {
                ++count;
            }
            if (pos >= str.size()) {
                return count;
            }
            ++pos;
        }
    }
    if (patterns.size() == 1) {
        const std::wstring& pattern = patterns[0];
        for (size_t offset = str.find(pattern); offset != std::wstring::npos;
            offset = str.find(pattern, offset + pattern.size())) {
            ++count;
        }
        return count;
    }
    size_t pos = 0;
    while (pos <= str.size()) {
        size_t best = std::wstring::npos;
        size_t bestLength = 0;
        for (const auto& pattern : patterns) {
            size_t found = str.find(pattern, pos);
            if (found < best) {
                best = found;
                bestLength = pattern.size();
            }
        }
        if (best == std::wstring::npos) {
            break;
        }
        ++count;
        pos = best + bestLength;
    }
    return count;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
