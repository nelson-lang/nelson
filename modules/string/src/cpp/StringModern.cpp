//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "StringModern.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "RegularExpression.hpp"
#include "StringHelpers.hpp"
#include "StringPattern.hpp"
#include "StringTrim.hpp"
#include "nlsBuildConfig.h"
#include "omp_for_loop.hpp"
#include <algorithm>
#include <functional>
//=============================================================================
namespace Nelson {
//=============================================================================
using TextTransform = std::function<std::wstring(const std::wstring&)>;
//=============================================================================
static ArrayOf
applyTextTransform(const ArrayOf& input, TextTransform transform, bool& needToOverload)
{
    needToOverload = false;
    if (input.isRowVectorCharacterArray()) {
        return ArrayOf::characterArray(transform(input.asWideString()));
    }
    if (input.isCellArrayOfCharacterVectors()) {
        ArrayOf res(input);
        res.ensureSingleOwner();
        auto* elements = static_cast<ArrayOf*>(res.readWriteDataPointer());
        ompIndexType elementCount = input.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            elements[k] = ArrayOf::characterArray(transform(elements[k].asWideString()));
        }
        return res;
    }
    if (input.isStringArray()) {
        ArrayOf res(input);
        res.ensureSingleOwner();
        auto* elements = static_cast<ArrayOf*>(res.readWriteDataPointer());
        ompIndexType elementCount = input.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (elements[k].isRowVectorCharacterArray()) {
                elements[k] = ArrayOf::characterArray(transform(elements[k].asWideString()));
            }
        }
        return res;
    }
    needToOverload = true;
    return ArrayOf();
}
//=============================================================================
static wstringVector
getTextList(const ArrayOf& value)
{
    if (value.isRowVectorCharacterArray() || value.isStringArray()
        || value.isCellArrayOfCharacterVectors()) {
        wstringVector text = value.asWideStringVector(false);
        if (text.empty() && value.isCharacterArray()) {
            text.push_back(value.asWideString());
        }
        return text;
    }
    Error(L"Nelson:invalidType", L"Wrong type for argument #%d: %s expected.", (1),
        L"string or cell");
    return wstringVector();
}
//=============================================================================
static std::wstring
textAt(const wstringVector& values, indexType index)
{
    if (values.empty()) {
        return L"";
    }
    return values[std::min(static_cast<size_t>(index), values.size() - 1)];
}
//=============================================================================
static ArrayOf
applyTextTransformIndexed(const ArrayOf& input, const wstringVector& matches,
    const std::function<std::wstring(const std::wstring&, const std::wstring&)>& transform,
    bool& needToOverload)
{
    needToOverload = false;
    if (input.isRowVectorCharacterArray()) {
        return ArrayOf::characterArray(transform(input.asWideString(), textAt(matches, 0)));
    }
    if (input.isCellArrayOfCharacterVectors()) {
        ArrayOf res(input);
        res.ensureSingleOwner();
        auto* elements = static_cast<ArrayOf*>(res.readWriteDataPointer());
        ompIndexType elementCount = input.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            elements[k] = ArrayOf::characterArray(
                transform(elements[k].asWideString(), textAt(matches, static_cast<indexType>(k))));
        }
        return res;
    }
    if (input.isStringArray()) {
        ArrayOf res(input);
        res.ensureSingleOwner();
        auto* elements = static_cast<ArrayOf*>(res.readWriteDataPointer());
        ompIndexType elementCount = input.elementCount();
        OMP_PARALLEL_FOR_LOOP(elementCount)
        for (ompIndexType k = 0; k < elementCount; k++) {
            if (elements[k].isRowVectorCharacterArray()) {
                elements[k] = ArrayOf::characterArray(transform(
                    elements[k].asWideString(), textAt(matches, static_cast<indexType>(k))));
            }
        }
        return res;
    }
    needToOverload = true;
    return ArrayOf();
}
//=============================================================================
static std::wstring
replaceAllLiteral(std::wstring text, const std::wstring& oldValue, const std::wstring& newValue)
{
    if (oldValue.empty()) {
        return text;
    }
    size_t pos = 0;
    while ((pos = text.find(oldValue, pos)) != std::wstring::npos) {
        text.replace(pos, oldValue.length(), newValue);
        pos += newValue.length();
    }
    return text;
}
//=============================================================================
ArrayOf
StringErase(const ArrayOf& str, const ArrayOf& match, bool& needToOverload)
{
    wstringVector matches = getTextList(match);
    auto eraseOne = [&matches](std::wstring text) {
        for (const auto& matchText : matches) {
            text = replaceAllLiteral(text, matchText, L"");
        }
        return text;
    };
    return applyTextTransform(str, eraseOne, needToOverload);
}
//=============================================================================
static std::wstring
reverseText(const std::wstring& text)
{
    return std::wstring(text.rbegin(), text.rend());
}
//=============================================================================
ArrayOf
StringReverse(const ArrayOf& str, bool& needToOverload)
{
    return applyTextTransform(str, reverseText, needToOverload);
}
//=============================================================================
static std::wstring
stripCharacters(const std::wstring& text, const std::wstring& side, const std::wstring& chars)
{
    // without a strip character, whitespace (as strtrim sees it) is removed
    auto isStripped = [&chars](wchar_t c) {
        return chars.empty() ? IsTextWhitespace(c) : chars.find(c) != std::wstring::npos;
    };
    size_t first = 0;
    size_t last = text.size();
    if (side == L"left" || side == L"both") {
        while (first < last && isStripped(text[first])) {
            ++first;
        }
    }
    if (side == L"right" || side == L"both") {
        while (last > first && isStripped(text[last - 1])) {
            --last;
        }
    }
    if (first >= last) {
        return L"";
    }
    return text.substr(first, last - first);
}
//=============================================================================
ArrayOf
StringStrip(const ArrayOf& str, const std::wstring& side, const std::wstring& stripCharactersValue,
    bool& needToOverload)
{
    auto stripOne = [&side, &stripCharactersValue](const std::wstring& text) {
        return stripCharacters(text, side, stripCharactersValue);
    };
    return applyTextTransform(str, stripOne, needToOverload);
}
//=============================================================================
static std::wstring
padText(const std::wstring& text, size_t width, const std::wstring& side, wchar_t padCharacter)
{
    if (text.size() >= width) {
        return text;
    }
    size_t count = width - text.size();
    if (side == L"left") {
        return std::wstring(count, padCharacter) + text;
    }
    if (side == L"both") {
        size_t left = count / 2;
        size_t right = count - left;
        return std::wstring(left, padCharacter) + text + std::wstring(right, padCharacter);
    }
    return text + std::wstring(count, padCharacter);
}
//=============================================================================
ArrayOf
StringPad(const ArrayOf& str, size_t width, const std::wstring& side, wchar_t padCharacter,
    bool& needToOverload)
{
    auto padOne = [width, &side, padCharacter](const std::wstring& text) {
        return padText(text, width, side, padCharacter);
    };
    return applyTextTransform(str, padOne, needToOverload);
}
//=============================================================================
static std::wstring
extractAfterLiteral(const std::wstring& text, const std::wstring& start)
{
    size_t pos = text.find(start);
    if (pos == std::wstring::npos) {
        return L"";
    }
    return text.substr(pos + start.size());
}
//=============================================================================
static std::wstring
extractBeforeLiteral(const std::wstring& text, const std::wstring& stop)
{
    size_t pos = text.find(stop);
    if (pos == std::wstring::npos) {
        return L"";
    }
    return text.substr(0, pos);
}
//=============================================================================
ArrayOf
StringExtractAfter(const ArrayOf& str, const ArrayOf& start, bool& needToOverload)
{
    if (IsPatternObject(start)) {
        std::wstring expression = PatternExpression(start);
        auto extractOne = [&expression](const std::wstring& text) {
            int first = 0;
            int last = 0;
            if (!RegexFirstMatchExtent(text, expression, first, last)) {
                return std::wstring();
            }
            return text.substr(static_cast<size_t>(last));
        };
        return applyTextTransform(str, extractOne, needToOverload);
    }
    if (start.isNumeric()) {
        size_t pos = static_cast<size_t>(start.asDoubleScalar(true, true));
        auto extractOne = [pos](const std::wstring& text) {
            if (pos >= text.size()) {
                return std::wstring();
            }
            return text.substr(pos);
        };
        return applyTextTransform(str, extractOne, needToOverload);
    }
    wstringVector starts = getTextList(start);
    auto extractOne = [](const std::wstring& text, const std::wstring& startText) {
        return extractAfterLiteral(text, startText);
    };
    return applyTextTransformIndexed(str, starts, extractOne, needToOverload);
}
//=============================================================================
ArrayOf
StringExtractBefore(const ArrayOf& str, const ArrayOf& stop, bool& needToOverload)
{
    if (IsPatternObject(stop)) {
        std::wstring expression = PatternExpression(stop);
        auto extractOne = [&expression](const std::wstring& text) {
            int first = 0;
            int last = 0;
            if (!RegexFirstMatchExtent(text, expression, first, last)) {
                return std::wstring();
            }
            return text.substr(0, static_cast<size_t>(first - 1));
        };
        return applyTextTransform(str, extractOne, needToOverload);
    }
    if (stop.isNumeric()) {
        size_t pos = static_cast<size_t>(stop.asDoubleScalar(true, true));
        auto extractOne = [pos](const std::wstring& text) {
            if (pos <= 1) {
                return std::wstring();
            }
            return text.substr(0, std::min(pos - 1, text.size()));
        };
        return applyTextTransform(str, extractOne, needToOverload);
    }
    wstringVector stops = getTextList(stop);
    auto extractOne = [](const std::wstring& text, const std::wstring& stopText) {
        return extractBeforeLiteral(text, stopText);
    };
    return applyTextTransformIndexed(str, stops, extractOne, needToOverload);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
