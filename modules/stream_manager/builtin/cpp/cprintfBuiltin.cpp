//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "cprintfBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Interface.hpp"
#include "PrintfFunction.hpp"
#include "OverloadHelpers.hpp"
#include "i18n.hpp"
#include <algorithm>
#include <array>
#include <cmath>
#include <cwchar>
#include <cwctype>
#include <string>
#include <vector>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace {
//=============================================================================
enum class StyleParseStatus
{
    Matched,
    NotAStyle,
    Ambiguous,
    Invalid
};
//=============================================================================
struct StyleParseResult
{
    StyleParseStatus status = StyleParseStatus::NotAStyle;
    TextStyle style;
};
//=============================================================================
struct NamedStyle
{
    const wchar_t* name;
    bool hasForeground;
    unsigned char red;
    unsigned char green;
    unsigned char blue;
    bool underline;
};
//=============================================================================
static bool
isTextArgument(const ArrayOf& value);
//=============================================================================
static size_t
firstNonSpace(const std::wstring& text, size_t first, size_t last);
//=============================================================================
static size_t
lastNonSpace(const std::wstring& text);
//=============================================================================
static std::wstring
normalizeStyleName(const std::wstring& text, size_t first, size_t last);
//=============================================================================
static bool
isPotentialStyleToken(const std::wstring& text);
//=============================================================================
static int
candidateMatch(const wchar_t* prefix, const wchar_t* name, const std::wstring& text);
//=============================================================================
static unsigned char
clampColor(int value);
//=============================================================================
static unsigned char
lightChannel(unsigned char value);
//=============================================================================
static unsigned char
darkChannel(unsigned char value);
//=============================================================================
static TextStyle
makeTextStyle(const NamedStyle& entry);
//=============================================================================
static TextStyle
makeLightStyle(const NamedStyle& entry);
//=============================================================================
static TextStyle
makeDarkStyle(const NamedStyle& entry);
//=============================================================================
static StyleParseResult
findNamedStyle(const std::wstring& normalizedName);
//=============================================================================
static int
hexValue(wchar_t c);
//=============================================================================
static StyleParseResult
parseHexStyle(const std::wstring& text, size_t first, size_t last);
//=============================================================================
static StyleParseResult
parseStringStyle(const std::wstring& rawStyle);
//=============================================================================
static unsigned char
rgbChannel(double value, bool normalized);
//=============================================================================
static StyleParseResult
parseRgbStyle(const ArrayOf& rawStyle);
//=============================================================================
static StyleParseResult
parseStyleArgument(const ArrayOf& value);
//=============================================================================
static size_t
emitStyled(Evaluator* eval, const std::wstring& msg, const TextStyle& style);
//=============================================================================
static size_t
emitDemo(Evaluator* eval);
//=============================================================================
static void
prepareFormatArguments(const ArrayOfVector& argIn, ArrayOfVector& args, TextStyle& style);
//=============================================================================
} // namespace
//=============================================================================
ArrayOfVector
Nelson::StreamGateway::cprintfBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    ArrayOfVector retval;
    nargoutcheck(nLhs, 0, 1);

    size_t count = 0;
    if (argIn.empty()) {
        count = emitDemo(eval);
    } else {
        TextStyle style;
        ArrayOfVector args;
        prepareFormatArguments(argIn, args, style);
        args = convertObjectPrintfArguments(eval, args);
        std::wstring result;
        std::wstring errorMessage;
        if (!printfFunction(args, errorMessage, result)) {
            Error(L"Nelson:cprintf:formatFailed", errorMessage);
        }
        count = emitStyled(eval, result, style);
    }

    if (nLhs > 0) {
        retval << ArrayOf::doubleScalar(static_cast<double>(count));
    }
    return retval;
}
//=============================================================================
namespace {
//=============================================================================
static bool
isTextArgument(const ArrayOf& value)
{
    return value.isRowVectorCharacterArray() || value.isScalarStringArray();
}
//=============================================================================
static size_t
firstNonSpace(const std::wstring& text, size_t first, size_t last)
{
    while (first < last && std::iswspace(text[first]) != 0) {
        ++first;
    }
    return first;
}
//=============================================================================
static size_t
lastNonSpace(const std::wstring& text)
{
    size_t last = text.size();
    while (last > 0 && std::iswspace(text[last - 1]) != 0) {
        --last;
    }
    return last;
}
//=============================================================================
static std::wstring
normalizeStyleName(const std::wstring& text, size_t first, size_t last)
{
    std::wstring normalized;
    normalized.reserve(last - first);
    for (size_t k = first; k < last; ++k) {
        wchar_t c = text[k];
        if (c == L' ' || c == L'_' || c == L'-') {
            continue;
        }
        normalized.push_back(static_cast<wchar_t>(std::towlower(c)));
    }
    return normalized;
}
//=============================================================================
static bool
isPotentialStyleToken(const std::wstring& text)
{
    if (text.find(L'%') != std::wstring::npos || text.find(L'\\') != std::wstring::npos) {
        return false;
    }
    for (wchar_t c : text) {
        if (std::iswalnum(c) || c == L' ' || c == L'_' || c == L'-' || c == L'*' || c == L'#') {
            continue;
        }
        return false;
    }
    return true;
}
//=============================================================================
static int
candidateMatch(const wchar_t* prefix, const wchar_t* name, const std::wstring& text)
{
    size_t prefixLength = std::wcslen(prefix);
    size_t nameLength = std::wcslen(name);
    size_t candidateLength = prefixLength + nameLength;
    if (candidateLength < text.size()) {
        return 0;
    }
    for (size_t k = 0; k < text.size(); ++k) {
        wchar_t candidate = k < prefixLength ? prefix[k] : name[k - prefixLength];
        if (candidate != text[k]) {
            return 0;
        }
    }
    return candidateLength == text.size() ? 2 : 1;
}
//=============================================================================
static unsigned char
clampColor(int value)
{
    return static_cast<unsigned char>(std::max(0, std::min(255, value)));
}
//=============================================================================
static unsigned char
lightChannel(unsigned char value)
{
    return clampColor(static_cast<int>(std::round(value + (255 - value) * 0.45)));
}
//=============================================================================
static unsigned char
darkChannel(unsigned char value)
{
    return clampColor(static_cast<int>(std::round(value * 0.60)));
}
//=============================================================================
static TextStyle
makeTextStyle(const NamedStyle& entry)
{
    TextStyle style;
    style.hasForeground = entry.hasForeground;
    style.red = entry.red;
    style.green = entry.green;
    style.blue = entry.blue;
    style.underline = entry.underline;
    return style;
}
//=============================================================================
static TextStyle
makeLightStyle(const NamedStyle& entry)
{
    TextStyle style = makeTextStyle(entry);
    if (style.hasForeground) {
        style.red = lightChannel(style.red);
        style.green = lightChannel(style.green);
        style.blue = lightChannel(style.blue);
    }
    return style;
}
//=============================================================================
static TextStyle
makeDarkStyle(const NamedStyle& entry)
{
    TextStyle style = makeTextStyle(entry);
    if (style.hasForeground) {
        style.red = darkChannel(style.red);
        style.green = darkChannel(style.green);
        style.blue = darkChannel(style.blue);
    }
    return style;
}
//=============================================================================
static StyleParseResult
findNamedStyle(const std::wstring& normalizedName)
{
    static const std::array<NamedStyle, 29> styles = { { { L"text", false, 0, 0, 0, false },
        { L"keywords", true, 0, 0, 255, false }, { L"comments", true, 0, 128, 0, false },
        { L"strings", true, 160, 32, 160, false },
        { L"unterminatedstrings", true, 139, 0, 0, false },
        { L"systemcommands", true, 255, 165, 0, false }, { L"errors", true, 255, 80, 80, false },
        { L"hyperlinks", true, 0, 0, 255, true }, { L"black", true, 0, 0, 0, false },
        { L"cyan", true, 0, 255, 255, false }, { L"magenta", true, 255, 0, 255, false },
        { L"blue", true, 0, 0, 255, false }, { L"green", true, 0, 160, 0, false },
        { L"red", true, 255, 0, 0, false }, { L"yellow", true, 255, 220, 0, false },
        { L"white", true, 255, 255, 255, false }, { L"gray", true, 128, 128, 128, false },
        { L"grey", true, 128, 128, 128, false }, { L"orange", true, 255, 165, 0, false },
        { L"pink", true, 255, 105, 180, false }, { L"silver", true, 192, 192, 192, false },
        { L"maroon", true, 128, 0, 0, false }, { L"purple", true, 128, 0, 128, false },
        { L"olive", true, 128, 128, 0, false }, { L"navy", true, 0, 0, 128, false },
        { L"teal", true, 0, 128, 128, false }, { L"violet", true, 138, 43, 226, false },
        { L"brown", true, 165, 42, 42, false }, { L"gold", true, 255, 215, 0, false } } };

    StyleParseResult result;
    bool found = false;
    auto tryCandidate = [&](const wchar_t* prefix, const NamedStyle& entry,
                            TextStyle (*makeStyle)(const NamedStyle&)) {
        int match = candidateMatch(prefix, entry.name, normalizedName);
        if (match == 2) {
            result.status = StyleParseStatus::Matched;
            result.style = makeStyle(entry);
            found = true;
            return true;
        }
        if (match == 1) {
            if (!found) {
                result.status = StyleParseStatus::Matched;
                result.style = makeStyle(entry);
                found = true;
            } else {
                result.status = StyleParseStatus::Ambiguous;
            }
        }
        return false;
    };

    for (const NamedStyle& entry : styles) {
        if (tryCandidate(L"", entry, makeTextStyle)) {
            return result;
        }
        if (entry.hasForeground) {
            if (tryCandidate(L"light", entry, makeLightStyle)) {
                return result;
            }
            if (tryCandidate(L"dark", entry, makeDarkStyle)) {
                return result;
            }
        }
    }
    return result;
}
//=============================================================================
static int
hexValue(wchar_t c)
{
    if (c >= L'0' && c <= L'9') {
        return static_cast<int>(c - L'0');
    }
    c = static_cast<wchar_t>(std::towlower(c));
    if (c >= L'a' && c <= L'f') {
        return 10 + static_cast<int>(c - L'a');
    }
    return -1;
}
//=============================================================================
static StyleParseResult
parseHexStyle(const std::wstring& text, size_t first, size_t last)
{
    StyleParseResult result;
    size_t length = last - first;
    if (!(length == 4 || length == 7)) {
        result.status = StyleParseStatus::Invalid;
        return result;
    }
    for (size_t k = first + 1; k < last; ++k) {
        if (hexValue(text[k]) < 0) {
            result.status = StyleParseStatus::Invalid;
            return result;
        }
    }
    TextStyle style;
    style.hasForeground = true;
    if (length == 4) {
        style.red = clampColor(hexValue(text[first + 1]) * 17);
        style.green = clampColor(hexValue(text[first + 2]) * 17);
        style.blue = clampColor(hexValue(text[first + 3]) * 17);
    } else {
        style.red = clampColor(hexValue(text[first + 1]) * 16 + hexValue(text[first + 2]));
        style.green = clampColor(hexValue(text[first + 3]) * 16 + hexValue(text[first + 4]));
        style.blue = clampColor(hexValue(text[first + 5]) * 16 + hexValue(text[first + 6]));
    }
    result.status = StyleParseStatus::Matched;
    result.style = style;
    return result;
}
//=============================================================================
static StyleParseResult
parseStringStyle(const std::wstring& rawStyle)
{
    StyleParseResult result;
    TextStyle prefixStyle;
    size_t last = lastNonSpace(rawStyle);
    size_t first = firstNonSpace(rawStyle, 0, last);
    while (first < last
        && (rawStyle[first] == L'*' || rawStyle[first] == L'-' || rawStyle[first] == L'_')) {
        if (rawStyle[first] == L'*') {
            prefixStyle.bold = true;
        } else {
            prefixStyle.underline = true;
        }
        first = firstNonSpace(rawStyle, first + 1, last);
    }
    if (first == last) {
        result.status = StyleParseStatus::Matched;
        result.style = prefixStyle;
        return result;
    }

    StyleParseResult parsed;
    if (rawStyle[first] == L'#') {
        parsed = parseHexStyle(rawStyle, first, last);
    } else {
        parsed = findNamedStyle(normalizeStyleName(rawStyle, first, last));
    }
    if (parsed.status == StyleParseStatus::Matched) {
        parsed.style.bold = parsed.style.bold || prefixStyle.bold;
        parsed.style.underline = parsed.style.underline || prefixStyle.underline;
    }
    return parsed;
}
//=============================================================================
static unsigned char
rgbChannel(double value, bool normalized)
{
    double scaled = normalized ? value * 255.0 : value;
    return clampColor(static_cast<int>(std::round(scaled)));
}
//=============================================================================
static StyleParseResult
parseRgbStyle(const ArrayOf& rawStyle)
{
    StyleParseResult result;
    if (!rawStyle.isNumeric() || !rawStyle.isReal()) {
        result.status = StyleParseStatus::Invalid;
        return result;
    }
    ArrayOf valuesArray(rawStyle);
    valuesArray.promoteType(NLS_DOUBLE);
    std::vector<double> values = valuesArray.asDoubleVector();
    if (values.size() != 3) {
        result.status = StyleParseStatus::Invalid;
        return result;
    }
    bool underline = false;
    bool normalized = true;
    for (double& value : values) {
        if (!std::isfinite(value)) {
            result.status = StyleParseStatus::Invalid;
            return result;
        }
        if (value < 0) {
            underline = true;
            value = std::abs(value);
        }
        normalized = normalized && value <= 1.0;
    }
    double maxValue = normalized ? 1.0 : 255.0;
    for (double value : values) {
        if (value < 0 || value > maxValue) {
            result.status = StyleParseStatus::Invalid;
            return result;
        }
    }
    TextStyle style;
    style.hasForeground = true;
    style.red = rgbChannel(values[0], normalized);
    style.green = rgbChannel(values[1], normalized);
    style.blue = rgbChannel(values[2], normalized);
    style.underline = underline;
    result.status = StyleParseStatus::Matched;
    result.style = style;
    return result;
}
//=============================================================================
static StyleParseResult
parseStyleArgument(const ArrayOf& value)
{
    if (isTextArgument(value)) {
        return parseStringStyle(value.asWideString());
    }
    return parseRgbStyle(value);
}
//=============================================================================
static size_t
emitStyled(Evaluator* eval, const std::wstring& msg, const TextStyle& style)
{
    Interface* io = eval ? eval->getInterface() : nullptr;
    if (io) {
        if (style.isPlain()) {
            io->outputMessage(msg);
        } else {
            io->outputStyledText(msg, style);
        }
    }
    return msg.length();
}
//=============================================================================
static size_t
emitDemo(Evaluator* eval)
{
    size_t count = 0;
    count += emitStyled(eval, L"cprintf demo\n", TextStyle());
    count += emitStyled(eval, L"plain text\n", parseStringStyle(L"text").style);
    count += emitStyled(eval, L"blue text\n", parseStringStyle(L"blue").style);
    count += emitStyled(eval, L"bold red text\n", parseStringStyle(L"*red").style);
    count += emitStyled(eval, L"underlined green text\n", parseStringStyle(L"_green").style);
    return count;
}
//=============================================================================
static void
prepareFormatArguments(const ArrayOfVector& argIn, ArrayOfVector& args, TextStyle& style)
{
    size_t firstArgumentPosition = 0;
    if (argIn.size() >= 2) {
        StyleParseResult parsed;
        bool parsedStyle = false;
        if (isTextArgument(argIn[0])) {
            std::wstring rawStyle = argIn[0].asWideString();
            if (isPotentialStyleToken(rawStyle)) {
                parsed = parseStringStyle(rawStyle);
                parsedStyle = true;
            }
        } else {
            parsed = parseRgbStyle(argIn[0]);
            parsedStyle = true;
        }

        if (parsedStyle) {
            if (parsed.status == StyleParseStatus::Matched) {
                style = parsed.style;
                firstArgumentPosition = 1;
            } else if (parsed.status == StyleParseStatus::Ambiguous) {
                Error(L"Nelson:cprintf:ambiguousStyleName", L"Ambiguous style name.");
            } else if (parsed.status == StyleParseStatus::Invalid
                || parsed.status == StyleParseStatus::NotAStyle) {
                Error(L"Nelson:cprintf:invalidStyle", L"Valid style expected.");
            }
        }
    }
    if (firstArgumentPosition >= argIn.size() || !isTextArgument(argIn[firstArgumentPosition])) {
        Error(L"Nelson:stream_manager:invalidFormat", L"Invalid format.");
    }
    for (size_t k = firstArgumentPosition; k < argIn.size(); ++k) {
        args.push_back(argIn[k]);
    }
}
//=============================================================================
} // namespace
//=============================================================================
