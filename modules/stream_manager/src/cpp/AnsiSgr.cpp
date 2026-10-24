//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "AnsiSgr.hpp"
#include <algorithm>
#include <array>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    constexpr wchar_t ESC = 0x1b;
    constexpr size_t MAX_SGR_PARAMS = 64;
    //=============================================================================
    struct SgrParseResult
    {
        bool valid = false;
        size_t next = 0;
        std::array<int, MAX_SGR_PARAMS> params {};
        size_t count = 0;
    };
    //=============================================================================
    static unsigned char
    clampByte(int value)
    {
        return static_cast<unsigned char>(std::max(0, std::min(255, value)));
    }
    //=============================================================================
    static bool
    parseSgrSequence(const std::wstring& text, size_t first, SgrParseResult& result)
    {
        result = SgrParseResult();
        if (first + 2 >= text.size() || text[first] != ESC || text[first + 1] != L'[') {
            return false;
        }

        size_t pos = first + 2;
        int value = -1;
        while (pos < text.size()) {
            wchar_t c = text[pos];
            if (c >= L'0' && c <= L'9') {
                if (value < 0) {
                    value = 0;
                }
                value = value * 10 + static_cast<int>(c - L'0');
                ++pos;
                continue;
            }
            if (c == L';' || c == L'm') {
                if (result.count >= result.params.size()) {
                    return false;
                }
                result.params[result.count++] = value < 0 ? 0 : value;
                value = -1;
                ++pos;
                if (c == L'm') {
                    result.valid = true;
                    result.next = pos;
                    return true;
                }
                continue;
            }
            return false;
        }
        return false;
    }
    //=============================================================================
    static void
    setIndexedColor(TextStyle& style, bool background, int code)
    {
        static constexpr unsigned char basic[16][3] = { { 0, 0, 0 }, { 170, 0, 0 }, { 0, 170, 0 },
            { 170, 85, 0 }, { 0, 0, 170 }, { 170, 0, 170 }, { 0, 170, 170 }, { 170, 170, 170 },
            { 85, 85, 85 }, { 255, 85, 85 }, { 85, 255, 85 }, { 255, 255, 85 }, { 85, 85, 255 },
            { 255, 85, 255 }, { 85, 255, 255 }, { 255, 255, 255 } };
        unsigned char red = 0;
        unsigned char green = 0;
        unsigned char blue = 0;
        if (code < 16) {
            red = basic[code][0];
            green = basic[code][1];
            blue = basic[code][2];
        } else if (code < 232) {
            int value = code - 16;
            int r = value / 36;
            int g = (value / 6) % 6;
            int b = value % 6;
            auto component = [](int level) { return clampByte(level == 0 ? 0 : 55 + level * 40); };
            red = component(r);
            green = component(g);
            blue = component(b);
        } else {
            unsigned char gray = clampByte(8 + (code - 232) * 10);
            red = gray;
            green = gray;
            blue = gray;
        }

        if (background) {
            style.hasBackground = true;
            style.backgroundRed = red;
            style.backgroundGreen = green;
            style.backgroundBlue = blue;
        } else {
            style.hasForeground = true;
            style.red = red;
            style.green = green;
            style.blue = blue;
        }
    }
    //=============================================================================
    static void
    setRgbColor(TextStyle& style, bool background, int red, int green, int blue)
    {
        if (background) {
            style.hasBackground = true;
            style.backgroundRed = clampByte(red);
            style.backgroundGreen = clampByte(green);
            style.backgroundBlue = clampByte(blue);
        } else {
            style.hasForeground = true;
            style.red = clampByte(red);
            style.green = clampByte(green);
            style.blue = clampByte(blue);
        }
    }
    //=============================================================================
    static bool
    applyExtendedColor(TextStyle& style, bool background, const SgrParseResult& sgr, size_t& index)
    {
        if (index + 1 >= sgr.count) {
            return false;
        }
        int mode = sgr.params[index + 1];
        if (mode == 5) {
            if (index + 2 >= sgr.count || sgr.params[index + 2] < 0
                || sgr.params[index + 2] > 255) {
                return false;
            }
            setIndexedColor(style, background, sgr.params[index + 2]);
            index += 2;
            return true;
        }
        if (mode == 2) {
            if (index + 4 >= sgr.count) {
                return false;
            }
            setRgbColor(style, background, sgr.params[index + 2], sgr.params[index + 3],
                sgr.params[index + 4]);
            index += 4;
            return true;
        }
        return false;
    }
    //=============================================================================
    static bool
    applySgr(TextStyle& style, const SgrParseResult& sgr)
    {
        for (size_t k = 0; k < sgr.count; ++k) {
            int code = sgr.params[k];
            if (code == 0) {
                style = TextStyle();
            } else if (code == 1) {
                style.bold = true;
            } else if (code == 3) {
                style.italic = true;
            } else if (code == 4) {
                style.underline = true;
            } else if (code == 9) {
                style.strikethrough = true;
            } else if (code == 22) {
                style.bold = false;
            } else if (code == 23) {
                style.italic = false;
            } else if (code == 24) {
                style.underline = false;
            } else if (code == 29) {
                style.strikethrough = false;
            } else if (code == 39) {
                style.hasForeground = false;
            } else if (code == 49) {
                style.hasBackground = false;
            } else if (code >= 30 && code <= 37) {
                setIndexedColor(style, false, code - 30);
            } else if (code >= 40 && code <= 47) {
                setIndexedColor(style, true, code - 40);
            } else if (code >= 90 && code <= 97) {
                setIndexedColor(style, false, code - 90 + 8);
            } else if (code >= 100 && code <= 107) {
                setIndexedColor(style, true, code - 100 + 8);
            } else if (code == 38 || code == 48) {
                if (!applyExtendedColor(style, code == 48, sgr, k)) {
                    return false;
                }
            }
        }
        return true;
    }
    //=============================================================================
    static bool
    parseAndApplySgr(const std::wstring& text, size_t first, TextStyle& style, size_t& next)
    {
        SgrParseResult sgr;
        if (!parseSgrSequence(text, first, sgr)) {
            return false;
        }
        TextStyle candidate = style;
        if (!applySgr(candidate, sgr)) {
            return false;
        }
        style = candidate;
        next = sgr.next;
        return true;
    }
    //=============================================================================
    static void
    appendSpan(
        std::vector<AnsiStyledSpan>& spans, size_t first, size_t last, const TextStyle& style)
    {
        if (last <= first) {
            return;
        }
        spans.push_back(AnsiStyledSpan { first, last - first, style });
    }
    //=============================================================================
} // namespace
//=============================================================================
bool
containsAnsiEscape(const std::wstring& text)
{
    return text.find(ESC) != std::wstring::npos;
}
//=============================================================================
size_t
ansiVisibleLength(const std::wstring& text)
{
    size_t firstEsc = text.find(ESC);
    if (firstEsc == std::wstring::npos) {
        return text.length();
    }
    TextStyle style;
    size_t visible = firstEsc;
    for (size_t pos = firstEsc; pos < text.size();) {
        if (text[pos] == ESC) {
            size_t next = pos;
            if (parseAndApplySgr(text, pos, style, next)) {
                pos = next;
                continue;
            }
        }
        ++visible;
        ++pos;
    }
    return visible;
}
//=============================================================================
std::wstring
stripAnsiSgr(const std::wstring& text)
{
    if (!containsAnsiEscape(text)) {
        return text;
    }
    std::wstring stripped;
    stripped.reserve(text.size());
    TextStyle style;
    for (size_t pos = 0; pos < text.size();) {
        if (text[pos] == ESC) {
            size_t next = pos;
            if (parseAndApplySgr(text, pos, style, next)) {
                pos = next;
                continue;
            }
        }
        stripped.push_back(text[pos]);
        ++pos;
    }
    return stripped;
}
//=============================================================================
std::string
buildAnsiStyledMessage(const std::string& text, const TextStyle& style)
{
    if (style.isPlain()) {
        return text;
    }
    std::string styled;
    styled.reserve(text.size() + 48);
    styled += "\x1b[";
    bool needSeparator = false;
    auto appendCode = [&](const std::string& code) {
        if (needSeparator) {
            styled += ";";
        }
        styled += code;
        needSeparator = true;
    };
    if (style.bold) {
        appendCode("1");
    }
    if (style.italic) {
        appendCode("3");
    }
    if (style.underline) {
        appendCode("4");
    }
    if (style.strikethrough) {
        appendCode("9");
    }
    if (style.hasForeground) {
        appendCode("38;2");
        styled += ";";
        styled += std::to_string(static_cast<int>(style.red));
        styled += ";";
        styled += std::to_string(static_cast<int>(style.green));
        styled += ";";
        styled += std::to_string(static_cast<int>(style.blue));
    }
    if (style.hasBackground) {
        appendCode("48;2");
        styled += ";";
        styled += std::to_string(static_cast<int>(style.backgroundRed));
        styled += ";";
        styled += std::to_string(static_cast<int>(style.backgroundGreen));
        styled += ";";
        styled += std::to_string(static_cast<int>(style.backgroundBlue));
    }
    styled += "m";
    styled += text;
    styled += "\x1b[0m";
    return styled;
}
//=============================================================================
std::string
buildOsc8Hyperlink(const std::string& uri, const std::string& label)
{
    return "\x1b]8;;" + uri + "\x1b\\" + label + "\x1b]8;;\x1b\\";
}
//=============================================================================
std::string
buildExpandableTextLink(long long id, const std::string& label)
{
    return buildOsc8Hyperlink("nelson-expand:" + std::to_string(id), label);
}
//=============================================================================
std::vector<AnsiStyledSpan>
parseAnsiStyledSpans(const std::wstring& text)
{
    std::vector<AnsiStyledSpan> spans;
    size_t firstEsc = text.find(ESC);
    if (firstEsc == std::wstring::npos) {
        if (!text.empty()) {
            spans.push_back(AnsiStyledSpan { 0, text.size(), TextStyle() });
        }
        return spans;
    }

    TextStyle style;
    size_t spanStart = 0;
    for (size_t pos = firstEsc; pos < text.size();) {
        if (text[pos] == ESC) {
            size_t next = pos;
            TextStyle previousStyle = style;
            if (parseAndApplySgr(text, pos, style, next)) {
                appendSpan(spans, spanStart, pos, previousStyle);
                pos = next;
                spanStart = pos;
                continue;
            }
        }
        ++pos;
    }
    appendSpan(spans, spanStart, text.size(), style);
    return spans;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
