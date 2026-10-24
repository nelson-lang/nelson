//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FontMetrics.hpp"
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cwctype>
#include <fstream>
#include <map>
#include <mutex>
#include <unordered_map>
//=============================================================================
#define STBTT_STATIC
#define STB_TRUETYPE_IMPLEMENTATION
#include "stb_truetype.h"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace FontMetrics {
    //=============================================================================
    static std::vector<std::wstring>
    splitTextLines(const std::wstring& text);
    //=============================================================================
    // Widths come from real platform TTF files when found, else from the core-font
    // advance tables (AFM units per 1000 em, ASCII 32..126): sans (Helvetica),
    // serif (Times) and monospace (Courier). The axes use them to place labels and
    // to decide how many ticks fit, so they must track Qt's default rendering.
    enum class GenericFamily
    {
        Sans,
        Serif,
        Mono
    };
    //=============================================================================
    static const short HELVETICA_ADVANCE[95] = {
        278, 278, 355, 556, 556, 889, 667, 191, 333, 333, 389, 584, 278, 333, 278, 278, // 32-47
        556, 556, 556, 556, 556, 556, 556, 556, 556, 556, // 0-9
        278, 278, 584, 584, 584, 556, 1015, // : ; < = > ? @
        667, 667, 722, 722, 667, 611, 778, 722, 278, 500, 667, 556, 833, 722, 778, 667, // A-P
        778, 722, 667, 611, 722, 667, 944, 667, 667, 611, // Q-Z
        278, 278, 278, 469, 556, 333, // [ \ ] ^ _ `
        556, 556, 500, 556, 556, 278, 556, 556, 222, 222, 500, 222, 833, 556, 556, 556, // a-p
        556, 333, 500, 278, 556, 500, 722, 500, 500, 500, // q-z
        334, 260, 334, 584 // { | } ~
    };
    //=============================================================================
    static const short TIMES_ADVANCE[95] = {
        250, 333, 408, 500, 500, 833, 778, 180, 333, 333, 500, 564, 250, 333, 250, 278, // 32-47
        500, 500, 500, 500, 500, 500, 500, 500, 500, 500, // 0-9
        278, 278, 564, 564, 564, 444, 921, // : ; < = > ? @
        722, 667, 667, 722, 611, 556, 722, 722, 333, 389, 722, 611, 889, 722, 722, 556, // A-P
        722, 667, 556, 611, 722, 722, 944, 722, 722, 611, // Q-Z
        333, 278, 333, 469, 500, 333, // [ \ ] ^ _ `
        444, 500, 444, 500, 444, 333, 500, 500, 278, 278, 500, 278, 778, 500, 500, 500, // a-p
        500, 333, 389, 278, 500, 500, 722, 500, 500, 444, // q-z
        480, 200, 480, 541 // { | } ~
    };
    //=============================================================================
    // TTF metric reader backed by stb_truetype. Advances are cached per codepoint
    // in em fractions. The AFM tables above are the fallback without a font file.
    class TtfFont
    {
    public:
        bool loaded = false;
        double ascent = 0.75; // em fractions
        double descent = 0.25;

        static const TtfFont*
        load(const std::string& path)
        {
            static std::mutex mutex;
            static std::map<std::string, TtfFont> cache;
            std::lock_guard<std::mutex> lock(mutex);
            auto found = cache.find(path);
            if (found != cache.end()) {
                return found->second.loaded ? &found->second : nullptr;
            }
            TtfFont& font = cache[path];
            font.parse(path);
            return font.loaded ? &font : nullptr;
        }

        const stbtt_fontinfo*
        fontInfo() const
        {
            return &m_info;
        }

        // Glyph index 0 is .notdef, which stb_truetype would rasterize as a "tofu" box.
        bool
        hasGlyph(wchar_t ch) const
        {
            return stbtt_FindGlyphIndex(&m_info, static_cast<int>(ch)) != 0;
        }

        double
        invUnitsPerEm() const
        {
            return m_invUnitsPerEm;
        }

        double
        advance(wchar_t c) const
        {
            std::lock_guard<std::mutex> lock(m_advanceMutex);
            auto found = m_advances.find(static_cast<uint32_t>(c));
            if (found != m_advances.end()) {
                return found->second;
            }
            int adv = 0;
            int lsb = 0;
            stbtt_GetCodepointHMetrics(&m_info, static_cast<int>(c), &adv, &lsb);
            double emAdvance = adv * m_invUnitsPerEm;
            if (emAdvance <= 0.0) {
                emAdvance = m_defaultAdvance;
            }
            m_advances[static_cast<uint32_t>(c)] = emAdvance;
            return emAdvance;
        }

    private:
        std::vector<unsigned char> m_data; // must outlive m_info (stbtt keeps pointers)
        stbtt_fontinfo m_info {};
        double m_invUnitsPerEm = 1.0 / 1000.0;
        double m_defaultAdvance = 0.5;
        mutable std::mutex m_advanceMutex;
        mutable std::unordered_map<uint32_t, double> m_advances; // em fractions

        void
        parse(const std::string& path)
        {
            std::ifstream in(path, std::ios::binary);
            if (!in.is_open()) {
                return;
            }
            m_data.assign((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
            if (m_data.empty()) {
                return;
            }
            const int offset = stbtt_GetFontOffsetForIndex(m_data.data(), 0);
            if (offset < 0 || !stbtt_InitFont(&m_info, m_data.data(), offset)) {
                return;
            }
            // ScaleForMappingEmToPixels(1.0) is 1/unitsPerEm.
            m_invUnitsPerEm = stbtt_ScaleForMappingEmToPixels(&m_info, 1.0f);
            int fontAscent = 0;
            int fontDescent = 0;
            int lineGap = 0;
            stbtt_GetFontVMetrics(&m_info, &fontAscent, &fontDescent, &lineGap);
            ascent = fontAscent * m_invUnitsPerEm;
            descent = -fontDescent * m_invUnitsPerEm;
            int adv = 0;
            int lsb = 0;
            stbtt_GetCodepointHMetrics(&m_info, 'x', &adv, &lsb);
            if (adv > 0) {
                m_defaultAdvance = adv * m_invUnitsPerEm;
            }
            loaded = true;
        }
    };
    //=============================================================================
    // Windows Fonts directory (honors %WINDIR%). On other platforms the prefix
    // never matches a file.
    static std::string
    windowsFontsDir()
    {
        std::string win = "C:/Windows/Fonts/";
#ifdef _MSC_VER
        char* windir = nullptr;
        size_t len = 0;
        if (_dupenv_s(&windir, &len, "WINDIR") == 0 && windir != nullptr) {
            win = std::string(windir) + "/Fonts/";
            free(windir);
        }
#else
        if (const char* windir = std::getenv("WINDIR")) {
            win = std::string(windir) + "/Fonts/";
        }
#endif
        return win;
    }
    //=============================================================================
    // The first candidate file that exists and parses wins; a missing bold/italic
    // variant falls back to the regular face.
    static const TtfFont*
    loadPlatformFont(GenericFamily family, bool bold, bool italic)
    {
        auto candidatesFor = [](GenericFamily f, bool b, bool i) -> std::vector<std::string> {
            std::string win = windowsFontsDir();
            // Variant name pieces: Windows suffix, DejaVu suffix, Liberation suffix.
            const char* winSfx = b ? (i ? "bi" : "bd") : (i ? "i" : "");
            const std::string dejaSfx = b ? (i ? "-BoldOblique" : "-Bold") : (i ? "-Oblique" : "");
            const std::string libSfx
                = b ? (i ? "-BoldItalic" : "-Bold") : (i ? "-Italic" : "-Regular");
            const std::string deja = "/usr/share/fonts/truetype/dejavu/DejaVu";
            const std::string lib = "/usr/share/fonts/truetype/liberation/Liberation";
            const std::string macSup = "/System/Library/Fonts/Supplemental/";
            auto macName = [&](const std::string& base) {
                return macSup + base + (b ? (i ? " Bold Italic" : " Bold") : (i ? " Italic" : ""))
                    + ".ttf";
            };
            switch (f) {
            case GenericFamily::Mono:
                return { win + "consola" + (b ? (i ? "z" : "b") : (i ? "i" : "")) + ".ttf",
                    win + "cour" + winSfx + ".ttf", macName("Courier New"),
                    "/System/Library/Fonts/Menlo.ttc", deja + "SansMono" + dejaSfx + ".ttf",
                    lib + "Mono" + libSfx + ".ttf" };
            case GenericFamily::Serif:
                return { win + "times" + winSfx + ".ttf", macName("Times New Roman"),
                    "/System/Library/Fonts/Times.ttc", deja + "Serif" + dejaSfx + ".ttf",
                    lib + "Serif" + libSfx + ".ttf" };
            case GenericFamily::Sans:
            default:
                return { win + "arial" + winSfx + ".ttf",
                    win + (b ? (i ? "segoeuiz" : "segoeuib") : (i ? "segoeuii" : "segoeui"))
                        + ".ttf",
                    macName("Arial"), "/System/Library/Fonts/Helvetica.ttc",
                    deja + "Sans" + dejaSfx + ".ttf", lib + "Sans" + libSfx + ".ttf" };
            }
        };
        for (const std::string& path : candidatesFor(family, bold, italic)) {
            const TtfFont* font = TtfFont::load(path);
            if (font != nullptr) {
                return font;
            }
        }
        if (bold || italic) {
            return loadPlatformFont(family, false, false); // regular fallback
        }
        return nullptr;
    }
    //=============================================================================
    static GenericFamily
    resolveFamily(const std::wstring& familyName)
    {
        std::wstring family = familyName;
        for (auto& c : family) {
            c = towlower(c);
        }
        auto has = [&family](
                       const wchar_t* needle) { return family.find(needle) != std::wstring::npos; };
        // "sans-serif" contains "serif": check sans (and mono) before serif.
        if (has(L"sans")) {
            return GenericFamily::Sans;
        }
        if (has(L"courier") || has(L"mono") || has(L"consol")) {
            return GenericFamily::Mono;
        }
        if (has(L"times") || has(L"serif") || has(L"georgia") || has(L"roman")) {
            return GenericFamily::Serif;
        }
        return GenericFamily::Sans;
    }
    //=============================================================================
    // Fallback faces for codepoints the active family cannot draw. CJK-capable
    // faces come first, broad symbol/Unicode faces last. Missing files are skipped
    // by the loader.
    static const std::vector<std::string>&
    fallbackFontPaths()
    {
        static const std::vector<std::string> paths = []() {
            const std::string win = windowsFontsDir();
            return std::vector<std::string> { // Windows: Japanese / broad CJK first.
                win + "YuGothR.ttc", // Yu Gothic (Japanese)
                win + "YuGothM.ttc", win + "msgothic.ttc", // MS Gothic (Japanese)
                win + "malgun.ttf", // Malgun Gothic (Korean)
                win + "msyh.ttc", // Microsoft YaHei (Simplified Chinese)
                win + "simsun.ttc", // SimSun (Simplified Chinese)
                win + "msjh.ttc", // Microsoft JhengHei (Traditional Chinese)
                win + "Nirmala.ttc", // Indic scripts
                // Windows: broad symbol / Unicode coverage.
                win + "seguisym.ttf", // Segoe UI Symbol
                win + "arialuni.ttf", // Arial Unicode MS (when installed)
                win + "segoeui.ttf",
                // Linux: Noto CJK, then DejaVu for broad Latin/symbol coverage.
                "/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc",
                "/usr/share/fonts/truetype/noto/NotoSansCJK-Regular.ttc",
                "/usr/share/fonts/noto-cjk/NotoSansCJK-Regular.ttc",
                "/usr/share/fonts/google-noto-cjk/NotoSansCJK-Regular.ttc",
                "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
                // macOS.
                "/System/Library/Fonts/Hiragino Sans GB.ttc", "/System/Library/Fonts/PingFang.ttc",
                "/System/Library/Fonts/Apple Symbols.ttf", "/Library/Fonts/Arial Unicode.ttf"
            };
        }();
        return paths;
    }
    //=============================================================================
    // First fallback face with an outline for the codepoint, or nullptr. The result
    // is cached per codepoint.
    static const TtfFont*
    fallbackGlyphFont(wchar_t ch)
    {
        static std::mutex mutex;
        static std::unordered_map<uint32_t, const TtfFont*> cache;
        const uint32_t key = static_cast<uint32_t>(ch);
        {
            std::lock_guard<std::mutex> lock(mutex);
            auto found = cache.find(key);
            if (found != cache.end()) {
                return found->second;
            }
        }
        const TtfFont* resolved = nullptr;
        for (const std::string& path : fallbackFontPaths()) {
            const TtfFont* font = TtfFont::load(path);
            if (font != nullptr && font->hasGlyph(ch)) {
                resolved = font;
                break;
            }
        }
        std::lock_guard<std::mutex> lock(mutex);
        cache[key] = resolved;
        return resolved;
    }
    //=============================================================================
    static double
    tableAdvance(wchar_t c, GenericFamily family)
    {
        if (family == GenericFamily::Mono) {
            return 0.6; // Courier is fixed-pitch at 600/1000 em
        }
        if (c < 32 || c >= 127) {
            return family == GenericFamily::Serif ? 0.5 : 0.556; // average advance
        }
        const short* table = family == GenericFamily::Serif ? TIMES_ADVANCE : HELVETICA_ADVANCE;
        return table[c - 32] / 1000.0;
    }
    //=============================================================================
    // Ascent/descent in em fractions when no font file could be loaded.
    static void
    fallbackVerticals(GenericFamily family, double& ascent, double& descent)
    {
        ascent = 0.718;
        descent = 0.207;
        if (family == GenericFamily::Serif) {
            ascent = 0.683;
            descent = 0.217;
        } else if (family == GenericFamily::Mono) {
            ascent = 0.629;
            descent = 0.157;
        }
    }
    //=============================================================================
    // TeX markup such as 10^{-1} is flattened to precomposed Unicode super/subscript
    // characters (e.g. U+207B SUPERSCRIPT MINUS). Many fonts carry only part of that
    // range: Arial has the superscript digits but not the minus, plus or
    // parentheses, nor the subscript block. A missing codepoint would be drawn as a
    // .notdef box, so the base character is drawn scaled down and shifted instead.
    static constexpr double SUPERSUB_SCALE = 0.58; // size relative to full glyph
    static constexpr double SUPERSCRIPT_RISE = 0.33; // em raised above the baseline
    static constexpr double SUBSCRIPT_DROP = 0.15; // em lowered below the baseline
    //=============================================================================
    // kind is +1 for a superscript, -1 for a subscript. Returns false when ch is
    // not a handled form.
    static bool
    superSubBase(wchar_t ch, wchar_t& base, int& kind)
    {
        switch (static_cast<char32_t>(ch)) {
        // Superscript digits (U+2070, U+00B9/B2/B3, U+2074..U+2079).
        case 0x2070:
            base = L'0';
            kind = 1;
            return true;
        case 0x00B9:
            base = L'1';
            kind = 1;
            return true;
        case 0x00B2:
            base = L'2';
            kind = 1;
            return true;
        case 0x00B3:
            base = L'3';
            kind = 1;
            return true;
        case 0x2074:
            base = L'4';
            kind = 1;
            return true;
        case 0x2075:
            base = L'5';
            kind = 1;
            return true;
        case 0x2076:
            base = L'6';
            kind = 1;
            return true;
        case 0x2077:
            base = L'7';
            kind = 1;
            return true;
        case 0x2078:
            base = L'8';
            kind = 1;
            return true;
        case 0x2079:
            base = L'9';
            kind = 1;
            return true;
        // Superscript operators / parentheses (U+207A..U+207E).
        case 0x207A:
            base = L'+';
            kind = 1;
            return true;
        case 0x207B:
            base = L'-';
            kind = 1;
            return true;
        case 0x207C:
            base = L'=';
            kind = 1;
            return true;
        case 0x207D:
            base = L'(';
            kind = 1;
            return true;
        case 0x207E:
            base = L')';
            kind = 1;
            return true;
        // Subscript digits (U+2080..U+2089).
        case 0x2080:
            base = L'0';
            kind = -1;
            return true;
        case 0x2081:
            base = L'1';
            kind = -1;
            return true;
        case 0x2082:
            base = L'2';
            kind = -1;
            return true;
        case 0x2083:
            base = L'3';
            kind = -1;
            return true;
        case 0x2084:
            base = L'4';
            kind = -1;
            return true;
        case 0x2085:
            base = L'5';
            kind = -1;
            return true;
        case 0x2086:
            base = L'6';
            kind = -1;
            return true;
        case 0x2087:
            base = L'7';
            kind = -1;
            return true;
        case 0x2088:
            base = L'8';
            kind = -1;
            return true;
        case 0x2089:
            base = L'9';
            kind = -1;
            return true;
        // Subscript operators / parentheses (U+208A..U+208E).
        case 0x208A:
            base = L'+';
            kind = -1;
            return true;
        case 0x208B:
            base = L'-';
            kind = -1;
            return true;
        case 0x208C:
            base = L'=';
            kind = -1;
            return true;
        case 0x208D:
            base = L'(';
            kind = -1;
            return true;
        case 0x208E:
            base = L')';
            kind = -1;
            return true;
        default:
            return false;
        }
    }
    //=============================================================================
    // Uses the same super/subscript synthesis as glyphBitmap, so measured widths
    // match what is drawn.
    static double
    charEmAdvance(const TtfFont* font, GenericFamily generic, wchar_t ch)
    {
        if (font == nullptr) {
            return tableAdvance(ch, generic);
        }
        wchar_t base = 0;
        int kind = 0;
        if (!font->hasGlyph(ch)) {
            // Measure on the face that will render the glyph, so the reserved
            // advance matches the drawn one.
            if (superSubBase(ch, base, kind)) {
                return font->advance(base) * SUPERSUB_SCALE;
            }
            const TtfFont* fallback = fallbackGlyphFont(ch);
            if (fallback != nullptr) {
                return fallback->advance(ch);
            }
        }
        return font->advance(ch);
    }
    //=============================================================================
    std::string
    genericFamily(const std::wstring& family)
    {
        switch (resolveFamily(family)) {
        case GenericFamily::Serif:
            return "serif";
        case GenericFamily::Mono:
            return "monospace";
        case GenericFamily::Sans:
        default:
            return "sans-serif";
        }
    }
    //=============================================================================
    double
    textWidthPixels(const std::wstring& text, const std::wstring& family, double pixelSize,
        bool bold, bool italic)
    {
        if (text.find(L'\n') != std::wstring::npos) {
            // A multi-line string is as wide as its widest line.
            double widest = 0.0;
            for (const std::wstring& line : splitTextLines(text)) {
                widest = std::max(widest, textWidthPixels(line, family, pixelSize, bold, italic));
            }
            return widest;
        }
        const GenericFamily generic = resolveFamily(family);
        const TtfFont* font = loadPlatformFont(generic, bold, italic);
        double advances = 0.0;
        for (wchar_t c : text) {
            advances += charEmAdvance(font, generic, c);
        }
        return advances * pixelSize;
    }
    //=============================================================================
    double
    textHeightPixels(const std::wstring& text, const std::wstring& family, double pixelSize,
        bool bold, bool italic)
    {
        if (text.find(L'\n') != std::wstring::npos) {
            // Ink top of the first line to ink bottom of the last: full line height
            // for every line but the last, tight height for the last.
            const std::vector<std::wstring> lines = splitTextLines(text);
            return (lines.size() - 1) * lineHeightPixels(family, pixelSize, bold, italic)
                + textHeightPixels(lines.back(), family, pixelSize, bold, italic);
        }
        // Ascent plus the descender band only when the text has a descender.
        const GenericFamily generic = resolveFamily(family);
        double ascent;
        double descentBand;
        const TtfFont* font = loadPlatformFont(generic, bold, italic);
        if (font != nullptr) {
            // hhea ascent covers accents; the ink cap band Qt reports is tighter.
            ascent = std::min(font->ascent, 0.9);
            descentBand = font->descent;
        } else {
            fallbackVerticals(generic, ascent, descentBand);
        }
        bool hasDescender = false;
        for (wchar_t c : text) {
            if (c == L'g' || c == L'j' || c == L'p' || c == L'q' || c == L'y' || c == L'Q'
                || c == L',' || c == L';') {
                hasDescender = true;
                break;
            }
        }
        return pixelSize * (ascent + (hasDescender ? descentBand : 0.0));
    }
    //=============================================================================
    double
    lineHeightPixels(const std::wstring& family, double pixelSize, bool bold, bool italic)
    {
        const GenericFamily generic = resolveFamily(family);
        const TtfFont* font = loadPlatformFont(generic, bold, italic);
        double ascent;
        double descent;
        if (font != nullptr) {
            ascent = font->ascent; // raw hhea values, no ink clamp
            descent = font->descent;
        } else {
            fallbackVerticals(generic, ascent, descent);
        }
        return pixelSize * (ascent + descent);
    }
    //=============================================================================
    double
    ascentPixels(const std::wstring& family, double pixelSize, bool bold, bool italic)
    {
        const GenericFamily generic = resolveFamily(family);
        const TtfFont* font = loadPlatformFont(generic, bold, italic);
        double ascent;
        double descent;
        if (font != nullptr) {
            ascent = std::min(font->ascent, 0.9);
        } else {
            fallbackVerticals(generic, ascent, descent);
        }
        return pixelSize * ascent;
    }
    //=============================================================================
    double
    descentPixels(const std::wstring& family, double pixelSize, bool bold, bool italic)
    {
        const GenericFamily generic = resolveFamily(family);
        const TtfFont* font = loadPlatformFont(generic, bold, italic);
        if (font != nullptr) {
            return pixelSize * font->descent;
        }
        double ascent;
        double descent;
        fallbackVerticals(generic, ascent, descent);
        return pixelSize * descent;
    }
    //=============================================================================
    double
    glyphAdvancePixels(
        wchar_t ch, const std::wstring& family, double pixelSize, bool bold, bool italic)
    {
        const GenericFamily generic = resolveFamily(family);
        const TtfFont* font = loadPlatformFont(generic, bold, italic);
        return charEmAdvance(font, generic, ch) * pixelSize;
    }
    //=============================================================================
    bool
    hasRealFont(const std::wstring& family, bool bold, bool italic)
    {
        return loadPlatformFont(resolveFamily(family), bold, italic) != nullptr;
    }
    //=============================================================================
    bool
    glyphBitmap(wchar_t ch, const std::wstring& family, double pixelSize, bool bold, bool italic,
        GlyphBitmap& out)
    {
        const TtfFont* font = loadPlatformFont(resolveFamily(family), bold, italic);
        if (font == nullptr) {
            return false;
        }
        wchar_t drawChar = ch;
        double riseEm = 0.0;
        double drawScale = 1.0;
        const TtfFont* glyphFont = font;
        wchar_t base = 0;
        int kind = 0;
        if (!font->hasGlyph(ch)) {
            if (superSubBase(ch, base, kind)) {
                drawChar = base;
                drawScale = SUPERSUB_SCALE;
                riseEm = (kind > 0) ? SUPERSCRIPT_RISE : -SUBSCRIPT_DROP;
            } else {
                // E.g. CJK in an Arial figure. The glyph is scaled by the fallback
                // face's own em, so it lands on the same baseline.
                const TtfFont* fallback = fallbackGlyphFont(ch);
                if (fallback != nullptr) {
                    glyphFont = fallback;
                }
            }
        }
        const float scale = static_cast<float>(glyphFont->invUnitsPerEm() * pixelSize * drawScale);
        int gw = 0, gh = 0, gx = 0, gy = 0;
        unsigned char* bitmap = stbtt_GetCodepointBitmap(
            glyphFont->fontInfo(), scale, scale, static_cast<int>(drawChar), &gw, &gh, &gx, &gy);
        if (bitmap == nullptr) {
            out = GlyphBitmap {};
            return true; // glyph with no ink (e.g. space); advance still applies
        }
        out.width = gw;
        out.height = gh;
        out.xoffset = gx;
        // yoffset is measured downward from the baseline: raising subtracts the rise.
        out.yoffset = gy - static_cast<int>(std::lround(riseEm * pixelSize));
        out.alpha.assign(bitmap, bitmap + static_cast<size_t>(gw) * gh);
        stbtt_FreeBitmap(bitmap, nullptr);
        return true;
    }
    //=============================================================================
    // Split on '\n' only; a '\r' left by a CRLF source would show as a missing glyph.
    std::vector<std::wstring>
    splitTextLines(const std::wstring& text)
    {
        std::vector<std::wstring> lines;
        std::wstring::size_type start = 0;
        while (true) {
            std::wstring::size_type stop = text.find(L'\n', start);
            std::wstring line = text.substr(
                start, (stop == std::wstring::npos) ? std::wstring::npos : stop - start);
            if (!line.empty() && line.back() == L'\r') {
                line.pop_back();
            }
            lines.push_back(line);
            if (stop == std::wstring::npos) {
                break;
            }
            start = stop + 1;
        }
        return lines;
    }
    //=============================================================================
} // namespace FontMetrics
//=============================================================================
} // namespace Nelson
//=============================================================================
