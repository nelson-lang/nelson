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
#include <string>
#include <vector>
#include "nlsFontMetrics_exports.h"
//=============================================================================
// Qt-free font engine (stb_truetype over the platform TTF files, AFM core-font
// advance tables as fallback), tuned to track QFontMetrics closely.
// The graphics model measures text with it (text Extent, tick label widths) and
// RenderWeb rasterizes glyphs from it. It sits below graphics and render_web so
// that render_qt stays Qt-only and render_web stays web-only.
//
// `family` accepts a real family name ("Times New Roman", "Consolas") or a CSS
// generic ("serif", "sans-serif", "monospace"); it resolves to one of three
// generic families.
//=============================================================================
namespace Nelson {
//=============================================================================
namespace FontMetrics {
    //=============================================================================
    // "sans-serif" | "serif" | "monospace"
    NLSFONTMETRICS_IMPEXP std::string
    genericFamily(const std::wstring& family);
    //=============================================================================
    NLSFONTMETRICS_IMPEXP double
    textWidthPixels(const std::wstring& text, const std::wstring& family, double pixelSize,
        bool bold, bool italic);
    //=============================================================================
    // Tight ink height: the descender band counts only when the text has one.
    NLSFONTMETRICS_IMPEXP double
    textHeightPixels(const std::wstring& text, const std::wstring& family, double pixelSize,
        bool bold, bool italic);
    //=============================================================================
    NLSFONTMETRICS_IMPEXP double
    ascentPixels(const std::wstring& family, double pixelSize, bool bold, bool italic);
    //=============================================================================
    // Full hhea line height (raw ascent + descent, no ink clamping) in pixels.
    // Round it to get the integers QFontMetrics reports for the '|' bounding box.
    NLSFONTMETRICS_IMPEXP double
    lineHeightPixels(const std::wstring& family, double pixelSize, bool bold, bool italic);
    //=============================================================================
    NLSFONTMETRICS_IMPEXP double
    descentPixels(const std::wstring& family, double pixelSize, bool bold, bool italic);
    //=============================================================================
    NLSFONTMETRICS_IMPEXP double
    glyphAdvancePixels(
        wchar_t ch, const std::wstring& family, double pixelSize, bool bold, bool italic);
    //=============================================================================
    // True when a real TTF was found for the family/style (glyph bitmaps possible).
    NLSFONTMETRICS_IMPEXP bool
    hasRealFont(const std::wstring& family, bool bold, bool italic);
    //=============================================================================
    // 8-bit alpha bitmap of one glyph at the given pixel size, with its offsets
    // relative to the baseline pen position (stb_truetype conventions).
    struct GlyphBitmap
    {
        std::vector<unsigned char> alpha; // width * height, row-major
        int width = 0;
        int height = 0;
        int xoffset = 0;
        int yoffset = 0;
    };
    //=============================================================================
    NLSFONTMETRICS_IMPEXP bool
    glyphBitmap(wchar_t ch, const std::wstring& family, double pixelSize, bool bold, bool italic,
        GlyphBitmap& out);
    //=============================================================================
} // namespace FontMetrics
//=============================================================================
} // namespace Nelson
//=============================================================================
