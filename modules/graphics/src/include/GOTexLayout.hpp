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
// Shared layout and rendering of TeX super/subscript runs for titles, axis labels, legends
// and other text.
//=============================================================================
#include <algorithm>
#include <cmath>
#include <string>
#include <vector>
#include "RenderInterface.hpp"
#include "GOFontSpecHelpers.hpp"
#include "TexToUnicode.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Size ratio per super/subscript level, and the deepest level that still shrinks the font.
constexpr double TEX_SCRIPT_SCALE = 0.7;
constexpr int TEX_MAX_SCALE_DEPTH = 3;
//=============================================================================
// Layout of one TeX run within a super/subscript block: its font, horizontal
// position (left edge, from the block origin) and width in pixels, plus the
// baseline shift applied for its level (super = up, sub = down).
struct TexRunLayout
{
    std::wstring text;
    FontSpec font;
    double x = 0.0;
    double width = 0.0;
    double baselineShift = 0.0;
};
//=============================================================================
// The whole laid-out block: the runs left-to-right, the total advance width,
// and the block extent above / below the baseline of the base-level text.
struct TexBlockLayout
{
    std::vector<TexRunLayout> runs;
    double totalWidth = 0.0;
    double topAbove = 0.0;
    double botBelow = 0.0;
};
//=============================================================================
inline FontSpec
levelFontSpec(const FontSpec& base, int level)
{
    const int depth = std::min(std::abs(level), TEX_MAX_SCALE_DEPTH);
    double scale = 1.0;
    for (int k = 0; k < depth; ++k) {
        scale *= TEX_SCRIPT_SCALE;
    }
    FontSpec f(base);
    if (f.pixelSize > 0) {
        f.pixelSize *= scale;
    } else {
        f.pointSizeF *= scale;
    }
    return f;
}
//=============================================================================
// Cumulative baseline shift (up positive) for a run at the given level. Each
// step up raises by 0.40x the base ascent, each step down lowers by 0.15x,
// both diminishing with the shrinking font so nested scripts stay compact.
inline double
levelBaselineShift(int level, double ascentBase)
{
    const double stepFraction = (level > 0) ? 0.40 : 0.15;
    const int steps = std::abs(level);
    double shift = 0.0;
    double stepSize = stepFraction * ascentBase;
    for (int k = 0; k < steps; ++k) {
        shift += stepSize;
        stepSize *= TEX_SCRIPT_SCALE;
    }
    return (level < 0) ? -shift : shift;
}
//=============================================================================
inline TexBlockLayout
buildTexBlockLayout(const std::vector<TexRun>& parsed, const FontSpec& baseFont)
{
    TexBlockLayout layout;
    const double ascentBase = goFontAscent(baseFont);
    const double descentBase = goFontDescent(baseFont);
    double x = 0.0;
    double topAbove = ascentBase;
    double botBelow = descentBase;
    for (const TexRun& r : parsed) {
        if (r.text.empty()) {
            continue;
        }
        TexRunLayout rl;
        rl.text = r.text;
        rl.font = (r.level == 0) ? baseFont : levelFontSpec(baseFont, r.level);
        rl.baselineShift = levelBaselineShift(r.level, ascentBase);
        int w = 0;
        int h = 0;
        measureGOText(rl.text, rl.font, w, h);
        rl.width = w;
        rl.x = x;
        x += w;
        const double asc = goFontAscent(rl.font);
        const double desc = goFontDescent(rl.font);
        topAbove = std::max(topAbove, rl.baselineShift + asc);
        botBelow = std::max(botBelow, desc - rl.baselineShift);
        layout.runs.push_back(rl);
    }
    layout.totalWidth = x;
    layout.topAbove = topAbove;
    layout.botBelow = botBelow;
    return layout;
}
//=============================================================================
// Every run is emitted with (Min, Min) so putText places its baseline-left origin. The
// per-run offset (Xloc, Yloc) is relative to the anchor and rotated like putText does, so
// rotated labels still lay out correctly. putText adds the run's own descent for
// (Min, Min), so it is pre-subtracted here.
inline void
drawTexBlockLayout(RenderInterface& gc, double x, double y, const TexBlockLayout& blockLayout,
    const std::vector<double>& color, RenderInterface::AlignmentFlag xalign,
    RenderInterface::AlignmentFlag yalign, double rotation)
{
    double B; // baseline of the base-level text, as a local offset (up +).
    if (yalign == RenderInterface::Max) {
        B = -blockLayout.topAbove;
    } else if (yalign == RenderInterface::Mean) {
        B = (blockLayout.botBelow - blockLayout.topAbove) / 2.0;
    } else {
        B = blockLayout.botBelow;
    }
    double startX;
    if (xalign == RenderInterface::Mean) {
        startX = -blockLayout.totalWidth / 2.0;
    } else if (xalign == RenderInterface::Max) {
        startX = -blockLayout.totalWidth;
    } else {
        startX = 0.0;
    }
    constexpr double piValue = 3.14159265358979323846;
    const double cosR = std::cos(rotation * piValue / 180.0);
    const double sinR = std::sin(rotation * piValue / 180.0);
    for (const TexRunLayout& run : blockLayout.runs) {
        const double Xloc = startX + run.x;
        const double Yloc = B + run.baselineShift;
        const double localY = Yloc - goFontDescent(run.font);
        const double a = x + Xloc * cosR - localY * sinR;
        const double b = y + Xloc * sinR + localY * cosR;
        gc.putText(
            a, b, run.text, color, RenderInterface::Min, RenderInterface::Min, run.font, rotation);
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
