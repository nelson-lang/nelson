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
// Qt-free font handling for the drawing model: build a
// FontSpec from the standard GO font properties and measure text through the
// shared font engine (module font_metrics) instead of QFontMetrics. The
// measurements are tuned to track Qt's, so the Qt desktop layout stays put.
//=============================================================================
#include <cmath>
#include <string>
#include "RenderInterface.hpp"
#include "FontMetrics.hpp"
#include "GOPropertyValues.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Nelson font points -> Qt point size, passed through 1:1 (96 dpi convention). A scale
// factor would enlarge all text and tie getframe text size to the OS display scaling.
inline double
goFontPointsToPointSize(double points)
{
    return points;
}
//=============================================================================
// Build a FontSpec from the raw GO font property values (FontName resolved,
// FontAngle / FontWeight as their property strings).
inline FontSpec
makeGOFontSpec(const std::wstring& fontName, bool sizeInPixels, double fontSize,
    const std::wstring& fontAngle, const std::wstring& fontWeight, bool antialias = true)
{
    FontSpec spec;
    spec.family = fontName;
    if (sizeInPixels) {
        spec.pixelSize = std::round(fontSize);
    } else {
        spec.pointSizeF = goFontPointsToPointSize(fontSize);
    }
    spec.italic
        = fontAngle == GO_PROPERTY_VALUE_ITALIC_STR || fontAngle == GO_PROPERTY_VALUE_OBLIQUE_STR;
    if (fontWeight == GO_PROPERTY_VALUE_BOLD_STR) {
        spec.setWeight(FontSpec::WeightBold);
    } else if (fontWeight == GO_PROPERTY_VALUE_LIGHT_STR) {
        spec.setWeight(FontSpec::WeightLight);
    } else if (fontWeight == GO_PROPERTY_VALUE_DEMI_STR) {
        spec.setWeight(FontSpec::WeightDemiBold);
    } else {
        spec.setWeight(FontSpec::WeightNormal);
    }
    spec.antialias = antialias;
    return spec;
}
//=============================================================================
// Pixel bounding box of a string (QFontMetrics::boundingRect equivalent).
inline void
measureGOText(const std::wstring& text, const FontSpec& font, int& width, int& height)
{
    const double px = fontSpecPixelSize(font);
    width = static_cast<int>(
        std::lround(FontMetrics::textWidthPixels(text, font.family, px, font.bold, font.italic)
            * (font.stretch > 0 ? font.stretch : 1.0)));
    height = static_cast<int>(
        std::lround(FontMetrics::textHeightPixels(text, font.family, px, font.bold, font.italic)));
}
//=============================================================================
// Full line height (ascent + descent), what QFontMetrics::height() reports.
inline int
goFontLineHeight(const FontSpec& font)
{
    const double px = fontSpecPixelSize(font);
    return static_cast<int>(
        std::lround(FontMetrics::ascentPixels(font.family, px, font.bold, font.italic)
            + FontMetrics::descentPixels(font.family, px, font.bold, font.italic)));
}
//=============================================================================
inline int
goFontAscent(const FontSpec& font)
{
    const double px = fontSpecPixelSize(font);
    return static_cast<int>(
        std::lround(FontMetrics::ascentPixels(font.family, px, font.bold, font.italic)));
}
//=============================================================================
inline int
goFontDescent(const FontSpec& font)
{
    const double px = fontSpecPixelSize(font);
    return static_cast<int>(
        std::lround(FontMetrics::descentPixels(font.family, px, font.bold, font.italic)));
}
//=============================================================================
} // namespace Nelson
//=============================================================================
