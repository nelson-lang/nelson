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
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
struct TexBlockLayout;
//=============================================================================
class NLSGRAPHICS_IMPEXP GOText : public GraphicsObject
{
private:
    FontSpec fnt;
    std::wstring text;
    // When the interpreter is TeX and the string carries a super/subscript
    // marker (and is single-line), 'text' holds the raw string (Greek/symbols
    // still resolved at layout time) and the paint path lays out true raised /
    // lowered runs. Otherwise 'text' holds the ready-to-draw string.
    bool texScript = false;
    bool stateInitialized = false;
    std::wstring
    replaceSpecialCharacters(const std::wstring& text);
    // Anchor of the text (its Position) in window pixels for the projection set on gc.
    bool
    anchorPixels(
        RenderInterface& gc, int& x, int& y, double* exactX = nullptr, double* exactY = nullptr);
    // A Units change converts Position so the text stays in place.
    void
    convertPositionOnUnitsChange();
    // Text box (no margin) relative to the anchor: width, height and offsets in pixels.
    void
    measureBox(RenderInterface& gc, const FontSpec& paintFont,
        RenderInterface::AlignmentFlag xalign, RenderInterface::AlignmentFlag yalign, int& width,
        int& height, int& xoffset, int& yoffset, TexBlockLayout* layout);

public:
    GOText();
    ~GOText() override;
    virtual std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_TEXT_STR;
    }
    wstringVector
    getVisibleFieldnames() override;
    virtual void
    constructProperties();
    virtual void
    setupDefaults();
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
    int
    getTextHeightInPixels();
    // Extent [left bottom width height] of the text box in the text Units.
    std::vector<double>
    computeExtent();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
