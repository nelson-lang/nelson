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
};
//=============================================================================
} // namespace Nelson
//=============================================================================
