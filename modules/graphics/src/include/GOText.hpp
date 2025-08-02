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
    QFont fnt;
    std::wstring text;
    std::wstring
    replaceSpecialCharacters(const std::wstring& text);

public:
    GOText();
    ~GOText() override;
    virtual std::wstring
    getType() override;
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
}
//=============================================================================
