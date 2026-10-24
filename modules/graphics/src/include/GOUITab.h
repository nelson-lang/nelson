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
#include "GOUIContainer.h"
//=============================================================================
namespace Nelson {
//=============================================================================
class NLSGRAPHICS_IMPEXP GOUITab : public GOUIContainer
{
public:
    //=============================================================================
    GOUITab();
    ~GOUITab() override = default;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    // called by the widget binding when the tab page changes: keep the public
    // tab geometry aligned with the parent tab group and relocate children
    void
    onPageResized(double width, double height);
    //=============================================================================
    bool
    managesOwnGeometry() override
    {
        return true;
    }
    //=============================================================================
    // A tab page fills the area its group leaves below (or beside) the tab bar,
    // which is the frame the group installs before painting it.
    std::vector<double>
    frameRect(RenderInterface& gc) override;
    //=============================================================================
private:
    //=============================================================================
    void
    constructProperties();
    void
    setupDefaults();
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
