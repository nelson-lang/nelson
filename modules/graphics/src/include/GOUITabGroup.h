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
class GOUITab;
//=============================================================================
// uitabgroup: tab group container (children are uitab objects)
class NLSGRAPHICS_IMPEXP GOUITabGroup : public GOUIContainer
{
public:
    //=============================================================================
    GOUITabGroup();
    ~GOUITabGroup() override = default;
    //=============================================================================
    std::wstring
    getType() override;
    //=============================================================================
    std::string
    getClassName() override;
    //=============================================================================
    void
    paintChrome(RenderInterface& gc) override;
    //=============================================================================
    void
    paintContent(RenderInterface& gc) override;
    //=============================================================================
    std::vector<double>
    contentRect(RenderInterface& gc) override;
    //=============================================================================
    // Thickness of the tab bar, on the side named by TabLocation.
    double
    tabBarThickness(RenderInterface& gc);
    //=============================================================================
    // The tab currently on top, the first one when SelectedTab says nothing.
    GOUITab*
    selectedTab();
    //=============================================================================
private:
    //=============================================================================
    bool
    isVerticalTabBar();
    // The whole tab bar, on the side named by TabLocation, in pixels of the
    // group frame.
    std::vector<double>
    tabBarRect(RenderInterface& gc, const std::vector<double>& page, double thickness);
    // Rectangle of one tab label on the bar, in pixels of the group frame.
    std::vector<double>
    tabLabelBox(const std::vector<double>& page, double thickness, double cursor, double extent);
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
