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
#include "GOUIComponent.h"
#include "RenderInterface.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Base class for the App Designer containers (uipanel, uibuttongroup, uitabgroup, uitab,
// uigridlayout).
class NLSGRAPHICS_IMPEXP GOUIContainer : public GOUIComponent
{
public:
    //=============================================================================
    GOUIContainer() = default;
    ~GOUIContainer() override = default;
    //=============================================================================
    std::vector<int64>
    getChildrenHandles();
    //=============================================================================
    // Reapplies children geometry after a resize and notifies SizeChangedFcn when set.
    void
    refreshChildrenGeometry();
    //=============================================================================
    void
    onGeometryChanged() override;
    //=============================================================================
    // The chrome and the graphics children are painted in the container's own coordinate
    // space. UI component children are left out: each carries its own widget (Qt) or
    // control description (web).
    void
    paintMe(RenderInterface& gc) override;
    //=============================================================================
    // Chrome and graphics children without the widget snapshot or web control entry of the
    // component base. A nested container is entered this way, its parent having already
    // described the whole component subtree.
    void
    paintContainer(RenderInterface& gc);
    //=============================================================================
    virtual void
    paintChrome(RenderInterface& gc);
    //=============================================================================
    virtual void
    paintContent(RenderInterface& gc);
    //=============================================================================
    // Entered from the container frame. The widget backends call this: the toolkit paints
    // the chrome, the graphics children are still painted here.
    void
    paintContentInFrame(RenderInterface& gc);
    //=============================================================================
    // In pixels of the enclosing frame, bottom-left origin. Empty when the geometry is unknown.
    virtual std::vector<double>
    frameRect(RenderInterface& gc);
    //=============================================================================
    // Area left to the children once the chrome is deducted, in pixels of the container frame.
    virtual std::vector<double>
    contentRect(RenderInterface& gc);
    //=============================================================================
protected:
    //=============================================================================
    // Only updates when changes are pending: an axes update marks the figure dirty and
    // requests a repaint, so an unconditional update from a paint would schedule the next one.
    static void
    updateStateIfChanged(GraphicsObject* child);
    //=============================================================================
    FontSpec
    chromeFont();
    //=============================================================================
    double
    chromeTextHeight(RenderInterface& gc);
    //=============================================================================
    void
    registerPanelLikeProperties();
    void
    setupPanelLikeDefaults(double x, double y, double w, double h);
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
