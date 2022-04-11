//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "GraphicObject.hpp"
#include "GraphicRenderer.hpp"
#include "GOWindow.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define FIGURE_TYPE_STR "figure"
//=============================================================================
class GOWindow;
//=============================================================================
class GOFigure : public GraphicObject
{
    bool m_resized;
    GOWindow* m_win;

public:
    GOFigure(GOWindow* win);
    ~GOFigure() override = default;
    void
    registerProperties() override;
    bool
    resized();
    void
    refreshProperties() override;
    virtual void
    paintMe(GraphicRenderer& gc);
    virtual void
    resizeGL(int width, int height);
    void
    initializeProperties();
    void
    repaint();
    GOWindow*
    getParentWindow();
    uint64
    id();

private:
    void
    refreshColorProperty();
    void
    refreshOuterPositionProperty();
    void
    refreshInnerPositionProperty();
    void
    refreshPositionProperty();

    void
    applyBackgroundProperty();
    void
    applyVisibleProperty();
    void
    applyNameProperty();
    void
    applyOuterPositionProperty();
    void
    applyInnerPositionProperty();
    void
    applyPositionProperty();
    int
    computeYOuterInnerOffset();
    int
    computeXOuterInnerOffset();
};
//=============================================================================
} // namespace Nelson
//=============================================================================
