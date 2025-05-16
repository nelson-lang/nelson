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
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOWindow;
//=============================================================================
class NLSGRAPHICS_IMPEXP GOFigure : public GraphicsObject
{
private:
    int m_width, m_height;
    bool _resized;
    bool renderingStateInvalid = false;
    void
    loadDefaultAlphaMap();
    void
    loadDefaultColorMap();
    GOWindow* m_win;
    bool lockResize = false;
    void
    refreshDrawLaterProperty(bool forceUpdate);

    int
    transformY(int y, int heightFrame, int screenHeight);

    void
    setOuterPosition(int x, int y, int w, int h);

    void
    getOuterPosition(int& x, int& y, int& w, int& h);

    void
    setInnerPosition(int x, int y, int w, int h);

    void
    getInnerPosition(int& x, int& y, int& w, int& h);

    void
    setPosition(int x, int y, int w, int h);

    void
    getPosition(int& x, int& y, int& w, int& h);

public:
    GOFigure(GOWindow* win, int number);
    ~GOFigure() override = default;
    void
    registerProperties() override;
    bool
    resized();
    int
    getWidth()
    {
        return m_width;
    }
    int
    getHeight()
    {
        return m_height;
    }
    std::wstring
    getType() override;
    void
    updateState(bool forceUpdate);
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
    virtual void
    resizeGL(int width, int height);
    void
    initializeProperties();
    void
    repaint();
    void
    setFocus();

    void
    refreshPositionProperties(bool forceUpdate);

    GOWindow*
    getGOWindow();

    bool
    isRenderingStateInvalid()
    {
        return renderingStateInvalid;
    };
    void
    setRenderingStateInvalid(bool invalid)
    {
        renderingStateInvalid = invalid;
    }
};
//=============================================================================
}
//=============================================================================
