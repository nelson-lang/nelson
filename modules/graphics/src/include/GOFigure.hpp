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
#include <cmath>
#include <vector>
#include "nlsGraphics_exports.h"
#include "GraphicsObject.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
// Default figure Position measured on a 1536x864 screen. Used as the headless fallback and as
// the fraction scaled with the actual screen by defaultFigurePosition().
constexpr int GO_DEFAULT_FIGURE_X = 338;
constexpr int GO_DEFAULT_FIGURE_Y = 144;
constexpr int GO_DEFAULT_FIGURE_WIDTH = 859;
constexpr int GO_DEFAULT_FIGURE_HEIGHT = 517;
constexpr int GO_DEFAULT_FIGURE_REFERENCE_SCREEN_WIDTH = 1536;
constexpr int GO_DEFAULT_FIGURE_REFERENCE_SCREEN_HEIGHT = 864;
//=============================================================================
NLSGRAPHICS_IMPEXP std::vector<double>
defaultFigurePosition();
//=============================================================================
class GOFigureHost;
//=============================================================================
class NLSGRAPHICS_IMPEXP GOFigure : public GraphicsObject
{
private:
    int m_width, m_height;
    // ratio between the rendering size above and the layout size: 1 for an
    // ordinary render, the device pixel ratio on screen, the export ratio when
    // rendering to a file. Layout geometry must always use the layout size.
    double m_renderScale = 1.0;
    bool m_childrenLayoutStale = false;
    // the host applied a size whose resize report is still to come
    bool m_sizeReportExpected = false;
    bool _resized;
    bool renderingStateInvalid = false;
    ArrayOf m_workspaceHistory;
    go_handle m_currentTiledLayout = -1;
    void
    loadDefaultAlphaMap();
    void
    loadDefaultColorMap();
    GOFigureHost* m_win;
    bool lockResize = false;
    bool m_hasExposedPositionOverride = false;
    std::vector<double> m_positionOverride;
    std::vector<double> m_innerPositionOverride;
    std::vector<double> m_outerPositionOverride;
    std::vector<double> m_paperPositionOverride;
    void
    refreshDrawLaterProperty(bool forceUpdate);

    void
    repaintInternal(bool immediate);

    void
    refreshPaperPosition();

    double
    transformY(double y, double heightFrame, double screenHeight);

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
    // Default figure background of each theme (light gray, dark gray). Shared by the theme
    // handling, reset, the Qt pre-clear and the export path.
    static constexpr double LIGHT_BACKGROUND_GRAY = 0.94;
    static constexpr double DARK_BACKGROUND_GRAY = 33.0 / 255.0;
    GOFigure(GOFigureHost* win, int number);
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
    void
    setRenderingSize(int width, int height, double renderScale = 1.0)
    {
        if (width != m_width || height != m_height) {
            m_childrenLayoutStale = true;
        }
        m_width = width;
        m_height = height;
        m_renderScale = renderScale > 0 ? renderScale : 1.0;
    }
    // The host applied a new size before its resize report arrives (Cocoa delivers the
    // resize later): that report, although it carries the size already set, must still run
    // the SizeChangedFcn.
    void
    expectSizeReport()
    {
        m_sizeReportExpected = true;
    }
    // The children were last laid out for a previous rendering size: an auto axes
    // Position (pixel insets for ticks, labels, title) must be re-derived before
    // the next paint. Set by setRenderingSize, consumed by updateState.
    bool
    childrenLayoutStale() const
    {
        return m_childrenLayoutStale;
    }
    void
    setChildrenLayoutStale(bool stale)
    {
        m_childrenLayoutStale = stale;
    }
    // Re-derive the children layout now when the rendering size changed, without
    // the host-state side effects of updateState (the web host keeps Position as
    // the script-side logical size, apart from the panel rendering size).
    void
    refreshChildrenLayout();
    bool
    applyWebWindowGeometry(int x, int y, int width, int height);
    double
    getRenderScale()
    {
        return m_renderScale;
    }
    int
    getLayoutWidth()
    {
        return static_cast<int>(std::lround(m_width / m_renderScale));
    }
    int
    getLayoutHeight()
    {
        return static_cast<int>(std::lround(m_height / m_renderScale));
    }
    std::wstring
    getType() override;
    std::string
    getClassName() override
    {
        return GO_CLASS_NAME_FIGURE_STR;
    }
    void
    updateState(bool forceUpdate);
    void
    updateState() override;
    void
    paintMe(RenderInterface& gc) override;
    void
    paintAnnotations(RenderInterface& gc);
    virtual void
    resizeGL(int width, int height);
    void
    initializeProperties();
    void
    repaint();
    void
    repaintNow();
    bool
    isDrawLater();
    void
    setFocus();
    void
    setTheme(const std::wstring& theme, bool manualMode);
    void
    setThemeMode(const std::wstring& mode);
    void
    applyTheme();
    void
    applyThemeToObject(GraphicsObject* object);

    void
    refreshPositionProperties(bool forceUpdate);

    void
    setExposedPositionOverride(const std::vector<double>& position,
        const std::vector<double>& innerPosition, const std::vector<double>& outerPosition,
        const std::vector<double>& paperPosition);

    GOFigureHost*
    getHost();

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

    ArrayOf&
    workspaceHistory()
    {
        return m_workspaceHistory;
    }

    go_handle
    getCurrentTiledLayout() const
    {
        return m_currentTiledLayout;
    }

    void
    setCurrentTiledLayout(go_handle h)
    {
        m_currentTiledLayout = h;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
