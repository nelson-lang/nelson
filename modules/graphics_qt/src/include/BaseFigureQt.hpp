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
#include <memory>
#include <QtCore/QtGlobal>
#include <QtOpenGLWidgets/QOpenGLWidget>
#include <QtGui/QImage>
#include <QtGui/QResizeEvent>
#include <QtGui/QScreen>
#include "GOFigure.hpp"
#include <QtCore/QBasicTimer>
#include <QtCore/QTimerEvent>
//=============================================================================
class RenderOpenGL;
//=============================================================================
namespace Nelson {
//=============================================================================
class BaseFigureQt : public QOpenGLWidget
{
    //=============================================================================
private:
    //=============================================================================
    bool firstTime = true;
    bool initializedGL = false;
    GOFigure* hfig = nullptr;
    std::unique_ptr<::RenderOpenGL> glRenderer;
    int glWidth = 0;
    int glHeight = 0;
    quint64 paintSerial = 0;
    //=============================================================================
    QBasicTimer resizeTimer;
    bool resizing = false;
    QImage resizeSnapshot;
    QSize resizeStartSize;
    void
    timerEvent(QTimerEvent* event) override;
    void
    syncFigureSize(int logicalWidth, int logicalHeight);
    bool
    hasExpectedRenderingSize();
    //=============================================================================
public:
    BaseFigureQt(QWidget* parent, GOFigure* fig);
    ~BaseFigureQt() override;
    void
    initializeGL() override;
    void
    paintGL() override;
    void
    resizeGL(int w, int h) override;
    void
    resizeEvent(QResizeEvent* e) override;
    void
    flushPendingResize();
    bool
    event(QEvent* e) override;
    static bool
    currentScreenResolution(int& w, int& h);
    static int
    getCurrentScreenHeight();
    static int
    getCurrentScreenWidth();
    QImage
    getFrame();
    QImage
    getOpenGLFrame();
    bool
    waitForRenderedFrame(int timeoutMs);
    void
    prerenderFirstFrame();
    bool
    hasRenderedFrame() const
    {
        return initializedGL && paintSerial > 0;
    }
    int
    getRenderWidth() const
    {
        return glWidth;
    }
    int
    getRenderHeight() const
    {
        return glHeight;
    }
};
//=============================================================================
} // namespace Nelson
//=============================================================================
