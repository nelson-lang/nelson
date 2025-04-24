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
#include <QtWidgets/QWidget>
#include <QtGui/QResizeEvent>
#include <QtGui/QScreen>
#include "GOFigure.hpp"
#include <QtCore/QBasicTimer>
#include <QtCore/QTimerEvent>
//=============================================================================
namespace Nelson {
//=============================================================================
class BaseFigureQt : public QWidget
{
    //=============================================================================
private:
    GOFigure* hfig = nullptr;
    QPixmap backStore;
    static QScreen*
    getActiveScreen();
    //=============================================================================
    QBasicTimer resizeTimer;
    bool resizing = false;
    void
    timerEvent(QTimerEvent* event);
    //=============================================================================
public:
    BaseFigureQt(QWidget* parent, GOFigure* fig);
    void
    paintEvent(QPaintEvent* e);
    void
    resizeEvent(QResizeEvent* e) override;
    static bool
    currentScreenResolution(int& w, int& h);
    static int
    getCurrentScreenHeight();
    static int
    getCurrentScreenWidth();
    QImage
    getFrame();
};
//=============================================================================
}
//=============================================================================
