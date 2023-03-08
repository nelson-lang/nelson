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
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QStackedWidget>
#include <QtWidgets/QTabWidget>
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QAction>
#else
#include <QtWidgets/QAction>
#endif
#include <QtCore/QEventLoop>
#include "GOFigure.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOWindow : public QMainWindow
{
    //=============================================================================
protected:
    bool initialized;
    int64 handle;
    QWidget* qtchild = nullptr;
    GOFigure* goFig = nullptr;
    QEventLoop m_loop;
    int click_x, click_y;
    //=============================================================================
    QAction* closeAction;
    QMenu* fileMenu;
    //=============================================================================
    void
    createActions();
    void
    createMenuBar();
    //=============================================================================
public:
    GOWindow(int64 ahandle);
    ~GOWindow() override
    {
        if (goFig) {
            delete goFig;
        }
    }
    int64
    getHandle();
    GOFigure*
    getGOFigure();
    void
    updateState();
    void
    closeEvent(QCloseEvent* e) override;
    void
    getClickPosition(int& x, int& y);
    void
    mousePressEvent(QMouseEvent* e) override;
    void
    focusInEvent(QFocusEvent* event) override;
    void
    moveEvent(QMoveEvent* e) override;
    QWidget*
    getMainQWigdet()
    {
        return qtchild;
    }
};
//=============================================================================
}
//=============================================================================
