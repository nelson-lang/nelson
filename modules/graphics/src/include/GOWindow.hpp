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
#include <QtWidgets/QStatusBar>
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
#include <QtGui/QAction>
#else
#include <QtWidgets/QAction>
#endif
#include <QtCore/QEventLoop>
#include <QtCore/QTimer>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QRubberBand>
#include <QtPrintSupport/QPrinter>
#include "GOFigure.hpp"
#include "nlsGraphics_exports.h"
//=============================================================================
namespace Nelson {
//=============================================================================
enum class MOUSE_MODE
{
    NONE = 0,
    ROTATION,
    ZOOM_IN,
    ZOOM_OUT,
    PAN,
};
//=============================================================================
enum class WAIT_PRESS_MODE
{
    NONE = 0,
    MOUSE,
    KEY,
    CLOSE,
    WAIT
};
//=============================================================================
class NLSGRAPHICS_IMPEXP GOWindow : public QMainWindow
{
    //=============================================================================
protected:
    bool initialized;
    int64 handle;
    QWidget* qtchild = nullptr;
    GOFigure* goFig = nullptr;
    //=============================================================================
    QAction* closeAction;
    QMenu* fileMenu;
    QMenu* windowMenu;
    void
    createActions();
    void
    createMenuBar();
    //=============================================================================
private:
    //=============================================================================
    bool keyPressed = false;
    QKeyEvent* lastKeyEvent;
    QTimer releaseKeyTimer;
    const int delayMs = 80;
    //=============================================================================
    MOUSE_MODE mouseMode = MOUSE_MODE::NONE;
    //=============================================================================
    GOAxis*
    findContainingAxis(GOFigure* fig, int x, int y);
    int
    remapX(int x);
    int
    remapY(int x);
    //=============================================================================
    QToolBar* toolBar;
    //=============================================================================
    WAIT_PRESS_MODE waitKeyOrMousePressedMode = WAIT_PRESS_MODE::NONE;
    //=============================================================================
    QAction* newFigureAction;
    QAction* printAction;
    QAction* exportAction;
    QAction* rotateAction;
    QAction* panAction;
    QAction* zoomInAction;
    QAction* zoomOutAction;
    QAction* restoreViewAction;
    QAction* helpAction;
    //=============================================================================
    QPoint mousePositionOrigin;
    //=============================================================================
    QRubberBand* zoomInRubberBand = nullptr;
    //=============================================================================
    bool isPanRunning = false;
    double panXRange;
    double panXMean;
    double panYRange;
    double panYMean;
    //=============================================================================
    bool isZoomInRunning = false;
    //=============================================================================
    bool isZoomOutRunning = false;
    //=============================================================================
    bool isRotateRunning = false;
    //=============================================================================
    void
    mouseMoveEventHandlePanMode(QMouseEvent* e);
    void
    mousePressEventHandlePanMode(QMouseEvent* e);
    void
    mousePressEventHandleZoomInMode(QMouseEvent* e);
    void
    mousePressEventHandleZoomOutMode(QMouseEvent* e);
    void
    mousePressEventHandleRotate(QMouseEvent* e);

    void
    mouseMoveEventHandleRotate(QMouseEvent* e);
    void
    forceCurrentAxes(QMouseEvent* e);
    void
    qtGetPosition(QMouseEvent* e, int& x, int& y);

    bool
    handleKeyEvent(
        QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType);
    bool
    handleMouseEvent(
        QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType);
    wstringVector
    getModifiers(QKeyEvent* keyEvent);
    std::wstring
    getKeyString(QKeyEvent* keyEvent);
    std::wstring
    getCharacterString(QKeyEvent* keyEvent);
    //=============================================================================
    QCursor rotateCursor;
    QCursor zoomInCursor;
    QCursor zoomOutCursor;
    //=============================================================================
    void
    contextMenuEvent(QContextMenuEvent* event);
    //=============================================================================
    void
    updateWindowMenuItems();
    //=============================================================================
public slots:
    void
    onZoomInAction();
    void
    onPanAction();
    void
    onRotateAction();

private slots:
    void
    onNewFigureAction();
    void
    onPrintAction();
    void
    onExportAction();
    void
    onZoomOutAction();
    void
    onHelpAction();
    void
    refreshWindowMenuItems();
    void
    onRestoreViewAction();
    void
    onKeyReleaseTimeout();

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
    updateState(bool forceUpdate);
    void
    updateState();
    void
    createToolbars();

    void
    closeEvent(QCloseEvent* e) override;
    void
    keyPressEvent(QKeyEvent* event) override;
    void
    keyReleaseEvent(QKeyEvent* event) override;
    void
    mousePressEvent(QMouseEvent* e) override;
    void
    mouseMoveEvent(QMouseEvent* e) override;
    void
    mouseReleaseEvent(QMouseEvent* e) override;
    void
    moveEvent(QMoveEvent* e) override;
    void
    wheelEvent(QWheelEvent* event) override;
    void
    focusInEvent(QFocusEvent* event) override;
    QWidget*
    getMainQWigdet()
    {
        return qtchild;
    }
    //=============================================================================
    MOUSE_MODE
    getCurrentMouseMode();
    //=============================================================================
    void
    setModeWaitMouseOrKeyPressEvent(bool wait);
    //=============================================================================
    bool
    isWaitMouseOrKeyPressEvent(WAIT_PRESS_MODE& waitPressMode);
    //=============================================================================
};
//=============================================================================
}
//=============================================================================
