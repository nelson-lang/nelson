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
#include <atomic>
#include <map>
#include <QtCore/QPointF>
#include <QtWidgets/QMainWindow>
#include <QtWidgets/QStackedWidget>
#include <QtWidgets/QTabWidget>
#include <QtWidgets/QStatusBar>
#include <QtGui/QAction>
#include <QtCore/QByteArray>
#include <QtCore/QEventLoop>
#include <QtCore/QElapsedTimer>
#include <QtCore/QTimer>
#include <QtGui/QImage>
#include <QtGui/QResizeEvent>
#include <QtGui/QShowEvent>
#include <QtWidgets/QToolBar>
#include <QtPrintSupport/QPrinter>
#include "GOFigure.hpp"
#include "GOFigureHost.hpp"
#include "KeyReleaseEventState.hpp"
#include "WindowKeyEventFilter.hpp"
#include "WindowMouseMotionFilter.hpp"
#include "nlsGraphics_qt_exports.h"
//=============================================================================
namespace Nelson {
class FigureWorkspaceQt;
//=============================================================================
// MOUSE_MODE / WAIT_PRESS_MODE live in GOFigureHost.hpp (shared with the model).
//=============================================================================
class NLSGRAPHICS_QT_IMPEXP GOWindow : public QMainWindow, public GOFigureHost
{
    friend class FigureWorkspaceQt;
    //=============================================================================
protected:
    bool initialized;
    bool m_headless = false;
    int64 handle;
    QWidget* qtchild = nullptr;
    GOFigure* goFig = nullptr;
    bool processingCloseEvent = false;
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
    std::unique_ptr<FigureWorkspaceQt> workspace;
    static const QIcon&
    sharedIcon(const std::wstring& relativePath);
    void
    printFigure();
    void
    exportFigure();
    //=============================================================================
    KeyReleaseEventState keyReleaseState;
    std::unique_ptr<WindowKeyEventFilter> windowKeyEventFilter;
    void
    installWindowKeyEventFilter();
    std::unique_ptr<WindowMouseMotionFilter> windowMouseMotionFilter;
    void
    installWindowMouseMotionFilter();
    // Buttons held by the events __figure_mouse_event__ injected.
    Qt::MouseButtons injectedMouseButtons = Qt::NoButton;
    QTimer releaseKeyTimer;
    QTimer repaintTimer;
    const int delayMs = 80;
    //=============================================================================
    MOUSE_MODE mouseMode = MOUSE_MODE::NONE;
    //=============================================================================
    GOAxis*
    findContainingAxis(GOFigure* fig, int x, int y);
    int64
    findContainingColorBar(GOFigure* fig, int x, int y);
    int
    remapX(int x);
    int
    remapY(int x);
    int
    figureWindowExtraHeight();
    void
    setFigureGeometryFromInnerPosition(int x, int y, int width, int height, int screenHeight);
    void
    adjustGeometryToDrawingArea(int x, int y, int width, int height);
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
    QAction* dockAction = nullptr;
    //=============================================================================
    QPoint mousePositionOrigin;
    //=============================================================================
    // Last pointer position in figure drawing-area pixels (bottom-left origin),
    // as updateCurrentPoint computed it: the coordinates object picking uses.
    double lastFigurePixelX = 0.0;
    double lastFigurePixelY = 0.0;
    // figure-pixel target of each injected mouse event by timestamp, used directly instead of reconstructing it from scenePosition
    std::map<quint64, QPointF> injectedFigurePoints;
    //=============================================================================
    bool nativeTitleBarThemeApplied = false;
    std::atomic_bool repaintPending { false };
    bool repaintInProgress = false;
    bool repaintDeferred = false;
    bool firstContentPresentationPending = false;
    int requestedInnerWidth = 0;
    int requestedInnerHeight = 0;
    bool drawingAreaRefitDone = false;
    //=============================================================================
    void
    changeEvent(QEvent* event) override;
    //=============================================================================
    // Picks the object under the pointer and queues WindowButtonDownFcn and
    // the object ButtonDownFcn. selectionType is written to the figure
    // SelectionType property first.
    void
    deliverButtonDownCallbacks(QMouseEvent* e, const std::wstring& selectionType);

    void
    forceCurrentAxes(QMouseEvent* e);
    void
    qtGetPosition(QMouseEvent* e, int& x, int& y);
    void
    performPendingRepaint(bool immediate);
    bool
    shouldRenderFirstContent();
    bool
    shouldPresentOnFirstContent();
    void
    presentNativeWindow();
    // geometry of the on-screen placement while the window is parked
    // off-screen for its first presentation (invalid otherwise)
    QRect presentationTargetGeometry;
    // parked off-screen or minimized: the Qt geometry is not a placement
    bool
    isTransientPlacement();
    void
    applyFigurePointerCursor();
#ifdef _MSC_VER
    bool inNativeResizeLoop = false;
    bool processingNativeResizeEvents = false;
    QSize pendingNativeResizeClientSize;
    QElapsedTimer lastNativeResizePump;
#endif

    bool
    handleKeyEvent(
        QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType);
    bool
    handleMouseEvent(
        QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType);
    bool
    handleMouseEvent(GraphicsObject* object, const std::wstring& callbackPropertyStr,
        const std::wstring& eventType);
    // figure and axes CurrentPoint from a mouse event scenePosition(), or from the injected figure point when eventTimestamp matches one
    void
    updateCurrentPoint(const QPointF& scenePosition, quint64 eventTimestamp);
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
    contextMenuEvent(QContextMenuEvent* event) override;
    //=============================================================================
    void
    updateWindowMenuItems();
    void
    applyNativeTitleBarThemeOnce();
    void
    updateDockActionState();
    void
    syncWindowStyleProperty();
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
    // headless: WEB_GUI figure host with no Qt chrome (menus, toolbars, cursors),
    // no central widget, no screen geometry; the figure renders through
    // RenderWeb on demand and repaint/sync are no-ops.
    GOWindow(int64 ahandle, bool headless = false);
    bool
    isHeadless() const
    {
        return m_headless;
    }
    // Native presentation can be impossible even with a QApplication and a screen: a
    // macOS background process without window-server access throws an Objective-C
    // exception from AppKit while a figure window is realized. The first failure latches
    // this flag process-wide. Figures then stay off-screen (still rendered for
    // getframe/saveas) and the realizing path is never entered again.
    static bool
    nativeWindowPresentationAvailable();
    static void
    disableNativeWindowPresentation();
    ~GOWindow() override;
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
    requestRepaint();
    void
    requestRepaintNow();
    bool
    syncRendering(int timeoutMs);
    QImage
    grabFrameImage(bool rawOpenGL = false, bool wait = true);
    void
    ensureBaseFigureWidget();
    // Re-fits the drawing area to the last requested Position once the realized
    // window chrome is known (called from the canvas resize that misses the request).
    void
    refitDrawingAreaToRequest();
#ifdef _MSC_VER
    void
    serviceNativeResize(bool force, const QSize& oldWindowSize = QSize());
#endif

    void
    closeEvent(QCloseEvent* e) override;
    void
    keyPressEvent(QKeyEvent* event) override;
    void
    keyReleaseEvent(QKeyEvent* event) override;
    void
    mousePressEvent(QMouseEvent* e) override;
    void
    mouseDoubleClickEvent(QMouseEvent* e) override;
    void
    mouseMoveEvent(QMouseEvent* e) override;
    void
    mouseReleaseEvent(QMouseEvent* e) override;
    void
    resizeEvent(QResizeEvent* event) override;
    void
    moveEvent(QMoveEvent* e) override;
    void
    wheelEvent(QWheelEvent* event) override;
    void
    focusInEvent(QFocusEvent* event) override;
    void
    showEvent(QShowEvent* event) override;
#ifdef _MSC_VER
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    bool
    nativeEvent(const QByteArray& eventType, void* message, qintptr* result) override;
#else
    bool
    nativeEvent(const QByteArray& eventType, void* message, long* result) override;
#endif
#endif
    QWidget*
    getMainQWidget();
    //=============================================================================
    MOUSE_MODE
    getCurrentMouseMode();
    //=============================================================================
    bool
    isDockedInDesktop() const;
    void
    toggleDockedInDesktop();
    // docks/undocks the figure and keeps the WindowStyle property in sync
    void
    setDockedInDesktop(bool docked);
    //=============================================================================
    void
    setModeWaitMouseOrKeyPressEvent(bool wait);
    //=============================================================================
    bool
    isWaitMouseOrKeyPressEvent(WAIT_PRESS_MODE& waitPressMode);
    //=============================================================================
    GOFigure*
    hostFigure() override
    {
        return goFig;
    }
    int64
    hostId() const override
    {
        return handle;
    }
    bool
    isHeadlessHost() const override
    {
        return m_headless;
    }
    void
    requestHostRepaint() override
    {
        requestRepaint();
    }
    void
    requestHostRepaintNow() override
    {
        requestRepaintNow();
    }
    bool
    syncHostRendering(int timeoutMs) override
    {
        return syncRendering(timeoutMs);
    }
    void
    updateHostState(bool force) override
    {
        updateState(force);
    }
    void
    presentHost() override;
    void
    prepareHostClose(bool forceClose) override;
    void
    destroyHost() override;
    bool
    isDockedHost() override
    {
        return isDockedInDesktop();
    }
    bool
    hostFrameGeometry(
        double& x, double& y, double& width, double& height, double& frameTop) override;
    void
    setWaitMouseOrKeyPressEvent(bool waiting) override
    {
        setModeWaitMouseOrKeyPressEvent(waiting);
    }
    bool
    isWaitingMouseOrKeyPressEvent(WAIT_PRESS_MODE& waitPressMode) override
    {
        return isWaitMouseOrKeyPressEvent(waitPressMode);
    }
    bool
    grabHostFrameRGBA(
        bool rawOpenGL, std::vector<unsigned char>& rgba, int& width, int& height) override;
    void*
    nativeParentWidget() override
    {
        return getMainQWidget();
    }
    void
    rebuildHostMenus() override;
    MOUSE_MODE
    currentHostMouseMode() override { return getCurrentMouseMode(); }
    void
    toggleHostMouseMode(MOUSE_MODE mode) override;
    bool
    injectHostMouseEvent(const std::wstring& action, double x, double y, int button = 0,
        bool shiftDown = false, bool ctrlDown = false, bool doubleClick = false) override;
    bool
    hostDrawingAreaScreenRect(double& x, double& y, double& width, double& height) override;
    //=============================================================================
};
//=============================================================================
} // namespace Nelson
//=============================================================================
