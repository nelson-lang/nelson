//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#include <algorithm>
#include <cmath>
#include <map>
#include <memory>
#include <QtGui/QImage>
#include <QtCore/QSettings>
#include <QtWidgets/QApplication>
#include <QtWidgets/QWidget>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QLayout>
#include <QtWidgets/QFileDialog>
#include <QtGui/QIcon>
#include <QtGui/QMouseEvent>
#include <QtGui/QPixmap>
#include <QtGui/QColor>
#include <QtGui/QPalette>
#include <QtGui/QGuiApplication>
#include <QtCore/QPointer>
#include <QtCore/QCoreApplication>
#include <QtCore/QEvent>
#include <QtGui/QWindow>
#include <QtPrintSupport/QPrintDialog>
#include <QtPrintSupport/QPrintPreviewDialog>
#include <QtPrintSupport/QPrinter>
#ifdef _MSC_VER
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <Windows.h>
#endif
#define FMT_HEADER_ONLY
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "QtTranslation.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOFigureMouseEvents.hpp"
#include "GOWindow.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOFiguresManager.hpp"
#include "GOStringProperty.hpp"
#include "BaseFigureQt.hpp"
#include "RenderQt.hpp"
#include "QStringConverter.hpp"
#include "Types.hpp"
#include "NelsonConfiguration.hpp"
#include "NelSon_engine_mode.h"
#include "i18n.hpp"
#include "GOHelpers.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "NelsonPalette.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOAxis.hpp"
#include "PostCommand.hpp"
#include "GOColorProperty.hpp"
#include "Nelson_VERSION.h"
#include "TextEditor.hpp"
#include "MainGuiObject.hpp"
#include "DesktopDocking.hpp"
#include "GOCallbackProperty.hpp"
#include "CallExportGraphicsGui.hpp"
#include "GOWindowStateProperty.hpp"
#include "RuntimeDispatch.hpp"
#include "GOColorBar.hpp"
#include "GOLegend.hpp"
#include "GOMenuQt.hpp"
#include "UnitsConversionHelpers.hpp"
#include "QueueProcessing.hpp"
#include "FigureWorkspaceQt.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define TITLE_WITH_FIGURE_NUMBER_FORMAT L"Figure {0}"
#define TITLE_WITH_FIGURE_NUMBER_AND_NAME_FORMAT L"Figure {0}: {1}"
#define TITLE_WITH_NAME_ONLY_FORMAT L"{0}"
constexpr int REPAINT_COALESCE_DELAY_MS = 16;
constexpr int RENDER_SYNC_TIMEOUT_MS = 250;
constexpr int FIRST_RENDER_SYNC_TIMEOUT_MS = 1000;
#ifdef _MSC_VER
constexpr int NATIVE_RESIZE_PUMP_INTERVAL_MS = 30;
#endif
//=============================================================================
static int
toDevicePixels(int logicalSize, qreal ratio)
{
    int deviceSize = static_cast<int>(logicalSize * ratio + 0.5);
    return deviceSize > 0 ? deviceSize : logicalSize;
}
//=============================================================================
static bool
getChangedFigurePosition(GOFigure* fig, std::vector<double>& position)
{
    if (fig->hasChanged(GO_POSITION_PROPERTY_NAME_STR)) {
        position = fig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        return true;
    }
    if (fig->hasChanged(GO_INNER_POSITION_PROPERTY_NAME_STR)) {
        position = fig->findVectorDoubleProperty(GO_INNER_POSITION_PROPERTY_NAME_STR);
        return true;
    }
    if (fig->hasChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR)) {
        position = fig->findVectorDoubleProperty(GO_OUTER_POSITION_PROPERTY_NAME_STR);
        return true;
    }
    return false;
}
//=============================================================================
static bool
isPickable(GraphicsObject* object)
{
    if (!object) {
        return false;
    }
    GOGenericProperty* visible = object->findProperty(GO_VISIBLE_PROPERTY_NAME_STR, false);
    if (visible && object->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return false;
    }
    GOGenericProperty* hitTest = object->findProperty(GO_HIT_TEST_PROPERTY_NAME_STR, false);
    if (hitTest && object->stringCheck(GO_HIT_TEST_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return false;
    }
    GOGenericProperty* pickableParts
        = object->findProperty(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, false);
    if (pickableParts
        && object->stringCheck(GO_PICKABLE_PARTS_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
        return false;
    }
    return true;
}
//=============================================================================
static void
syncFigurePositionProperties(GOFigure* fig, const std::vector<double>& position)
{
    if (!fig || position.size() < 4) {
        return;
    }
    fig->setFourVectorDefault(
        GO_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
    fig->setFourVectorDefault(
        GO_INNER_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
    fig->setFourVectorDefault(
        GO_OUTER_POSITION_PROPERTY_NAME_STR, position[0], position[1], position[2], position[3]);
}
//=============================================================================
static void
clearFigurePositionChanges(GOFigure* fig)
{
    fig->clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    fig->clearChanged(GO_INNER_POSITION_PROPERTY_NAME_STR);
    fig->clearChanged(GO_OUTER_POSITION_PROPERTY_NAME_STR);
}
//=============================================================================
static std::vector<double>
figurePositionToPixels(GOFigure* fig, const std::vector<double>& position)
{
    if (!fig || position.size() < 4) {
        return position;
    }
    int screenWidth = 0;
    int screenHeight = 0;
    BaseFigureQt::currentScreenResolution(screenWidth, screenHeight);
    std::wstring units = fig->findStringProperty(GO_UNITS_PROPERTY_NAME_STR);
    return convertUnitsVector(units, GO_PROPERTY_VALUE_PIXELS_STR, position[0], position[1],
        position[2], position[3], screenWidth, screenHeight);
}
//=============================================================================
static void
applyFigureBackgroundFill(QWidget* widget, GOFigure* fig)
{
    if (!widget || !fig) {
        return;
    }
    std::vector<double> background = fig->findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    if (background.size() < 3) {
        return;
    }
    QColor backgroundColor = QColor::fromRgbF(static_cast<float>(background[0]),
        static_cast<float>(background[1]), static_cast<float>(background[2]));
    QPalette palette = widget->palette();
    palette.setColor(QPalette::Window, backgroundColor);
    widget->setPalette(palette);
    // A QOpenGLWidget must keep autoFillBackground off: filling its backing
    // store replaces the composited GL framebuffer and the canvas stays black.
    if (!widget->inherits("QOpenGLWidget")) {
        widget->setAutoFillBackground(true);
    }
}
//=============================================================================
static bool
figureNeedsDarkChrome(GOFigure* fig)
{
    if (!fig) {
        return false;
    }
    if (fig->stringCheck(GO_THEME_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DARK_STR)) {
        return true;
    }
    // A dark figure Color needs the readable dark chrome even with a light theme.
    // Otherwise the menu bar and toolbar text is unreadable over the inherited dark palette.
    std::vector<double> background = fig->findVectorDoubleProperty(GO_COLOR_PROPERTY_NAME_STR);
    if (background.size() < 3) {
        return false;
    }
    double luminance = 0.299 * background[0] + 0.587 * background[1] + 0.114 * background[2];
    return luminance < 0.5;
}
//=============================================================================
static void
applyWindowTheme(GOWindow* window, GOFigure* fig)
{
    if (!window || !fig) {
        return;
    }
    // Fill the window and its central widget with the figure background colour.
    // The frame shown before the OpenGL surface first composites is then the figure
    // colour, not the black GL clear. A uicontrol-only figure has no plot content to
    // warm the canvas, so it would flash black before the grey appears.
    applyFigureBackgroundFill(window, fig);
    applyFigureBackgroundFill(window->centralWidget(), fig);
    if (figureNeedsDarkChrome(fig)) {
        window->setStyleSheet("QMainWindow, QMenuBar, QMenu, QToolBar {"
                              " background-color: rgb(33, 33, 33); color: rgb(217, 217, 217);"
                              "}"
                              "QMenuBar::item:selected, QMenu::item:selected {"
                              " background-color: rgb(66, 66, 66);"
                              "}"
                              "QToolBar { border: 0px; }");
    } else {
        window->setStyleSheet("");
    }
}
//=============================================================================
static QCursor
customPointerCursor(GOFigure* fig)
{
    ArrayOf cdata = fig->findArrayOfProperty(GO_POINTER_SHAPE_C_DATA_PROPERTY_NAME_STR);
    if (cdata.isEmpty() || !cdata.isNumeric() || cdata.isComplex()) {
        return QCursor(Qt::ArrowCursor);
    }
    cdata.promoteType(NLS_DOUBLE);
    indexType rows = cdata.rows();
    indexType cols = cdata.columns();
    if (rows <= 0 || cols <= 0 || rows > 64 || cols > 64) {
        return QCursor(Qt::ArrowCursor);
    }

    const double* data = static_cast<const double*>(cdata.dataPointer());
    QImage image(static_cast<int>(cols), static_cast<int>(rows), QImage::Format_ARGB32);
    image.fill(Qt::transparent);
    for (indexType col = 0; col < cols; ++col) {
        for (indexType row = 0; row < rows; ++row) {
            double value = data[row + col * rows];
            if (!std::isfinite(value)) {
                continue;
            }
            if (value == 1.0) {
                image.setPixelColor(static_cast<int>(col), static_cast<int>(row), Qt::black);
            } else if (value == 2.0) {
                image.setPixelColor(static_cast<int>(col), static_cast<int>(row), Qt::white);
            }
        }
    }

    std::vector<double> hotSpot
        = fig->findVectorDoubleProperty(GO_POINTER_SHAPE_HOT_SPOT_PROPERTY_NAME_STR);
    int hotX = 0;
    int hotY = 0;
    if (hotSpot.size() >= 2) {
        hotX = std::max(0, std::min(static_cast<int>(cols) - 1, static_cast<int>(hotSpot[0]) - 1));
        hotY = std::max(0, std::min(static_cast<int>(rows) - 1, static_cast<int>(hotSpot[1]) - 1));
    }
    return QCursor(QPixmap::fromImage(image), hotX, hotY);
}
//=============================================================================
static QCursor
figurePointerCursor(GOFigure* fig)
{
    QString pointer
        = wstringToQString(fig->findStringProperty(GO_POINTER_PROPERTY_NAME_STR)).toLower();
    if (pointer == "custom") {
        return customPointerCursor(fig);
    }
    if (pointer == "crosshair" || pointer == "cross") {
        return QCursor(Qt::CrossCursor);
    }
    if (pointer == "watch") {
        return QCursor(Qt::WaitCursor);
    }
    if (pointer == "ibeam") {
        return QCursor(Qt::IBeamCursor);
    }
    if (pointer == "fleur") {
        return QCursor(Qt::SizeAllCursor);
    }
    if (pointer == "hand") {
        return QCursor(Qt::PointingHandCursor);
    }
    if (pointer == "left" || pointer == "right") {
        return QCursor(Qt::SizeHorCursor);
    }
    if (pointer == "top" || pointer == "bottom") {
        return QCursor(Qt::SizeVerCursor);
    }
    if (pointer == "topl" || pointer == "botr") {
        return QCursor(Qt::SizeFDiagCursor);
    }
    if (pointer == "topr" || pointer == "botl") {
        return QCursor(Qt::SizeBDiagCursor);
    }
    if (pointer == "circle") {
        return QCursor(Qt::ForbiddenCursor);
    }
    return QCursor(Qt::ArrowCursor);
}
//=============================================================================
static QString
rootResourcePath(const std::wstring& relativePath)
{
    return wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + relativePath);
}
//=============================================================================
const QIcon&
GOWindow::sharedIcon(const std::wstring& relativePath)
{
    static auto* icons = new std::map<std::wstring, QIcon>();
    auto it = icons->find(relativePath);
    if (it == icons->end()) {
        it = icons->emplace(relativePath, QIcon(rootResourcePath(relativePath))).first;
    }
    return it->second;
}
//=============================================================================
static const QCursor&
sharedCursor(const std::wstring& relativePath)
{
    static auto* cursors = new std::map<std::wstring, QCursor>();
    auto it = cursors->find(relativePath);
    if (it == cursors->end()) {
        it = cursors->emplace(relativePath, QCursor(rootResourcePath(relativePath))).first;
    }
    return it->second;
}
//=============================================================================
static bool
figureHasChildren(GOFigure* fig)
{
    if (!fig) {
        return false;
    }
    auto* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    return children && !children->data().empty();
}
//=============================================================================
static void
updateGraphicsChildPages(QWidget* widget, bool immediate)
{
    if (!widget) {
        return;
    }
    for (QObject* object : widget->children()) {
        QWidget* child = qobject_cast<QWidget*>(object);
        if (!child || child->isWindow()) {
            continue;
        }
        if (child->property("nelsonPaintsGraphicsChildren").toBool() && child->isVisibleTo(widget)
            && !child->geometry().isEmpty()) {
            if (immediate) {
                child->repaint();
            } else {
                child->update();
            }
        }
        updateGraphicsChildPages(child, immediate);
    }
}
//=============================================================================
#ifdef _MSC_VER
//=============================================================================
static void
syncFigureSizeFromLogicalSize(GOFigure* fig, QWidget* widget, int logicalWidth, int logicalHeight)
{
    if (!fig || !widget || logicalWidth <= 0 || logicalHeight <= 0) {
        return;
    }
    qreal ratio = widget->devicePixelRatioF();
    if (ratio <= 0) {
        ratio = 1;
    }
    fig->setScalarDoubleDefault(
        GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, static_cast<double>(ratio));
    fig->resizeGL(toDevicePixels(logicalWidth, ratio), toDevicePixels(logicalHeight, ratio));
    fig->setRenderingStateInvalid(true);
}
//=============================================================================
static QSize
figureSizeFromNativeClientSize(const QSize& nativeClientSize, int extraHeight)
{
    if (!nativeClientSize.isValid() || nativeClientSize.width() <= 0
        || nativeClientSize.height() <= 0) {
        return QSize();
    }
    return QSize(nativeClientSize.width(), std::max(1, nativeClientSize.height() - extraHeight));
}
//=============================================================================
static QSize
figureSizeFromNativeWindow(QWidget* window, QWidget* widget, const QSize& oldWindowSize)
{
    if (!window || !widget || !oldWindowSize.isValid()) {
        return QSize();
    }
    const int chromeWidth = oldWindowSize.width() - widget->width();
    const int chromeHeight = oldWindowSize.height() - widget->height();
    if (chromeWidth < 0 || chromeHeight < 0) {
        return QSize();
    }
    return QSize(
        std::max(1, window->width() - chromeWidth), std::max(1, window->height() - chromeHeight));
}
//=============================================================================
static void
syncFigureSizeFromWidget(GOFigure* fig, QWidget* widget, QWidget* window,
    const QSize& oldWindowSize, const QSize& nativeClientSize, int extraHeight)
{
    QSize logicalSize = figureSizeFromNativeClientSize(nativeClientSize, extraHeight);
    if (!logicalSize.isValid()) {
        logicalSize = figureSizeFromNativeWindow(window, widget, oldWindowSize);
    }
    if (!logicalSize.isValid()) {
        logicalSize = widget->size();
    }
    syncFigureSizeFromLogicalSize(fig, widget, logicalSize.width(), logicalSize.height());
}
//=============================================================================
#endif
//=============================================================================
static bool
objectHasDrawableContent(GraphicsObject* object)
{
    if (!object) {
        return false;
    }
    if (object->haveProperty(GO_VISIBLE_PROPERTY_NAME_STR)
        && object->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
        return false;
    }
    if (object->isType(GO_PROPERTY_VALUE_UIMENU_STR)
        || object->isType(GO_PROPERTY_VALUE_UICONTEXTMENU_STR)) {
        return false;
    }
    if (object->isType(GO_PROPERTY_VALUE_TEXT_STR)) {
        return !object->findStringProperty(GO_STRING_PROPERTY_NAME_STR).empty();
    }
    if (!object->isType(GO_PROPERTY_VALUE_AXES_STR)
        && !object->isType(GO_PROPERTY_VALUE_POLAR_AXES_STR)
        && !object->isType(GO_PROPERTY_VALUE_TILED_LAYOUT_STR)) {
        return true;
    }
    auto* children = static_cast<GOGObjectsProperty*>(
        object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return false;
    }
    for (int64 childHandle : children->data()) {
        if (objectHasDrawableContent(findGraphicsObject(childHandle, false))) {
            return true;
        }
    }
    return false;
}
//=============================================================================
static bool
figureHasDrawableContent(GOFigure* fig)
{
    if (!fig) {
        return false;
    }
    auto* children
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
    if (!children) {
        return false;
    }
    for (int64 childHandle : children->data()) {
        if (objectHasDrawableContent(findGraphicsObject(childHandle, false))) {
            return true;
        }
    }
    return false;
}
//=============================================================================
// Latched the first time realizing a native figure window throws (see
// GOWindow::nativeWindowPresentationAvailable in the header).
static std::atomic<bool> g_nativeWindowPresentationUnavailable { false };
//=============================================================================
bool
GOWindow::nativeWindowPresentationAvailable()
{
    return !g_nativeWindowPresentationUnavailable.load();
}
//=============================================================================
void
GOWindow::disableNativeWindowPresentation()
{
    g_nativeWindowPresentationUnavailable.store(true);
}
//=============================================================================
static bool
canShowFigureWidgets()
{
    return GOWindow::nativeWindowPresentationAvailable()
        && QGuiApplication::primaryScreen() != nullptr;
}
//=============================================================================
GOWindow::GOWindow(int64 ahandle, bool headless) : QMainWindow()
{
    initialized = false;
    m_headless = headless;
    handle = ahandle;
    int figureId = handle + 1;
    if (m_headless) {
        // WEB_GUI figure host: only the graphics object matters, the browser is the window.
        goFig = new GOFigure(this, figureId);
        qtchild = nullptr;
        initialized = true;
        return;
    }

    // On-screen window: tell the interpreter a Qt figure window is live so it pumps
    // the GUI event loop while a script runs. Balanced by the decrement in ~GOWindow().
    NelsonConfiguration::getInstance()->incrementOpenFigureWindowCount();

    releaseKeyTimer.setSingleShot(true);
    connect(&releaseKeyTimer, &QTimer::timeout, this, &GOWindow::onKeyReleaseTimeout);
    repaintTimer.setSingleShot(true);
    connect(&repaintTimer, &QTimer::timeout, this, [this]() { performPendingRepaint(false); });

    setWindowIcon(sharedIcon(L"/modules/graphics/resources/icon-mathematical-plot.svg"));
    goFig = new GOFigure(this, figureId);
    setWindowTitle(wstringToQString(fmt::format(TITLE_WITH_FIGURE_NUMBER_FORMAT, figureId)));
    setFocusPolicy(Qt::ClickFocus);
    installWindowKeyEventFilter();
    installWindowMouseMotionFilter();
    qtchild = new QWidget(this);
    setCentralWidget(qtchild);

    rotateCursor = sharedCursor(L"/resources/rotate.svg");
    zoomInCursor = sharedCursor(L"/resources/zoom-in.svg");
    zoomOutCursor = sharedCursor(L"/resources/zoom-out.svg");

    createActions();
    createMenuBar();
    workspace = std::make_unique<FigureWorkspaceQt>(this);
    createToolbars();

    std::vector<double> initialPosition = figurePositionToPixels(
        goFig, goFig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
    if (initialPosition.size() < 4 || initialPosition[2] <= 0 || initialPosition[3] <= 0) {
        initialPosition = defaultFigurePosition();
    }
    setFigureGeometryFromInnerPosition(static_cast<int>(initialPosition[0]),
        static_cast<int>(initialPosition[1]), static_cast<int>(initialPosition[2]),
        static_cast<int>(initialPosition[3]), Nelson::BaseFigureQt::getCurrentScreenHeight());
    setMouseTracking(true);
    statusBar()->setVisible(false);
    mousePositionOrigin.setX(width() / 2);
    mousePositionOrigin.setY(height() / 2);
    firstContentPresentationPending = true;
    if (goFig && qtchild) {
        goFig->setScalarDoubleDefault(
            GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, qtchild->devicePixelRatio());
    }
    initialized = true;
}
//=============================================================================
void
GOWindow::applyNativeTitleBarThemeOnce()
{
    if (nativeTitleBarThemeApplied) {
        return;
    }
    nativeTitleBarThemeApplied = true;
#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif
}
//=============================================================================
void
GOWindow::showEvent(QShowEvent* event)
{
    applyNativeTitleBarThemeOnce();
    QMainWindow::showEvent(event);
}
//=============================================================================
#ifdef _MSC_VER
void
GOWindow::serviceNativeResize(bool force, const QSize& oldWindowSize)
{
    if (m_headless || processingNativeResizeEvents || !qtchild || !goFig) {
        return;
    }
    if (!force && !inNativeResizeLoop) {
        return;
    }
    if (!force && lastNativeResizePump.isValid()
        && lastNativeResizePump.elapsed() < NATIVE_RESIZE_PUMP_INTERVAL_MS) {
        return;
    }
    if (figureHasChildren(goFig)) {
        ensureBaseFigureWidget();
    }
    BaseFigureQt* baseFigure = dynamic_cast<BaseFigureQt*>(qtchild);
    processingNativeResizeEvents = true;

    QPointer<GOWindow> window(this);
    QPointer<QWidget> child(qtchild);
    if (baseFigure) {
        baseFigure->flushPendingResize();
    } else {
        syncFigureSizeFromWidget(goFig, qtchild, this, oldWindowSize, pendingNativeResizeClientSize,
            figureWindowExtraHeight());
    }
    if (!window) {
        return;
    }
    processPendingCallbacksAndTimers();
    if (window && child) {
        QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents, 10);
    }
    if (window && child) {
        child->repaint();
        updateGraphicsChildPages(child, true);
        QCoreApplication::processEvents(QEventLoop::ExcludeUserInputEvents, 10);
    }
    if (window) {
        lastNativeResizePump.restart();
        processingNativeResizeEvents = false;
    }
}
//=============================================================================
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
bool
GOWindow::nativeEvent(const QByteArray& eventType, void* message, qintptr* result)
#else
bool
GOWindow::nativeEvent(const QByteArray& eventType, void* message, long* result)
#endif
{
    MSG* msg = static_cast<MSG*>(message);
    if (!msg) {
        return QMainWindow::nativeEvent(eventType, message, result);
    }
    auto finishNativeResize = [this]() {
        if (!inNativeResizeLoop) {
            return true;
        }
        QPointer<GOWindow> window(this);
        serviceNativeResize(true);
        if (!window) {
            return false;
        }
        inNativeResizeLoop = false;
        pendingNativeResizeClientSize = QSize();
        return true;
    };
    switch (msg->message) {
    case WM_ENTERSIZEMOVE: {
        inNativeResizeLoop = true;
        pendingNativeResizeClientSize = QSize();
        lastNativeResizePump.invalidate();
    } break;
    case WM_SIZE: {
        if (inNativeResizeLoop) {
            pendingNativeResizeClientSize = QSize(
                static_cast<int>(LOWORD(msg->lParam)), static_cast<int>(HIWORD(msg->lParam)));
        }
    } break;
    case WM_EXITSIZEMOVE:
    case WM_CAPTURECHANGED:
    case WM_CANCELMODE: {
        if (!finishNativeResize()) {
            return false;
        }
    } break;
    default: {
    } break;
    }
    return QMainWindow::nativeEvent(eventType, message, result);
}
//=============================================================================
#endif
//=============================================================================
void
GOWindow::resizeEvent(QResizeEvent* event)
{
    QMainWindow::resizeEvent(event);
#ifdef _MSC_VER
    if (initialized && inNativeResizeLoop) {
        if (!dynamic_cast<BaseFigureQt*>(qtchild)) {
            serviceNativeResize(false, event->oldSize());
        }
    }
#endif
}
//=============================================================================
void
GOWindow::ensureBaseFigureWidget()
{
    if (m_headless) {
        return; // no Qt render widget in WEB_GUI; RenderWeb paints on demand
    }
    if (dynamic_cast<BaseFigureQt*>(qtchild)) {
        return;
    }
    BaseFigureQt* baseFigure = new BaseFigureQt(this, goFig);
    std::vector<double> position = goFig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
    if (position.size() >= 4) {
        baseFigure->resize(static_cast<int>(position[2]), static_cast<int>(position[3]));
    }
    qtchild = baseFigure;
    setCentralWidget(qtchild);
    // The GL canvas is created lazily, after applyWindowTheme ran. Give it the same
    // figure-background fill so it does not flash black before the OpenGL surface exists.
    applyFigureBackgroundFill(qtchild, goFig);
    if (workspace) {
        workspace->attachCanvas(qtchild);
    }
    if (goFig && qtchild) {
        goFig->setScalarDoubleDefault(
            GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, qtchild->devicePixelRatio());
    }
}
//=============================================================================
QWidget*
GOWindow::getMainQWidget()
{
    ensureBaseFigureWidget();
    return qtchild;
}
//=============================================================================
void
GOWindow::closeEvent(QCloseEvent* e)
{
    if (!goFig) {
        return;
    }

    if (waitKeyOrMousePressedMode != WAIT_PRESS_MODE::NONE) {
        waitKeyOrMousePressedMode = WAIT_PRESS_MODE::CLOSE;
    }

    GOCallbackProperty* goCallback
        = (GOCallbackProperty*)goFig->findProperty(GO_CLOSE_REQUEST_FCN_PROPERTY_NAME_STR);
    if (goCallback) {
        ArrayOf cgo = goCallback->data();
        if (cgo.isRowVectorCharacterArray() || cgo.isScalarStringArray()) {
            std::unique_ptr<wchar_t[]> buffer(cgo.toAllocatedWideCString());
            std::wstring callbackString = buffer.get();
            if (callbackString == GO_PROPERTY_VALUE_CLOSEREQ_STR) {

                processingCloseEvent = true;
                bool closed = closeFigure(handle, true);
                processingCloseEvent = false;
                if (closed) {
                    e->accept();
                } else {
                    e->ignore();
                }
                return;
            }
        }
        goCallback->pushEvent(goFig, L"WindowCloseRequestData", L"Close");
    }

    e->ignore();
}
//=============================================================================
void
GOWindow::destroyHost()
{
    workspace.reset();
    repaintTimer.stop();
    releaseKeyTimer.stop();
#ifdef _MSC_VER
    pendingNativeResizeClientSize = QSize();
    inNativeResizeLoop = false;
    processingNativeResizeEvents = false;
#endif
    if (processingCloseEvent) {
        deleteLater();
        return;
    }
    delete this;
}
//=============================================================================
GOFigure*
GOWindow::getGOFigure()
{
    return goFig;
}
//=============================================================================
void
GOWindow::applyFigurePointerCursor()
{
    if (goFig) {
        setCursor(figurePointerCursor(goFig));
    }
}
//=============================================================================
int64
GOWindow::getHandle()
{
    return handle;
}
//=============================================================================
int
GOWindow::figureWindowExtraHeight()
{
    int extraHeight = 1;
    if (menuBar() && !menuBar()->isHidden()) {
        extraHeight += menuBar()->sizeHint().height();
    }
    if (toolBar && !toolBar->isHidden()) {
        extraHeight += toolBar->sizeHint().height();
    }
    return extraHeight;
}
//=============================================================================
void
GOWindow::performPendingRepaint(bool immediate)
{
    GOFigure* fig = getGOFigure();
    if (fig && fig->isDrawLater()) {
        repaintPending.store(false);
        return;
    }
    if (repaintInProgress) {
        repaintDeferred = true;
        return;
    }
    repaintInProgress = true;
    repaintPending.store(true);
    if (figureHasChildren(fig)) {
        ensureBaseFigureWidget();
    }
    bool firstContent = shouldRenderFirstContent();
    if (shouldPresentOnFirstContent()) {
        presentNativeWindow();
        immediate = true;
    } else if (firstContent) {
        immediate = true;
    }
    QWidget* repaintTarget = qtchild ? qtchild : this;
    if (immediate) {
        repaintTarget->repaint();
    } else {
        repaintTarget->update();
        if (qtchild) {
            update();
        }
    }
    updateGraphicsChildPages(repaintTarget, immediate);
    if (immediate) {
        // A window composed through the GL path (it hosts a QOpenGLWidget) defers even
        // QWidget::repaint() to an update request on a coarse timer, about 31 ms per animation
        // frame. Flush the events posted by the dirty marking first (they end in
        // QWindow::requestUpdate, which arms updateRequestPending), then hand the window its
        // update request directly so the paint happens synchronously.
        QCoreApplication::sendPostedEvents(repaintTarget, 0);
        QCoreApplication::sendPostedEvents(this, 0);
        if (QWindow* nativeWindow = windowHandle()) {
            QCoreApplication::sendPostedEvents(nativeWindow, 0);
            QEvent updateRequest(QEvent::UpdateRequest);
            QCoreApplication::sendEvent(nativeWindow, &updateRequest);
        }
    }
    if (firstContent) {
        bool rendered = true;
        BaseFigureQt* baseFigure = dynamic_cast<BaseFigureQt*>(qtchild);
        if (baseFigure) {
            rendered = baseFigure->waitForRenderedFrame(FIRST_RENDER_SYNC_TIMEOUT_MS);
        }
        firstContentPresentationPending = false;
        if (!rendered && !repaintTimer.isActive()) {
            repaintTimer.start(REPAINT_COALESCE_DELAY_MS);
        }
    }
    repaintPending.store(false);
    repaintInProgress = false;
    if (repaintDeferred) {
        repaintDeferred = false;
        if (!repaintTimer.isActive()) {
            repaintTimer.start(REPAINT_COALESCE_DELAY_MS);
        }
    }
}
//=============================================================================
void
GOWindow::requestRepaint()
{
    if (m_headless) {
        if (goFig) {
            goFig->setRenderingStateInvalid(true);
        }
        return;
    }
    if (!hasQtRuntime()) {
        if (repaintPending.exchange(true)) {
            return;
        }
        if (figureHasChildren(goFig)) {
            ensureBaseFigureWidget();
        }
        if (qtchild) {
            qtchild->update();
        } else {
            update();
        }
        repaintPending.store(false);
        return;
    }

    if (figureHasChildren(goFig)) {
        ensureBaseFigureWidget();
    }
    repaintPending.store(true);
    QPointer<GOWindow> window(this);
    auto scheduleRepaint = [window]() {
        if (window.isNull()) {
            return;
        }
        // Coalesce, do not defer: restarting an already armed single shot timer
        // on every request means a loop that repaints faster than the delay
        // keeps pushing the deadline back and never gets painted at all.
        if (!window->repaintTimer.isActive()) {
            window->repaintTimer.start(REPAINT_COALESCE_DELAY_MS);
        }
    };
    if (isGuiThread()) {
        scheduleRepaint();
    } else {
        postToGui(scheduleRepaint);
    }
}
//=============================================================================
bool
GOWindow::shouldRenderFirstContent()
{
    if (m_headless || !firstContentPresentationPending || !canShowFigureWidgets()
        || isDockedInDesktop()) {
        return false;
    }
    if (!goFig || goFig->isDrawLater()
        || !goFig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
        return false;
    }
    return figureHasDrawableContent(goFig);
}
//=============================================================================
bool
GOWindow::shouldPresentOnFirstContent()
{
    if (isVisible()) {
        return false;
    }
    return shouldRenderFirstContent();
}
//=============================================================================
void
GOWindow::requestRepaintNow()
{
    if (m_headless) {
        if (goFig) {
            goFig->setRenderingStateInvalid(true);
        }
        return;
    }
    QPointer<GOWindow> window(this);
    auto repaintNow = [window]() {
        if (window.isNull()) {
            return;
        }
        window->repaintTimer.stop();
        window->performPendingRepaint(true);
    };

    if (!hasQtRuntime() || isGuiThread()) {
        repaintNow();
    } else {
        callGuiSync(repaintNow);
    }
}
//=============================================================================
bool
GOWindow::syncRendering(int timeoutMs)
{
    if (m_headless) {
        return true; // nothing to flush; the client pulls the display list
    }
    bool rendered = true;
    QPointer<GOWindow> window(this);
    auto syncNow = [window, timeoutMs, &rendered]() {
        rendered = true;
        if (window.isNull()) {
            return;
        }
        GOFigure* fig = window->getGOFigure();
        if (fig && fig->isDrawLater()) {
            return;
        }
        if (figureHasChildren(fig)) {
            window->ensureBaseFigureWidget();
        }
        BaseFigureQt* baseFigure = dynamic_cast<BaseFigureQt*>(window->qtchild);
        if (!baseFigure) {
            return;
        }
        if (fig && !fig->isRenderingStateInvalid() && baseFigure->hasRenderedFrame()) {
            return;
        }
        const int effectiveTimeoutMs = baseFigure->hasRenderedFrame()
            ? timeoutMs
            : std::max(timeoutMs, FIRST_RENDER_SYNC_TIMEOUT_MS);
        rendered = baseFigure->waitForRenderedFrame(effectiveTimeoutMs);
        window->repaintPending.store(false);
    };

    if (!hasQtRuntime() || isGuiThread()) {
        syncNow();
    } else {
        callGuiSync(syncNow);
    }
    return rendered;
}
//=============================================================================
QImage
GOWindow::grabFrameImage(bool rawOpenGL, bool wait)
{
    QImage image;
    QPointer<GOWindow> window(this);
    auto grabNow = [window, rawOpenGL, wait, &image]() {
        if (window.isNull()) {
            return;
        }
        window->ensureBaseFigureWidget();
        if (window->goFig
            && window->goFig->stringCheck(
                GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
            std::vector<double> position
                = window->goFig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
            std::vector<double> pixels = figurePositionToPixels(window->goFig, position);
            if (pixels.size() >= 4 && pixels[2] > 0 && pixels[3] > 0) {
                window->goFig->setRenderingSize(static_cast<int>(std::lround(pixels[2])),
                    static_cast<int>(std::lround(pixels[3])));
            }
        }
        BaseFigureQt* baseFigure = dynamic_cast<BaseFigureQt*>(window->qtchild);
        if (!baseFigure) {
            return;
        }
        if (wait) {
            const int timeoutMs = baseFigure->hasRenderedFrame() ? RENDER_SYNC_TIMEOUT_MS
                                                                 : FIRST_RENDER_SYNC_TIMEOUT_MS;
            baseFigure->waitForRenderedFrame(timeoutMs);
        }
        image = rawOpenGL ? baseFigure->getOpenGLFrame() : baseFigure->getFrame();
    };

    if (!hasQtRuntime() || isGuiThread()) {
        grabNow();
    } else {
        callGuiSync(grabNow);
    }
    return image;
}
//=============================================================================
void
GOWindow::updateState(bool forceUpdate)
{
    if (m_headless) {
        return; // window chrome (theme, cursors, menus, toolbar) has no meaning
    }
    if (goFig->hasChanged(GO_THEME_PROPERTY_NAME_STR)
        || goFig->hasChanged(GO_THEME_MODE_PROPERTY_NAME_STR) || forceUpdate) {
        applyWindowTheme(this, goFig);
        goFig->clearChanged(GO_THEME_PROPERTY_NAME_STR);
        goFig->clearChanged(GO_THEME_MODE_PROPERTY_NAME_STR);
    }
    if (goFig->hasChanged(GO_POINTER_PROPERTY_NAME_STR)
        || goFig->hasChanged(GO_POINTER_SHAPE_C_DATA_PROPERTY_NAME_STR)
        || goFig->hasChanged(GO_POINTER_SHAPE_HOT_SPOT_PROPERTY_NAME_STR) || forceUpdate) {
        if (mouseMode == MOUSE_MODE::NONE) {
            applyFigurePointerCursor();
        }
        goFig->clearChanged(GO_POINTER_PROPERTY_NAME_STR);
        goFig->clearChanged(GO_POINTER_SHAPE_C_DATA_PROPERTY_NAME_STR);
        goFig->clearChanged(GO_POINTER_SHAPE_HOT_SPOT_PROPERTY_NAME_STR);
    }
    if (goFig->hasChanged(GO_TOOL_BAR_PROPERTY_NAME_STR) || forceUpdate) {
        if (!canShowFigureWidgets()
            || goFig->stringCheck(GO_TOOL_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
            toolBar->hide();
        } else {
            toolBar->show();
        }
        goFig->clearChanged(GO_TOOL_BAR_PROPERTY_NAME_STR);
    }
    if (goFig->hasChanged(GO_MENU_BAR_PROPERTY_NAME_STR) || forceUpdate) {
        menuBar()->setVisible(canShowFigureWidgets()
            && goFig->stringCheck(GO_MENU_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FIGURE_STR));
        goFig->clearChanged(GO_MENU_BAR_PROPERTY_NAME_STR);
    }
    if (workspace) {
        workspace->syncVisibility();
    }
    if (goFig->hasChanged(GO_NAME_PROPERTY_NAME_STR)
        || goFig->hasChanged(GO_NUMBER_PROPERTY_NAME_STR)
        || goFig->hasChanged(GO_NUMBER_TITLE_PROPERTY_NAME_STR) || forceUpdate) {
        bool withNumberTitle
            = (goFig->stringCheck(GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
        int figureId = handle + 1;
        GOStringProperty* name = (GOStringProperty*)goFig->findProperty(GO_NAME_PROPERTY_NAME_STR);
        if (withNumberTitle) {
            if (name->data().empty()) {
                setWindowTitle(
                    wstringToQString(fmt::format(TITLE_WITH_FIGURE_NUMBER_FORMAT, figureId)));
            } else {
                setWindowTitle(wstringToQString(
                    fmt::format(TITLE_WITH_FIGURE_NUMBER_AND_NAME_FORMAT, figureId, name->data())));
            }
        } else {
            setWindowTitle(
                wstringToQString(fmt::format(TITLE_WITH_NAME_ONLY_FORMAT, name->data())));
        }
        goFig->clearChanged(GO_NAME_PROPERTY_NAME_STR);
        goFig->clearChanged(GO_NUMBER_PROPERTY_NAME_STR);
        goFig->clearChanged(GO_NUMBER_TITLE_PROPERTY_NAME_STR);
    }

    std::vector<double> figurePosition;
    if (getChangedFigurePosition(goFig, figurePosition) || forceUpdate) {
        if (figurePosition.size() < 4) {
            figurePosition = goFig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);
        }
        // a docked figure has no screen geometry of its own: position requests
        // are ignored while docked (the dock layout owns the geometry).
        if (figurePosition.size() >= 4 && !isDockedInDesktop()) {
            syncFigurePositionProperties(goFig, figurePosition);
            std::vector<double> pixelPosition = figurePositionToPixels(goFig, figurePosition);

            int ws = 0;
            int hs = 0;
            BaseFigureQt::currentScreenResolution(ws, hs);
            int w = (int)(pixelPosition[2]);
            int h = (int)(pixelPosition[3]);

            int x = (int)(pixelPosition[0]);
            int y = (int)(pixelPosition[1]);

            setFigureGeometryFromInnerPosition(x, y, w, h, hs);
        }
        clearFigurePositionChanges(goFig);
    }

    if ((goFig->hasChanged(GO_RESIZE_PROPERTY_NAME_STR) || forceUpdate) && !isDockedInDesktop()) {
        int wmin = 0;
        int hmin = 0;
        int wmax = QWIDGETSIZE_MAX;
        int hmax = QWIDGETSIZE_MAX;

        Qt::WindowFlags windowFlagsOptions;
        if (goFig->stringCheck(GO_RESIZE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR)) {
            wmin = width();
            hmin = height();
            wmax = width();
            hmax = height();
            windowFlagsOptions = (windowFlags() & ~Qt::WindowMaximizeButtonHint);
        } else {
            wmin = 0;
            hmin = 0;
            wmax = QWIDGETSIZE_MAX;
            hmax = QWIDGETSIZE_MAX;
            windowFlagsOptions = (windowFlags() | Qt::WindowMaximizeButtonHint);
        }

        setMinimumSize(QSize(wmin, hmin));
        setMaximumSize(QSize(wmax, hmax));
        setWindowFlags(windowFlagsOptions);
        forceUpdate = true;
    }
    if (goFig->hasChanged(GO_WINDOW_STYLE_PROPERTY_NAME_STR) || forceUpdate) {
        bool wantDocked
            = goFig->stringCheck(GO_WINDOW_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_DOCKED_STR);
        // handled before the Visible block below so that a figure created with
        // ('WindowStyle', 'docked') shows up directly inside the desktop.
        setDockedInDesktop(wantDocked);
        if (!isDockedInDesktop()) {
            const bool visible
                = goFig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
            // 'alwaysontop' only makes sense for a real top-level window
            bool wantOnTop = goFig->stringCheck(
                GO_WINDOW_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ALWAYS_ON_TOP_STR);
            if (windowFlags().testFlag(Qt::WindowStaysOnTopHint) != wantOnTop) {
                // changing the flag recreates the native window
                nativeTitleBarThemeApplied = false;
                setWindowFlag(Qt::WindowStaysOnTopHint, wantOnTop);
                if (visible && canShowFigureWidgets()
                    && NelsonConfiguration::getInstance()->getNelsonEngineMode() != WEB_GUI) {
                    show();
                }
            }
            // 'modal' blocks input to the other Nelson windows while visible
            bool wantModal = goFig->stringCheck(
                GO_WINDOW_STYLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MODAL_STR);
            Qt::WindowModality wantedModality = wantModal ? Qt::ApplicationModal : Qt::NonModal;
            if (windowModality() != wantedModality) {
                // Qt only honors a modality change while the window is hidden
                bool wasVisible = isVisible();
                if (wasVisible) {
                    hide();
                }
                setWindowModality(wantedModality);
                if (wasVisible && visible && canShowFigureWidgets()
                    && NelsonConfiguration::getInstance()->getNelsonEngineMode() != WEB_GUI) {
                    show();
                }
            }
        }
        goFig->clearChanged(GO_WINDOW_STYLE_PROPERTY_NAME_STR);
    }

    const bool figureVisible
        = goFig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    const bool visibleChanged = goFig->hasChanged(GO_VISIBLE_PROPERTY_NAME_STR);
    // In the web desktop the figure is rendered offscreen and streamed to the browser
    // panel: keep the render widget, never show or hide a Qt window.
    const bool webGui = NelsonConfiguration::getInstance()->getNelsonEngineMode() == WEB_GUI;
    if (visibleChanged || forceUpdate) {
        if (figureVisible) {
            if (figureHasChildren(goFig)) {
                ensureBaseFigureWidget();
            }
            if (canShowFigureWidgets() && !webGui) {
                if (isDockedInDesktop()) {
                    DesktopShowDockedWindow(this);
                } else if (figureHasDrawableContent(goFig) || !firstContentPresentationPending
                    || forceUpdate) {
                    presentNativeWindow();
                }
            }
            if (!webGui) {
                saveFocus();
                if (visibleChanged && !goFig->isDrawLater()) {
                    requestRepaintNow();
                }
            }
        } else if (!webGui) {
            if (isDockedInDesktop()) {
                DesktopHideDockedWindow(this);
            }
            hide();
        }
        goFig->clearChanged(GO_VISIBLE_PROPERTY_NAME_STR);
    }

    if (figureVisible && canShowFigureWidgets() && !isDockedInDesktop() && !webGui
        && (goFig->hasChanged(GO_WINDOW_STATE_PROPERTY_NAME_STR) || forceUpdate)) {
        GOWindowStateProperty* wsp
            = (GOWindowStateProperty*)goFig->findProperty(GO_WINDOW_STATE_PROPERTY_NAME_STR);
        const std::wstring& stateValue = wsp ? wsp->data() : L"";
        const bool wasFullScreen = isFullScreen();
        if (wasFullScreen
            && (stateValue == GO_PROPERTY_VALUE_NORMAL_STR
                || stateValue == GO_PROPERTY_VALUE_MINIMIZED_STR
                || stateValue == GO_PROPERTY_VALUE_MAXIMIZED_STR)) {
            setWindowState(Qt::WindowActive);
        }

        if (stateValue == GO_PROPERTY_VALUE_NORMAL_STR) {
            showNormal();
        } else if (stateValue == GO_PROPERTY_VALUE_FULLSCREEN_STR) {
            showFullScreen();
        } else if (stateValue == GO_PROPERTY_VALUE_MINIMIZED_STR) {
            showMinimized();
        } else if (stateValue == GO_PROPERTY_VALUE_MAXIMIZED_STR) {
            showMaximized();
        }
    }
    if (forceUpdate) {
        rebuildFigureMenus(this);
    }
}
//=============================================================================
void
GOWindow::updateState()
{
    if (!initialized) {
        return;
    }
    updateState(false);
}
//=============================================================================
void
GOWindow::changeEvent(QEvent* event)
{
    if (event->type() == QEvent::ParentChange) {
        // docked into / undocked from the desktop: the title bar undock
        // button acts without going through setDockedInDesktop, so the
        // action state and the WindowStyle property are synced here.
        updateDockActionState();
        if (initialized) {
            syncWindowStyleProperty();
        }
    }
    if (event->type() == QEvent::WindowStateChange && !isDockedInDesktop()) {
        GOWindowStateProperty* hp
            = (GOWindowStateProperty*)goFig->findProperty(GO_WINDOW_STATE_PROPERTY_NAME_STR);
        if (!hp) {
            QMainWindow::changeEvent(event);
            return;
        }
        Qt::WindowStates newState = windowState();
        if (newState.testFlag(Qt::WindowMinimized)) {
            hp->data(GO_PROPERTY_VALUE_MINIMIZED_STR);
        } else if (newState.testFlag(Qt::WindowMaximized)) {
            hp->data(GO_PROPERTY_VALUE_MAXIMIZED_STR);
        } else if (newState.testFlag(Qt::WindowFullScreen)) {
            hp->data(GO_PROPERTY_VALUE_FULLSCREEN_STR);
        } else if (newState.testFlag(Qt::WindowNoState)) {
            hp->data(GO_PROPERTY_VALUE_NORMAL_STR);
        }
    }

    QMainWindow::changeEvent(event);
}
//=============================================================================
void
GOWindow::focusInEvent(QFocusEvent* event)
{
    QWidget::focusInEvent(event);
    goFig->setFocus();
}
//=============================================================================
bool
GOWindow::isDockedInDesktop() const
{
    return DesktopIsWindowDocked(this);
}
//=============================================================================
void
GOWindow::toggleDockedInDesktop()
{
    setDockedInDesktop(!isDockedInDesktop());
}
//=============================================================================
void
GOWindow::setDockedInDesktop(bool docked)
{
    if (docked == isDockedInDesktop()) {
        return;
    }
    bool figureVisible
        = goFig && goFig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    if (docked) {
        if (DesktopDockingAvailable()) {
            DesktopDockWindow(this, QString("nelsonFigureDock_%1").arg(handle + 1));
            if (!figureVisible) {
                DesktopHideDockedWindow(this);
                hide();
            }
        }
    } else {
        // reparenting recreates the native window: the dark title bar
        // theme must be applied again on the next show.
        nativeTitleBarThemeApplied = false;
        DesktopUndockWindow(this, figureVisible);
    }
    updateDockActionState();
    // the WindowStyle property is synced by the ParentChange event, so a
    // 'docked' request kept when no desktop is available (CLI) is preserved.
}
//=============================================================================
void
GOWindow::syncWindowStyleProperty()
{
    if (!goFig) {
        return;
    }
    auto* windowStyle = static_cast<GOStringProperty*>(
        goFig->findProperty(GO_WINDOW_STYLE_PROPERTY_NAME_STR, false));
    if (!windowStyle) {
        return;
    }
    if (isDockedInDesktop()) {
        if (windowStyle->data() != GO_PROPERTY_VALUE_DOCKED_STR) {
            windowStyle->data(GO_PROPERTY_VALUE_DOCKED_STR);
        }
    } else if (windowStyle->data() == GO_PROPERTY_VALUE_DOCKED_STR) {
        // 'modal' is an undocked style: only a stale 'docked' value is reset
        windowStyle->data(GO_PROPERTY_VALUE_NORMAL_STR);
    }
    goFig->clearChanged(GO_WINDOW_STYLE_PROPERTY_NAME_STR);
}
//=============================================================================
void
GOWindow::updateDockActionState()
{
    if (!dockAction) {
        return;
    }
    bool docked = isDockedInDesktop();
    dockAction->setVisible(docked || DesktopDockingAvailable());
    if (docked) {
        dockAction->setIcon(sharedIcon(L"/resources/undock-window.svg"));
        dockAction->setText(TR("&Undock"));
        dockAction->setToolTip(TR("Undock the figure from the Nelson desktop"));
    } else {
        dockAction->setIcon(sharedIcon(L"/resources/dock-window.svg"));
        dockAction->setText(TR("&Dock"));
        dockAction->setToolTip(TR("Dock the figure into the Nelson desktop"));
    }
}
//=============================================================================
void
GOWindow::createMenuBar()
{
#if defined(Q_OS_MACOS)
    QMenuBar* macMenuBar = new QMenuBar(this);
    macMenuBar->setNativeMenuBar(false);
    setMenuBar(macMenuBar);
#endif
    QMenuBar* figureMenuBar = this->menuBar();
    fileMenu = figureMenuBar->addMenu(TR("&File"));
    fileMenu->addAction(closeAction);
    windowMenu = figureMenuBar->addMenu(TR("&Window"));
    windowMenu->menuAction()->setProperty("nelsonWindowMenu", true);
    updateWindowMenuItems();
    connect(windowMenu, &QMenu::aboutToShow, this, &GOWindow::refreshWindowMenuItems);
}
//=============================================================================
void
GOWindow::updateWindowMenuItems()
{
    windowMenu->clear();
    updateDockActionState();
    if (dockAction->isVisible()) {
        windowMenu->addAction(dockAction);
        windowMenu->addSeparator();
    }
    std::map<int64, GOFigureHost*> figureList = getFigureList();

    for (const auto& pair : figureList) {
        if (pair.second && this->getHandle() != pair.second->hostId()) {
            GOFigure* fig = pair.second->hostFigure();
            std::wstring nameFigure = fig->findStringProperty(GO_NAME_PROPERTY_NAME_STR);
            QString menuItem;
            bool withNumberTitle
                = (goFig->stringCheck(GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
            int figureId = static_cast<int>(pair.second->hostId()) + 1;
            if (withNumberTitle) {
                if (nameFigure.empty()) {
                    menuItem
                        = wstringToQString(fmt::format(TITLE_WITH_FIGURE_NUMBER_FORMAT, figureId));
                } else {
                    menuItem = wstringToQString(fmt::format(
                        TITLE_WITH_FIGURE_NUMBER_AND_NAME_FORMAT, figureId, nameFigure));
                }
            } else {
                menuItem = wstringToQString(nameFigure);
            }
            QAction* action
                = new QAction(sharedIcon(L"/modules/graphics/resources/icon-mathematical-plot.svg"),
                    menuItem, this);
            connect(action, &QAction::triggered, [pair]() {
                GOWindow* other = dynamic_cast<GOWindow*>(pair.second);
                if (other == nullptr) {
                    return;
                }
                if (DesktopIsWindowDocked(other)) {
                    DesktopShowDockedWindow(other);
                } else {
                    other->activateWindow();
                    other->raise();
                }
                other->setFocus();
            });
            windowMenu->addAction(action);
        }
    }
    QMainWindow* mainWindow = (QMainWindow*)GetMainGuiObject();
    if (mainWindow && mainWindow->isVisible()) {
        QString menuItem = TR("&Command Window");
        QAction* action = new QAction(sharedIcon(L"/resources/console-clear.svg"), menuItem, this);
        connect(action, &QAction::triggered, [mainWindow]() {
            mainWindow->activateWindow();
            mainWindow->raise();
            mainWindow->setFocus();
        });
        windowMenu->addSeparator();
        windowMenu->addAction(action);
    }
    if (isTextEditorVisible()) {
        QString menuItem = TR("&Editor");
        QAction* action = new QAction(sharedIcon(L"/resources/document-open.svg"), menuItem, this);
        connect(action, &QAction::triggered, []() { showTextEditor(); });
        windowMenu->addSeparator();
        windowMenu->addAction(action);
    }
}
//=============================================================================
void
GOWindow::refreshWindowMenuItems()
{
    updateWindowMenuItems();
}
//=============================================================================
void
GOWindow::contextMenuEvent(QContextMenuEvent* event)
{
    int x = event->pos().x();
    int y = event->pos().y();
    GOAxis* axis = findContainingAxis(goFig, remapX(x), remapY(y));
    if (axis && popupContextMenuForObject(this, axis, event->globalPos())) {
        event->accept();
        return;
    }
    int64 colorBarHandle = findContainingColorBar(goFig, remapX(x), remapY(y));
    GraphicsObject* colorBar = graphicsObjectFromHandle(colorBarHandle, false);
    if (colorBar && popupContextMenuForObject(this, colorBar, event->globalPos())) {
        event->accept();
        return;
    }
    if (popupContextMenuForObject(this, goFig, event->globalPos())) {
        event->accept();
        return;
    }
    event->accept();
}
//=============================================================================
void
GOWindow::deliverButtonDownCallbacks(QMouseEvent* e, const std::wstring& selectionType)
{
    setFigureSelectionType(goFig, selectionType);
    if (NelsonConfiguration::getInstance()->isCurrentAxesOnClick()) {
        forceCurrentAxes(e);
    }
    // The object drawn under the cursor (line, ...) is the primary target; when
    // nothing is hit the click falls back to the colorbar/legend, then to the
    // current axes, so an empty area still reaches the axes.
    int64 pickedObject = pickObjectAtPixel(goFig, lastFigurePixelX, lastFigurePixelY);
    if (pickedObject < HANDLE_OFFSET_OBJECT) {
        int x, y;
        qtGetPosition(e, x, y);
        int64 pickedColorBar = findContainingColorBar(goFig, remapX(x), remapY(y));
        if (pickedColorBar >= HANDLE_OFFSET_OBJECT) {
            pickedObject = pickedColorBar;
        } else {
            int64 currentAxes = goFig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR);
            if (currentAxes >= HANDLE_OFFSET_OBJECT && !isDeletedGraphicsObject(currentAxes)) {
                GraphicsObject* currentAxesObject = findGraphicsObject(currentAxes, false);
                if (isPickable(currentAxesObject)) {
                    pickedObject = currentAxes;
                }
            }
        }
    }
    if (pickedObject >= HANDLE_OFFSET_OBJECT) {
        goFig->setGoProperty(GO_CURRENT_OBJECT_PROPERTY_NAME_STR, pickedObject);
    }

    handleMouseEvent(e, GO_WINDOW_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"WindowMousePress");
    GraphicsObject* object
        = pickedObject >= HANDLE_OFFSET_OBJECT ? findGraphicsObject(pickedObject, false) : nullptr;
    if (isPickable(object)) {
        handleMouseEvent(object, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    } else if (object == nullptr) {
        handleMouseEvent(e, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    }
    deliverLegendItemHit(e, selectionType);
}
//=============================================================================
// A click on a legend entry fires that legend's ItemHitFcn with the entry's
// plotted object as Peer and the clicked Region ('icon' or 'label'), like MATLAB.
void
GOWindow::deliverLegendItemHit(QMouseEvent* e, const std::wstring& selectionType)
{
    if (!goFig) {
        return;
    }
    int px = 0;
    int py = 0;
    qtGetPosition(e, px, py);
    int mx = remapX(px);
    int my = remapY(py);
    GOGObjectsProperty* cp
        = static_cast<GOGObjectsProperty*>(goFig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> children(cp->data());
    QRect plotRegion(qtchild->geometry());
    for (auto it = children.rbegin(); it != children.rend(); ++it) {
        GOLegend* legend = dynamic_cast<GOLegend*>(findGraphicsObject(*it, false));
        if (!legend) {
            continue;
        }
        int64 peerHandle = 0;
        int region = 0;
        if (!legend->hitTestEntry(mx, my, plotRegion.width(), plotRegion.height(), peerHandle, region)) {
            continue;
        }
        GOCallbackProperty* cb = static_cast<GOCallbackProperty*>(
            legend->findProperty(GO_ITEM_HIT_FCN_PROPERTY_NAME_STR, false));
        if (cb && !cb->get().isEmpty()) {
            wstringVector fieldNames = { L"Peer", L"Region", L"SelectionType" };
            ArrayOfVector fieldValues;
            fieldValues << graphicsObjectHandle(peerHandle, false);
            fieldValues << ArrayOf::characterArray(region == 0 ? std::wstring(L"icon")
                                                              : std::wstring(L"label"));
            fieldValues << ArrayOf::characterArray(selectionType);
            cb->pushEvent(legend, L"ItemHitData", L"ItemHit", fieldNames, fieldValues);
        }
        return;
    }
}
//=============================================================================
int64
GOWindow::findContainingColorBar(GOFigure* fig, int x, int y)
{
    if (!fig) {
        return -1;
    }
    GOGObjectsProperty* cp
        = static_cast<GOGObjectsProperty*>(fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR));
    std::vector<int64> children(cp->data());
    QRect plotRegion(qtchild->geometry());
    for (auto it = children.rbegin(); it != children.rend(); ++it) {
        GraphicsObject* object = findGraphicsObject(*it, false);
        GOColorBar* colorBar = dynamic_cast<GOColorBar*>(object);
        if (colorBar && colorBar->hitTest(x, y, plotRegion.width(), plotRegion.height())) {
            return *it;
        }
        GOLegend* legend = dynamic_cast<GOLegend*>(object);
        if (legend && legend->hitTest(x, y, plotRegion.width(), plotRegion.height())) {
            return *it;
        }
    }
    return -1;
}
//=============================================================================
int
GOWindow::remapX(int x)
{
    // Mouse coordinates are logical pixels, but axis client areas (m_box) are
    // stored in device pixels (the GL surface is sized in device pixels). Scale
    // to device pixels so hit-testing matches on HiDPI displays.
    QRect plot_region(qtchild->geometry());
    return toDevicePixels(x - plot_region.x(), qtchild->devicePixelRatioF());
}
//=============================================================================
int
GOWindow::remapY(int y)
{
    QRect plot_region(qtchild->geometry());
    return toDevicePixels(plot_region.height() - y + plot_region.y(), qtchild->devicePixelRatioF());
}
//=============================================================================
MOUSE_MODE
GOWindow::getCurrentMouseMode() { return mouseMode; }
//=============================================================================
void
GOWindow::setModeWaitMouseOrKeyPressEvent(bool wait)
{
    if (wait) {
        waitKeyOrMousePressedMode = WAIT_PRESS_MODE::WAIT;
    } else {
        waitKeyOrMousePressedMode = WAIT_PRESS_MODE::NONE;
    }
}
//=============================================================================
bool
GOWindow::isWaitMouseOrKeyPressEvent(WAIT_PRESS_MODE& waitPressMode)
{
    bool isWaiting = waitKeyOrMousePressedMode == WAIT_PRESS_MODE::WAIT;
    if (!isWaiting) {
        waitPressMode = waitKeyOrMousePressedMode;
    }
    return isWaiting;
}
//=============================================================================
GOWindow::~GOWindow()
{
    workspace.reset();
    if (!m_headless) {
        NelsonConfiguration::getInstance()->decrementOpenFigureWindowCount();
    }
    if (goFig) {
        delete goFig;
    }
}
//=============================================================================
void
GOWindow::presentHost()
{
    if (m_headless || !nativeWindowPresentationAvailable()) {
        return;
    }
    callGuiSync([this]() {
        try {
            if (DesktopIsWindowDocked(this)) {
                DesktopShowDockedWindow(this);
            } else {
                presentNativeWindow();
            }
        } catch (...) {
            // Presenting the native window failed (see nativeWindowPresentationAvailable).
            // presentNativeWindow() latches for its own path; cover the docked path too.
            disableNativeWindowPresentation();
        }
    });
}
//=============================================================================
void
GOWindow::prepareHostClose(bool forceClose)
{
    if (m_headless || !forceClose) {
        return;
    }
    // a docked figure is a child of the desktop: it must leave its dock
    // (destroyed with it) before being deleted.
    DesktopUndockWindow(this, false);
    hide();
    // Destroy the native component tree before the model deletes component bindings
    // recursively. The QWidget hierarchy clears the QPointers in those bindings, so their
    // destructors skip hundreds of individual QWidget deletions on a large tabbed interface.
    QWidget* centralWidget = takeCentralWidget();
    qtchild = nullptr;
    delete centralWidget;
}
//=============================================================================
bool
GOWindow::grabHostFrameRGBA(
    bool rawOpenGL, std::vector<unsigned char>& rgba, int& width, int& height)
{
    if (m_headless) {
        return false;
    }
    QImage image = grabFrameImage(rawOpenGL, true).convertToFormat(QImage::Format_RGBA8888);
    if (image.isNull()) {
        return false;
    }
    width = image.width();
    height = image.height();
    rgba.resize((size_t)width * height * 4);
    for (int row = 0; row < height; ++row) {
        memcpy(rgba.data() + (size_t)row * width * 4, image.constScanLine(row), (size_t)width * 4);
    }
    return true;
}
//=============================================================================
void
GOWindow::rebuildHostMenus()
{
    if (m_headless) {
        return;
    }
    rebuildFigureMenus(this);
}
//=============================================================================
void
GOWindow::toggleHostMouseMode(MOUSE_MODE mode)
{
    if (m_headless) {
        return;
    }
    switch (mode) {
    case MOUSE_MODE::PAN:
        onPanAction();
        break;
    case MOUSE_MODE::ZOOM_IN:
        onZoomInAction();
        break;
    case MOUSE_MODE::ZOOM_OUT:
        onZoomOutAction();
        break;
    case MOUSE_MODE::ROTATION:
        onRotateAction();
        break;
    case MOUSE_MODE::NONE:
    default:
        break;
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
