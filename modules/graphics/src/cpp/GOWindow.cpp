//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _USE_MATH_DEFINES
#include <cmath>
#include <QtCore/QSettings>
#include <QtWidgets/QApplication>
#include <QtWidgets/QWidget>
#include <QtWidgets/QMenuBar>
#include <QtWidgets/QFileDialog>
#include <QtGui/QIcon>
#include <QtGui/QMouseEvent>
#include <QtPrintSupport/QPrintDialog>
#include <QtPrintSupport/QPrintPreviewDialog>
#include <QtPrintSupport/QPrinter>
#define FMT_HEADER_ONLY
#include <fmt/printf.h>
#include <fmt/format.h>
#include <fmt/xchar.h>
#include "QtTranslation.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOWindow.hpp"
#include "GOVectorFourDoubleProperty.hpp"
#include "GOFiguresManager.hpp"
#include "GOStringProperty.hpp"
#include "BaseFigureQt.hpp"
#include "RenderQt.hpp"
#include "QStringConverter.hpp"
#include "Types.hpp"
#include "NelsonConfiguration.hpp"
#include "i18n.hpp"
#include "GOHelpers.hpp"
#include "ForceWindowsTitleBarToDark.hpp"
#include "NelsonPalette.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOAxis.hpp"
#include "PostCommand.hpp"
#include "GOColorProperty.hpp"
#include "Nelson_VERSION.h"
#include "HelpBrowser.hpp"
#include "TextEditor.hpp"
#include "MainGuiObject.hpp"
#include "GOCallbackProperty.hpp"
#include "CallExportGraphicsGui.hpp"
#include "GOWindowStateProperty.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
#define TITLE_WITH_FIGURE_NUMBER_FORMAT L"Figure %d"
#define TITLE_WITH_FIGURE_NUMBER_AND_NAME_FORMAT L"Figure %d: %s"
#define TITLE_WITH_NAME_ONLY_FORMAT L"%s"
//=============================================================================
GOWindow::GOWindow(int64 ahandle) : QMainWindow()
{
    initialized = false;

    releaseKeyTimer.setSingleShot(true);
    connect(&releaseKeyTimer, &QTimer::timeout, this, &GOWindow::onKeyReleaseTimeout);

    QString fileNameIcon
        = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
            + L"/modules/graphics/resources/icon-mathematical-plot.svg");
    setWindowIcon(QPixmap(fileNameIcon));
    handle = ahandle;
    int figureId = handle + 1;
    goFig = new GOFigure(this, figureId);
    setWindowTitle(wstringToQString(fmt::sprintf(TITLE_WITH_FIGURE_NUMBER_FORMAT, figureId)));
    setFocusPolicy(Qt::ClickFocus);
    qtchild = new BaseFigureQt(this, goFig);
    setCentralWidget(qtchild);

#ifdef _MSC_VER
    forceWindowsTitleBarToDark(this->winId());
#endif

    fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/rotate.svg");
    rotateCursor = QCursor(fileNameIcon);

    fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/zoom-in.svg");
    zoomInCursor = QCursor(fileNameIcon);

    fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/zoom-out.svg");
    zoomOutCursor = QCursor(fileNameIcon);

    createActions();
    createMenuBar();
    createToolbars();
    resize(560, 470);
    setMouseTracking(true);
    statusBar()->setVisible(false);
    mousePositionOrigin.setX(width() / 2);
    mousePositionOrigin.setY(height() / 2);
    zoomInRubberBand = nullptr;
    if (goFig && qtchild) {
        goFig->setScalarDoubleDefault(
            GO_DEVICE_PIXEL_RATIO_PROPERTY_NAME_STR, qtchild->devicePixelRatio());
    }
    initialized = true;
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
        = (GOCallbackProperty*)goFig->findProperty(GO_CLOSE_REQUEST_FCN_NAME_STR);
    if (goCallback) {
        ArrayOf cgo = goCallback->data();
        if (cgo.isRowVectorCharacterArray() || cgo.isScalarStringArray()) {
            std::wstring callbackString = cgo.getContentAsWideCharactersPointer();
            if (callbackString == GO_PROPERTY_VALUE_CLOSEREQ_STR) {
                closeFigure(handle, false);
                e->accept();
                return;
            }
        }
        goCallback->pushEvent(goFig, L"WindowCloseRequestData", L"Close");
    }

    e->ignore();
}
//=============================================================================
GOFigure*
GOWindow::getGOFigure()
{
    return goFig;
}
//=============================================================================
int64
GOWindow::getHandle()
{
    return handle;
}
//=============================================================================
void
GOWindow::updateState(bool forceUpdate)
{
    if (goFig->hasChanged(GO_TOOL_BAR_PROPERTY_NAME_STR) || forceUpdate) {
        if (goFig->stringCheck(GO_TOOL_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_NONE_STR)) {
            toolBar->hide();
        } else {
            toolBar->show();
        }
        goFig->clearChanged(GO_TOOL_BAR_PROPERTY_NAME_STR);
    }
    if (goFig->hasChanged(GO_MENU_BAR_PROPERTY_NAME_STR) || forceUpdate) {
        menuBar()->setVisible(
            goFig->stringCheck(GO_MENU_BAR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_FIGURE_STR));
        goFig->clearChanged(GO_MENU_BAR_PROPERTY_NAME_STR);
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
                    wstringToQString(fmt::sprintf(TITLE_WITH_FIGURE_NUMBER_FORMAT, figureId)));
            } else {
                setWindowTitle(wstringToQString(fmt::sprintf(
                    TITLE_WITH_FIGURE_NUMBER_AND_NAME_FORMAT, figureId, name->data())));
            }
        } else {
            setWindowTitle(
                wstringToQString(fmt::sprintf(TITLE_WITH_NAME_ONLY_FORMAT, name->data())));
        }
        goFig->clearChanged(GO_NAME_PROPERTY_NAME_STR);
        goFig->clearChanged(GO_NUMBER_PROPERTY_NAME_STR);
        goFig->clearChanged(GO_NUMBER_TITLE_PROPERTY_NAME_STR);
    }

    if (goFig->hasChanged(GO_POSITION_PROPERTY_NAME_STR) || forceUpdate) {
        std::vector<double> position
            = goFig->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR);

        int ws = 0;
        int hs = 0;
        ((BaseFigureQt*)(qtchild))->currentScreenResolution(ws, hs);
        int w = (int)(position[2]);
        int h = (int)(position[3]);

        int x = (int)(position[0]);
        int y = (int)(position[1]);

        int transformedY = hs - h - y;
        setGeometry(x, transformedY, w, h);
        goFig->clearChanged(GO_POSITION_PROPERTY_NAME_STR);
    }

    if (goFig->hasChanged(GO_RESIZE_PROPERTY_NAME_STR) || forceUpdate) {
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
    if (goFig->hasChanged(GO_VISIBLE_PROPERTY_NAME_STR) || forceUpdate) {
        if (goFig->stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR)) {
            show();
            saveFocus();
        } else {
            hide();
        }
        goFig->clearChanged(GO_VISIBLE_PROPERTY_NAME_STR);
    }

    if (goFig->hasChanged(GO_WINDOW_STATE_PROPERTY_NAME_STR) || forceUpdate) {
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
    if (event->type() == QEvent::WindowStateChange) {
        GOWindowStateProperty* hp
            = (GOWindowStateProperty*)goFig->findProperty(GO_WINDOW_STATE_PROPERTY_NAME_STR);
        if (!hp) {
            QMainWindow::changeEvent(event);
            return;
        }
        auto* stateEvent = static_cast<QWindowStateChangeEvent*>(event);
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
GOWindow::moveEvent(QMoveEvent* e)
{
    QWidget::moveEvent(e);
    if (initialized) {
        goFig->refreshPositionProperties(false);
    }
}
//=============================================================================
void
GOWindow::focusInEvent(QFocusEvent* event)
{
    QWidget::focusInEvent(event);
    goFig->setFocus();
}
//=============================================================================
void
GOWindow::createActions()
{
    QString fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/exit.svg");
    this->closeAction = new QAction(QIcon(fileNameIcon), TR("&Close"), this);
    connect(closeAction, SIGNAL(triggered()), this, SLOT(close()));

    fileNameIcon = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
        + L"/resources/document-new.svg");
    newFigureAction = new QAction(QIcon(fileNameIcon), TR("&New Figure"), this);
    connect(newFigureAction, &QAction::triggered, this, &GOWindow::onNewFigureAction);

    fileNameIcon = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
        + L"/resources/document-print.svg");
    printAction = new QAction(QIcon(fileNameIcon), TR("&Print"), this);
    connect(printAction, &QAction::triggered, this, &GOWindow::onPrintAction);

    fileNameIcon = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
        + L"/resources/document-save-as.svg");
    exportAction = new QAction(QIcon(fileNameIcon), TR("&Export to ..."), this);
    connect(exportAction, &QAction::triggered, this, &GOWindow::onExportAction);

    fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/rotate.svg");
    rotateAction = new QAction(QIcon(fileNameIcon), TR("&Rotate"), this);
    connect(rotateAction, &QAction::triggered, this, &GOWindow::onRotateAction);
    rotateAction->setCheckable(true);

    fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/pan.svg");
    panAction = new QAction(QIcon(fileNameIcon), TR("&Pan"), this);
    connect(panAction, &QAction::triggered, this, &GOWindow::onPanAction);
    panAction->setCheckable(true);

    fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/zoom-in.svg");
    zoomInAction = new QAction(QIcon(fileNameIcon), TR("Zoom &In"), this);
    connect(zoomInAction, &QAction::triggered, this, &GOWindow::onZoomInAction);
    zoomInAction->setCheckable(true);

    fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/zoom-out.svg");
    zoomOutAction = new QAction(QIcon(fileNameIcon), TR("Zoom &Out"), this);
    connect(zoomOutAction, &QAction::triggered, this, &GOWindow::onZoomOutAction);
    zoomOutAction->setCheckable(true);

    fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/refresh.svg");
    restoreViewAction = new QAction(QIcon(fileNameIcon), TR("&Restore View"), this);
    connect(restoreViewAction, &QAction::triggered, this, &GOWindow::onRestoreViewAction);

    fileNameIcon = wstringToQString(
        NelsonConfiguration::getInstance()->getNelsonRootDirectory() + L"/resources/help-icon.svg");
    helpAction = new QAction(QIcon(fileNameIcon), TR("&Graphics help"), this);
    connect(helpAction, &QAction::triggered, this, &GOWindow::onHelpAction);
}
//=============================================================================
void
GOWindow::createMenuBar()
{
    fileMenu = this->menuBar()->addMenu(TR("&File"));
    fileMenu->addAction(closeAction);
    windowMenu = this->menuBar()->addMenu(TR("&Window"));
    updateWindowMenuItems();
    connect(windowMenu, &QMenu::aboutToShow, this, &GOWindow::refreshWindowMenuItems);
}
//=============================================================================
void
GOWindow::createToolbars()
{
    toolBar = addToolBar(TR("Graphics tools"));
    toolBar->setIconSize(QSize(18, 18));
    toolBar->addAction(newFigureAction);
    toolBar->addAction(printAction);
    toolBar->addAction(exportAction);
    toolBar->addSeparator();
    toolBar->addAction(rotateAction);
    toolBar->addAction(panAction);
    toolBar->addAction(zoomInAction);
    toolBar->addAction(zoomOutAction);
    toolBar->addAction(restoreViewAction);
    toolBar->addSeparator();
    toolBar->addAction(helpAction);
    toolBar->addSeparator();
}
//=============================================================================
void
GOWindow::updateWindowMenuItems()
{
    windowMenu->clear();
    std::map<int64, GOWindow*> figureList = getFigureList();
    QString fileNameIcon
        = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
            + L"/modules/graphics/resources/icon-mathematical-plot.svg");

    for (const auto& pair : figureList) {
        if (pair.second && this->getHandle() != pair.second->getHandle()) {
            GOFigure* fig = pair.second->getGOFigure();
            std::wstring nameFigure = fig->findStringProperty(GO_NAME_PROPERTY_NAME_STR);
            QString menuItem;
            bool withNumberTitle
                = (goFig->stringCheck(GO_NUMBER_TITLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR));
            int figureId = pair.second->getHandle() + 1;
            if (withNumberTitle) {
                if (nameFigure.empty()) {
                    menuItem
                        = wstringToQString(fmt::sprintf(TITLE_WITH_FIGURE_NUMBER_FORMAT, figureId));
                } else {
                    menuItem = wstringToQString(fmt::sprintf(
                        TITLE_WITH_FIGURE_NUMBER_AND_NAME_FORMAT, figureId, nameFigure));
                }
            } else {
                menuItem = wstringToQString(nameFigure);
            }
            QAction* action = new QAction(QIcon(fileNameIcon), menuItem, this);
            connect(action, &QAction::triggered, [pair]() {
                pair.second->activateWindow();
                pair.second->raise();
                pair.second->setFocus();
            });
            windowMenu->addAction(action);
        }
    }
    QMainWindow* mainWindow = (QMainWindow*)GetMainGuiObject();
    if (mainWindow && mainWindow->isVisible()) {
        fileNameIcon = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
            + L"/resources/console-clear.svg");
        QString menuItem = TR("&Command Window");
        QAction* action = new QAction(QIcon(fileNameIcon), menuItem, this);
        connect(action, &QAction::triggered, [mainWindow]() {
            mainWindow->activateWindow();
            mainWindow->raise();
            mainWindow->setFocus();
        });
        windowMenu->addSeparator();
        windowMenu->addAction(action);
    }
    if (HelpBrowser::getInstance()->isVisible()) {
        fileNameIcon = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
            + L"/resources/help-icon.svg");
        QString menuItem = TR("&Help");
        QAction* action = new QAction(QIcon(fileNameIcon), menuItem, this);
        connect(action, &QAction::triggered, []() { HelpBrowser::getInstance()->show(); });
        windowMenu->addSeparator();
        windowMenu->addAction(action);
    }
    if (isTextEditorVisible()) {
        QString menuItem = TR("&Editor");
        fileNameIcon = wstringToQString(NelsonConfiguration::getInstance()->getNelsonRootDirectory()
            + L"/resources/document-open.svg");
        QAction* action = new QAction(QIcon(fileNameIcon), menuItem, this);
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
    event->accept();
}
//=============================================================================
void
GOWindow::forceCurrentAxes(QMouseEvent* e)
{
    mousePositionOrigin = e->pos();
    int x, y;
    qtGetPosition(e, x, y);
    GOAxis* h = findContainingAxis(goFig, remapX(x), remapY(y));
    if (h) {
        goFig->setGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, assignGraphicsObject(h));
    }
}
//=============================================================================
void
GOWindow::mousePressEvent(QMouseEvent* e)
{
    e->accept();

    if (waitKeyOrMousePressedMode == WAIT_PRESS_MODE::WAIT) {
        waitKeyOrMousePressedMode = WAIT_PRESS_MODE::MOUSE;
        return;
    }
    if (NelsonConfiguration::getInstance()->isCurrentAxesOnClick()) {
        forceCurrentAxes(e);
    }

    handleMouseEvent(e, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");

    if (mouseMode == MOUSE_MODE::PAN) {
        mousePressEventHandlePanMode(e);
    }
    if (mouseMode == MOUSE_MODE::ZOOM_IN) {
        if (e->button() == Qt::LeftButton) {
            mousePositionOrigin = e->pos();
            if (!zoomInRubberBand) {
                zoomInRubberBand = new QRubberBand(QRubberBand::Rectangle, this);
            }
            zoomInRubberBand->setGeometry(QRect(mousePositionOrigin, QSize()));
            zoomInRubberBand->show();
            isZoomInRunning = true;
        } else {
            isZoomInRunning = false;
        }
    }
    if (mouseMode == MOUSE_MODE::ZOOM_OUT) {
        if (e->button() == Qt::LeftButton) {
            mousePositionOrigin = e->pos();
            isZoomOutRunning = true;
        } else {
            isZoomOutRunning = false;
        }
    }
    if (mouseMode == MOUSE_MODE::ROTATION) {
        isRotateRunning = (e->button() == Qt::LeftButton);
        if (isRotateRunning) {
            mousePressEventHandleRotate(e);
        }
    }
    this->getGOFigure()->setRenderingStateInvalid(true);
}
//=============================================================================
void
GOWindow::mousePressEventHandleRotate(QMouseEvent* e)
{
    mousePositionOrigin = e->pos();
}
//=============================================================================
void
GOWindow::mousePressEventHandlePanMode(QMouseEvent* e)
{
    setCursor(Qt::ClosedHandCursor);
    mousePositionOrigin = e->pos();
    int x, y;
    qtGetPosition(e, x, y);
    GOAxis* axis = findContainingAxis(goFig, remapX(x), remapY(y));
    if (axis) {
        GOVectorProperty* hp = (GOVectorProperty*)axis->findProperty(GO_X_LIM_PROPERTY_NAME_STR);
        panXRange = (hp->data()[1] - hp->data()[0]);
        panXMean = (hp->data()[1] + hp->data()[0]) / 2;

        hp = (GOVectorProperty*)axis->findProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        panYRange = (hp->data()[1] - hp->data()[0]);
        panYMean = (hp->data()[1] + hp->data()[0]) / 2;
        isPanRunning = true;
    } else {
        isPanRunning = false;
    }
}
//=============================================================================
void
GOWindow::mouseMoveEvent(QMouseEvent* e)
{
    e->accept();
    if (mouseMode == MOUSE_MODE::PAN && isPanRunning) {
        mouseMoveEventHandlePanMode(e);
        this->getGOFigure()->setRenderingStateInvalid(true);
    }
    if (mouseMode == MOUSE_MODE::ZOOM_IN && isZoomInRunning) {
        zoomInRubberBand->setGeometry(QRect(mousePositionOrigin, e->pos()).normalized());
        this->getGOFigure()->setRenderingStateInvalid(true);
    }
    if (mouseMode == MOUSE_MODE::ROTATION && isRotateRunning) {
        mouseMoveEventHandleRotate(e);
        this->getGOFigure()->setRenderingStateInvalid(true);
    }
}
//=============================================================================
void
GOWindow::mouseMoveEventHandleRotate(QMouseEvent* e)
{
    QPoint dest(e->pos());
    GOAxis* axis = findContainingAxis(
        goFig, remapX(mousePositionOrigin.x()), remapY(mousePositionOrigin.y()));
    if (axis) {
        int x, y;
        qtGetPosition(e, x, y);

        axis->rotateCamera(
            mousePositionOrigin.x(), e->pos().x(), mousePositionOrigin.y(), e->pos().y());
        axis->updateState();
        mousePositionOrigin = e->pos();
    }
}
//=============================================================================
void
GOWindow::mouseMoveEventHandlePanMode(QMouseEvent* e)
{
    QPoint dest(e->pos());
    GOAxis* axis = findContainingAxis(
        goFig, remapX(mousePositionOrigin.x()), remapY(mousePositionOrigin.y()));
    if (axis) {
        std::vector<double> position(axis->getClientAreaAsPixels());
        int x, y;
        qtGetPosition(e, x, y);

        double delx = -(e->pos().x() - mousePositionOrigin.x()) / position[2];
        double dely = (e->pos().y() - mousePositionOrigin.y()) / position[3];

        if (axis->stringCheck(GO_X_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
            delx = (x - mousePositionOrigin.x()) / position[2];
        }
        if (axis->stringCheck(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
            dely = -(y - mousePositionOrigin.y()) / position[3];
        }

        axis->setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR,
            panXMean + panXRange * delx - panXRange / 2,
            panXMean + panXRange * delx + panXRange / 2);
        axis->setRestrictedStringDefault(
            GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);

        axis->setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR,
            panYMean + panYRange * dely - panYRange / 2,
            panYMean + panYRange * dely + panYRange / 2);
        axis->setRestrictedStringDefault(
            GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);

        axis->updateState();
    }
}
//=============================================================================
void
GOWindow::mouseReleaseEvent(QMouseEvent* e)
{
    e->accept();
    if (mouseMode == MOUSE_MODE::PAN && isPanRunning) {
        setCursor(Qt::OpenHandCursor);
        isPanRunning = false;
        this->getGOFigure()->setRenderingStateInvalid(true);
    }
    if (mouseMode == MOUSE_MODE::ZOOM_IN && isZoomInRunning) {
        mousePressEventHandleZoomInMode(e);
        this->getGOFigure()->setRenderingStateInvalid(true);
    }
    if (mouseMode == MOUSE_MODE::ZOOM_OUT && isZoomOutRunning) {
        mousePressEventHandleZoomOutMode(e);
        this->getGOFigure()->setRenderingStateInvalid(true);
    }
    if (mouseMode == MOUSE_MODE::ROTATION && isRotateRunning) {
        isRotateRunning = false;
        this->getGOFigure()->setRenderingStateInvalid(true);
    }
}
//=============================================================================
void
GOWindow::mousePressEventHandleZoomInMode(QMouseEvent* e)
{
    zoomInRubberBand->hide();
    QRect rect(zoomInRubberBand->geometry().normalized());
    delete zoomInRubberBand;
    zoomInRubberBand = nullptr;
    if ((rect.width() * rect.height()) < 20) {
        int click_x, click_y;
        qtGetPosition(e, click_x, click_y);
        GOAxis* h = findContainingAxis(goFig, remapX(click_x), remapY(click_y));
        if (h) {
            GOVectorProperty* hp = (GOVectorProperty*)h->findProperty(GO_X_LIM_PROPERTY_NAME_STR);
            double range = (hp->data()[1] - hp->data()[0]) / 2;
            double mean = (hp->data()[1] + hp->data()[0]) / 2;
            h->setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, mean - range / 2, mean + range / 2);
            h->setRestrictedStringDefault(
                GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);

            hp = (GOVectorProperty*)h->findProperty(GO_Y_LIM_PROPERTY_NAME_STR);
            range = (hp->data()[1] - hp->data()[0]) / 2;
            mean = (hp->data()[1] + hp->data()[0]) / 2;
            h->setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, mean - range / 2, mean + range / 2);
            h->setRestrictedStringDefault(
                GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
            h->updateState();
        }
    } else {
        GOAxis* axis = findContainingAxis(goFig, remapX(rect.x()), remapY(rect.y()));
        if (axis) {
            std::vector<double> position(axis->getClientAreaAsPixels());

            double x = position[0];
            double y = position[1];
            double width = position[2];
            double height = position[3];

            double xminfrac = (remapX(rect.x()) - x) / width;
            double xmaxfrac = (remapX(rect.x() + rect.width()) - x) / width;
            double yminfrac = (remapY(rect.y() + rect.height()) - y) / height;
            double ymaxfrac = (remapY(rect.y()) - y) / height;
            xminfrac = qMax(0., qMin(1., xminfrac));
            xmaxfrac = qMax(0., qMin(1., xmaxfrac));
            yminfrac = qMax(0., qMin(1., yminfrac));
            ymaxfrac = qMax(0., qMin(1., ymaxfrac));
            if (axis->stringCheck(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
                double y1 = 1 - yminfrac;
                double y2 = 1 - ymaxfrac;
                yminfrac = y2;
                ymaxfrac = y1;
            }
            if (axis->stringCheck(GO_Y_DIR_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_REVERSE_STR)) {
                double x1 = 1 - xminfrac;
                double x2 = 1 - xmaxfrac;
                xminfrac = x2;
                xmaxfrac = x1;
            }
            GOVectorProperty* hp
                = (GOVectorProperty*)axis->findProperty(GO_X_LIM_PROPERTY_NAME_STR);
            double range = (hp->data()[1] - hp->data()[0]);
            double mean = (hp->data()[1] + hp->data()[0]) / 2;
            axis->setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR,
                mean - range / 2 + xminfrac * range, mean - range / 2 + xmaxfrac * range);
            axis->setRestrictedStringDefault(
                GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);

            hp = (GOVectorProperty*)axis->findProperty(GO_Y_LIM_PROPERTY_NAME_STR);
            range = (hp->data()[1] - hp->data()[0]);
            mean = (hp->data()[1] + hp->data()[0]) / 2;
            axis->setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR,
                mean - range / 2 + yminfrac * range, mean - range / 2 + ymaxfrac * range);
            axis->setRestrictedStringDefault(
                GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
            axis->updateState();
        }
    }
    isZoomInRunning = false;
}
//=============================================================================
void
GOWindow::mousePressEventHandleZoomOutMode(QMouseEvent* e)
{
    int x, y;
    qtGetPosition(e, x, y);
    GOAxis* axis = findContainingAxis(goFig, remapX(x), remapY(y));
    if (axis) {
        GOVectorProperty* hp = (GOVectorProperty*)axis->findProperty(GO_X_LIM_PROPERTY_NAME_STR);
        double range = (hp->data()[1] - hp->data()[0]) / 2;
        double mean = (hp->data()[1] + hp->data()[0]) / 2;
        axis->setTwoVectorDefault(GO_X_LIM_PROPERTY_NAME_STR, mean - range * 2, mean + range * 2);
        axis->setRestrictedStringDefault(
            GO_X_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);

        hp = (GOVectorProperty*)axis->findProperty(GO_Y_LIM_PROPERTY_NAME_STR);
        range = (hp->data()[1] - hp->data()[0]) / 2;
        mean = (hp->data()[1] + hp->data()[0]) / 2;
        axis->setTwoVectorDefault(GO_Y_LIM_PROPERTY_NAME_STR, mean - range * 2, mean + range * 2);
        axis->setRestrictedStringDefault(
            GO_Y_LIM_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_MANUAL_STR);
        axis->updateState();
    }
    isZoomOutRunning = false;
}
//=============================================================================
GOAxis*
GOWindow::findContainingAxis(GOFigure* fig, int x, int y)
{
    GOGObjectsProperty* cp = (GOGObjectsProperty*)fig->findProperty(GO_CHILDREN_PROPERTY_NAME_STR);
    std::vector<int64> children(cp->data());
    for (int i = 0; i < children.size(); i++) {
        GraphicsObject* hp = findGraphicsObject(children[i]);
        if (hp->isType(GO_PROPERTY_VALUE_AXES_STR)) {
            std::vector<double> position(((GOAxis*)hp)->getClientAreaAsPixels());
            if (position.size() != 4) {
                return nullptr;
            }
            if ((x >= position[0]) && (x < (position[0] + position[2])) && (y >= position[1])
                && (y < (position[1] + position[3])))
                return (GOAxis*)hp;
        }
    }
    return nullptr;
}
//=============================================================================
int
GOWindow::remapX(int x)
{
    QRect plot_region(qtchild->geometry());
    return x - plot_region.x();
}
//=============================================================================
int
GOWindow::remapY(int y)
{
    QRect plot_region(qtchild->geometry());
    return (plot_region.height() - y + plot_region.y());
}
//=============================================================================
void
GOWindow::onNewFigureAction()
{
    setCursor(Qt::ArrowCursor);
    createNewFigure(true);
}
//=============================================================================
void
GOWindow::onPrintAction()
{
    QPrinter printer(QPrinter::ScreenResolution);
    printer.setPageSize(QPageSize(QPageSize::A4));
    printer.setPageOrientation(QPageLayout::Portrait);
    QPrintPreviewDialog* printPreview = nullptr;
    try {
        printPreview = new QPrintPreviewDialog(
            &printer, this, Qt::WindowCloseButtonHint | Qt::WindowMaximizeButtonHint);
    } catch (std::bad_alloc&) {
        printPreview = nullptr;
    }
    if (printPreview != nullptr) {
        /* save/set color */
        GOColorProperty* color
            = static_cast<GOColorProperty*>(goFig->findProperty(GO_COLOR_PROPERTY_NAME_STR));
        double cr = color->at(0);
        double cg = color->at(1);
        double cb = color->at(2);
        goFig->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, 1, 1, 1);
        goFig->updateState();

        connect(printPreview, &QPrintPreviewDialog::paintRequested, this, [=](QPrinter* printer) {
            if (!printer) {
                return;
            }

            QPainter painter;
            painter.begin(printer);
            const auto pageLayout = printer->pageLayout();
            const auto pageRect = pageLayout.paintRectPixels(printer->resolution());
            const auto paperRect = pageLayout.fullRectPixels(printer->resolution());
            double xscale = pageRect.width() / double(qtchild->width());
            double yscale = pageRect.height() / double(qtchild->height());
            double scale = qMin(xscale, yscale);
            painter.translate(
                pageRect.x() + paperRect.width() / 2., pageRect.y() + paperRect.height() / 2.);
            painter.scale(scale, scale);
            painter.translate(-qtchild->width() / 2., -qtchild->height() / 2.);
            RenderQt gc(&painter, 0, 0, qtchild->width(), qtchild->height(), L"PDF");
            goFig->paintMe(gc);
        });
        printPreview->resize(800, 600);
        printPreview->exec();
        delete printPreview;

        /* restore color */
        goFig->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, cr, cg, cb);
        goFig->updateState();
    }
}
//=============================================================================
void
GOWindow::onExportAction()
{
    callExportGraphicsGui(this);
}
//=============================================================================
void
GOWindow::onRotateAction()
{
    setCursor(Qt::ArrowCursor);
    if (mouseMode == MOUSE_MODE::ROTATION) {
        rotateAction->setChecked(false);
        mouseMode = MOUSE_MODE::NONE;
    } else {
        setCursor(rotateCursor);
        rotateAction->setChecked(true);
        mouseMode = MOUSE_MODE::ROTATION;
    }
    panAction->setChecked(false);
    zoomInAction->setChecked(false);
    zoomOutAction->setChecked(false);
}
//=============================================================================
void
GOWindow::onPanAction()
{
    isPanRunning = false;
    setCursor(Qt::ArrowCursor);
    if (mouseMode == MOUSE_MODE::PAN) {
        panAction->setChecked(false);
        mouseMode = MOUSE_MODE::NONE;
    } else {
        setCursor(Qt::OpenHandCursor);
        panAction->setChecked(true);
        mouseMode = MOUSE_MODE::PAN;
    }
    zoomInAction->setChecked(false);
    zoomOutAction->setChecked(false);
    rotateAction->setChecked(false);
}
//=============================================================================
void
GOWindow::onZoomInAction()
{
    setCursor(Qt::ArrowCursor);
    if (mouseMode == MOUSE_MODE::ZOOM_IN) {
        zoomInAction->setChecked(false);
        mouseMode = MOUSE_MODE::NONE;
    } else {
        zoomInAction->setChecked(true);
        setCursor(zoomInCursor);
        mouseMode = MOUSE_MODE::ZOOM_IN;
    }
    zoomOutAction->setChecked(false);
    rotateAction->setChecked(false);
    panAction->setChecked(false);
}
//=============================================================================
void
GOWindow::onZoomOutAction()
{
    setCursor(Qt::ArrowCursor);
    if (mouseMode == MOUSE_MODE::ZOOM_OUT) {
        zoomOutAction->setChecked(false);
        mouseMode = MOUSE_MODE::NONE;
    } else {
        zoomOutAction->setChecked(true);
        setCursor(zoomOutCursor);
        mouseMode = MOUSE_MODE::ZOOM_OUT;
    }
    rotateAction->setChecked(false);
    panAction->setChecked(false);
    zoomInAction->setChecked(false);
}
//=============================================================================
void
GOWindow::onRestoreViewAction()
{
    setCursor(Qt::ArrowCursor);
    GOGObjectsProperty* currentAxes
        = (GOGObjectsProperty*)goFig->findProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR, false);
    if (currentAxes) {
        std::vector<int64> handlesCurrentAxes(currentAxes->data());
        if (handlesCurrentAxes.size() == 1) {
            GraphicsObject* fp = findGraphicsObject(handlesCurrentAxes[0], false);
            if (fp) {
                GOAxis* axis = (GOAxis*)fp;
                axis->resetView();
            }
        }
    }
}
//=============================================================================
void
GOWindow::onHelpAction()
{
    postCommand(L"doc graphics");
}
//=============================================================================
void
GOWindow::qtGetPosition(QMouseEvent* e, int& x, int& y)
{
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    x = (int)e->position().x();
    y = (int)e->position().y();
#else
    x = (int)e->x();
    y = (int)e->y();
#endif
}
//=============================================================================
void
GOWindow::wheelEvent(QWheelEvent* event)
{
    double scaleFactor = 1.15;

    if (event->angleDelta().y() >= 0) {
        scaleFactor = 1.0 / scaleFactor;
    }

    QPointF mousePos = event->position();
    int click_x = (int)mousePos.x();
    int click_y = (int)mousePos.y();

    GOAxis* axis = findContainingAxis(goFig, remapX(click_x), remapY(click_y));
    if (axis) {
        axis->zoom(scaleFactor);
    }
    this->getGOFigure()->setRenderingStateInvalid(true);
}
//=============================================================================
void
GOWindow::keyReleaseEvent(QKeyEvent* event)
{
    event->accept();
#if QT_VERSION >= QT_VERSION_CHECK(6, 0, 0)
    lastKeyEvent = event->clone();
#else
    lastKeyEvent = new QKeyEvent(event->type(), event->key(), event->modifiers(), event->text(),
        event->isAutoRepeat(), event->count());
#endif
    this->getGOFigure()->setRenderingStateInvalid(true);
    if (keyPressed) {
        return;
    }
    handleKeyEvent(event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
}
//=============================================================================
void
GOWindow::keyPressEvent(QKeyEvent* event)
{
    event->accept();

    keyPressed = true;
    releaseKeyTimer.start(80);

    handleKeyEvent(event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    if (waitKeyOrMousePressedMode == WAIT_PRESS_MODE::WAIT) {
        waitKeyOrMousePressedMode = WAIT_PRESS_MODE::KEY;
        return;
    }

    GOAxis* axis = findContainingAxis(
        goFig, remapX(mousePositionOrigin.x()), remapY(mousePositionOrigin.y()));

    if (!axis) {
        return;
    }

    int step = 4;
    switch (mouseMode) {
    case MOUSE_MODE::ROTATION: {
        QPoint newPoint = mousePositionOrigin;
        if (event->key() == Qt::Key_Left) {
            newPoint.setX(newPoint.x() - step);
        } else if (event->key() == Qt::Key_Right) {
            newPoint.setX(newPoint.x() + step);
        } else if (event->key() == Qt::Key_Up) {
            newPoint.setY(newPoint.y() - step);
        } else if (event->key() == Qt::Key_Down) {
            newPoint.setY(newPoint.y() + step);
        }
        axis->rotateCamera(
            mousePositionOrigin.x(), newPoint.x(), mousePositionOrigin.y(), newPoint.y());
        axis->updateState();
    } break;
    case MOUSE_MODE::PAN: {
        double delx = 0.0;
        double dely = 0.0;

        if (event->key() == Qt::Key_Left) {
            delx = step;
        } else if (event->key() == Qt::Key_Right) {
            delx = -step;
        } else if (event->key() == Qt::Key_Up) {
            dely = -step;
        } else if (event->key() == Qt::Key_Down) {
            dely = step;
        }

        if (delx != 0.0 || dely != 0.0) {
            axis->pan(delx, dely);
            axis->updateState();
        }
    } break;
    case MOUSE_MODE::ZOOM_IN:
    case MOUSE_MODE::ZOOM_OUT: {
        double scaleFactor = 0.;
        if (event->key() == Qt::Key_Up) {
            scaleFactor = 1.0 / 1.15;
        }
        if (event->key() == Qt::Key_Down) {
            scaleFactor = 1.15;
        }
        if (scaleFactor != 0.) {
            axis->zoom(scaleFactor);
        }
    } break;
    default: {
    } break;
    }
}
//=============================================================================
void
GOWindow::onKeyReleaseTimeout()
{
    keyPressed = false;
    handleKeyEvent(lastKeyEvent, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
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
bool
GOWindow::handleKeyEvent(
    QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType)
{
    GOCallbackProperty* goCallback = (GOCallbackProperty*)goFig->findProperty(callbackPropertyStr);
    ArrayOf callbackArrayOf = goCallback->get();
    if (callbackArrayOf.isEmpty()) {
        return true;
    }

    QKeyEvent* keyEvent = static_cast<QKeyEvent*>(event);
    wstringVector modifiersWstringVector = getModifiers(keyEvent);
    std::wstring key = getKeyString(keyEvent);
    std::wstring character = getCharacterString(keyEvent);

    goCallback->pushKeyEvent(goFig, L"KeyData", eventType, character, key, modifiersWstringVector);
    return true;
}
//=============================================================================
bool
GOWindow::handleMouseEvent(
    QEvent* event, const std::wstring& callbackPropertyStr, const std::wstring& eventType)
{
    GOCallbackProperty* goCallback = (GOCallbackProperty*)goFig->findProperty(callbackPropertyStr);
    ArrayOf callbackArrayOf = goCallback->get();
    if (callbackArrayOf.isEmpty()) {
        return true;
    }
    goCallback->pushEvent(goFig, L"MouseData", eventType);
    return true;
}
//=============================================================================
wstringVector
GOWindow::getModifiers(QKeyEvent* keyEvent)
{
    wstringVector modifiersWstringVector;
    modifiersWstringVector.reserve(4);
    Qt::KeyboardModifiers modifiers = keyEvent->modifiers();
    if (modifiers & Qt::ShiftModifier) {
        modifiersWstringVector.push_back(L"shift");
    }
    if (modifiers & Qt::ControlModifier) {
        modifiersWstringVector.push_back(L"control");
    }
    if (modifiers & Qt::AltModifier) {
        modifiersWstringVector.push_back(L"alt");
    }
    if (modifiers & Qt::MetaModifier) {
        modifiersWstringVector.push_back(L"meta");
    }
    return modifiersWstringVector;
}
//=============================================================================
std::wstring
GOWindow::getKeyString(QKeyEvent* keyEvent)
{
    return QStringTowstring(QKeySequence(keyEvent->key()).toString());
}
//=============================================================================
std::wstring
GOWindow::getCharacterString(QKeyEvent* keyEvent)
{
    return QStringTowstring(keyEvent->text());
}
//=============================================================================
}
//=============================================================================
