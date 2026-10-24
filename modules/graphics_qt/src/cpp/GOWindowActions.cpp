//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "GOWindow.hpp"
#include "FigureWorkspaceQt.hpp"
#include "QtTranslation.hpp"
#include "CallExportGraphicsGui.hpp"
#include "GOColorProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "PostCommand.hpp"
#include "RenderQt.hpp"
#include <QtGui/QIcon>
#include <QtGui/QPainter>
#include <QtPrintSupport/QPrintPreviewDialog>
//=============================================================================
namespace Nelson {
void
GOWindow::createActions()
{
    this->closeAction = new QAction(sharedIcon(L"/resources/exit.svg"), TR("&Close"), this);
    connect(closeAction, SIGNAL(triggered()), this, SLOT(close()));

    newFigureAction
        = new QAction(sharedIcon(L"/resources/document-new.svg"), TR("&New Figure"), this);
    connect(newFigureAction, &QAction::triggered, this, &GOWindow::onNewFigureAction);

    printAction = new QAction(sharedIcon(L"/resources/document-print.svg"), TR("&Print"), this);
    printAction->setObjectName("figurePrintAction");
    connect(printAction, &QAction::triggered, this, &GOWindow::onPrintAction);

    exportAction
        = new QAction(sharedIcon(L"/resources/document-save-as.svg"), TR("&Export to ..."), this);
    exportAction->setObjectName("figureExportAction");
    connect(exportAction, &QAction::triggered, this, &GOWindow::onExportAction);

    rotateAction = new QAction(sharedIcon(L"/resources/rotate.svg"), TR("&Rotate"), this);
    connect(rotateAction, &QAction::triggered, this, &GOWindow::onRotateAction);
    rotateAction->setObjectName("figureRotateAction");
    rotateAction->setCheckable(true);

    panAction = new QAction(sharedIcon(L"/resources/action-pan.svg"), TR("&Pan"), this);
    connect(panAction, &QAction::triggered, this, &GOWindow::onPanAction);
    panAction->setObjectName("figurePanAction");
    panAction->setCheckable(true);

    zoomInAction = new QAction(sharedIcon(L"/resources/zoom-in.svg"), TR("Zoom &In"), this);
    connect(zoomInAction, &QAction::triggered, this, &GOWindow::onZoomInAction);
    zoomInAction->setCheckable(true);

    zoomOutAction = new QAction(sharedIcon(L"/resources/zoom-out.svg"), TR("Zoom &Out"), this);
    connect(zoomOutAction, &QAction::triggered, this, &GOWindow::onZoomOutAction);
    zoomOutAction->setCheckable(true);

    restoreViewAction
        = new QAction(sharedIcon(L"/resources/refresh.svg"), TR("&Restore View"), this);
    connect(restoreViewAction, &QAction::triggered, this, &GOWindow::onRestoreViewAction);

    helpAction = new QAction(sharedIcon(L"/resources/help-icon.svg"), TR("&Graphics help"), this);
    connect(helpAction, &QAction::triggered, this, &GOWindow::onHelpAction);

    dockAction = new QAction(sharedIcon(L"/resources/dock-window.svg"), TR("&Dock"), this);
    dockAction->setObjectName("figureDockAction");
    connect(dockAction, &QAction::triggered, this, [this]() { toggleDockedInDesktop(); });
    updateDockActionState();
}
//=============================================================================
void
GOWindow::createToolbars()
{
    toolBar = addToolBar(TR("Graphics tools"));
    toolBar->setObjectName("figureToolbar");
    toolBar->setIconSize(QSize(18, 18));
    toolBar->addAction(newFigureAction);
    toolBar->addAction(helpAction);
    toolBar->addAction(dockAction);
    workspace->createTools(toolBar, rotateAction, panAction);
}
//=============================================================================
void
GOWindow::onNewFigureAction()
{
    setCursor(Qt::ArrowCursor);
    postCommand(L"figure()");
}
//=============================================================================
void
GOWindow::onPrintAction()
{
    workspace->action({ { "action", "nativePrint" } });
}
//=============================================================================
void
GOWindow::printFigure()
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

        goFig->setThreeVectorDefault(GO_COLOR_PROPERTY_NAME_STR, cr, cg, cb);
        goFig->updateState();
    }
}
//=============================================================================
void
GOWindow::onExportAction()
{
    workspace->action({ { "action", "nativeExport" } });
}
//=============================================================================
void
GOWindow::exportFigure()
{
    callExportGraphicsGui(this);
}
//=============================================================================
void
GOWindow::onRotateAction()
{
    if (workspace) {
        workspace->release();
    }
    if (mouseMode == MOUSE_MODE::ROTATION) {
        rotateAction->setChecked(false);
        mouseMode = MOUSE_MODE::NONE;
        applyFigurePointerCursor();
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
    if (workspace) {
        workspace->release();
    }
    if (mouseMode == MOUSE_MODE::PAN) {
        panAction->setChecked(false);
        mouseMode = MOUSE_MODE::NONE;
        applyFigurePointerCursor();
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
    if (workspace) {
        workspace->release();
    }
    if (mouseMode == MOUSE_MODE::ZOOM_IN) {
        zoomInAction->setChecked(false);
        mouseMode = MOUSE_MODE::NONE;
        applyFigurePointerCursor();
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
    if (workspace) {
        workspace->release();
    }
    if (mouseMode == MOUSE_MODE::ZOOM_OUT) {
        zoomOutAction->setChecked(false);
        mouseMode = MOUSE_MODE::NONE;
        applyFigurePointerCursor();
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
    if (workspace) {
        workspace->action({ { "action", "restore" } });
    }
}
//=============================================================================
void
GOWindow::onHelpAction()
{
    postCommand(L"doc graphics");
}
//=============================================================================
} // namespace Nelson
//=============================================================================
