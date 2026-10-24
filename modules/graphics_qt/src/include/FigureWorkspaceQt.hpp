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
#include <deque>
#include <memory>
#include <QtCore/QByteArray>
#include <QtCore/QJsonObject>
#include <QtCore/QJsonArray>
#include <QtCore/QObject>
#include <QtCore/QPointer>
#include <QtCore/QTimer>
#include <QtWidgets/QComboBox>
#include <QtWidgets/QDockWidget>
#include <QtWidgets/QLabel>
#include <QtWidgets/QTreeWidget>
#include "Types.hpp"
//=============================================================================
class QMouseEvent;
class QKeyEvent;
class QWheelEvent;
class QToolBar;
class QRubberBand;
class QSlider;
class QVBoxLayout;
class QLineEdit;
//=============================================================================
namespace Nelson {
class GOWindow;
class GOAxis;
class FigureWorkspaceQt : public QObject
{
public:
    explicit FigureWorkspaceQt(GOWindow* window);
    ~FigureWorkspaceQt() override;
    void
    attachCanvas(QWidget* canvas);
    void
    createTools(QToolBar* primary, QAction* rotate, QAction* pan);
    void
    action(QJsonObject request);
    void
    refresh();
    void
    selectAxes(int64 handle);
    int64
    selectedAxes() const;
    GOAxis*
    axisAt(const QPointF& position) const;
    void
    press(QMouseEvent* event);
    void
    move(QMouseEvent* event);
    void
    release();
    void
    doubleClick(QMouseEvent* event);
    void
    wheel(QWheelEvent* event);
    bool
    shortcut(QKeyEvent* event);
    void
    hover(const QPointF& windowPosition);
    void
    syncVisibility();
    void
    saveAs();

protected:
    bool
    eventFilter(QObject* object, QEvent* event) override;

private:
    QPointer<GOWindow> window;
    QPointer<QWidget> canvas;
    QDockWidget* inspector = nullptr;
    QTreeWidget* properties = nullptr;
    QTreeWidget* browser = nullptr;
    QLineEdit* propertyFilter = nullptr;
    QComboBox* axesSelector = nullptr;
    QToolBar* navigation = nullptr;
    QToolBar* legend = nullptr;
    QSlider* azimuthSlider = nullptr;
    QSlider* elevationSlider = nullptr;
    QLabel* azimuthValue = nullptr;
    QLabel* elevationValue = nullptr;
    QLabel* details = nullptr;
    QLabel* errorLabel = nullptr;
    QWidget* indicator = nullptr;
    QWidget* pointMarker = nullptr;
    QLabel* pointTip = nullptr;
    QString historyGroup;
    QTimer preferencesTimer;
    QRubberBand* zoomRectangle = nullptr;
    QTimer refreshTimer;
    QTimer motionTimer;
    QJsonObject state;
    QJsonObject inspectedPoint;
    QJsonArray legendObjects;
    std::deque<QJsonObject> commands;
    std::shared_ptr<std::atomic_bool> alive = std::make_shared<std::atomic_bool>(true);
    bool running = false;
    bool preferencesReady = false;
    int preferredInspectorWidth = 320;
    bool updating = false;
    bool propertiesDirty = true;
    bool navigationRequested = false;
    bool detailsRequested = false;
    int64 selected = 0;
    int64 selectedObject = 0;
    int64 inspectObject = -1;
    QJsonObject inspectorModel;
    QByteArray inspectorModelKey;
    QByteArray browserSignature;
    QByteArray structureSig;
    int64 legendAxes = 0;
    int64 dragAxis = 0;
    int64 hoverAxis = 0;
    int dragMode = 0;
    bool rotationHistoryPending = false;
    QPointF lastPointer;
    QPointF pendingPointer;
    QPointF dragOrigin;
    QSizeF dragSize;
    QJsonArray initialView;
    QRectF dragBounds;
    QJsonObject rectangleState;
    void
    dispatch();
    void
    accept(const QJsonObject& result);
    void
    renderProperties();
    void
    updateDetails();
    void
    createViewControls(QVBoxLayout* layout);
    void
    createPropertyEditor(QWidget* parent, QVBoxLayout* layout);
    void
    updateViewControls();
    void
    createEnhancements(QToolBar* primary);
    void
    restorePreferences();
    void
    savePreferences();
    void
    restoreInspectorWidth();
    void
    schedulePreferences();
    void
    createLegend();
    void
    updateLegend();
    void
    updatePointMarker();
    void
    selectObjectInProperties();
    void
    commitProperty(QTreeWidgetItem* item);
    void
    populateBrowser(const QJsonObject& root);
    void
    requestInspectModel(int64 handle);
    void
    renderObjectModel();
    void
    syncObjectValues(const QJsonObject& values);
    void
    applyPropertyFilter(const QString& text);
    int64
    currentInspectObject() const;
    void
    flushMotion();
    void
    finishRectangle(int64 handle);
    void
    showIndicator(int64 handle);
    QJsonObject
    axesState() const;
};
//=============================================================================
} // namespace Nelson
//=============================================================================
