//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "FigureWorkspaceQt.hpp"
#include "FigureWorkspaceHelpers.hpp"
#include "GOWindow.hpp"
#include "GOAxis.hpp"
#include "GOFiguresManager.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "Evaluator.hpp"
#include "Interface.hpp"
#include "FunctionDef.hpp"
#include "MainThreadTaskQueue.hpp"
#include "NelsonException.hpp"
#include "RuntimeDispatch.hpp"
#include "WebFigureBridgeExports.hpp"
#include "characters_encoding.hpp"
#include <QtCore/QJsonDocument>
#include <QtCore/QSignalBlocker>
#include <QtWidgets/QApplication>
#include <QtWidgets/QFileDialog>
#include <QtWidgets/QFrame>
#include <QtWidgets/QRubberBand>
#include <QtWidgets/QStatusBar>
#include <stdexcept>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    QJsonObject
    executeFigureCommand(Evaluator* evaluator, int64 handle, QJsonObject request)
    {
        // Interactive rotation frames bypass the interpreted __web_figure_action__ macro:
        // setting the axes View in C++ and flushing one repaint is about 90x cheaper per
        // frame than the macro's history capture and state serialization (Stanford bunny:
        // ~1.0 ms vs ~0.01 ms). The first and last frames of the gesture still carry a
        // historyGroup and go through the macro, so undo/redo and inspector state stay
        // correct.
        if (request["action"] == "view" && request["interactive"].toBool()) {
            const int64 axesHandle = static_cast<int64>(request["axesHandle"].toDouble());
            const double azimuth = request["azimuth"].toDouble();
            const double elevation = request["elevation"].toDouble();
            if (nlsWebGuiSetAxesView(handle, axesHandle, azimuth, elevation)) {
                FunctionDefPtr draw = nullptr;
                if (evaluator->lookupFunction("drawnow", draw) && draw) {
                    draw->evaluateFunction(evaluator, ArrayOfVector(), 0);
                }
            }
            return { { "viewOnly", true } };
        }
        if (request["action"] == "state") {
            FunctionDefPtr snapshot = nullptr;
            FunctionDefPtr encode = nullptr;
            if (!evaluator->lookupFunction("__web_figure_state_full__", snapshot) || !snapshot
                || !evaluator->lookupFunction("jsonencode", encode) || !encode) {
                throw std::runtime_error("Figure properties are unavailable.");
            }
            ArrayOfVector inputs;
            inputs << ArrayOf::doubleScalar(static_cast<double>(handle));
            inputs << ArrayOf::doubleScalar(
                request.contains("inspectHandle") ? request["inspectHandle"].toDouble() : -1.0);
            const auto state = snapshot->evaluateFunction(evaluator, inputs, 1);
            const auto encoded = encode->evaluateFunction(evaluator, state, 1);
            return { { "state",
                QJsonDocument::fromJson(
                    QByteArray::fromStdString(wstring_to_utf8(encoded[0].asWideString())))
                    .object() } };
        }
        if (request["action"] == "inspect") {
            FunctionDefPtr inspectFn = nullptr;
            FunctionDefPtr encode = nullptr;
            if (!evaluator->lookupFunction("__web_figure_inspect__", inspectFn) || !inspectFn
                || !evaluator->lookupFunction("jsonencode", encode) || !encode) {
                throw std::runtime_error("Figure inspector is unavailable.");
            }
            ArrayOfVector inputs;
            inputs << ArrayOf::doubleScalar(static_cast<double>(handle));
            inputs << ArrayOf::doubleScalar(request["objectHandle"].toDouble());
            const auto payload = inspectFn->evaluateFunction(evaluator, inputs, 1);
            const auto encoded = encode->evaluateFunction(evaluator, payload, 1);
            return QJsonDocument::fromJson(
                QByteArray::fromStdString(wstring_to_utf8(encoded[0].asWideString())))
                .object();
        }
        if (request["action"] == "save") {
            FunctionDefPtr save = nullptr;
            if (!evaluator->lookupFunction("saveas", save) || !save) {
                throw std::runtime_error("Figure saving is unavailable.");
            }
            ArrayOfVector inputs;
            inputs << graphicsObjectHandle(handle);
            inputs << ArrayOf::characterArray(request["path"].toString().toStdWString());
            save->evaluateFunction(evaluator, inputs, 0);
            request = { { "action", "state" } };
        }
        FunctionDefPtr function = nullptr;
        if (!evaluator->lookupFunction("__web_figure_action__", function) || !function) {
            throw std::runtime_error("Figure actions are unavailable.");
        }
        ArrayOfVector inputs;
        inputs << ArrayOf::doubleScalar(static_cast<double>(handle));
        inputs << ArrayOf::characterArray(
            utf8_to_wstring(QJsonDocument(request).toJson(QJsonDocument::Compact).toStdString()));
        inputs << ArrayOf::logicalScalar(false);
        const auto outputs = function->evaluateFunction(evaluator, inputs, 1);
        return outputs.empty()
            ? QJsonObject {}
            : QJsonDocument::fromJson(
                  QByteArray::fromStdString(wstring_to_utf8(outputs[0].asWideString())))
                  .object();
    }
    //=============================================================================
} // namespace
//=============================================================================
FigureWorkspaceQt::FigureWorkspaceQt(GOWindow* owner) : QObject(owner), window(owner)
{
    refreshTimer.setParent(this);
    refreshTimer.setObjectName("figureRefreshTimer");
    refreshTimer.setInterval(250);
    connect(&refreshTimer, &QTimer::timeout, this, [this]() {
        const bool requested = refreshTimer.property("userRequested").toBool();
        const bool observing = inspector
            && (inspector->isVisible() || (navigation && navigation->isVisible())
                || detailsRequested);
        if ((requested || observing) && window && window->isVisible() && !running
            && commands.empty()) {
            action({ { "action", "state" }, { "idleOnly", !requested } });
        }
    });
    refreshTimer.start();
    motionTimer.setSingleShot(true);
    motionTimer.setParent(this);
    motionTimer.setObjectName("figureMotionTimer");
    motionTimer.setInterval(16);
    connect(&motionTimer, &QTimer::timeout, this, &FigureWorkspaceQt::flushMotion);
    indicator = new QFrame(owner);
    indicator->setObjectName("figureAxesIndicator");
    indicator->setAttribute(Qt::WA_TransparentForMouseEvents);
    indicator->setStyleSheet(
        "#figureAxesIndicator { border: 2px solid #368adc; background: transparent; }");
    indicator->hide();
    pointMarker = new QFrame(owner);
    pointMarker->setObjectName("figurePointMarker");
    pointMarker->setAttribute(Qt::WA_TransparentForMouseEvents);
    pointMarker->setFixedSize(12, 12);
    pointMarker->setStyleSheet("#figurePointMarker { border: 2px solid #1976d2;"
                               " border-radius: 6px; background: white; }");
    pointMarker->hide();
    pointTip = new QLabel(owner);
    pointTip->setObjectName("figurePointTip");
    pointTip->setTextFormat(Qt::PlainText);
    pointTip->setWordWrap(true);
    pointTip->setAttribute(Qt::WA_TransparentForMouseEvents);
    pointTip->setStyleSheet(
        "#figurePointTip { background: palette(toolTipBase);"
        "color: palette(toolTipText); border: 1px solid #808080; padding: 5px; }");
    pointTip->hide();
    zoomRectangle = new QRubberBand(QRubberBand::Rectangle, owner);
    attachCanvas(owner->qtchild);
    owner->installEventFilter(this);
}
//=============================================================================
FigureWorkspaceQt::~FigureWorkspaceQt()
{
    alive->store(false);
    commands.clear();
    delete indicator;
    delete pointMarker;
    delete pointTip;
    delete zoomRectangle;
}
//=============================================================================
void
FigureWorkspaceQt::attachCanvas(QWidget* next)
{
    canvas = next;
    if (canvas) {
        canvas->setMouseTracking(true);
        canvas->installEventFilter(this);
    }
}
//=============================================================================
void
FigureWorkspaceQt::refresh()
{
    if (!window || !window->isVisible() || running || !commands.empty()) {
        return;
    }
    action({ { "action", "state" } });
}
//=============================================================================
void
FigureWorkspaceQt::action(QJsonObject request)
{
    const QString kind = request["action"].toString();
    if (kind != "state" && kind != "pick" && kind != "step" && kind != "inspect") {
        inspectedPoint = {};
        pointMarker->hide();
        pointTip->hide();
    }
    if (kind != "pan" && kind != "view" && kind != "state" && kind != "select"
        && kind != "inspect") {
        release();
    }
    if (kind != "state" && !request.contains("axesHandle") && selectedAxes() > 0) {
        request["axesHandle"] = static_cast<double>(selectedAxes());
    }
    if (kind == "state" && !request.contains("inspectHandle")) {
        const bool inspecting = inspector && inspector->isVisible() && inspectObject >= 0;
        request["inspectHandle"] = static_cast<double>(inspecting ? inspectObject : -1);
    }
    enqueueFigureCommand(commands, request);
    dispatch();
}
//=============================================================================
void
FigureWorkspaceQt::dispatch()
{
    if (running || commands.empty() || !window) {
        return;
    }
    running = true;
    const QJsonObject request = commands.front();
    commands.pop_front();
    const auto kind = request["action"].toString();
    const bool background = kind == "state";
    errorLabel->setProperty("pending", true);
    if (!background) {
        errorLabel->setText(tr("Pending…"));
    }
    if (kind == "nativePrint" || kind == "nativeExport") {
        QPointer<FigureWorkspaceQt> guard(this);
        if (kind == "nativePrint") {
            window->printFigure();
        } else {
            window->exportFigure();
        }
        if (guard) {
            running = false;
            errorLabel->setProperty("pending", false);
            errorLabel->clear();
            dispatch();
        }
        return;
    }
    const int64 figureHandle = window->getHandle();
    GOFigure* expected = window->getGOFigure();
    const auto lifetime = alive;
    QPointer<FigureWorkspaceQt> guard(this);
    MainThreadTaskQueue::getInstance()->postToInterpreterThread(
        [guard, lifetime, figureHandle, expected, request, background](Evaluator* evaluator) {
            if (!lifetime->load() || !evaluator || findGOFigure(figureHandle) != expected) {
                return;
            }
            if (request["idleOnly"].toBool()
                && (evaluator->callstack.size() > 0
                    || (evaluator->getInterface() && !evaluator->getInterface()->isAtPrompt()))) {
                postToGui([guard]() {
                    if (guard) {
                        guard->running = false;
                        guard->errorLabel->setProperty("pending", false);
                        guard->dispatch();
                    }
                });
                return;
            }
            QJsonObject result;
            QString failure;
            try {
                result = executeFigureCommand(evaluator, figureHandle, request);
            } catch (const NelsonException& exception) {
                failure = QString::fromStdString(wstring_to_utf8(exception.what()));
            } catch (const std::exception& exception) {
                failure = QString::fromUtf8(exception.what());
            }
            postToGui([guard, lifetime, result, failure, background]() {
                if (!lifetime->load() || !guard) {
                    return;
                }
                guard->running = false;
                guard->errorLabel->setProperty("pending", false);
                if (!background || !failure.isEmpty()) {
                    guard->errorLabel->setText(failure);
                }
                if (!failure.isEmpty() && guard->window) {
                    guard->window->statusBar()->showMessage(failure, 8000);
                    QTimer::singleShot(0, guard, [guard]() {
                        if (guard) {
                            guard->renderProperties();
                        }
                    });
                }
                guard->accept(result);
                guard->dispatch();
            });
        });
}
//=============================================================================
int64
FigureWorkspaceQt::selectedAxes() const
{
    if (selected > 0 && !isDeletedGraphicsObject(selected)) {
        return selected;
    }
    return window ? window->goFig->findGoProperty(GO_CURRENT_AXES_PROPERTY_NAME_STR) : 0;
}
//=============================================================================
void
FigureWorkspaceQt::selectAxes(int64 next)
{
    if (next <= 0 || selected == next) {
        return;
    }
    selected = next;
    selectedObject = 0;
    action({ { "action", "select" }, { "axesHandle", static_cast<double>(next) } });
    if (inspector && inspector->isVisible()) {
        requestInspectModel(next);
    }
}
//=============================================================================
QJsonObject
FigureWorkspaceQt::axesState() const
{
    for (const auto& value : figureObjectArray(state["axes"])) {
        const auto axis = value.toObject();
        if (static_cast<int64>(axis["handle"].toDouble()) == selectedAxes()) {
            return axis;
        }
    }
    return {};
}
//=============================================================================
void
FigureWorkspaceQt::accept(const QJsonObject& result)
{
    if (result.contains("tree")) {
        populateBrowser(result["tree"].toObject());
    }
    if (result.contains("model")) {
        inspectorModel = result["model"].toObject();
        renderObjectModel();
    }
    if (result.contains("state")) {
        const auto next = result["state"].toObject();
        // The inspector channel (target/close) and the per-object inspectorValues
        // are compared out of 'changed' so inspector-only updates never clear the
        // data tip or rebuild the figure view.
        QJsonObject prevCore = state;
        QJsonObject nextCore = next;
        for (const char* key : { "inspector", "inspectorValues" }) {
            prevCore.remove(key);
            nextCore.remove(key);
        }
        const bool changed = prevCore != nextCore;
        const auto inspectorState = next["inspector"].toObject();
        if (inspectorState["close"].toBool() && inspector && inspector->isVisible()) {
            inspector->hide();
        }
        const int64 inspectTarget = inspectorState.contains("target")
            ? static_cast<int64>(inspectorState["target"].toDouble())
            : -1;
        if (inspectTarget >= 0) {
            if (inspector && !inspector->isVisible()) {
                inspector->show();
            }
            requestInspectModel(inspectTarget);
        }
        if (next.contains("inspectorValues")) {
            syncObjectValues(next["inspectorValues"].toObject());
        }
        if (!changed && !propertiesDirty && !result.contains("point")) {
            updatePointMarker();
            updateDetails();
            return;
        }
        if (changed) {
            inspectedPoint = {};
        }
        state = next;
        if (commands.empty() && next["currentAxes"].toDouble() > 0) {
            selected = static_cast<int64>(next["currentAxes"].toDouble());
        }
        const QSignalBlocker blocker(axesSelector);
        const auto axes = figureObjectArray(state["axes"]);
        bool rebuildSelector = axesSelector->count() != axes.size();
        for (int index = 0; !rebuildSelector && index < axes.size(); ++index) {
            rebuildSelector = axesSelector->itemData(index).toLongLong()
                != static_cast<int64>(axes[index].toObject()["handle"].toDouble());
        }
        if (rebuildSelector) {
            axesSelector->clear();
        }
        bool found = false;
        for (int index = 0; index < axes.size(); ++index) {
            const auto axis = axes[index].toObject();
            const auto handle = static_cast<int64>(axis["handle"].toDouble());
            QString title = axis["title"].toString();
            const auto label = title.isEmpty() ? tr("Axes %1").arg(index + 1) : title;
            if (rebuildSelector) {
                axesSelector->addItem(label, QVariant::fromValue<qlonglong>(handle));
            } else if (axesSelector->itemText(index) != label) {
                axesSelector->setItemText(index, label);
            }
            if (handle == selectedAxes()) {
                axesSelector->setCurrentIndex(index);
                found = true;
            }
        }
        if (!found) {
            selected
                = axes.empty() ? 0 : static_cast<int64>(axes[0].toObject()["handle"].toDouble());
            axesSelector->setCurrentIndex(0);
        }
        if (changed || propertiesDirty) {
            renderProperties();
        }
        auto* floor = window->findChild<QAction*>("figureFloorAction");
        if (floor) {
            const QSignalBlocker blocker(floor);
            floor->setChecked(axesState()["projectionFloor"].toBool());
        }
    }
    if (result.contains("point") && commands.empty()) {
        inspectedPoint = result["point"].toObject();
        if (inspectedPoint["found"].toBool()) {
            selected = static_cast<int64>(inspectedPoint["axesHandle"].toDouble());
            selectedObject = static_cast<int64>(inspectedPoint["objectHandle"].toDouble());
            selectObjectInProperties();
        }
    }
    updateViewControls();
    updateLegend();
    updatePointMarker();
    updateDetails();
    for (const auto& kind : { QString("undo"), QString("redo") }) {
        if (auto* control = window->findChild<QAction*>("figure" + kind + "Action")) {
            control->setEnabled(state[kind == "undo" ? "canUndo" : "canRedo"].toBool());
        }
    }
}
//=============================================================================
void
FigureWorkspaceQt::saveAs()
{
    const QString path = QFileDialog::getSaveFileName(
        window, tr("Save figure image"), QString(), tr("PNG (*.png);;SVG (*.svg);;PDF (*.pdf)"));
    if (!path.isEmpty()) {
        action({ { "action", "save" }, { "path", path } });
    }
}
//=============================================================================
} // namespace Nelson
//=============================================================================
