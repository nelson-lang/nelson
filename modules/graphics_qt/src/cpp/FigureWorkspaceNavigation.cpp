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
#include "GOWindow.hpp"
#include "GOAxis.hpp"
#include "GOFiguresManager.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOHelpers.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include <QtCore/QJsonArray>
#include <QtCore/QUuid>
#include <QtGui/QKeyEvent>
#include <QtGui/QMouseEvent>
#include <QtGui/QWheelEvent>
#include <QtWidgets/QApplication>
#include <QtWidgets/QAbstractSpinBox>
#include <QtWidgets/QLineEdit>
#include <QtWidgets/QPlainTextEdit>
#include <QtWidgets/QTextEdit>
#include <QtWidgets/QRubberBand>
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    GOAxis*
    axisAtPixel(GraphicsObject* object, double pixelX, double pixelY)
    {
        if (auto* axis = dynamic_cast<GOAxis*>(object)) {
            if (axis->isPolarAxes()) {
                return nullptr;
            }
            const auto box = axis->getClientAreaAsPixels();
            return box.size() == 4 && box[2] > 0 && box[3] > 0 && pixelX >= box[0]
                    && pixelX < box[0] + box[2] && pixelY >= box[1] && pixelY < box[1] + box[3]
                ? axis
                : nullptr;
        }
        auto* children = dynamic_cast<GOGObjectsProperty*>(
            object->findProperty(GO_CHILDREN_PROPERTY_NAME_STR, false));
        if (children) {
            const auto handles = children->data();
            for (auto handle = handles.rbegin(); handle != handles.rend(); ++handle) {
                auto* child = findGraphicsObject(*handle, false);
                if (child) {
                    if (auto* axis = axisAtPixel(child, pixelX, pixelY)) {
                        return axis;
                    }
                }
            }
        }
        return nullptr;
    }
    //=============================================================================
    // disableDefaultInteractivity turns off the modeless drag-pan/rotate and scroll zoom.
    bool
    defaultInteractivityEnabled(GOAxis* axis)
    {
        return axis != nullptr
            && !axis->stringCheck(GO_DEFAULT_INTERACTIVITY_INTERNAL_PROPERTY_NAME_STR,
                GO_PROPERTY_VALUE_OFF_STR);
    }
    //=============================================================================
} // namespace
//=============================================================================
GOAxis*
FigureWorkspaceQt::axisAt(const QPointF& position) const
{
    if (!canvas || !window || !canvas->geometry().contains(position.toPoint())) {
        return nullptr;
    }
    const auto local = position - canvas->geometry().topLeft();
    auto* control = canvas->childAt(local.toPoint());
    if (control && !control->testAttribute(Qt::WA_TransparentForMouseEvents)) {
        return nullptr;
    }
    const double ratio = canvas->devicePixelRatioF();
    return axisAtPixel(window->goFig, local.x() * ratio, (canvas->height() - local.y()) * ratio);
}
//=============================================================================
void
FigureWorkspaceQt::press(QMouseEvent* event)
{
    release();
    auto* axis = axisAt(event->position());
    if (!axis || event->button() != Qt::LeftButton) {
        return;
    }
    // Modeless press (pan/rotate/data-tip) is suppressed when default interactivity is off;
    // the toolbar pan/zoom/rotate modes keep working.
    if (window->mouseMode == MOUSE_MODE::NONE && !defaultInteractivityEnabled(axis)) {
        return;
    }
    dragAxis = findGraphicsObjectHandle(axis);
    historyGroup = QUuid::createUuid().toString(QUuid::WithoutBraces);
    rotationHistoryPending = true;
    selectAxes(dragAxis);
    showIndicator(dragAxis);
    lastPointer = pendingPointer = dragOrigin = event->position();
    const auto box = axis->getClientAreaAsPixels();
    dragSize = QSizeF(box[2] / canvas->devicePixelRatioF(), box[3] / canvas->devicePixelRatioF());
    dragMode = static_cast<int>(window->mouseMode);
    double azimuth = 0, elevation = 0;
    axis->getView(azimuth, elevation);
    initialView = { azimuth, elevation };
    if (window->mouseMode == MOUSE_MODE::ZOOM_IN) {
        const double ratio = canvas->devicePixelRatioF();
        const auto origin = canvas->geometry().topLeft();
        dragBounds = QRectF(origin.x() + box[0] / ratio,
            origin.y() + canvas->height() - (box[1] + box[3]) / ratio, box[2] / ratio,
            box[3] / ratio);
        rectangleState = {};
        for (const auto& dimension : { QString("X"), QString("Y") }) {
            const auto limits = axis->findVectorDoubleProperty((dimension + "Lim").toStdWString());
            rectangleState[dimension + "Lim"] = QJsonArray { limits[0], limits[1] };
            rectangleState[dimension + "Scale"] = QString::fromStdWString(
                axis->findStringProperty((dimension + "Scale").toStdWString()));
            rectangleState[dimension + "Dir"] = QString::fromStdWString(
                axis->findStringProperty((dimension + "Dir").toStdWString()));
        }
        // Shift/Alt makes a click zoom out; a 3D axes always point-zooms (camera).
        rectangleState["zoomOut"]
            = (event->modifiers() & (Qt::ShiftModifier | Qt::AltModifier)) != 0;
        rectangleState["is2D"] = axis->is2D();
        zoomRectangle->setGeometry(QRect(event->position().toPoint(), QSize()));
        zoomRectangle->show();
    } else if (window->mouseMode == MOUSE_MODE::ZOOM_OUT) {
        dragAxis = 0;
        action({ { "action", "zoom" }, { "factor", 0.5 } });
    }
    // MOUSE_MODE::NONE is modeless: a click picks a data point while a drag pans
    // (2D) or rotates (3D). The choice is deferred to move()/release() so a plain
    // click still inspects without first selecting a tool.
}
//=============================================================================
void
FigureWorkspaceQt::move(QMouseEvent* event)
{
    if (!dragAxis) {
        hover(event->position());
        return;
    }
    pendingPointer = event->position();
    if (dragMode == static_cast<int>(MOUSE_MODE::ZOOM_IN)) {
        zoomRectangle->setGeometry(QRectF(dragOrigin, pendingPointer).normalized().toAlignedRect());
        return;
    }
    if (dragMode == static_cast<int>(MOUSE_MODE::NONE)) {
        // Promote a modeless press to pan (2D) or rotate (3D) once it becomes a
        // drag; a shorter movement stays a click (data-tip pick on release).
        if ((pendingPointer - dragOrigin).manhattanLength() < 4) {
            return;
        }
        auto* axis = dynamic_cast<GOAxis*>(findGraphicsObject(dragAxis, false));
        dragMode = static_cast<int>(axis && axis->is2D() ? MOUSE_MODE::PAN : MOUSE_MODE::ROTATION);
    }
    if (!motionTimer.isActive()) {
        motionTimer.start();
    }
}
//=============================================================================
void
FigureWorkspaceQt::flushMotion()
{
    motionTimer.stop();
    if (!dragAxis || pendingPointer == lastPointer || !window) {
        return;
    }
    const auto delta = pendingPointer - lastPointer;
    lastPointer = pendingPointer;
    const double handle = static_cast<double>(dragAxis);
    if (dragMode == static_cast<int>(MOUSE_MODE::PAN) && dragSize.width() > 0
        && dragSize.height() > 0) {
        action({ { "action", "pan" }, { "axesHandle", handle }, { "historyGroup", historyGroup },
            { "dxFrac", delta.x() / dragSize.width() },
            { "dyFrac", delta.y() / dragSize.height() } });
    } else if (dragMode == static_cast<int>(MOUSE_MODE::ROTATION)) {
        const auto distance = pendingPointer - dragOrigin;
        QJsonObject request { { "action", "view" }, { "axesHandle", handle },
            { "azimuth", initialView[0].toDouble() - distance.x() * 0.5 },
            { "elevation",
                std::clamp(initialView[1].toDouble() + distance.y() * 0.5, -90.0, 90.0) } };
        if (rotationHistoryPending) {
            // First frame of the gesture: record the pre-drag view so undo can
            // restore it, and refresh the full inspector state once.
            request["historyGroup"] = historyGroup;
            rotationHistoryPending = false;
        } else {
            // Intermediate frames take the lean C++ fast path (no macro, no
            // history, no state serialization). release() sends the final frame.
            request["interactive"] = true;
        }
        action(request);
    }
}
//=============================================================================
void
FigureWorkspaceQt::release()
{
    flushMotion();
    const auto handle = dragAxis;
    const bool rectangle = dragAxis && dragMode == static_cast<int>(MOUSE_MODE::ZOOM_IN);
    const bool modelessClick = dragAxis && dragMode == static_cast<int>(MOUSE_MODE::NONE);
    if (dragAxis && dragMode == static_cast<int>(MOUSE_MODE::ROTATION) && !rotationHistoryPending) {
        // Intermediate frames skipped history and the state refresh. Send one final view
        // through the macro so undo captures the whole gesture and the inspector shows the
        // exact final view.
        const auto distance = pendingPointer - dragOrigin;
        action({ { "action", "view" }, { "axesHandle", static_cast<double>(dragAxis) },
            { "historyGroup", historyGroup },
            { "azimuth", initialView[0].toDouble() - distance.x() * 0.5 },
            { "elevation",
                std::clamp(initialView[1].toDouble() + distance.y() * 0.5, -90.0, 90.0) } });
    }
    rotationHistoryPending = false;
    dragAxis = 0;
    dragMode = 0;
    zoomRectangle->hide();
    if (rectangle) {
        finishRectangle(handle);
    } else if (modelessClick && window && canvas) {
        const auto point = dragOrigin - canvas->geometry().topLeft();
        action({ { "action", "pick" }, { "x", point.x() }, { "y", canvas->height() - point.y() } });
    }
}
//=============================================================================
void
FigureWorkspaceQt::finishRectangle(int64 handle)
{
    const bool zoomOut = rectangleState["zoomOut"].toBool();
    const bool twoD = rectangleState["is2D"].toBool();
    if ((pendingPointer - dragOrigin).manhattanLength() < 4 || !twoD) {
        // A click, or any zoom gesture on a 3D axes, zooms at the cursor (the engine
        // ignores the focus for 3D). Shift/Alt zooms out.
        QJsonObject request { { "action", "zoom" }, { "axesHandle", static_cast<double>(handle) },
            { "factor", zoomOut ? 0.5 : 2 } };
        if (dragBounds.width() > 0 && dragBounds.height() > 0) {
            request["focusXFrac"] = std::clamp(
                (pendingPointer.x() - dragBounds.left()) / dragBounds.width(), 0.0, 1.0);
            request["focusYFrac"] = std::clamp(
                (pendingPointer.y() - dragBounds.top()) / dragBounds.height(), 0.0, 1.0);
        }
        action(request);
        return;
    }
    if (dragBounds.width() <= 0 || dragBounds.height() <= 0) {
        return;
    }
    const auto rectangle = QRectF(dragOrigin, pendingPointer).normalized().intersected(dragBounds);
    if (rectangle.width() < 2 || rectangle.height() < 2) {
        return;
    }
    QJsonObject properties;
    for (const auto& dimension : { QString("X"), QString("Y") }) {
        double lower = dimension == "X"
            ? (rectangle.left() - dragBounds.left()) / dragBounds.width()
            : (dragBounds.bottom() - rectangle.bottom()) / dragBounds.height();
        double upper = dimension == "X"
            ? (rectangle.right() - dragBounds.left()) / dragBounds.width()
            : (dragBounds.bottom() - rectangle.top()) / dragBounds.height();
        if (rectangleState[dimension + "Dir"] == "reverse") {
            const double previous = lower;
            lower = 1 - upper;
            upper = 1 - previous;
        }
        const auto limits = rectangleState[dimension + "Lim"].toArray();
        const bool logarithmic = rectangleState[dimension + "Scale"] == "log";
        const double minimum
            = logarithmic ? std::log10(limits[0].toDouble()) : limits[0].toDouble();
        const double maximum
            = logarithmic ? std::log10(limits[1].toDouble()) : limits[1].toDouble();
        lower = minimum + lower * (maximum - minimum);
        upper = minimum + upper * (maximum - minimum);
        properties[dimension + "Lim"] = QJsonArray { logarithmic ? std::pow(10, lower) : lower,
            logarithmic ? std::pow(10, upper) : upper };
    }
    action({ { "action", "properties" }, { "axesHandle", static_cast<double>(handle) },
        { "properties", properties } });
}
//=============================================================================
void
FigureWorkspaceQt::doubleClick(QMouseEvent* event)
{
    release();
    auto* axis = axisAt(event->position());
    if (axis && event->button() == Qt::LeftButton) {
        selectAxes(findGraphicsObjectHandle(axis));
        action({ { "action", "fit" } });
    }
}
//=============================================================================
void
FigureWorkspaceQt::wheel(QWheelEvent* event)
{
    if (event->modifiers() & (Qt::ControlModifier | Qt::AltModifier | Qt::MetaModifier)) {
        return;
    }
    auto* axis = axisAt(event->position());
    if (!axis) {
        return;
    }
    // Scroll-to-zoom is a default interaction, so it stops when it is turned off.
    if (!defaultInteractivityEnabled(axis)) {
        return;
    }
    double exponent = event->pixelDelta().isNull()
        ? event->angleDelta().y() / 120.0 * std::log(1.15)
        : event->pixelDelta().y() * 0.002;
    if (exponent == 0) {
        return;
    }
    selectAxes(findGraphicsObjectHandle(axis));
    QJsonObject request { { "action", "zoom" },
        { "factor", std::exp(std::clamp(exponent, -2.0, 2.0)) } };
    // Anchor the zoom under the cursor (fraction of the axes rectangle, top-origin)
    // so the pointed-at point stays fixed. The shared engine macro ignores the focus for
    // 3D axes.
    const auto box = axis->getClientAreaAsPixels();
    if (box.size() == 4 && box[2] > 0 && box[3] > 0 && canvas) {
        const double ratio = canvas->devicePixelRatioF();
        const auto local = event->position() - canvas->geometry().topLeft();
        const double axLeft = box[0] / ratio;
        const double axWidth = box[2] / ratio;
        const double axTop = canvas->height() - (box[1] + box[3]) / ratio;
        const double axHeight = box[3] / ratio;
        request["focusXFrac"] = std::clamp((local.x() - axLeft) / axWidth, 0.0, 1.0);
        request["focusYFrac"] = std::clamp((local.y() - axTop) / axHeight, 0.0, 1.0);
    }
    action(request);
}
//=============================================================================
bool
FigureWorkspaceQt::shortcut(QKeyEvent* event)
{
    if ((event->isAutoRepeat()
            && (event->key() == Qt::Key_P || event->key() == Qt::Key_R || event->key() == Qt::Key_Z
                || event->key() == Qt::Key_F))
        || event->modifiers() & Qt::AltModifier
        || window->waitKeyOrMousePressedMode == WAIT_PRESS_MODE::WAIT) {
        return false;
    }
    auto* focus = QApplication::focusWidget();
    if (focus
        && (qobject_cast<QLineEdit*>(focus) || qobject_cast<QTextEdit*>(focus)
            || qobject_cast<QPlainTextEdit*>(focus) || qobject_cast<QAbstractSpinBox*>(focus)
            || qobject_cast<QComboBox*>(focus) || inspector->isAncestorOf(focus))) {
        return false;
    }
    for (const auto& name :
        { GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, GO_WINDOW_KEY_PRESS_FCN_PROPERTY_NAME_STR }) {
        auto* callback = window->goFig->findProperty(name, false);
        if (callback && !callback->get().isEmpty()) {
            return false;
        }
    }
    if (event->modifiers() & (Qt::ControlModifier | Qt::MetaModifier)) {
        if (event->key() == Qt::Key_Z || event->key() == Qt::Key_Y) {
            const bool redo = event->key() == Qt::Key_Y || event->modifiers() & Qt::ShiftModifier;
            action({ { "action", redo ? "redo" : "undo" } });
            return true;
        }
        return false;
    }
    if (hoverAxis && !dragAxis) {
        selectAxes(hoverAxis);
    }
    switch (event->key()) {
    case Qt::Key_P:
        release();
        if (window->mouseMode != MOUSE_MODE::PAN) {
            window->onPanAction();
        }
        break;
    case Qt::Key_R:
        release();
        if (window->mouseMode != MOUSE_MODE::ROTATION) {
            window->onRotateAction();
        }
        break;
    case Qt::Key_Z:
        release();
        if (window->mouseMode != MOUSE_MODE::ZOOM_IN) {
            window->onZoomInAction();
        }
        break;
    case Qt::Key_F:
        action({ { "action", "fit" } });
        break;
    default:
        if (event->key() < Qt::Key_Left || event->key() > Qt::Key_Down) {
            return false;
        }
        auto* axis = dynamic_cast<GOAxis*>(findGraphicsObject(selectedAxes(), false));
        if (!axis) {
            return false;
        }
        const double horizontal = event->key() == Qt::Key_Left ? -4
            : event->key() == Qt::Key_Right                    ? 4
                                                               : 0;
        const double vertical = event->key() == Qt::Key_Up ? -4
            : event->key() == Qt::Key_Down                 ? 4
                                                           : 0;
        if (window->mouseMode == MOUSE_MODE::PAN) {
            const auto box = axis->getClientAreaAsPixels();
            if (box.size() != 4 || box[2] <= 0 || box[3] <= 0) {
                return false;
            }
            action({ { "action", "pan" }, { "dxFrac", horizontal / box[2] },
                { "dyFrac", vertical / box[3] } });
        } else if (window->mouseMode == MOUSE_MODE::ROTATION) {
            double azimuth = 0, elevation = 0;
            axis->getView(azimuth, elevation);
            action({ { "action", "view" }, { "azimuth", azimuth - horizontal },
                { "elevation", elevation + vertical } });
        } else if (window->mouseMode == MOUSE_MODE::ZOOM_IN
            || window->mouseMode == MOUSE_MODE::ZOOM_OUT) {
            if (vertical == 0) {
                return false;
            }
            action({ { "action", "zoom" }, { "factor", vertical < 0 ? 1.15 : 1 / 1.15 } });
        } else if (window->mouseMode == MOUSE_MODE::NONE && inspectedPoint["found"].toBool()) {
            const int offset
                = event->key() == Qt::Key_Left || event->key() == Qt::Key_Down ? -1 : 1;
            const int index = std::clamp(
                inspectedPoint["index"].toInt() + offset, 1, inspectedPoint["count"].toInt(1));
            inspectedPoint["index"] = index;
            action({ { "action", "step" }, { "objectHandle", inspectedPoint["objectHandle"] },
                { "index", index } });
        } else {
            return false;
        }
        break;
    }
    showIndicator(selectedAxes());
    return true;
}
//=============================================================================
void
FigureWorkspaceQt::showIndicator(int64 handle)
{
    auto* axis = dynamic_cast<GOAxis*>(findGraphicsObject(handle, false));
    if (!axis || !canvas || !window || axis->getParentFigure() != window->goFig) {
        indicator->hide();
        return;
    }
    // SelectionHighlight 'off' on the axes or its figure hides the hover frame.
    if (!highlightsSelection(axis) || !highlightsSelection(window->goFig)) {
        indicator->hide();
        return;
    }
    const auto box = axis->getClientAreaAsPixels();
    if (box.size() != 4) {
        indicator->hide();
        return;
    }
    const double ratio = canvas->devicePixelRatioF();
    const auto origin = canvas->mapTo(window, QPoint());
    indicator->setGeometry(QRectF(origin.x() + box[0] / ratio,
        origin.y() + canvas->height() - (box[1] + box[3]) / ratio, box[2] / ratio, box[3] / ratio)
            .toAlignedRect());
    indicator->setProperty("axesHandle", QVariant::fromValue<qlonglong>(handle));
    indicator->show();
    indicator->raise();
}
//=============================================================================
void
FigureWorkspaceQt::hover(const QPointF& position)
{
    auto* axis = axisAt(position);
    hoverAxis = axis ? findGraphicsObjectHandle(axis) : 0;
    if (dragAxis || hoverAxis) {
        showIndicator(dragAxis ? dragAxis : hoverAxis);
    } else {
        indicator->hide();
    }
}
//=============================================================================
bool
FigureWorkspaceQt::eventFilter(QObject* object, QEvent* event)
{
    if (object == axesSelector
        && (event->type() == QEvent::FocusIn || event->type() == QEvent::MouseButtonPress
            || event->type() == QEvent::KeyPress)) {
        refresh();
    }
    if (object == inspector && event->type() == QEvent::Resize) {
        schedulePreferences();
    }
    if (object == window) {
        if (event->type() == QEvent::Show) {
            QTimer::singleShot(0, this, &FigureWorkspaceQt::restoreInspectorWidth);
        }
        if (event->type() == QEvent::Close) {
            savePreferences();
        }
        if (event->type() == QEvent::WindowDeactivate || event->type() == QEvent::Hide) {
            release();
            indicator->hide();
            pointMarker->hide();
            pointTip->hide();
        }
    }
    if (object == canvas) {
        if (event->type() == QEvent::Leave && !dragAxis) {
            hoverAxis = 0;
            indicator->hide();
        } else if (event->type() == QEvent::Resize) {
            inspectedPoint = {};
            pointMarker->hide();
            pointTip->hide();
            showIndicator(dragAxis ? dragAxis : hoverAxis);
        } else if (event->type() == QEvent::Move || event->type() == QEvent::Show) {
            updatePointMarker();
            if (event->type() == QEvent::Show) {
                QTimer::singleShot(0, this, &FigureWorkspaceQt::restoreInspectorWidth);
            }
        }
    }
    return false;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
