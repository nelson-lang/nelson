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
#include "FigureLegendClickFilter.hpp"
#include "GOWindow.hpp"
#include "GOPropertyNames.hpp"
#include <QtCore/QSignalBlocker>
#include <QtGui/QPainter>
#include <QtWidgets/QGroupBox>
#include <QtWidgets/QHBoxLayout>
#include <QtWidgets/QSlider>
#include <QtWidgets/QToolBar>
#include <QtWidgets/QToolButton>
#include <QtWidgets/QTreeWidgetItemIterator>
#include <QtWidgets/QVBoxLayout>
#include <algorithm>
#include <cmath>
//=============================================================================
namespace Nelson {
void
FigureWorkspaceQt::createViewControls(QVBoxLayout* layout)
{
    auto* group = new QGroupBox(tr("View"), inspector);
    auto* rows = new QVBoxLayout(group);
    auto add = [this, group, rows](const QString& title, const QString& name, int minimum,
                   int maximum, QLabel*& value) {
        auto* row = new QHBoxLayout();
        auto* label = new QLabel(title, group);
        auto* slider = new QSlider(Qt::Horizontal, group);
        slider->setObjectName(name);
        slider->setAccessibleName(title);
        slider->setRange(minimum, maximum);
        slider->setSingleStep(1);
        slider->setPageStep(10);
        slider->setTracking(false);
        label->setBuddy(slider);
        value = new QLabel(group);
        value->setMinimumWidth(55);
        row->addWidget(label);
        row->addWidget(slider, 1);
        row->addWidget(value);
        rows->addLayout(row);
        auto* display = value;
        connect(slider, &QSlider::sliderMoved, this, [display](int position) {
            display->setText(QString::number(position / 2.0, 'f', 1) + QChar(0x00b0));
        });
        connect(slider, &QSlider::valueChanged, this, [this]() {
            if (selectedAxes() > 0) {
                action({ { "action", "view" }, { "azimuth", azimuthSlider->value() / 2.0 },
                    { "elevation", elevationSlider->value() / 2.0 } });
            }
        });
        return slider;
    };
    azimuthSlider = add(tr("Azimuth"), "figureAzimuthSlider", -360, 360, azimuthValue);
    elevationSlider = add(tr("Elevation"), "figureElevationSlider", -180, 180, elevationValue);
    layout->addWidget(group);
    updateViewControls();
}
//=============================================================================
void
FigureWorkspaceQt::updateViewControls()
{
    if (!azimuthSlider || !elevationSlider || !commands.empty()) {
        return;
    }
    const auto view = axesState()["View"].toArray();
    const bool available = view.size() == 2;
    azimuthSlider->setEnabled(available);
    elevationSlider->setEnabled(available);
    const auto update = [available, &view](QSlider* slider, QLabel* label, int index) {
        if (slider->isSliderDown()) {
            return;
        }
        const QSignalBlocker blocker(slider);
        const double angle = available ? view[index].toDouble() : 0;
        slider->setValue(static_cast<int>(std::round(angle * 2)));
        label->setText(available ? QString::number(angle, 'f', 1) + QChar(0x00b0) : QString());
    };
    update(azimuthSlider, azimuthValue, 0);
    update(elevationSlider, elevationValue, 1);
}
//=============================================================================
void
FigureWorkspaceQt::createLegend()
{
    legend = new QToolBar(tr("Plotted objects"), window);
    legend->setObjectName("figureLegendToolbar");
    legend->setMovable(false);
    legend->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
    legend->setIconSize(QSize(16, 16));
    window->addToolBar(Qt::BottomToolBarArea, legend);
    legend->hide();
}
//=============================================================================
void
FigureWorkspaceQt::updateLegend()
{
    const auto objects = figureObjectArray(axesState()["objects"]);
    if (legendAxes == selectedAxes() && legendObjects == objects) {
        return;
    }
    legendAxes = selectedAxes();
    legendObjects = objects;
    const auto previous = legend->actions();
    legend->clear();
    for (auto* item : previous) {
        item->setObjectName(QString());
        item->deleteLater();
    }
    for (int index = 0; index < objects.size(); ++index) {
        const auto object = objects[index].toObject();
        const auto handle = static_cast<int64>(object["handle"].toDouble());
        const auto rgb
            = object["Color"].isArray() ? object["Color"].toArray() : object["FaceColor"].toArray();
        QColor color = legend->palette().color(QPalette::Highlight);
        if (rgb.size() == 3) {
            color = QColor::fromRgbF(std::clamp(rgb[0].toDouble(), 0.0, 1.0),
                std::clamp(rgb[1].toDouble(), 0.0, 1.0), std::clamp(rgb[2].toDouble(), 0.0, 1.0));
        }
        QPixmap swatch(16, 16);
        swatch.fill(Qt::transparent);
        QPainter painter(&swatch);
        painter.setPen(QPen(color, 3));
        painter.drawLine(1, 8, 15, 8);
        painter.end();
        const QString name = object["DisplayName"].toString();
        auto* toggle = legend->addAction(QIcon(swatch),
            name.isEmpty() ? tr("%1 %2").arg(object["type"].toString()).arg(index + 1) : name);
        toggle->setObjectName(QString("figureObjectToggle_%1").arg(handle));
        toggle->setData(QVariant::fromValue<qlonglong>(handle));
        toggle->setCheckable(true);
        const bool visible = object["visible"] == "on";
        toggle->setChecked(visible);
        toggle->setToolTip(tr("Show or hide %1").arg(toggle->text()));
        const int64 axes = legendAxes;
        connect(
            toggle, &QAction::toggled, this, [this, toggle, handle, axes, visible](bool checked) {
                const QSignalBlocker blocker(toggle);
                toggle->setChecked(visible);
                action({ { "action", "properties" }, { "axesHandle", static_cast<double>(axes) },
                    { "objectHandle", static_cast<double>(handle) },
                    { "properties", QJsonObject { { "Visible", checked ? "on" : "off" } } } });
            });
        if (auto* button = qobject_cast<QToolButton*>(legend->widgetForAction(toggle))) {
            new FigureLegendClickFilter(button, [this, handle, axes]() {
                action({ { "action", "isolate" }, { "axesHandle", static_cast<double>(axes) },
                    { "objectHandle", static_cast<double>(handle) } });
            });
        }
    }
    syncVisibility();
}
//=============================================================================
void
FigureWorkspaceQt::selectObjectInProperties()
{
    if (!inspector || !inspector->isVisible() || selectedObject <= 0) {
        return;
    }
    if (selectedObject != inspectObject) {
        requestInspectModel(selectedObject);
    }
}
//=============================================================================
void
FigureWorkspaceQt::updatePointMarker()
{
    if (!pointMarker || !canvas || !window) {
        return;
    }
    const auto pixel = inspectedPoint["pixel"].toArray();
    if (!canvas->isVisible() || window->mouseMode != MOUSE_MODE::NONE
        || !inspectedPoint["found"].toBool() || pixel.size() != 2
        || inspectedPoint["axesHandle"].toDouble() != selectedAxes()) {
        pointMarker->hide();
        pointTip->hide();
        return;
    }
    const auto objects = figureObjectArray(axesState()["objects"]);
    bool visible = false;
    for (const auto& value : objects) {
        const auto object = value.toObject();
        if (object["handle"] == inspectedPoint["objectHandle"] && object["visible"] == "on") {
            visible = true;
            break;
        }
    }
    const double ratio = canvas->devicePixelRatioF();
    const QPointF local(
        pixel[0].toDouble() / ratio, canvas->height() - pixel[1].toDouble() / ratio);
    if (!visible || !QRectF(canvas->rect()).contains(local)) {
        pointMarker->hide();
        pointTip->hide();
        return;
    }
    const auto origin = canvas->mapTo(window, QPoint());
    pointMarker->move(origin.x() + qRound(local.x()) - pointMarker->width() / 2,
        origin.y() + qRound(local.y()) - pointMarker->height() / 2);
    pointMarker->setProperty("objectHandle", inspectedPoint["objectHandle"].toDouble());
    pointMarker->show();
    pointMarker->raise();
    QString name;
    for (const auto& value : objects) {
        const auto object = value.toObject();
        if (object["handle"] == inspectedPoint["objectHandle"]) {
            name = object["DisplayName"].toString();
            if (name.isEmpty()) {
                name = object["type"].toString();
            }
        }
    }
    const auto position = inspectedPoint["position"].toArray();
    pointTip->setText(tr("%1 · Point %2\nX %3 · Y %4 · Z %5")
            .arg(name)
            .arg(inspectedPoint["index"].toInt())
            .arg(QString::number(position[0].toDouble(), 'g', 7))
            .arg(QString::number(position[1].toDouble(), 'g', 7))
            .arg(QString::number(position[2].toDouble(), 'g', 7)));
    pointTip->setMaximumWidth(std::max(1, std::min(320, canvas->width())));
    pointTip->adjustSize();
    const int left = std::clamp(pointMarker->x() + 18, origin.x(),
        std::max(origin.x(), origin.x() + canvas->width() - pointTip->width()));
    const int top = std::clamp(pointMarker->y() - pointTip->height() - 6, origin.y(),
        std::max(origin.y(), origin.y() + canvas->height() - pointTip->height()));
    pointTip->move(left, top);
    pointTip->show();
    pointTip->raise();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
