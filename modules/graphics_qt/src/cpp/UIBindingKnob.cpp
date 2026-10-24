//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <algorithm>
#include <cmath>
#include <QtWidgets/QWidget>
#include <QtGui/QPainter>
#include <QtGui/QMouseEvent>
#include <QtGui/QKeyEvent>
#include <QtGui/QPolygonF>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIKnob.h"
#include "UITicksHelpers.h"
#include "GOColorProperty.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    // knob sweeps 270 degrees from 225 (min) clockwise down to -45 (max)
    class QUIKnobWidget : public QWidget
    {
        GOUIKnob* goKnob;

    public:
        double minimum = 0;
        double maximum = 100;
        double value = 0;
        bool isDiscrete = false;
        int itemCount = 0;
        int itemIndex = 0;
        std::vector<double> majorTicks;
        std::vector<double> minorTicks;
        bool minorTicksAuto = true;
        // ticks, item dots and labels follow FontColor
        QColor fontColor { 33, 33, 33 };
        std::vector<std::wstring> majorTickLabels;
        std::vector<std::wstring> items;
        //=============================================================================
        QUIKnobWidget(GOUIKnob* owner, QWidget* parent) : QWidget(parent), goKnob(owner) { }
        //=============================================================================
    protected:
        //=============================================================================
        double
        fractionOf(double v) const
        {
            if (isDiscrete) {
                return itemCount > 1 ? (double)itemIndex / (double)(itemCount - 1) : 0.0;
            }
            return (maximum > minimum) ? std::clamp((v - minimum) / (maximum - minimum), 0.0, 1.0)
                                       : 0.0;
        }
        //=============================================================================
        double
        fractionFromPosition(const QPoint& pos)
        {
            double dx = pos.x() - width() / 2.0;
            double dy = pos.y() - height() / 2.0;
            double angle = std::atan2(-dy, dx) * 180.0 / M_PI; // 0=east, ccw positive
            // map to knob fraction: 225 deg -> 0, -45 deg -> 1 (clockwise)
            double sweep = 225.0 - angle;
            if (sweep < 0) {
                sweep += 360.0;
            }
            return std::clamp(sweep / 270.0, 0.0, 1.0);
        }
        //=============================================================================
        QPointF
        pointOnCircle(const QPointF& center, double radius, double angleDegrees) const
        {
            double angle = angleDegrees * M_PI / 180.0;
            return QPointF(
                center.x() + std::cos(angle) * radius, center.y() - std::sin(angle) * radius);
        }
        //=============================================================================
        double
        tickFraction(double tick) const
        {
            return (maximum > minimum)
                ? std::clamp((tick - minimum) / (maximum - minimum), 0.0, 1.0)
                : 0.0;
        }
        //=============================================================================
        // ticks outside Limits are not drawn (instead of piling up at the ends)
        bool
        isWithinLimits(double tick) const
        {
            double tolerance = (maximum - minimum) * 1e-9;
            return tick >= minimum - tolerance && tick <= maximum + tolerance;
        }
        //=============================================================================
        void
        drawContinuousKnob(QPainter& painter)
        {
            QPointF center(width() / 2.0, height() / 2.0);
            double outerRadius = std::min(width(), height()) / 2.0 - 24.0;
            double faceRadius = outerRadius - 20.0;
            painter.setPen(QPen(fontColor, 1));
            std::vector<double> drawingMinorTicks = minorTicks;
            if (drawingMinorTicks.empty() && minorTicksAuto && maximum > minimum) {
                // auto minor ticks: 40 intervals over Limits
                double step = (maximum - minimum) / 40.0;
                for (int k = 0; k <= 40; ++k) {
                    drawingMinorTicks.push_back(minimum + k * step);
                }
            }
            for (double tick : drawingMinorTicks) {
                if (!isWithinLimits(tick)) {
                    continue;
                }
                double angle = 225.0 - tickFraction(tick) * 270.0;
                QPointF outer = pointOnCircle(center, outerRadius, angle);
                QPointF inner = pointOnCircle(center, outerRadius - 7.0, angle);
                painter.drawLine(outer, inner);
            }
            // Draw the model's MajorTicks as given, so every backend shows the same scale.
            painter.setPen(QPen(fontColor, 2));
            for (size_t k = 0; k < majorTicks.size(); ++k) {
                if (!isWithinLimits(majorTicks[k])) {
                    continue;
                }
                double angle = 225.0 - tickFraction(majorTicks[k]) * 270.0;
                QPointF outer = pointOnCircle(center, outerRadius, angle);
                QPointF inner = pointOnCircle(center, outerRadius - 12.0, angle);
                painter.drawLine(outer, inner);
                if (k < majorTickLabels.size()) {
                    QPointF label = pointOnCircle(center, outerRadius + 9.0, angle);
                    painter.drawText(QRectF(label.x() - 15, label.y() - 7, 30, 14), Qt::AlignCenter,
                        wstringToQString(majorTickLabels[k]));
                }
            }
            painter.setPen(QPen(QColor(90, 90, 90), 1));
            painter.setBrush(QColor(202, 202, 202));
            painter.drawEllipse(center, faceRadius, faceRadius);
            painter.setBrush(QColor(238, 238, 238));
            double innerRadius = faceRadius * 0.65;
            painter.drawEllipse(center, innerRadius, innerRadius);
            double angle = 225.0 - fractionOf(value) * 270.0;
            QPolygonF pointer;
            pointer << pointOnCircle(center, faceRadius + 5.0, angle)
                    << pointOnCircle(center, innerRadius - 2.0, angle - 8.0)
                    << pointOnCircle(center, innerRadius - 2.0, angle + 8.0);
            painter.setPen(Qt::NoPen);
            painter.setBrush(QColor(196, 38, 46));
            painter.drawPolygon(pointer);
        }
        //=============================================================================
        void
        drawDiscreteKnob(QPainter& painter)
        {
            QPointF center(width() / 2.0, height() / 2.0);
            double outerRadius = std::min(width(), height()) / 2.0 - 62.0;
            double labelRadius = outerRadius + 35.0;
            painter.setPen(QPen(QColor(80, 80, 80), 1));
            painter.setBrush(QColor(202, 202, 202));
            painter.drawEllipse(center, outerRadius, outerRadius);

            int count = std::max(1, itemCount);
            double activeFraction = count > 1 ? (double)itemIndex / (double)(count - 1) : 0.0;
            double activeAngle = 180.0 - activeFraction * 180.0;
            QPointF axisStart = pointOnCircle(center, outerRadius, activeAngle);
            QPointF axisEnd = pointOnCircle(center, outerRadius, activeAngle + 180.0);
            QPointF normal = pointOnCircle(QPointF(0, 0), 10.0, activeAngle + 90.0);
            QPolygonF band;
            band << QPointF(axisStart.x() + normal.x(), axisStart.y() + normal.y())
                 << QPointF(axisStart.x() - normal.x(), axisStart.y() - normal.y())
                 << QPointF(axisEnd.x() - normal.x(), axisEnd.y() - normal.y())
                 << QPointF(axisEnd.x() + normal.x(), axisEnd.y() + normal.y());
            painter.setBrush(QColor(238, 238, 238));
            painter.setPen(QPen(QColor(110, 110, 110), 1));
            painter.drawPolygon(band);
            for (int k = 0; k < count; ++k) {
                double fraction = count > 1 ? (double)k / (double)(count - 1) : 0.0;
                double angle = 180.0 - fraction * 180.0;
                QPointF dot = pointOnCircle(center, outerRadius + 13.0, angle);
                painter.setPen(Qt::NoPen);
                painter.setBrush(fontColor);
                painter.drawEllipse(dot, 3.5, 3.5);
                if (k < (int)items.size()) {
                    QPointF label = pointOnCircle(center, labelRadius + 2.0, angle);
                    painter.setPen(fontColor);
                    painter.drawText(QRectF(label.x() - 28, label.y() - 8, 56, 16), Qt::AlignCenter,
                        wstringToQString(items[k]));
                }
            }
            QPointF start = pointOnCircle(center, outerRadius * 0.32, activeAngle);
            QPointF tip = pointOnCircle(center, outerRadius - 9.0, activeAngle);
            painter.setPen(QPen(QColor(200, 30, 30), 3));
            painter.drawLine(start, tip);
        }
        //=============================================================================
        void
        applyFraction(double fraction, bool commit)
        {
            if (isDiscrete) {
                if (itemCount > 0) {
                    int index = (int)std::round(fraction * (double)(itemCount - 1));
                    itemIndex = std::clamp(index, 0, itemCount - 1);
                    update();
                    if (commit && goKnob) {
                        goKnob->onUserDiscreteChanged(itemIndex);
                    }
                }
            } else {
                value = minimum + fraction * (maximum - minimum);
                update();
                if (goKnob) {
                    if (commit) {
                        goKnob->onUserValueChanged(value);
                    } else {
                        goKnob->onUserValueChanging(value);
                    }
                }
            }
        }
        //=============================================================================
        void
        mousePressEvent(QMouseEvent* event) override
        {
            applyFraction(fractionFromPosition(event->pos()), false);
        }
        void
        mouseMoveEvent(QMouseEvent* event) override
        {
            if (event->buttons() & Qt::LeftButton) {
                applyFraction(fractionFromPosition(event->pos()), false);
            }
        }
        void
        mouseReleaseEvent(QMouseEvent* event) override
        {
            applyFraction(fractionFromPosition(event->pos()), true);
        }
        //=============================================================================
        // Arrow keys adjust a focused knob: Up/Right raise the value, Down/Left lower it,
        // by one fortieth of the full sweep per press (range/40). A discrete knob steps one
        // item. Other keys fall through to the base widget.
        void
        keyPressEvent(QKeyEvent* event) override
        {
            const int key = event->key();
            const bool increase = (key == Qt::Key_Up || key == Qt::Key_Right);
            const bool decrease = (key == Qt::Key_Down || key == Qt::Key_Left);
            if (!increase && !decrease) {
                QWidget::keyPressEvent(event);
                return;
            }
            if (isDiscrete) {
                if (itemCount > 1) {
                    int target = std::clamp(itemIndex + (increase ? 1 : -1), 0, itemCount - 1);
                    applyFraction((double)target / (double)(itemCount - 1), true);
                }
            } else {
                double fraction = fractionOf(value) + (increase ? 1.0 : -1.0) / 40.0;
                applyFraction(std::clamp(fraction, 0.0, 1.0), true);
            }
            event->accept();
        }
        //=============================================================================
        void
        paintEvent(QPaintEvent* /*event*/) override
        {
            QPainter painter(this);
            painter.setRenderHint(QPainter::Antialiasing);
            if (isDiscrete) {
                drawDiscreteKnob(painter);
            } else {
                drawContinuousKnob(painter);
            }
        }
        //=============================================================================
    };
    //=============================================================================
    class UIKnobBinding : public QtUIComponentBinding
    {
    public:
        using QtUIComponentBinding::QtUIComponentBinding;
        //=============================================================================
    protected:
        //=============================================================================
        void
        createWidget() override
        {
            QWidget* parent = getParentQWidget();
            if (!parent) {
                return;
            }
            GOUIKnob* goKnob = static_cast<GOUIKnob*>(model);
            QUIKnobWidget* knob = new QUIKnobWidget(goKnob, parent);
            knob->isDiscrete = goKnob->isDiscrete();
            knob->setAttribute(Qt::WA_TranslucentBackground);
            // Accept keyboard focus (by click and by Tab) so the arrow keys can
            // adjust the knob once it is focused.
            knob->setFocusPolicy(Qt::StrongFocus);
            widget = knob;
        }
        //=============================================================================
        QMargins
        widgetGeometryMargins() override
        {
            return static_cast<GOUIKnob*>(model)->isDiscrete() ? QMargins(60, 60, 60, 60)
                                                               : QMargins(38, 38, 38, 38);
        }
        //=============================================================================
        void
        applyStateToWidget(bool /*isNewWidget*/) override
        {
            QUIKnobWidget* knob = static_cast<QUIKnobWidget*>(widget.data());
            if (!knob) {
                return;
            }
            GOUIKnob* goKnob = static_cast<GOUIKnob*>(model);
            if (goKnob->isDiscrete()) {
                wstringVector items = model->findStringVectorProperty(GO_ITEMS_PROPERTY_NAME_STR);
                int index = goKnob->syncDiscreteValueIndex();
                knob->itemCount = (int)items.size();
                knob->itemIndex = index - 1;
                knob->items = items;
            } else {
                std::vector<double> limits(
                    model->findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
                if (limits.size() == 2) {
                    knob->minimum = limits[0];
                    knob->maximum = limits[1];
                }
                knob->value = model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);
                knob->minorTicks
                    = model->findVectorDoubleProperty(GO_MINOR_TICKS_PROPERTY_NAME_STR);
                knob->minorTicksAuto = model->stringCheck(
                    GO_MINOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
                // The knob labels the densest "nice" scale sized to the ring, while MajorTicks
                // stays coarse.
                std::vector<double> position(
                    model->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
                const double extent
                    = position.size() >= 4 ? std::min(position[2], position[3]) : 0.0;
                const bool majorAuto = model->stringCheck(
                    GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
                uiDisplayMajorTicks(knob->minimum, knob->maximum, majorAuto,
                    model->findVectorDoubleProperty(GO_MAJOR_TICKS_PROPERTY_NAME_STR),
                    model->findStringVectorProperty(GO_MAJOR_TICK_LABELS_PROPERTY_NAME_STR), extent,
                    8.0, knob->majorTicks, knob->majorTickLabels);
            }
            GOColorProperty* fontColor = static_cast<GOColorProperty*>(
                model->findProperty(GO_FONT_COLOR_PROPERTY_NAME_STR, false));
            if (fontColor && !fontColor->isNone()) {
                std::vector<double> rgb = fontColor->data();
                if (rgb.size() >= 3) {
                    knob->fontColor = QColor::fromRgb((int)(rgb[0] * 255.0 + 0.5),
                        (int)(rgb[1] * 255.0 + 0.5), (int)(rgb[2] * 255.0 + 0.5));
                }
            }
            knob->update();
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIKnobBinding(GOUIComponent* component)
{
    return new UIKnobBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
