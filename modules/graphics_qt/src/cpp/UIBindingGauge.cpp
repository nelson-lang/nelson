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
#include <QtGui/QPainterPath>
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUIGauge.h"
#include "UITicksHelpers.h"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOColorProperty.hpp"
#include "QStringConverter.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
namespace {
    //=============================================================================
    class QUIGaugeWidget : public QWidget
    {
    public:
        double minimum = 0;
        double maximum = 100;
        double value = 0;
        UIGaugeStyle style = UIGaugeStyle::Circular;
        bool isVertical = false;
        std::vector<double> majorTicks;
        std::vector<double> minorTicks;
        std::vector<std::wstring> majorTickLabels;
        QColor backgroundColor = Qt::white;
        //=============================================================================
        QUIGaugeWidget(QWidget* parent) : QWidget(parent) { }
        //=============================================================================
    protected:
        //=============================================================================
        double
        fraction() const
        {
            return (maximum > minimum)
                ? std::clamp((value - minimum) / (maximum - minimum), 0.0, 1.0)
                : 0.0;
        }
        //=============================================================================
        double
        fractionOf(double tick) const
        {
            return (maximum > minimum)
                ? std::clamp((tick - minimum) / (maximum - minimum), 0.0, 1.0)
                : 0.0;
        }
        //=============================================================================
        bool
        isWithinLimits(double tick) const
        {
            double tolerance = (maximum - minimum) * 1e-9;
            return tick >= minimum - tolerance && tick <= maximum + tolerance;
        }
        //=============================================================================
        // MinorTicks empty: evenly spaced marks, 'divisions' per scale
        std::vector<double>
        drawingMinorTicksOf(int divisions) const
        {
            if (!minorTicks.empty() || maximum <= minimum) {
                return minorTicks;
            }
            std::vector<double> ticks;
            double step = (maximum - minimum) / divisions;
            for (int k = 0; k <= divisions; ++k) {
                ticks.push_back(minimum + k * step);
            }
            return ticks;
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
        void
        drawLinearGauge(QPainter& painter)
        {
            QColor frameColor(70, 70, 70);
            QColor needleColor(200, 30, 30);
            QRectF frame(1, 1, width() - 2, height() - 2);
            painter.setPen(QPen(frameColor, 1));
            painter.setBrush(backgroundColor);
            painter.drawRect(frame);
            if (isVertical) {
                int x = width() / 2;
                int top = 8;
                int bottom = height() - 8;
                for (double tick : drawingMinorTicksOf(25)) {
                    if (!isWithinLimits(tick)) {
                        continue;
                    }
                    int y = bottom - (int)((bottom - top) * fractionOf(tick));
                    painter.drawLine(x - 4, y, x - 1, y);
                }
                for (size_t k = 0; k < majorTicks.size(); ++k) {
                    if (!isWithinLimits(majorTicks[k])) {
                        continue;
                    }
                    int y = bottom - (int)((bottom - top) * fractionOf(majorTicks[k]));
                    painter.drawLine(x - 8, y, x - 1, y);
                    if (k < majorTickLabels.size()) {
                        painter.drawText(QRectF(x + 2, y - 7, width() - x - 4, 14),
                            Qt::AlignLeft | Qt::AlignVCenter, wstringToQString(majorTickLabels[k]));
                    }
                }
                int y = bottom - (int)((bottom - top) * fraction());
                painter.setPen(QPen(needleColor, 2));
                painter.drawLine(x - 12, y, x + 12, y);
            } else {
                int left = 8;
                int right = width() - 8;
                int baseline = 11;
                painter.setPen(QPen(frameColor, 1));
                for (double tick : drawingMinorTicksOf(25)) {
                    if (!isWithinLimits(tick)) {
                        continue;
                    }
                    int x = left + (int)((right - left) * fractionOf(tick));
                    painter.drawLine(x, baseline, x, baseline + 4);
                }
                for (size_t k = 0; k < majorTicks.size(); ++k) {
                    if (!isWithinLimits(majorTicks[k])) {
                        continue;
                    }
                    int x = left + (int)((right - left) * fractionOf(majorTicks[k]));
                    painter.drawLine(x, baseline, x, baseline + 8);
                    if (k < majorTickLabels.size()) {
                        painter.drawText(QRectF(x - 18, baseline + 8, 36, 14),
                            Qt::AlignHCenter | Qt::AlignTop, wstringToQString(majorTickLabels[k]));
                    }
                }
                int x = left + (int)((right - left) * fraction());
                painter.setPen(QPen(needleColor, 2));
                painter.drawLine(x, baseline + 5, x, height() - 5);
            }
        }
        //=============================================================================
        void
        drawCircularGauge(QPainter& painter)
        {
            QColor frameColor(70, 70, 70);
            QColor tickColor(40, 40, 40);
            QColor needleColor(200, 30, 30);
            double startAngle = 225;
            double sweep = -270;
            QPointF center(width() / 2.0, height() / 2.0 + 2.0);
            double radius = std::min(width(), height()) / 2.0 - 8.0;
            if (style == UIGaugeStyle::Semicircular) {
                startAngle = 180;
                sweep = -180;
                center = QPointF(width() / 2.0, height() - 7.0);
                radius = std::min(width() / 2.0 - 8.0, height() - 12.0);
            } else if (style == UIGaugeStyle::NinetyDegree) {
                startAngle = 180;
                sweep = -90;
                center = QPointF(width() - 8.0, height() - 8.0);
                radius = std::min(width() - 14.0, height() - 14.0);
            }
            QRectF arcRect(center.x() - radius, center.y() - radius, radius * 2, radius * 2);
            painter.setPen(QPen(frameColor, 1));
            painter.setBrush(backgroundColor);
            if (style == UIGaugeStyle::Circular) {
                painter.drawEllipse(arcRect);
            } else {
                QPainterPath gaugePath;
                gaugePath.moveTo(center);
                gaugePath.arcTo(arcRect, startAngle, sweep);
                gaugePath.closeSubpath();
                painter.fillPath(gaugePath, backgroundColor);
                painter.drawArc(arcRect, (int)(startAngle * 16), (int)(sweep * 16));
                painter.drawLine(center, pointOnCircle(center, radius, startAngle));
                painter.drawLine(center, pointOnCircle(center, radius, startAngle + sweep));
            }
            painter.setPen(QPen(tickColor, 1));
            int minorDivisions = (style == UIGaugeStyle::NinetyDegree) ? 20 : 25;
            for (double tick : drawingMinorTicksOf(minorDivisions)) {
                if (!isWithinLimits(tick)) {
                    continue;
                }
                double angle = startAngle + sweep * fractionOf(tick);
                QPointF outer = pointOnCircle(center, radius - 2, angle);
                QPointF inner = pointOnCircle(center, radius - 7, angle);
                painter.drawLine(outer, inner);
            }
            for (size_t k = 0; k < majorTicks.size(); ++k) {
                if (!isWithinLimits(majorTicks[k])) {
                    continue;
                }
                double angle = startAngle + sweep * fractionOf(majorTicks[k]);
                QPointF outer = pointOnCircle(center, radius - 2, angle);
                QPointF inner = pointOnCircle(center, radius - 12, angle);
                painter.drawLine(outer, inner);
                if (k < majorTickLabels.size()) {
                    QPointF label = pointOnCircle(center, radius - 25, angle);
                    painter.drawText(QRectF(label.x() - 15, label.y() - 7, 30, 14), Qt::AlignCenter,
                        wstringToQString(majorTickLabels[k]));
                }
            }
            double valueAngle = startAngle + sweep * fraction();
            QPointF needleEnd = pointOnCircle(center, radius - 24, valueAngle);
            painter.setPen(QPen(needleColor, 2));
            painter.drawLine(center, needleEnd);
        }
        //=============================================================================
        void
        paintEvent(QPaintEvent* /*event*/) override
        {
            QPainter painter(this);
            painter.setRenderHint(QPainter::Antialiasing);
            if (style == UIGaugeStyle::Linear) {
                drawLinearGauge(painter);
                return;
            }
            drawCircularGauge(painter);
        }
        //=============================================================================
    };
    //=============================================================================
    class UIGaugeBinding : public QtUIComponentBinding
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
            QUIGaugeWidget* gauge = new QUIGaugeWidget(parent);
            gauge->style = static_cast<GOUIGauge*>(model)->getGaugeStyle();
            gauge->setAutoFillBackground(false);
            gauge->setAttribute(Qt::WA_TranslucentBackground);
            widget = gauge;
        }
        //=============================================================================
        void
        applyColorsToWidget() override
        {
            if (widget) {
                widget->setStyleSheet({});
            }
        }
        //=============================================================================
        void
        applyStateToWidget(bool /*isNewWidget*/) override
        {
            QUIGaugeWidget* gauge = static_cast<QUIGaugeWidget*>(widget.data());
            if (!gauge) {
                return;
            }
            std::vector<double> limits(
                model->findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
            if (limits.size() == 2) {
                gauge->minimum = limits[0];
                gauge->maximum = limits[1];
            }
            gauge->value = model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);
            GOColorProperty* backgroundColor = static_cast<GOColorProperty*>(
                model->findProperty(GO_BACKGROUND_COLOR_PROPERTY_NAME_STR, false));
            if (backgroundColor && !backgroundColor->isNone()) {
                std::vector<double> rgb = backgroundColor->data();
                if (rgb.size() >= 3) {
                    gauge->backgroundColor = QColor::fromRgb((int)(rgb[0] * 255.0 + 0.5),
                        (int)(rgb[1] * 255.0 + 0.5), (int)(rgb[2] * 255.0 + 0.5));
                }
            }
            gauge->minorTicks = model->findVectorDoubleProperty(GO_MINOR_TICKS_PROPERTY_NAME_STR);
            const bool isLinear
                = static_cast<GOUIGauge*>(model)->getGaugeStyle() == UIGaugeStyle::Linear;
            if (isLinear) {
                gauge->isVertical = model->stringCheck(
                    GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
            }
            // Label a denser "nice" scale sized to the widget, while MajorTicks stays coarse.
            std::vector<double> position(
                model->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
            const double pw = position.size() >= 4 ? position[2] : 0.0;
            const double ph = position.size() >= 4 ? position[3] : 0.0;
            const bool majorAuto = model->stringCheck(
                GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
            const double extent = isLinear ? (gauge->isVertical ? ph : pw) : std::min(pw, ph);
            const double divisor = isLinear ? 30.0 : 18.0;
            uiDisplayMajorTicks(gauge->minimum, gauge->maximum, majorAuto,
                model->findVectorDoubleProperty(GO_MAJOR_TICKS_PROPERTY_NAME_STR),
                model->findStringVectorProperty(GO_MAJOR_TICK_LABELS_PROPERTY_NAME_STR), extent,
                divisor, gauge->majorTicks, gauge->majorTickLabels);
            gauge->update();
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUIGaugeBinding(GOUIComponent* component)
{
    return new UIGaugeBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
