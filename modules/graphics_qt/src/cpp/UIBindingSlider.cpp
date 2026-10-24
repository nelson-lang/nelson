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
#include "UIComponentBindings.hpp"
#include "UIComponentBindingBase.hpp"
#include "GOUISlider.h"
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
    class QUISliderWidget : public QWidget
    {
        GOUISlider* goSlider;

    public:
        double minimum = 0;
        double maximum = 100;
        double value = 0;
        double highValue = 100; // range style only
        bool isRange = false;
        bool isVertical = false;
        std::vector<double> majorTicks;
        std::vector<double> minorTicks;
        std::vector<std::wstring> majorTickLabels;
        QColor fontColor { 33, 33, 33 };
        //=============================================================================
        QUISliderWidget(GOUISlider* owner, QWidget* parent) : QWidget(parent), goSlider(owner)
        {
            setMouseTracking(false);
        }
        //=============================================================================
    protected:
        //=============================================================================
        int
        trackStart() const
        {
            return 8;
        }
        int
        trackLength() const
        {
            return (isVertical ? height() : width()) - 16;
        }
        //=============================================================================
        double
        positionToValue(const QPoint& pos) const
        {
            double t = isVertical ? 1.0 - ((double)(pos.y() - trackStart()) / (double)trackLength())
                                  : (double)(pos.x() - trackStart()) / (double)trackLength();
            t = std::clamp(t, 0.0, 1.0);
            return minimum + t * (maximum - minimum);
        }
        //=============================================================================
        int
        valueToPosition(double v) const
        {
            double t = (v - minimum) / (maximum - minimum);
            t = std::clamp(t, 0.0, 1.0);
            if (isVertical) {
                return trackStart() + (int)((1.0 - t) * trackLength());
            }
            return trackStart() + (int)(t * trackLength());
        }
        //=============================================================================
        bool draggingHigh = false;
        //=============================================================================
        void
        mousePressEvent(QMouseEvent* event) override
        {
            double clicked = positionToValue(event->pos());
            if (isRange) {
                draggingHigh = std::abs(clicked - highValue) < std::abs(clicked - value);
                if (draggingHigh) {
                    highValue = std::max(clicked, value);
                } else {
                    value = std::min(clicked, highValue);
                }
            } else {
                value = clicked;
            }
            update();
            if (goSlider) {
                goSlider->onUserValueChanging(value, highValue);
            }
        }
        //=============================================================================
        void
        mouseMoveEvent(QMouseEvent* event) override
        {
            if (!(event->buttons() & Qt::LeftButton)) {
                return;
            }
            double dragged = positionToValue(event->pos());
            if (isRange) {
                if (draggingHigh) {
                    highValue = std::max(dragged, value);
                } else {
                    value = std::min(dragged, highValue);
                }
            } else {
                value = dragged;
            }
            update();
            if (goSlider) {
                goSlider->onUserValueChanging(value, highValue);
            }
        }
        //=============================================================================
        void
        mouseReleaseEvent(QMouseEvent* /*event*/) override
        {
            if (goSlider) {
                goSlider->onUserValueChanged(value, highValue);
            }
        }
        //=============================================================================
        void
        paintEvent(QPaintEvent* /*event*/) override
        {
            QPainter painter(this);
            painter.setRenderHint(QPainter::Antialiasing);
            QColor trackColor(160, 160, 160);
            QColor activeColor(160, 160, 160);
            QColor thumbColor(240, 240, 240);
            QColor thumbBorder(100, 100, 100);
            if (isVertical) {
                int x = 10;
                painter.setPen(QPen(trackColor, 3));
                painter.drawLine(x, trackStart(), x, trackStart() + trackLength());
                int y1 = valueToPosition(value);
                if (isRange) {
                    int y2 = valueToPosition(highValue);
                    painter.setPen(QPen(activeColor, 3));
                    painter.drawLine(x, y1, x, y2);
                }
                painter.setPen(QPen(trackColor, 1));
                for (double tick : minorTicks) {
                    int y = valueToPosition(tick);
                    painter.drawLine(x + 4, y, x + 7, y);
                }
                painter.setPen(fontColor);
                for (size_t k = 0; k < majorTicks.size(); ++k) {
                    int y = valueToPosition(majorTicks[k]);
                    painter.setPen(QPen(trackColor, 1));
                    painter.drawLine(x + 4, y, x + 10, y);
                    if (k < majorTickLabels.size()) {
                        painter.setPen(fontColor);
                        painter.drawText(QRect(x + 12, y - 8, width() - x - 12, 16),
                            Qt::AlignLeft | Qt::AlignVCenter, wstringToQString(majorTickLabels[k]));
                    }
                }
                painter.setPen(QPen(thumbBorder, 1));
                painter.setBrush(thumbColor);
                painter.drawRect(QRect(x - 5, y1 - 4, 10, 8));
                if (isRange) {
                    int y2 = valueToPosition(highValue);
                    painter.drawRect(QRect(x - 5, y2 - 4, 10, 8));
                }
            } else {
                int y = 10;
                painter.setPen(QPen(trackColor, 3));
                painter.drawLine(trackStart(), y, trackStart() + trackLength(), y);
                int x1 = valueToPosition(value);
                if (isRange) {
                    int x2 = valueToPosition(highValue);
                    painter.setPen(QPen(activeColor, 3));
                    painter.drawLine(x1, y, x2, y);
                }
                painter.setPen(QPen(trackColor, 1));
                for (double tick : minorTicks) {
                    int x = valueToPosition(tick);
                    painter.drawLine(x, y + 4, x, y + 7);
                }
                for (size_t k = 0; k < majorTicks.size(); ++k) {
                    int x = valueToPosition(majorTicks[k]);
                    painter.setPen(QPen(trackColor, 1));
                    painter.drawLine(x, y + 4, x, y + 10);
                    if (k < majorTickLabels.size()) {
                        painter.setPen(fontColor);
                        painter.drawText(QRect(x - 25, y + 11, 50, 14), Qt::AlignHCenter,
                            wstringToQString(majorTickLabels[k]));
                    }
                }
                painter.setPen(QPen(thumbBorder, 1));
                painter.setBrush(thumbColor);
                painter.drawRect(QRect(x1 - 5, y - 4, 10, 8));
                if (isRange) {
                    int x2 = valueToPosition(highValue);
                    painter.drawRect(QRect(x2 - 5, y - 4, 10, 8));
                }
            }
        }
        //=============================================================================
    };
    //=============================================================================
    class UISliderBinding : public QtUIComponentBinding
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
            GOUISlider* goSlider = static_cast<GOUISlider*>(model);
            QUISliderWidget* slider = new QUISliderWidget(goSlider, parent);
            slider->isRange = goSlider->isRangeSlider();
            slider->setAttribute(Qt::WA_TranslucentBackground);
            widget = slider;
        }
        //=============================================================================
        // The drawable widget is larger than the 'track only' Position: the track sits
        // 10px in from the top (horizontal) and the tick labels extend past it. Declaring
        // the extra paint area as margins lets the base expand the geometry on every
        // placement, so a re-laid-out slider keeps rendering (not just the first one).
        QMargins
        widgetGeometryMargins() override
        {
            const bool vertical = model->stringCheck(
                GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
            if (vertical) {
                return QMargins(0, 8, 57, 8);
            }
            return QMargins(8, 20, 8, 21);
        }
        //=============================================================================
        void
        applyStateToWidget(bool /*isNewWidget*/) override
        {
            QUISliderWidget* slider = static_cast<QUISliderWidget*>(widget.data());
            if (!slider) {
                return;
            }
            GOUISlider* goSlider = static_cast<GOUISlider*>(model);
            if (model->hasChanged(GO_LIMITS_PROPERTY_NAME_STR)) {
                goSlider->updateAutoTicks();
            }
            std::vector<double> limits(
                model->findVectorDoubleProperty(GO_LIMITS_PROPERTY_NAME_STR));
            if (limits.size() == 2) {
                slider->minimum = limits[0];
                slider->maximum = limits[1];
            }
            if (goSlider->isRangeSlider()) {
                std::vector<double> values(
                    model->findVectorDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));
                if (values.size() == 2) {
                    slider->value = values[0];
                    slider->highValue = values[1];
                }
            } else {
                slider->value = model->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR);
            }
            slider->isVertical = model->stringCheck(
                GO_ORIENTATION_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_VERTICAL_STR);
            slider->minorTicks = model->findVectorDoubleProperty(GO_MINOR_TICKS_PROPERTY_NAME_STR);
            // Label a denser "nice" scale sized to the track, while MajorTicks stays coarse.
            std::vector<double> position(
                model->findVectorDoubleProperty(GO_POSITION_PROPERTY_NAME_STR));
            const double extent
                = position.size() >= 4 ? (slider->isVertical ? position[3] : position[2]) : 0.0;
            const bool majorAuto = model->stringCheck(
                GO_MAJOR_TICKS_MODE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_AUTO_STR);
            uiDisplayMajorTicks(slider->minimum, slider->maximum, majorAuto,
                model->findVectorDoubleProperty(GO_MAJOR_TICKS_PROPERTY_NAME_STR),
                model->findStringVectorProperty(GO_MAJOR_TICK_LABELS_PROPERTY_NAME_STR), extent,
                30.0, slider->majorTicks, slider->majorTickLabels);
            GOColorProperty* fontColor = static_cast<GOColorProperty*>(
                model->findProperty(GO_FONT_COLOR_PROPERTY_NAME_STR, false));
            if (fontColor && !fontColor->isNone()) {
                std::vector<double> rgb = fontColor->data();
                if (rgb.size() >= 3) {
                    slider->fontColor = QColor::fromRgb((int)(rgb[0] * 255.0 + 0.5),
                        (int)(rgb[1] * 255.0 + 0.5), (int)(rgb[2] * 255.0 + 0.5));
                }
            }
            slider->update();
        }
        //=============================================================================
    };
    //=============================================================================
} // namespace
//=============================================================================
UIComponentBinding*
createUISliderBinding(GOUIComponent* component)
{
    return new UISliderBinding(component);
}
//=============================================================================
} // namespace Nelson
//=============================================================================
