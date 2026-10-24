//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "QSliderWidget.h"
#include "UIControlBinding.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QSliderWidget::QSliderWidget(bool vertical, GOUIControl* gouiParent, QWidget* parent)
    : goParent(gouiParent)
{
    setObjectName("QSliderWidget");
    if (vertical) {
        setOrientation(Qt::Vertical);
    } else {
        setOrientation(Qt::Horizontal);
    }
    setParent(parent);
    setTracking(false);
    setFocusPolicy(Qt::ClickFocus);
    connect(this, SIGNAL(valueChanged(int)), this, SLOT(onValueChanged(int)));
}
//=============================================================================
void
QSliderWidget::keyPressEvent(QKeyEvent* event)
{
    bool isIncreaseKey = (event->key() == Qt::Key_Up || event->key() == Qt::Key_Right);
    bool isDecreaseKey = (event->key() == Qt::Key_Down || event->key() == Qt::Key_Left);
    if (isIncreaseKey || isDecreaseKey) {
        std::vector<double> steps
            = goParent->findVectorDoubleProperty(GO_SLIDER_STEP_PROPERTY_NAME_STR);
        double min(goParent->findScalarDoubleProperty(GO_MIN_PROPERTY_NAME_STR));
        double max(goParent->findScalarDoubleProperty(GO_MAX_PROPERTY_NAME_STR));
        double value(goParent->findScalarDoubleProperty(GO_VALUE_PROPERTY_NAME_STR));

        int stepValue = static_cast<int>(std::round((max - min) * steps[0]));
        int newValue;

        if (isIncreaseKey) {
            newValue = static_cast<int>(std::round(((value + stepValue) - min) / steps[0]));
        } else {
            newValue = static_cast<int>(std::round(((value - stepValue) - min) / steps[0]));
        }
        setValue(newValue);
    }
    UIControlHandleKeyEvent(goParent, event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    event->accept();
    QSlider::keyPressEvent(event);
}
//=============================================================================
void
QSliderWidget::keyReleaseEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    event->accept();
    QSlider::keyReleaseEvent(event);
}
//=============================================================================
bool
QSliderWidget::focusNextPrevChild(bool next)
{
    // Tab and Shift+Tab follow the uicontrol order of the figure
    return UIControlFocusNextPrevious(goParent, next) || QSlider::focusNextPrevChild(next);
}
//=============================================================================
void
QSliderWidget::mousePressEvent(QMouseEvent* event)
{
    UIControlHandleMouseEvent(goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
    if (goParent && goParent->isEnableInactive()) {
        event->accept();
        return;
    }
    event->accept();
    QSlider::mousePressEvent(event);
}
//=============================================================================
void
QSliderWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        event->accept();
        return;
    }
    event->accept();
    QSlider::mouseReleaseEvent(event);
}
//=============================================================================
void
QSliderWidget::onValueChanged(int position)
{
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    UIControlSyncValueFromWidget(goParent, this);
    goParent->clicked();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
