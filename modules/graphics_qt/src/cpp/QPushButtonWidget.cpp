//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtGui/QPixmap>
#include "QPushButtonWidget.h"
#include "UIControlBinding.hpp"
#include "QStringConverter.hpp"
#include "GOPropertyNames.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
QPushButtonWidget::QPushButtonWidget(GOUIControl* gouiParent, QWidget* parent)
    : goParent(gouiParent)
{
    setObjectName("QPushButtonWidget");
    this->setParent(parent);
    setAutoRepeat(false);
    setFlat(false);
    setFocusPolicy(Qt::ClickFocus);
    withImage = false;
}
//=============================================================================
void
QPushButtonWidget::setText(const std::wstring& txt)
{
    text = txt;
    if (!withImage) {
        QPushButton::setText(wstringToQString(txt));
        return;
    }
    if (!textLabel) {
        textLabel = new QLabel(this);
        textLabel->setAlignment(Qt::AlignCenter);
        textLabel->setStyleSheet("background-color: rgba(0, 0, 0, 0);");
    }
    textLabel->setText(wstringToQString(txt));

    if (!layout) {
        layout = new QVBoxLayout(this);
        layout->addWidget(textLabel);
        layout->setAlignment(textLabel, Qt::AlignCenter);
        setLayout(layout);
    }
}
//=============================================================================
void
QPushButtonWidget::setImage(const QImage& img)
{
    QPixmap pixMap = QPixmap::fromImage(img);
    setIcon(QIcon(pixMap));
    setIconSize(pixMap.size());
    withImage = !(pixMap.size().isEmpty());

    if (!withImage) {
        if (textLabel) {
            delete textLabel;
            textLabel = nullptr;
        }
        if (layout) {
            delete layout;
            layout = nullptr;
        }
        QPushButton::setText(wstringToQString(text));
        return;
    }

    QPushButton::setText("");
    if (!textLabel) {
        textLabel = new QLabel(this);
        textLabel->setText(wstringToQString(text));
        textLabel->setAlignment(Qt::AlignCenter);
        textLabel->setStyleSheet("background-color: rgba(0, 0, 0, 0);");
    }

    if (!layout) {
        layout = new QVBoxLayout(this);
        layout->addWidget(textLabel);
        layout->setAlignment(textLabel, Qt::AlignCenter);
        setLayout(layout);
    }
}
//=============================================================================
void
QPushButtonWidget::keyPressEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_PRESS_FCN_PROPERTY_NAME_STR, L"KeyPress");
    QPushButton::keyPressEvent(event);
}
//=============================================================================
void
QPushButtonWidget::keyReleaseEvent(QKeyEvent* event)
{
    UIControlHandleKeyEvent(goParent, event, GO_KEY_RELEASE_FCN_PROPERTY_NAME_STR, L"KeyRelease");
    QPushButton::keyReleaseEvent(event);
    // Space presses the button like a click does
    if (UIControlIsActivationKey(event) && goParent && !goParent->isEnableInactive()) {
        UIControlSyncValueFromWidget(goParent, this);
        goParent->clicked();
    }
}
//=============================================================================
bool
QPushButtonWidget::focusNextPrevChild(bool next)
{
    // Tab and Shift+Tab follow the uicontrol order of the figure
    return UIControlFocusNextPrevious(goParent, next) || QPushButton::focusNextPrevChild(next);
}
//=============================================================================
void
QPushButtonWidget::mousePressEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        // Looks normal but does not press: still fire ButtonDownFcn.
        UIControlHandleMouseEvent(
            goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
        return;
    }
    QPushButton::mousePressEvent(event);
    setDown(true);
    UIControlHandleMouseEvent(goParent, event, GO_BUTTON_DOWN_FCN_PROPERTY_NAME_STR, L"ButtonDown");
}
//=============================================================================
void
QPushButtonWidget::mouseReleaseEvent(QMouseEvent* event)
{
    if (goParent && goParent->isEnableInactive()) {
        return;
    }
    QPushButton::mouseReleaseEvent(event);
    setDown(false);
    UIControlSyncValueFromWidget(goParent, this);
    goParent->clicked();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
