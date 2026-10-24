//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtCore/QCoreApplication>
#include <QtGui/QKeyEvent>
#include <QtWidgets/QWidget>
#include "UIControlBinding.hpp"
#include "GOUIControl.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static int
qtKeyFromName(const std::wstring& key);
//=============================================================================
bool
injectUIControlKey(GOUIControl* control, const std::wstring& key, GOUIControl** focused)
{
    int qtKey = qtKeyFromName(key);
    UIComponentBinding* binding = control ? control->getBinding() : nullptr;
    QWidget* widget
        = binding && binding->exists() ? static_cast<QWidget*>(binding->nativeWidget()) : nullptr;
    if (qtKey == 0 || widget == nullptr) {
        return false;
    }
    QString text = qtKey == Qt::Key_Space ? QStringLiteral(" ") : QString();
    Qt::KeyboardModifiers modifiers = qtKey == Qt::Key_Backtab ? Qt::ShiftModifier : Qt::NoModifier;
    QKeyEvent press(QEvent::KeyPress, qtKey, modifiers, text);
    QKeyEvent release(QEvent::KeyRelease, qtKey, modifiers, text);
    QCoreApplication::sendEvent(widget, &press);
    QCoreApplication::sendEvent(widget, &release);
    if (focused != nullptr) {
        *focused = UIControlWithFocus(control);
    }
    return true;
}
//=============================================================================
static int
qtKeyFromName(const std::wstring& key)
{
    if (key == L"space") {
        return Qt::Key_Space;
    }
    if (key == L"up") {
        return Qt::Key_Up;
    }
    if (key == L"down") {
        return Qt::Key_Down;
    }
    if (key == L"home") {
        return Qt::Key_Home;
    }
    if (key == L"end") {
        return Qt::Key_End;
    }
    if (key == L"tab") {
        return Qt::Key_Tab;
    }
    if (key == L"shift+tab") {
        return Qt::Key_Backtab;
    }
    return 0;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
