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
#include <string>
#include <QtCore/QEvent>
#include <QtGui/QKeyEvent>
#include <QtWidgets/QWidget>
#include "GOUIWidgetBinding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
class GOUIControl;
//=============================================================================
// Factory registered via setUIControlBindingFactory: the Qt widget backend of
// the classic uicontrol (pushbutton, slider, listbox, ...).
UIComponentBinding*
createUIControlBinding(GOUIControl* control);
//=============================================================================
// Injector registered via setUIControlKeyInjector (__uicontrol_key_event__).
bool
injectUIControlKey(GOUIControl* control, const std::wstring& key, GOUIControl** focused);
//=============================================================================
// Tab/Shift+Tab from the widget of `control`: gives the focus to the next
// uicontrol of the figure (UIControlTabOrder); false when there is none.
bool
UIControlFocusNextPrevious(GOUIControl* control, bool next);
// The uicontrol of the figure of `control` whose widget has the focus.
GOUIControl*
UIControlWithFocus(GOUIControl* control);
//=============================================================================
// Space activates a focused button-like uicontrol, as a click does (auto-repeat
// of a held key does not activate it again).
inline bool
UIControlIsActivationKey(QKeyEvent* event)
{
    return event->key() == Qt::Key_Space && !event->isAutoRepeat();
}
//=============================================================================
// Event helpers called by the Q*Widget classes (they receive the QEvent, the
// model must not): push the matching callback on the CallbackQueue.
bool
UIControlHandleMouseEvent(GOUIControl* control, QEvent* event,
    const std::wstring& callbackPropertyStr, const std::wstring& eventType);
bool
UIControlHandleKeyEvent(GOUIControl* control, QEvent* event,
    const std::wstring& callbackPropertyStr, const std::wstring& eventType);
//=============================================================================
// Style-dispatched read-back of the widget state into the model properties
// (Value/String), called by the widgets before control->clicked().
void
UIControlSyncValueFromWidget(GOUIControl* control, QWidget* widget);
//=============================================================================
} // namespace Nelson
//=============================================================================
