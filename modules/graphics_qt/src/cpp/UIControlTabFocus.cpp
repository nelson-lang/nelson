//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <QtWidgets/QWidget>
#include "UIControlBinding.hpp"
#include "UIControlTabOrder.hpp"
#include "GOUIControl.h"
//=============================================================================
namespace Nelson {
//=============================================================================
static QWidget*
uiControlWidget(GOUIControl* control);
//=============================================================================
bool
UIControlFocusNextPrevious(GOUIControl* control, bool next)
{
    GOUIControl* target = control ? nextUIControlInTabOrder(control, next) : nullptr;
    QWidget* widget = target ? uiControlWidget(target) : nullptr;
    if (widget == nullptr) {
        return false;
    }
    widget->setFocus(next ? Qt::TabFocusReason : Qt::BacktabFocusReason);
    return true;
}
//=============================================================================
GOUIControl*
UIControlWithFocus(GOUIControl* control)
{
    QWidget* widget = control ? uiControlWidget(control) : nullptr;
    QWidget* focused = widget ? widget->window()->focusWidget() : nullptr;
    if (focused == nullptr) {
        return nullptr;
    }
    for (GOUIControl* candidate : uiControlTabOrder(control, false)) {
        if (uiControlWidget(candidate) == focused) {
            return candidate;
        }
    }
    return nullptr;
}
//=============================================================================
static QWidget*
uiControlWidget(GOUIControl* control)
{
    UIComponentBinding* binding = control->getBinding();
    return binding && binding->exists() ? static_cast<QWidget*>(binding->nativeWidget()) : nullptr;
}
//=============================================================================
} // namespace Nelson
//=============================================================================
