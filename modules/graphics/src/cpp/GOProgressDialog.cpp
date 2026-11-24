//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <atomic>
#include "GOProgressDialog.hpp"
#include "GOPropertyNames.hpp"
#include "GOPropertyValues.hpp"
#include "GOScalarDoubleProperty.hpp"
#include "GOStringProperty.hpp"
#include "GOStringOnOffProperty.hpp"
#include "GOGObjectsProperty.hpp"
#include "GOArrayOfProperty.hpp"
#include "GuiDialogProvider.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
static std::atomic<long long> progressDialogSequence { 0 };
//=============================================================================
GOProgressDialog::GOProgressDialog() : m_id(++progressDialogSequence)
{
    constructProperties();
    setupDefaults();
}
//=============================================================================
GOProgressDialog::~GOProgressDialog()
{
    if (!m_hidden) {
        publish(false);
    }
}
//=============================================================================
std::wstring
GOProgressDialog::getType()
{
    return GO_PROPERTY_VALUE_PROGRESSDIALOG_STR;
}
//=============================================================================
void
GOProgressDialog::paintMe(RenderInterface& /*gc*/)
{
    // The progress dialog is served through the provider, never drawn on a canvas.
}
//=============================================================================
void
GOProgressDialog::constructProperties()
{
    registerProperty(new GOOnOffProperty, GO_BEING_DELETED_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_CHILDREN_PROPERTY_NAME_STR, false);
    registerProperty(new GOGObjectsProperty, GO_PARENT_PROPERTY_NAME_STR);
    registerProperty(new GOStringProperty, GO_TYPE_PROPERTY_NAME_STR, false);
    registerProperty(new GOStringProperty, GO_TAG_PROPERTY_NAME_STR);
    registerProperty(new GOArrayOfProperty, GO_USER_DATA_PROPERTY_NAME_STR);
    registerProperty(new GOHandleVisibilityProperty, GO_HANDLE_VISIBILITY_PROPERTY_NAME_STR);
    registerProperty(new GOOnOffProperty, GO_VISIBLE_PROPERTY_NAME_STR);
    registerProperty(new GOScalarProperty, L"Value");
    registerProperty(new GOStringProperty, L"Message");
    registerProperty(new GOStringProperty, L"Title");
    registerProperty(new GOOnOffProperty, L"Indeterminate");
    registerProperty(new GOStringProperty, L"Icon");
    registerProperty(new GOOnOffProperty, L"ShowPercentage");
    registerProperty(new GOOnOffProperty, L"Cancelable");
    registerProperty(new GOStringProperty, L"CancelText");
    registerProperty(new GOStringProperty, L"Interpreter");
    registerProperty(new GOOnOffProperty, L"CancelRequested");
    sortProperties();
}
//=============================================================================
void
GOProgressDialog::setupDefaults()
{
    setStringDefault(GO_TYPE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_PROGRESSDIALOG_STR);
    setRestrictedStringDefault(GO_BEING_DELETED_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    setScalarDoubleDefault(L"Value", 0.0);
    setStringDefault(L"Message", L"");
    setStringDefault(L"Title", L"");
    setRestrictedStringDefault(L"Indeterminate", GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(L"Icon", L"");
    setRestrictedStringDefault(L"ShowPercentage", GO_PROPERTY_VALUE_OFF_STR);
    setRestrictedStringDefault(L"Cancelable", GO_PROPERTY_VALUE_OFF_STR);
    setStringDefault(L"CancelText", L"Cancel");
    setStringDefault(L"Interpreter", L"none");
    setRestrictedStringDefault(L"CancelRequested", GO_PROPERTY_VALUE_OFF_STR);
}
//=============================================================================
void
GOProgressDialog::publish(bool visible)
{
    if (!hasGuiDialogProvider()) {
        return;
    }
    const bool indeterminate = stringCheck(L"Indeterminate", GO_PROPERTY_VALUE_ON_STR);
    double value = findScalarDoubleProperty(L"Value");
    int percent = static_cast<int>(value * 100.0 + 0.5);
    if (percent < 0) {
        percent = 0;
    }
    if (percent > 100) {
        percent = 100;
    }
    GuiDialogRequest request;
    request.kind = L"progress";
    request.progressId = m_id;
    request.progressVisible = visible;
    request.title = findStringProperty(L"Title");
    request.message = findStringProperty(L"Message");
    request.progressValue = percent;
    request.progressMinimum = 0;
    request.progressMaximum = indeterminate ? 0 : 100;
    request.progressIndeterminate = indeterminate;
    GuiDialogResponse response;
    runGuiDialog(request, response);
}
//=============================================================================
void
GOProgressDialog::updateState()
{
    const bool visible = stringCheck(GO_VISIBLE_PROPERTY_NAME_STR, GO_PROPERTY_VALUE_ON_STR);
    m_hidden = !visible;
    publish(visible);
    clearAllChanged();
}
//=============================================================================
} // namespace Nelson
//=============================================================================
